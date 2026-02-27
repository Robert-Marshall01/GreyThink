package com.greylegacy.batch.springbatch;

import org.quartz.Job;
import org.quartz.JobDataMap;
import org.quartz.JobExecutionContext;
import org.quartz.JobExecutionException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.core.JobExecution;
import org.springframework.batch.core.JobParameters;
import org.springframework.batch.core.JobParametersBuilder;
import org.springframework.batch.core.launch.JobLauncher;
import org.springframework.batch.core.repository.JobRepository;
import org.springframework.context.ApplicationContext;

import java.util.Date;

/**
 * Quartz {@link Job} that bridges Quartz scheduling to Spring Batch
 * job execution.
 *
 * <h3>Problem</h3>
 * <p>Grey Legacy uses Quartz for scheduling (via {@code applicationContext-batch.xml})
 * and Spring Batch for chunk-oriented processing (via
 * {@code applicationContext-spring-batch.xml}). These two frameworks
 * don't natively integrate — Quartz fires a {@code Job.execute()}, but
 * Spring Batch needs a {@code JobLauncher.run(jobName, params)}.</p>
 *
 * <h3>Solution</h3>
 * <p>This class acts as a bridge: Quartz invokes this as a standard
 * Quartz job, and it in turn resolves the Spring Batch job from the
 * {@link ApplicationContext} and launches it via the
 * {@link JobLauncher}.</p>
 *
 * <h3>Configuration</h3>
 * <p>The Quartz JobDetail must set the following data map entries:</p>
 * <ul>
 *   <li>{@code springBatchJobName} — the bean name of the Spring Batch
 *       {@code org.springframework.batch.core.Job} to launch</li>
 * </ul>
 *
 * <p>The {@link ApplicationContext} is injected via Quartz's
 * {@code applicationContextSchedulerContextKey} mechanism (configured
 * in the {@code SchedulerFactoryBean}).</p>
 *
 * <h3>Job Parameters</h3>
 * <p>Each launch receives a unique {@code run.timestamp} parameter to
 * ensure Spring Batch treats it as a new instance. Additional parameters
 * can be passed via the Quartz {@link JobDataMap}.</p>
 *
 * @author Grey Legacy Batch Team
 * @since 1.0.0
 */
public class SpringBatchQuartzJobLauncher implements Job {

    private static final Logger log = LoggerFactory.getLogger(SpringBatchQuartzJobLauncher.class);

    /** Key in the Quartz JobDataMap that specifies the Spring Batch job bean name. */
    public static final String BATCH_JOB_NAME_KEY = "springBatchJobName";

    /** Key used to store the ApplicationContext in the Quartz SchedulerContext. */
    public static final String APP_CONTEXT_KEY = "applicationContext";

    // -------------------------------------------------------------------------
    // Quartz Job implementation
    // -------------------------------------------------------------------------

    /**
     * {@inheritDoc}
     *
     * Resolves the Spring Batch job and launcher from the application context,
     * builds unique job parameters, and launches the batch job.
     *
     * @param context the Quartz execution context
     * @throws JobExecutionException if the Spring Batch job fails to launch
     */
    @Override
    public void execute(JobExecutionContext context) throws JobExecutionException {
        JobDataMap dataMap = context.getMergedJobDataMap();
        String batchJobName = dataMap.getString(BATCH_JOB_NAME_KEY);

        if (batchJobName == null || batchJobName.trim().isEmpty()) {
            throw new JobExecutionException(
                    "Quartz JobDataMap must contain '" + BATCH_JOB_NAME_KEY + "'");
        }

        log.info("Quartz trigger fired for Spring Batch job [{}]", batchJobName);

        // Retrieve the Spring ApplicationContext from Quartz's SchedulerContext
        ApplicationContext appContext;
        try {
            appContext = (ApplicationContext) context.getScheduler()
                    .getContext().get(APP_CONTEXT_KEY);
        } catch (Exception e) {
            throw new JobExecutionException(
                    "Could not retrieve ApplicationContext from Quartz SchedulerContext", e);
        }

        if (appContext == null) {
            throw new JobExecutionException(
                    "ApplicationContext not found in Quartz SchedulerContext. "
                    + "Ensure SchedulerFactoryBean sets "
                    + "applicationContextSchedulerContextKey='" + APP_CONTEXT_KEY + "'.");
        }

        // Resolve Spring Batch beans
        org.springframework.batch.core.Job batchJob;
        JobLauncher jobLauncher;

        try {
            batchJob = appContext.getBean(batchJobName,
                    org.springframework.batch.core.Job.class);
            jobLauncher = appContext.getBean("jobLauncher", JobLauncher.class);
        } catch (Exception e) {
            throw new JobExecutionException(
                    "Failed to resolve Spring Batch beans for job [" + batchJobName + "]", e);
        }

        // Build job parameters — unique timestamp ensures a new job instance
        JobParametersBuilder paramsBuilder = new JobParametersBuilder();
        paramsBuilder.addDate("run.timestamp", new Date());
        paramsBuilder.addString("triggered.by", "quartz");

        // Forward any additional Quartz data map entries as job parameters
        for (String key : dataMap.getKeys()) {
            if (!BATCH_JOB_NAME_KEY.equals(key) && !APP_CONTEXT_KEY.equals(key)) {
                Object value = dataMap.get(key);
                if (value instanceof String) {
                    paramsBuilder.addString(key, (String) value);
                } else if (value instanceof Long) {
                    paramsBuilder.addLong(key, (Long) value);
                } else if (value instanceof Double) {
                    paramsBuilder.addDouble(key, (Double) value);
                }
            }
        }

        JobParameters jobParameters = paramsBuilder.toJobParameters();

        // Launch the Spring Batch job
        try {
            log.info("Launching Spring Batch job [{}] with parameters: {}",
                    batchJobName, jobParameters);

            JobExecution execution = jobLauncher.run(batchJob, jobParameters);

            log.info("Spring Batch job [{}] completed with status: {}",
                    batchJobName, execution.getStatus());

            if (execution.getStatus().isUnsuccessful()) {
                log.error("Spring Batch job [{}] finished with status {} — "
                        + "check Spring Batch tables for details",
                        batchJobName, execution.getStatus());

                // Do NOT throw here — the job ran but failed internally.
                // Throwing would cause Quartz to fire its retry logic,
                // but the Spring Batch job repository already tracks the failure
                // and supports restarting via the job launcher.
            }

        } catch (Exception e) {
            log.error("Failed to launch Spring Batch job [{}]", batchJobName, e);
            throw new JobExecutionException(
                    "Failed to launch Spring Batch job [" + batchJobName + "]", e);
        }
    }
}
