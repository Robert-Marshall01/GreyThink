package com.greylegacy.batch.springbatch;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.core.BatchStatus;
import org.springframework.batch.core.JobExecution;
import org.springframework.batch.core.JobExecutionListener;
import org.springframework.batch.core.StepExecution;

import java.text.SimpleDateFormat;
import java.util.Collection;

/**
 * Spring Batch {@link JobExecutionListener} that logs job lifecycle events
 * including start/end times, duration, status, and item read/write/skip
 * counts from each step execution.
 *
 * <p>On job failure, a notification placeholder is logged (intended to be
 * wired to an email service in production).</p>
 *
 * @author Grey Legacy Batch Team
 * @since 1.0.0
 */
public class BatchJobCompletionListener implements JobExecutionListener {

    private static final Logger log = LoggerFactory.getLogger(BatchJobCompletionListener.class);
    private static final SimpleDateFormat TS_FORMAT = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");

    // -------------------------------------------------------------------------
    // JobExecutionListener implementation
    // -------------------------------------------------------------------------

    /** {@inheritDoc} */
    @Override
    public void beforeJob(JobExecution jobExecution) {
        log.info("===================================================================");
        log.info("JOB STARTING: {}", jobExecution.getJobInstance().getJobName());
        log.info("  Start Time : {}", TS_FORMAT.format(jobExecution.getStartTime()));
        log.info("  Parameters : {}", jobExecution.getJobParameters());
        log.info("===================================================================");
    }

    /** {@inheritDoc} */
    @Override
    public void afterJob(JobExecution jobExecution) {
        String jobName = jobExecution.getJobInstance().getJobName();
        long durationMs = jobExecution.getEndTime().getTime()
                - jobExecution.getStartTime().getTime();

        log.info("===================================================================");
        log.info("JOB FINISHED: {}", jobName);
        log.info("  End Time   : {}", TS_FORMAT.format(jobExecution.getEndTime()));
        log.info("  Duration   : {} ms", durationMs);
        log.info("  Status     : {}", jobExecution.getStatus());

        // Log step-level metrics
        Collection<StepExecution> stepExecutions = jobExecution.getStepExecutions();
        for (StepExecution step : stepExecutions) {
            log.info("  Step [{}]: read={}, written={}, filtered={}, skipped={}",
                    step.getStepName(),
                    step.getReadCount(),
                    step.getWriteCount(),
                    step.getFilterCount(),
                    step.getSkipCount());
        }

        log.info("===================================================================");

        // Failure notification placeholder
        if (jobExecution.getStatus() == BatchStatus.FAILED) {
            log.error("*** NOTIFICATION: Job [{}] FAILED. ***", jobName);
            log.error("  Failure exceptions:");
            for (Throwable t : jobExecution.getAllFailureExceptions()) {
                log.error("    - {}", t.getMessage());
            }
            sendFailureNotification(jobName, jobExecution);
        }
    }

    // -------------------------------------------------------------------------
    // Notification placeholder
    // -------------------------------------------------------------------------

    /**
     * Placeholder for sending a failure notification (e.g. email).
     * In production this would be wired to an SMTP service or message queue.
     */
    private void sendFailureNotification(String jobName, JobExecution jobExecution) {
        log.warn("EMAIL PLACEHOLDER: Would send failure alert for job [{}] to ops team. " +
                 "Status={}, ExitStatus={}",
                jobName,
                jobExecution.getStatus(),
                jobExecution.getExitStatus().getExitCode());
    }
}
