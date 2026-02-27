package com.greylegacy.web.jmx;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Implementation of {@link ClaimProcessingMBean}.
 * <p>
 * All counters use atomic variables to guarantee thread-safe reads and writes
 * from concurrent claim-processing threads and JMX polling threads.
 * </p>
 *
 * @since 2.1.0
 */
public class ClaimProcessingMBeanImpl implements ClaimProcessingMBean {

    private static final Logger LOG = LoggerFactory.getLogger(ClaimProcessingMBeanImpl.class);

    private final AtomicLong totalClaimsProcessed = new AtomicLong(0);
    private final AtomicLong totalProcessingTimeMs = new AtomicLong(0);
    private final AtomicInteger openClaimsCount = new AtomicInteger(0);
    private final AtomicInteger pendingPaymentsCount = new AtomicInteger(0);
    private final AtomicLong claimsProcessedLastHour = new AtomicLong(0);
    private final AtomicLong fraudDetectedCount = new AtomicLong(0);

    private volatile String lastProcessedClaimNumber = "N/A";
    private volatile String lastProcessedTimestamp = "N/A";

    // ── Internal recording methods ──────────────────────────────────────

    /**
     * Records a single processed claim. Called internally by the service layer.
     *
     * @param processingTimeMs elapsed processing time in milliseconds
     * @param claimNumber      the claim number that was processed
     */
    public void recordClaimProcessed(long processingTimeMs, String claimNumber) {
        totalClaimsProcessed.incrementAndGet();
        totalProcessingTimeMs.addAndGet(processingTimeMs);
        claimsProcessedLastHour.incrementAndGet();
        lastProcessedClaimNumber = claimNumber;
        lastProcessedTimestamp = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSZ").format(new Date());
        LOG.debug("Recorded claim processing: claimNumber={}, timeMs={}", claimNumber, processingTimeMs);
    }

    /**
     * Increments the fraud-detected counter. Called when the fraud scoring
     * engine flags a claim.
     */
    public void incrementFraudDetected() {
        long count = fraudDetectedCount.incrementAndGet();
        LOG.info("Fraud detected count incremented to {}", count);
    }

    // ── MBean attribute implementations ─────────────────────────────────

    @Override
    public long getTotalClaimsProcessed() {
        return totalClaimsProcessed.get();
    }

    @Override
    public long getAverageProcessingTimeMs() {
        long total = totalClaimsProcessed.get();
        return total == 0 ? 0 : totalProcessingTimeMs.get() / total;
    }

    @Override
    public int getOpenClaimsCount() {
        return openClaimsCount.get();
    }

    @Override
    public int getPendingPaymentsCount() {
        return pendingPaymentsCount.get();
    }

    @Override
    public long getClaimsProcessedLastHour() {
        return claimsProcessedLastHour.get();
    }

    @Override
    public long getFraudDetectedCount() {
        return fraudDetectedCount.get();
    }

    @Override
    public String getLastProcessedClaimNumber() {
        return lastProcessedClaimNumber;
    }

    @Override
    public String getLastProcessedTimestamp() {
        return lastProcessedTimestamp;
    }

    // ── MBean operations ────────────────────────────────────────────────

    @Override
    public void resetCounters() {
        LOG.info("Resetting all claim-processing JMX counters");
        totalClaimsProcessed.set(0);
        totalProcessingTimeMs.set(0);
        openClaimsCount.set(0);
        pendingPaymentsCount.set(0);
        claimsProcessedLastHour.set(0);
        fraudDetectedCount.set(0);
        lastProcessedClaimNumber = "N/A";
        lastProcessedTimestamp = "N/A";
    }

    @Override
    public void forceClaimAging() {
        LOG.warn("forceClaimAging invoked via JMX — would trigger ClaimAgingJob");
        // In production this delegates to the Spring Batch launcher:
        // batchJobLauncher.run(claimAgingJob, new JobParameters());
    }

    @Override
    public void triggerFraudRescan() {
        LOG.warn("triggerFraudRescan invoked via JMX — would trigger FraudScoringJob");
        // In production this delegates to the Spring Batch launcher:
        // batchJobLauncher.run(fraudScoringJob, new JobParameters());
    }

    // ── Mutators used by service layer ──────────────────────────────────

    public void setOpenClaimsCount(int count) {
        openClaimsCount.set(count);
    }

    public void setPendingPaymentsCount(int count) {
        pendingPaymentsCount.set(count);
    }
}
