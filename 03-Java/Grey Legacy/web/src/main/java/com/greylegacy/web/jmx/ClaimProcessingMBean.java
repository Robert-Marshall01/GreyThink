package com.greylegacy.web.jmx;

/**
 * JMX MBean interface for monitoring claim processing metrics.
 * <p>
 * Exposes real-time statistics about claim throughput, processing times,
 * and fraud detection counts to JMX-capable management consoles.
 * </p>
 *
 * @since 2.1.0
 */
public interface ClaimProcessingMBean {

    // ── Attributes ──────────────────────────────────────────────────────

    /** Total number of claims processed since the last counter reset. */
    long getTotalClaimsProcessed();

    /** Rolling average processing time in milliseconds. */
    long getAverageProcessingTimeMs();

    /** Current count of claims in OPEN status. */
    int getOpenClaimsCount();

    /** Number of approved payments not yet disbursed. */
    int getPendingPaymentsCount();

    /** Number of claims processed in the last 60 minutes. */
    long getClaimsProcessedLastHour();

    /** Number of claims flagged by fraud detection. */
    long getFraudDetectedCount();

    /** Claim number of the most recently processed claim. */
    String getLastProcessedClaimNumber();

    /** ISO-8601 timestamp of the last processed claim. */
    String getLastProcessedTimestamp();

    // ── Operations ──────────────────────────────────────────────────────

    /** Resets all counters and metrics to zero. */
    void resetCounters();

    /** Forces the claim-aging batch job to execute immediately. */
    void forceClaimAging();

    /** Triggers a full fraud-rescoring pass across open claims. */
    void triggerFraudRescan();
}
