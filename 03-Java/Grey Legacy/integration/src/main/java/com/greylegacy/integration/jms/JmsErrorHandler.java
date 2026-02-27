package com.greylegacy.integration.jms;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.MDC;
import org.springframework.util.ErrorHandler;

import java.util.concurrent.atomic.AtomicLong;

/**
 * Custom JMS {@link ErrorHandler} that handles uncaught exceptions
 * from JMS listener containers.
 *
 * <p>In a production system, this handler would integrate with an
 * alerting system (PagerDuty, SNMP traps, email) to notify operations
 * teams of persistent message processing failures.</p>
 *
 * <p>This implementation:</p>
 * <ul>
 *   <li>Logs the full exception with MDC correlation context</li>
 *   <li>Tracks cumulative error counts for monitoring</li>
 *   <li>Classifies errors as transient (retryable) or permanent</li>
 *   <li>Suppresses log flooding for repeated identical errors</li>
 * </ul>
 *
 * @author Grey Legacy Integration Team
 * @since 1.0
 */
public class JmsErrorHandler implements ErrorHandler {

    private static final Logger LOG = LoggerFactory.getLogger(JmsErrorHandler.class);

    /** Suppress duplicate log entries if the same error repeats within this window. */
    private static final long DUPLICATE_SUPPRESSION_MS = 30_000L;

    private final AtomicLong totalErrorCount = new AtomicLong(0);
    private final AtomicLong transientErrorCount = new AtomicLong(0);
    private final AtomicLong permanentErrorCount = new AtomicLong(0);

    /** Last error message hash — used for deduplication. */
    private volatile int lastErrorHash = 0;
    private volatile long lastErrorTimestamp = 0;

    @Override
    public void handleError(Throwable t) {
        long errorNum = totalErrorCount.incrementAndGet();

        // Classify the error
        boolean isTransient = isTransientError(t);
        if (isTransient) {
            transientErrorCount.incrementAndGet();
        } else {
            permanentErrorCount.incrementAndGet();
        }

        // Suppress duplicate log entries
        int currentHash = t.getMessage() != null ? t.getMessage().hashCode() : 0;
        long now = System.currentTimeMillis();
        if (currentHash == lastErrorHash
                && (now - lastErrorTimestamp) < DUPLICATE_SUPPRESSION_MS) {
            LOG.debug("Suppressing duplicate JMS error log (error #{}, hash={})",
                    errorNum, currentHash);
            return;
        }
        lastErrorHash = currentHash;
        lastErrorTimestamp = now;

        // Log with context
        String correlationId = MDC.get("correlationId");
        if (isTransient) {
            LOG.warn("Transient JMS processing error #{} [correlationId={}]: {}. "
                    + "Message will be redelivered by the broker.",
                    errorNum, correlationId, t.getMessage());
        } else {
            LOG.error("Permanent JMS processing error #{} [correlationId={}]: {}. "
                    + "Message may be routed to DLQ after max redelivery attempts.",
                    errorNum, correlationId, t.getMessage(), t);
        }
    }

    /**
     * Determines whether an error is transient (retryable) or permanent.
     *
     * <p>Transient errors include connection timeouts, temporary database
     * locks, and resource exhaustion.  Permanent errors include null pointer
     * exceptions, class cast errors, and data validation failures.</p>
     */
    private boolean isTransientError(Throwable t) {
        if (t == null) return false;

        String className = t.getClass().getName();
        String message = t.getMessage() != null ? t.getMessage().toLowerCase() : "";

        // Transient patterns
        if (className.contains("Timeout")
                || className.contains("ConnectException")
                || className.contains("TransientDataAccessException")
                || message.contains("connection reset")
                || message.contains("lock timeout")
                || message.contains("deadlock")
                || message.contains("resource temporarily unavailable")) {
            return true;
        }

        // Check wrapped causes
        if (t.getCause() != null && t.getCause() != t) {
            return isTransientError(t.getCause());
        }

        return false;
    }

    // -------------------------------------------------------------------------
    // Monitoring accessors (for JMX / health checks)
    // -------------------------------------------------------------------------

    public long getTotalErrorCount()     { return totalErrorCount.get(); }
    public long getTransientErrorCount() { return transientErrorCount.get(); }
    public long getPermanentErrorCount() { return permanentErrorCount.get(); }

    /** Resets all counters (typically called after an alerting cycle). */
    public void resetCounters() {
        totalErrorCount.set(0);
        transientErrorCount.set(0);
        permanentErrorCount.set(0);
    }
}
