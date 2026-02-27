package com.greylegacy.web.jmx;

/**
 * JMX MBean interface for monitoring the C3P0 database connection pool.
 * <p>
 * Exposes real-time pool metrics so operations teams can detect connection
 * exhaustion, slow checkouts, and sizing issues through standard JMX tooling.
 * </p>
 *
 * @since 2.1.0
 */
public interface ConnectionPoolMonitorMBean {

    // ── Attributes ──────────────────────────────────────────────────────

    /** Number of connections currently checked out and in use. */
    int getActiveConnections();

    /** Number of connections sitting idle in the pool. */
    int getIdleConnections();

    /** Total connections managed by the pool (active + idle). */
    int getTotalConnections();

    /** Maximum pool size as configured. */
    int getMaxPoolSize();

    /** Minimum pool size as configured. */
    int getMinPoolSize();

    /** Average time (ms) threads waited to obtain a connection. */
    long getConnectionWaitTimeMs();

    /** Cumulative count of failed connection checkout attempts. */
    long getFailedCheckoutCount();

    /** Number of times the pool was fully exhausted. */
    long getPoolExhaustedCount();

    // ── Operations ──────────────────────────────────────────────────────

    /** Resets pool statistics counters. */
    void resetPoolStatistics();

    /** Performs a quick connection health check and returns a status message. */
    String testConnection();
}
