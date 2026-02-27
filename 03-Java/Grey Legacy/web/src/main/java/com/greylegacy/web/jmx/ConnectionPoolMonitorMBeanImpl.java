package com.greylegacy.web.jmx;

import java.sql.Connection;
import java.sql.SQLException;
import java.util.concurrent.atomic.AtomicLong;

import com.mchange.v2.c3p0.ComboPooledDataSource;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Implementation of {@link ConnectionPoolMonitorMBean} backed by a C3P0
 * {@link ComboPooledDataSource}.
 * <p>
 * Reads live pool metrics directly from the C3P0 management API and
 * exposes them via JMX for monitoring dashboards and alerting.
 * </p>
 *
 * @since 2.1.0
 */
public class ConnectionPoolMonitorMBeanImpl implements ConnectionPoolMonitorMBean {

    private static final Logger LOG = LoggerFactory.getLogger(ConnectionPoolMonitorMBeanImpl.class);

    private ComboPooledDataSource dataSource;

    private final AtomicLong failedCheckoutCount = new AtomicLong(0);
    private final AtomicLong poolExhaustedCount = new AtomicLong(0);
    private final AtomicLong totalWaitTimeMs = new AtomicLong(0);
    private final AtomicLong checkoutCount = new AtomicLong(0);

    public void setDataSource(ComboPooledDataSource dataSource) {
        this.dataSource = dataSource;
    }

    // ── Attributes ──────────────────────────────────────────────────────

    @Override
    public int getActiveConnections() {
        try {
            return dataSource.getNumBusyConnectionsDefaultUser();
        } catch (SQLException e) {
            LOG.error("Failed to read active connections from C3P0", e);
            return -1;
        }
    }

    @Override
    public int getIdleConnections() {
        try {
            return dataSource.getNumIdleConnectionsDefaultUser();
        } catch (SQLException e) {
            LOG.error("Failed to read idle connections from C3P0", e);
            return -1;
        }
    }

    @Override
    public int getTotalConnections() {
        try {
            return dataSource.getNumConnectionsDefaultUser();
        } catch (SQLException e) {
            LOG.error("Failed to read total connections from C3P0", e);
            return -1;
        }
    }

    @Override
    public int getMaxPoolSize() {
        return dataSource.getMaxPoolSize();
    }

    @Override
    public int getMinPoolSize() {
        return dataSource.getMinPoolSize();
    }

    @Override
    public long getConnectionWaitTimeMs() {
        long count = checkoutCount.get();
        return count == 0 ? 0 : totalWaitTimeMs.get() / count;
    }

    @Override
    public long getFailedCheckoutCount() {
        return failedCheckoutCount.get();
    }

    @Override
    public long getPoolExhaustedCount() {
        return poolExhaustedCount.get();
    }

    // ── Operations ──────────────────────────────────────────────────────

    @Override
    public void resetPoolStatistics() {
        LOG.info("Resetting connection pool statistics via JMX");
        failedCheckoutCount.set(0);
        poolExhaustedCount.set(0);
        totalWaitTimeMs.set(0);
        checkoutCount.set(0);
    }

    @Override
    public String testConnection() {
        LOG.info("Testing database connection via JMX");
        Connection conn = null;
        try {
            long start = System.currentTimeMillis();
            conn = dataSource.getConnection();
            long elapsed = System.currentTimeMillis() - start;
            String msg = "Connection OK — checkout in " + elapsed + " ms";
            LOG.info(msg);
            return msg;
        } catch (SQLException e) {
            failedCheckoutCount.incrementAndGet();
            String msg = "Connection FAILED: " + e.getMessage();
            LOG.error(msg, e);
            return msg;
        } finally {
            if (conn != null) {
                try {
                    conn.close();
                } catch (SQLException e) {
                    LOG.warn("Failed to close test connection", e);
                }
            }
        }
    }

    // ── Internal helpers (called by service layer) ──────────────────────

    public void recordCheckout(long waitTimeMs) {
        checkoutCount.incrementAndGet();
        totalWaitTimeMs.addAndGet(waitTimeMs);
    }

    public void recordPoolExhausted() {
        poolExhaustedCount.incrementAndGet();
    }
}
