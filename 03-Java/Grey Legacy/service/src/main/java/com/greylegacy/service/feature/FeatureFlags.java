package com.greylegacy.service.feature;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.concurrent.locks.ReentrantReadWriteLock;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Simple property-file-based feature flag system.
 * <p>
 * Loads flags from {@code feature-flags.properties} on the classpath.
 * Thread-safe via {@link ReentrantReadWriteLock} and employs double-checked
 * locking for lazy singleton initialisation (pre-Spring-managed era).
 * </p>
 *
 * @since 1.8.0
 */
public final class FeatureFlags {

    private static final Logger LOG = LoggerFactory.getLogger(FeatureFlags.class);
    private static final String PROPERTIES_FILE = "feature-flags.properties";

    // ── Pre-defined flag constants ──────────────────────────────────────
    public static final String ENABLE_FRAUD_SCORING       = "feature.fraud.scoring.enabled";
    public static final String ENABLE_AUTO_ADJUDICATION   = "feature.auto.adjudication.enabled";
    public static final String ENABLE_SOAP_ENDPOINTS      = "feature.soap.endpoints.enabled";
    public static final String ENABLE_JMS_NOTIFICATIONS   = "feature.jms.notifications.enabled";
    public static final String ENABLE_BATCH_PROCESSING    = "feature.batch.processing.enabled";
    public static final String ENABLE_REST_API            = "feature.rest.api.enabled";
    public static final String ENABLE_CSV_IMPORT          = "feature.csv.import.enabled";
    public static final String ENABLE_SCD_HISTORY         = "feature.scd.history.enabled";
    public static final String ENABLE_PESSIMISTIC_LOCKING = "feature.pessimistic.locking.enabled";

    // ── Singleton ───────────────────────────────────────────────────────
    private static volatile FeatureFlags instance;

    private final ReentrantReadWriteLock lock = new ReentrantReadWriteLock();
    private Properties properties;

    private FeatureFlags() {
        this.properties = loadProperties();
    }

    /**
     * Returns the singleton instance, creating it on first access with
     * double-checked locking.
     */
    public static FeatureFlags getInstance() {
        if (instance == null) {
            synchronized (FeatureFlags.class) {
                if (instance == null) {
                    instance = new FeatureFlags();
                    LOG.info("FeatureFlags singleton initialised from {}", PROPERTIES_FILE);
                }
            }
        }
        return instance;
    }

    // ── Public API ──────────────────────────────────────────────────────

    /**
     * Checks whether a feature flag is enabled (defaults to {@code false}).
     */
    public boolean isEnabled(String featureName) {
        return isEnabled(featureName, false);
    }

    /**
     * Checks whether a feature flag is enabled, returning {@code defaultValue}
     * if the flag is not defined.
     */
    public boolean isEnabled(String featureName, boolean defaultValue) {
        lock.readLock().lock();
        try {
            String value = properties.getProperty(featureName);
            boolean result = value != null ? Boolean.parseBoolean(value.trim()) : defaultValue;
            LOG.debug("Feature flag check: {}={}", featureName, result);
            return result;
        } finally {
            lock.readLock().unlock();
        }
    }

    /**
     * Re-reads the properties file without requiring a JVM restart.
     */
    public void reload() {
        lock.writeLock().lock();
        try {
            Properties reloaded = loadProperties();
            this.properties = reloaded;
            LOG.info("Feature flags reloaded from {}", PROPERTIES_FILE);
        } finally {
            lock.writeLock().unlock();
        }
    }

    /**
     * Returns a list of all feature names that are currently enabled.
     */
    public List<String> getEnabledFeatures() {
        lock.readLock().lock();
        try {
            List<String> enabled = new ArrayList<String>();
            for (Map.Entry<Object, Object> entry : properties.entrySet()) {
                if (Boolean.parseBoolean(entry.getValue().toString().trim())) {
                    enabled.add(entry.getKey().toString());
                }
            }
            return enabled;
        } finally {
            lock.readLock().unlock();
        }
    }

    // ── Internal ────────────────────────────────────────────────────────

    private Properties loadProperties() {
        Properties props = new Properties();
        InputStream in = null;
        try {
            in = getClass().getClassLoader().getResourceAsStream(PROPERTIES_FILE);
            if (in == null) {
                LOG.warn("Feature flags file not found on classpath: {}", PROPERTIES_FILE);
                return props;
            }
            props.load(in);
            LOG.info("Loaded {} feature flag(s) from {}", props.size(), PROPERTIES_FILE);
        } catch (IOException e) {
            LOG.error("Failed to load feature flags from " + PROPERTIES_FILE, e);
        } finally {
            if (in != null) {
                try { in.close(); } catch (IOException ignored) { }
            }
        }
        return props;
    }
}
