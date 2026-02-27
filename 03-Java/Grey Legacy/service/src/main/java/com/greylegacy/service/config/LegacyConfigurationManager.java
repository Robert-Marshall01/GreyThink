package com.greylegacy.service.config;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.math.BigDecimal;
import java.util.*;

/**
 * Centralised configuration manager using Apache Commons Configuration patterns.
 * <p>
 * In legacy enterprise Java applications, configuration was managed through
 * a patchwork of sources:
 * <ul>
 *   <li>Properties files on the classpath ({@code database.properties}, {@code application.properties})</li>
 *   <li>System properties ({@code -D} JVM arguments)</li>
 *   <li>JNDI lookups for app server-managed resources</li>
 *   <li>XML configuration files ({@code applicationContext-*.xml})</li>
 *   <li>Database-stored configuration (feature flags, business rules)</li>
 *   <li>Environment variables (for container/cloud deployments)</li>
 * </ul>
 * <p>
 * This class demonstrates the <b>Apache Commons Configuration</b> pattern
 * of composing multiple configuration sources into a single hierarchical
 * configuration with override semantics. While the actual Apache Commons
 * Configuration library ({@code commons-configuration}) provided
 * {@code CompositeConfiguration}, {@code PropertiesConfiguration}, etc.,
 * many legacy teams rolled their own equivalent using standard Java
 * {@code Properties} with layered loading — which is what this class does.
 * <p>
 * <b>Override hierarchy (highest priority first):</b>
 * <ol>
 *   <li>System properties ({@code -Dgreylegacy.claim.maxPayment=500000})</li>
 *   <li>Environment variables ({@code GREYLEGACY_CLAIM_MAX_PAYMENT})</li>
 *   <li>External file override ({@code /etc/greylegacy/override.properties})</li>
 *   <li>Feature flags file ({@code feature-flags.properties})</li>
 *   <li>Application defaults ({@code application.properties})</li>
 * </ol>
 * <p>
 * This pattern is the precursor to Spring Boot's {@code @ConfigurationProperties}
 * and externalized configuration model.
 *
 * @see org.apache.commons.lang.StringUtils
 */
@Component("legacyConfigurationManager")
public class LegacyConfigurationManager {

    private static final Logger log = LoggerFactory.getLogger(LegacyConfigurationManager.class);

    /** Merged configuration from all sources, with override semantics. */
    private final Properties mergedConfig = new Properties();

    /** Cache of typed values to avoid repeated parsing. */
    private final Map<String, Object> typedCache = new HashMap<>();

    // --- Injected Defaults ---

    @Value("${app.config.external.path:/etc/greylegacy/override.properties}")
    private String externalConfigPath;

    @Value("${app.config.reload.enabled:false}")
    private boolean reloadEnabled;

    @Value("${app.config.reload.intervalMs:60000}")
    private long reloadIntervalMs;

    // ========================================================================
    // Lifecycle
    // ========================================================================

    /**
     * Initialize the composite configuration on bean creation.
     * Loads configuration sources in order of ascending priority,
     * so higher-priority sources overwrite lower-priority ones.
     */
    @PostConstruct
    public void init() {
        log.info("Initializing LegacyConfigurationManager");

        // Layer 1: Application defaults (lowest priority)
        loadClasspathProperties("application.properties");

        // Layer 2: Feature flags
        loadClasspathProperties("feature-flags.properties");

        // Layer 3: Database properties
        loadClasspathProperties("database.properties");

        // Layer 4: External file override (if exists)
        loadExternalProperties(externalConfigPath);

        // Layer 5: Environment variables (GREYLEGACY_ prefix)
        loadEnvironmentVariables("GREYLEGACY_");

        // Layer 6: System properties (highest priority, -D flags)
        loadSystemProperties("greylegacy.");

        log.info("Configuration loaded: {} properties from all sources", mergedConfig.size());

        if (log.isDebugEnabled()) {
            logConfigurationSummary();
        }
    }

    // ========================================================================
    // Typed Getters (Apache Commons Configuration API pattern)
    // ========================================================================

    /**
     * Get a string property value.
     * Uses Apache Commons Lang {@link StringUtils} for empty/blank checks.
     */
    public String getString(String key) {
        return mergedConfig.getProperty(key);
    }

    public String getString(String key, String defaultValue) {
        String value = mergedConfig.getProperty(key);
        return StringUtils.isNotBlank(value) ? value : defaultValue;
    }

    /**
     * Get an integer property, with default.
     */
    public int getInt(String key, int defaultValue) {
        String cached = (String) typedCache.get(key);
        if (cached != null) {
            return Integer.parseInt(cached);
        }

        String value = mergedConfig.getProperty(key);
        if (StringUtils.isBlank(value)) {
            return defaultValue;
        }

        try {
            int parsed = Integer.parseInt(value.trim());
            typedCache.put(key, value.trim());
            return parsed;
        } catch (NumberFormatException e) {
            log.warn("Invalid integer value for {}: '{}', using default {}", key, value, defaultValue);
            return defaultValue;
        }
    }

    /**
     * Get a long property, with default.
     */
    public long getLong(String key, long defaultValue) {
        String value = mergedConfig.getProperty(key);
        if (StringUtils.isBlank(value)) {
            return defaultValue;
        }
        try {
            return Long.parseLong(value.trim());
        } catch (NumberFormatException e) {
            log.warn("Invalid long value for {}: '{}', using default {}", key, value, defaultValue);
            return defaultValue;
        }
    }

    /**
     * Get a boolean property, with default.
     * Recognizes: true/false, yes/no, on/off, 1/0.
     */
    public boolean getBoolean(String key, boolean defaultValue) {
        String value = mergedConfig.getProperty(key);
        if (StringUtils.isBlank(value)) {
            return defaultValue;
        }
        value = value.trim().toLowerCase();
        if ("true".equals(value) || "yes".equals(value) || "on".equals(value) || "1".equals(value)) {
            return true;
        }
        if ("false".equals(value) || "no".equals(value) || "off".equals(value) || "0".equals(value)) {
            return false;
        }
        log.warn("Invalid boolean value for {}: '{}', using default {}", key, value, defaultValue);
        return defaultValue;
    }

    /**
     * Get a BigDecimal property — common for financial amounts in insurance.
     */
    public BigDecimal getBigDecimal(String key, BigDecimal defaultValue) {
        String value = mergedConfig.getProperty(key);
        if (StringUtils.isBlank(value)) {
            return defaultValue;
        }
        try {
            return new BigDecimal(value.trim());
        } catch (NumberFormatException e) {
            log.warn("Invalid BigDecimal value for {}: '{}', using default {}", key, value, defaultValue);
            return defaultValue;
        }
    }

    /**
     * Get a comma-separated list property as a List of Strings.
     * E.g., {@code claim.allowed.types=AUTO,PROPERTY,LIABILITY}
     */
    public List<String> getList(String key) {
        String value = mergedConfig.getProperty(key);
        if (StringUtils.isBlank(value)) {
            return Collections.emptyList();
        }
        String[] parts = StringUtils.split(value, ",");
        List<String> result = new ArrayList<>();
        for (String part : parts) {
            String trimmed = StringUtils.trimToNull(part);
            if (trimmed != null) {
                result.add(trimmed);
            }
        }
        return result;
    }

    // ========================================================================
    // Feature Flag Methods
    // ========================================================================

    /**
     * Check if a feature flag is enabled.
     * Feature flags follow the naming convention: {@code feature.FLAG_NAME.enabled}
     * <p>
     * This predates modern feature flag systems (LaunchDarkly, Unleash).
     * In legacy systems, feature flags were simple properties that controlled
     * code paths, typically toggled by deployment scripts or ops teams
     * editing properties files.
     */
    public boolean isFeatureEnabled(String featureName) {
        return getBoolean("feature." + featureName + ".enabled", false);
    }

    // ========================================================================
    // Business Rule Configuration
    // ========================================================================

    /**
     * Maximum allowed claim payment amount (business rule threshold).
     */
    public BigDecimal getMaxClaimPayment() {
        return getBigDecimal("claim.max.payment.amount", new BigDecimal("250000.00"));
    }

    /**
     * Number of days before a claim is considered "aged" and escalated.
     */
    public int getClaimAgingThresholdDays() {
        return getInt("claim.aging.threshold.days", 90);
    }

    /**
     * Fraud score threshold above which a claim is flagged for SIU review.
     */
    public int getFraudScoreThreshold() {
        return getInt("fraud.score.threshold", 75);
    }

    /**
     * SMTP server for email notifications (legacy JNDI resource pattern).
     */
    public String getSmtpHost() {
        return getString("mail.smtp.host", "localhost");
    }

    // ========================================================================
    // Configuration Source Loaders
    // ========================================================================

    private void loadClasspathProperties(String filename) {
        try {
            Properties props = new Properties();
            ClassLoader cl = Thread.currentThread().getContextClassLoader();
            if (cl.getResource(filename) != null) {
                props.load(cl.getResourceAsStream(filename));
                mergedConfig.putAll(props);
                log.info("Loaded {} properties from classpath:{}", props.size(), filename);
            } else {
                log.debug("Classpath resource not found: {}", filename);
            }
        } catch (IOException e) {
            log.warn("Failed to load classpath properties: {}", filename, e);
        }
    }

    private void loadExternalProperties(String path) {
        if (StringUtils.isBlank(path)) {
            return;
        }
        File file = new File(path);
        if (file.exists() && file.canRead()) {
            try (FileInputStream fis = new FileInputStream(file)) {
                Properties props = new Properties();
                props.load(fis);
                mergedConfig.putAll(props);
                log.info("Loaded {} properties from external file: {}", props.size(), path);
            } catch (IOException e) {
                log.warn("Failed to load external properties: {}", path, e);
            }
        } else {
            log.debug("External config file does not exist or is not readable: {}", path);
        }
    }

    private void loadEnvironmentVariables(String prefix) {
        int count = 0;
        for (Map.Entry<String, String> entry : System.getenv().entrySet()) {
            if (entry.getKey().startsWith(prefix)) {
                // Convert GREYLEGACY_CLAIM_MAX_PAYMENT → greylegacy.claim.max.payment
                String key = entry.getKey()
                        .toLowerCase()
                        .replace('_', '.');
                mergedConfig.setProperty(key, entry.getValue());
                count++;
            }
        }
        if (count > 0) {
            log.info("Loaded {} environment variables with prefix '{}'", count, prefix);
        }
    }

    private void loadSystemProperties(String prefix) {
        int count = 0;
        for (String key : System.getProperties().stringPropertyNames()) {
            if (key.startsWith(prefix)) {
                mergedConfig.setProperty(key, System.getProperty(key));
                count++;
            }
        }
        if (count > 0) {
            log.info("Loaded {} system properties with prefix '{}'", count, prefix);
        }
    }

    private void logConfigurationSummary() {
        log.debug("=== Configuration Summary ===");
        TreeMap<Object, Object> sorted = new TreeMap<>(mergedConfig);
        for (Map.Entry<Object, Object> entry : sorted.entrySet()) {
            String key = entry.getKey().toString();
            String value = entry.getValue().toString();

            // Mask sensitive values
            if (key.contains("password") || key.contains("secret") || key.contains("ssn")) {
                value = "****";
            }
            log.debug("  {} = {}", key, value);
        }
    }
}
