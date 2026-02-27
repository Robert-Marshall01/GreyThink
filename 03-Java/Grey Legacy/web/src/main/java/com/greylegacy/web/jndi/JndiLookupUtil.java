package com.greylegacy.web.jndi;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.naming.Context;
import javax.naming.InitialContext;
import javax.naming.NamingException;
import javax.sql.DataSource;

/**
 * Centralised JNDI lookup utility for container-managed resources.
 *
 * <h3>Why JNDI?</h3>
 * <p>In legacy enterprise Java, the application server (Tomcat, JBoss, WebLogic,
 * WebSphere) owns the database connection pool, JMS factories, and mail sessions.
 * The application retrieves them via JNDI at runtime rather than creating them
 * directly. This separation allows:</p>
 * <ul>
 *   <li>Operations to change credentials without redeploying code</li>
 *   <li>Connection pool tuning without application restarts</li>
 *   <li>The same WAR/EAR to run in dev (H2) and prod (PostgreSQL)</li>
 * </ul>
 *
 * <h3>Tomcat vs JBoss JNDI Names</h3>
 * <pre>
 *   Tomcat:  java:comp/env/jdbc/GreyLegacyDS
 *   JBoss:   java:jboss/datasources/GreyLegacyDS
 *   WebLogic: jdbc/GreyLegacyDS
 *   WebSphere: jdbc/GreyLegacyDS  (with custom JNDI ref in ibm-web-bnd.xml)
 * </pre>
 *
 * <h3>Spring Integration</h3>
 * <p>In Spring XML, JNDI lookups are done via {@code <jee:jndi-lookup>}:
 * <pre>
 *   &lt;jee:jndi-lookup id="dataSource"
 *                      jndi-name="jdbc/GreyLegacyDS"
 *                      resource-ref="true"/&gt;
 * </pre>
 * The {@code resource-ref="true"} flag tells Spring to prepend
 * {@code java:comp/env/} automatically for Tomcat compatibility.</p>
 *
 * <p>This utility class exists for non-Spring components (legacy servlets,
 * EJB session beans, Quartz jobss) that need manual JNDI access.</p>
 *
 * @author Grey Legacy Infrastructure Team
 * @since 1.0.0
 */
public final class JndiLookupUtil {

    private static final Logger log = LoggerFactory.getLogger(JndiLookupUtil.class);

    /** Standard JNDI prefix for Tomcat web application resources. */
    private static final String TOMCAT_PREFIX = "java:comp/env/";

    /** JNDI prefix for JBoss-managed resources. */
    private static final String JBOSS_PREFIX = "java:jboss/";

    // Cached InitialContext — thread-safe after construction
    private static volatile Context cachedContext;

    private JndiLookupUtil() {
        // Utility class — no instantiation
    }

    // -------------------------------------------------------------------------
    // Public API
    // -------------------------------------------------------------------------

    /**
     * Looks up the primary claims database DataSource from JNDI.
     *
     * <p>Tries multiple JNDI names for portability across containers:</p>
     * <ol>
     *   <li>{@code java:comp/env/jdbc/GreyLegacyDS} (Tomcat)</li>
     *   <li>{@code java:jboss/datasources/GreyLegacyDS} (JBoss/WildFly)</li>
     *   <li>{@code jdbc/GreyLegacyDS} (WebLogic/WebSphere)</li>
     * </ol>
     *
     * @return the container-managed DataSource
     * @throws NamingException if no matching JNDI resource is found
     */
    public static DataSource lookupDataSource() throws NamingException {
        return lookup("jdbc/GreyLegacyDS", DataSource.class);
    }

    /**
     * Looks up the JMS ConnectionFactory from JNDI.
     *
     * @return the container-managed ConnectionFactory
     * @throws NamingException if not found
     */
    public static javax.jms.ConnectionFactory lookupJmsConnectionFactory()
            throws NamingException {
        return lookup("jms/ConnectionFactory", javax.jms.ConnectionFactory.class);
    }

    /**
     * Generic JNDI lookup with multi-container fallback.
     *
     * <p>Iterates through known JNDI prefix conventions until a match
     * is found.  This makes the same code work on Tomcat, JBoss,
     * WebLogic, and WebSphere without configuration changes.</p>
     *
     * @param <T>          the expected resource type
     * @param resourceName the simple resource name (e.g. "jdbc/GreyLegacyDS")
     * @param type         the expected class
     * @return the JNDI resource
     * @throws NamingException if the resource cannot be found under any prefix
     */
    @SuppressWarnings("unchecked")
    public static <T> T lookup(String resourceName, Class<T> type)
            throws NamingException {

        Context ctx = getContext();

        // Try multiple JNDI naming conventions
        String[] candidates = {
                TOMCAT_PREFIX + resourceName,       // Tomcat: java:comp/env/
                JBOSS_PREFIX + resourceName,        // JBoss:  java:jboss/
                "java:/" + resourceName,            // JBoss alt
                resourceName                        // WebLogic/WebSphere (no prefix)
        };

        NamingException lastException = null;
        for (String jndiName : candidates) {
            try {
                Object result = ctx.lookup(jndiName);
                if (type.isInstance(result)) {
                    log.info("JNDI lookup succeeded: {} → {}", jndiName, type.getSimpleName());
                    return (T) result;
                }
                log.warn("JNDI name {} found but wrong type: expected={}, actual={}",
                        jndiName, type.getName(), result.getClass().getName());
            } catch (NamingException e) {
                log.trace("JNDI name {} not found, trying next...", jndiName);
                lastException = e;
            }
        }

        log.error("JNDI resource '{}' not found under any known prefix", resourceName);
        throw lastException != null ? lastException :
                new NamingException("Resource not found: " + resourceName);
    }

    /**
     * Reads an environment entry (String) from JNDI.
     *
     * <p>Environment entries are configured in context.xml:
     * <pre>
     *   &lt;Environment name="app/environment" value="production"
     *                type="java.lang.String"/&gt;
     * </pre>
     * </p>
     *
     * @param name the environment entry name (e.g. "app/environment")
     * @return the string value, or {@code null} if not found
     */
    public static String lookupEnvironmentEntry(String name) {
        try {
            return lookup(name, String.class);
        } catch (NamingException e) {
            log.debug("Environment entry '{}' not available via JNDI", name);
            return null;
        }
    }

    // -------------------------------------------------------------------------
    // Internal
    // -------------------------------------------------------------------------

    /**
     * Returns a cached {@link InitialContext}. Thread-safe via double-checked
     * locking (safe in Java 5+ with volatile).
     */
    private static Context getContext() throws NamingException {
        if (cachedContext == null) {
            synchronized (JndiLookupUtil.class) {
                if (cachedContext == null) {
                    cachedContext = new InitialContext();
                    log.debug("InitialContext created");
                }
            }
        }
        return cachedContext;
    }
}
