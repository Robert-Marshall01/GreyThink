package com.greylegacy.web.security;

import javax.security.auth.Subject;
import javax.security.auth.callback.Callback;
import javax.security.auth.callback.CallbackHandler;
import javax.security.auth.callback.NameCallback;
import javax.security.auth.callback.PasswordCallback;
import javax.security.auth.login.LoginException;
import javax.security.auth.spi.LoginModule;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.Principal;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.naming.InitialContext;
import javax.naming.NamingException;
import javax.sql.DataSource;

/**
 * JAAS LoginModule implementation for the Grey Legacy Claims System.
 *
 * <h3>Overview</h3>
 * <p>Performs database-backed authentication and role loading against the
 * {@code APP_USER} and {@code APP_USER_ROLE} tables. Passwords are hashed
 * with SHA-256 (hex-encoded) and compared against the stored hash.</p>
 *
 * <h3>JAAS Configuration (jaas.conf)</h3>
 * <pre>
 * GreyLegacy {
 *     com.greylegacy.web.security.GreyLegacyLoginModule required
 *         dsJndiName="java:comp/env/jdbc/GreyLegacyDS"
 *         hashAlgorithm="SHA-256"
 *         debug="true";
 * };
 * </pre>
 *
 * <h3>Database Schema</h3>
 * <pre>
 * CREATE TABLE APP_USER (
 *     USERNAME    VARCHAR(50)  PRIMARY KEY,
 *     PASSWORD    VARCHAR(128) NOT NULL,   -- SHA-256 hex hash
 *     ENABLED     BOOLEAN      NOT NULL DEFAULT TRUE,
 *     LOCKED      BOOLEAN      NOT NULL DEFAULT FALSE,
 *     FAILED_ATTEMPTS INTEGER  NOT NULL DEFAULT 0,
 *     LAST_LOGIN  TIMESTAMP,
 *     CREATED_DATE TIMESTAMP   DEFAULT CURRENT_TIMESTAMP
 * );
 *
 * CREATE TABLE APP_USER_ROLE (
 *     USERNAME    VARCHAR(50)  NOT NULL,
 *     ROLE_NAME   VARCHAR(50)  NOT NULL,
 *     CONSTRAINT PK_USER_ROLE PRIMARY KEY (USERNAME, ROLE_NAME),
 *     CONSTRAINT FK_USER_ROLE_USER FOREIGN KEY (USERNAME)
 *         REFERENCES APP_USER(USERNAME)
 * );
 * </pre>
 *
 * <h3>Container Integration</h3>
 * <ul>
 *   <li><b>Tomcat:</b> Configure via {@code JAASRealm} in server.xml</li>
 *   <li><b>JBoss/WildFly:</b> Configure as a security-domain in standalone.xml</li>
 *   <li><b>WebSphere:</b> Register as custom JAAS login module in admin console</li>
 *   <li><b>WebLogic:</b> Register in security realm configuration</li>
 * </ul>
 *
 * <h3>Two-Phase JAAS Contract</h3>
 * <ol>
 *   <li>{@code login()} - Phase 1: Authenticate the user (validate credentials)</li>
 *   <li>{@code commit()} - Phase 2: Add Principals to the Subject if login succeeded</li>
 *   <li>{@code abort()} - Called if the overall login fails (clean up state)</li>
 *   <li>{@code logout()} - Remove Principals added during commit</li>
 * </ol>
 *
 * @author Grey Legacy Security Team
 * @since 1.0
 */
public class GreyLegacyLoginModule implements LoginModule {

    private static final Logger LOG = Logger.getLogger(GreyLegacyLoginModule.class.getName());

    // -----------------------------------------------------------------------
    // Module options (configured in jaas.conf or container security config)
    // -----------------------------------------------------------------------

    /** JNDI name of the DataSource for user/role queries. */
    private static final String OPT_DS_JNDI_NAME = "dsJndiName";
    private static final String DEFAULT_DS_JNDI = "java:comp/env/jdbc/GreyLegacyDS";

    /** Password hash algorithm (SHA-256, SHA-512, MD5). */
    private static final String OPT_HASH_ALGORITHM = "hashAlgorithm";
    private static final String DEFAULT_HASH = "SHA-256";

    /** SQL to authenticate user. Must return PASSWORD, ENABLED, LOCKED columns. */
    private static final String OPT_USER_QUERY = "userQuery";
    private static final String DEFAULT_USER_QUERY =
            "SELECT PASSWORD, ENABLED, LOCKED FROM APP_USER WHERE USERNAME = ?";

    /** SQL to load roles. Must return ROLE_NAME column. */
    private static final String OPT_ROLE_QUERY = "roleQuery";
    private static final String DEFAULT_ROLE_QUERY =
            "SELECT ROLE_NAME FROM APP_USER_ROLE WHERE USERNAME = ?";

    /** SQL to update last login timestamp. */
    private static final String UPDATE_LAST_LOGIN_SQL =
            "UPDATE APP_USER SET LAST_LOGIN = CURRENT_TIMESTAMP, FAILED_ATTEMPTS = 0 WHERE USERNAME = ?";

    /** SQL to increment failed login attempts. */
    private static final String INCREMENT_FAILED_SQL =
            "UPDATE APP_USER SET FAILED_ATTEMPTS = FAILED_ATTEMPTS + 1 WHERE USERNAME = ?";

    /** Max failed attempts before account lock. */
    private static final int MAX_FAILED_ATTEMPTS = 5;

    /** SQL to lock account after max failed attempts. */
    private static final String LOCK_ACCOUNT_SQL =
            "UPDATE APP_USER SET LOCKED = TRUE WHERE USERNAME = ? AND FAILED_ATTEMPTS >= " + MAX_FAILED_ATTEMPTS;

    // -----------------------------------------------------------------------
    // JAAS LoginModule state
    // -----------------------------------------------------------------------

    private Subject subject;
    private CallbackHandler callbackHandler;
    private Map<String, ?> sharedState;
    private Map<String, ?> options;

    // Resolved option values
    private String dsJndiName;
    private String hashAlgorithm;
    private String userQuery;
    private String roleQuery;
    private boolean debug;

    // Authentication state (populated in login(), used in commit())
    private boolean loginSucceeded = false;
    private boolean commitSucceeded = false;
    private String authenticatedUsername;
    private final List<String> authenticatedRoles = new ArrayList<>();

    // Principals added to Subject (tracked for logout/abort cleanup)
    private GreyLegacyUserPrincipal userPrincipal;
    private final List<GreyLegacyRolePrincipal> rolePrincipals = new ArrayList<>();

    // -----------------------------------------------------------------------
    // LoginModule interface implementation
    // -----------------------------------------------------------------------

    /**
     * Initialize the LoginModule.
     *
     * <p>Called by the JAAS framework before any login attempt. Stores references
     * to the Subject, CallbackHandler, and module options.</p>
     *
     * @param subject         the Subject to populate on successful authentication
     * @param callbackHandler handler for acquiring username/password from the container
     * @param sharedState     shared state between stacked LoginModules
     * @param options         module-specific options from jaas.conf
     */
    @Override
    public void initialize(Subject subject, CallbackHandler callbackHandler,
                           Map<String, ?> sharedState, Map<String, ?> options) {
        this.subject = subject;
        this.callbackHandler = callbackHandler;
        this.sharedState = sharedState;
        this.options = options;

        // Resolve module options with defaults
        this.dsJndiName    = getOption(OPT_DS_JNDI_NAME, DEFAULT_DS_JNDI);
        this.hashAlgorithm = getOption(OPT_HASH_ALGORITHM, DEFAULT_HASH);
        this.userQuery     = getOption(OPT_USER_QUERY, DEFAULT_USER_QUERY);
        this.roleQuery     = getOption(OPT_ROLE_QUERY, DEFAULT_ROLE_QUERY);
        this.debug         = "true".equalsIgnoreCase(getOption("debug", "false"));

        if (debug) {
            LOG.info("[GreyLegacyLoginModule] Initialized with dsJndiName=" + dsJndiName
                    + ", hashAlgorithm=" + hashAlgorithm);
        }
    }

    /**
     * Phase 1: Authenticate the user.
     *
     * <p>Acquires username and password via the CallbackHandler, hashes the
     * password, and validates against the database. Does NOT modify the Subject
     * yet — that happens in {@link #commit()}.</p>
     *
     * @return true if authentication succeeded
     * @throws LoginException if authentication fails or an error occurs
     */
    @Override
    public boolean login() throws LoginException {
        if (callbackHandler == null) {
            throw new LoginException("No CallbackHandler configured");
        }

        // ---- Acquire credentials via JAAS callbacks ----
        NameCallback nameCallback = new NameCallback("Username: ");
        PasswordCallback passwordCallback = new PasswordCallback("Password: ", false);

        try {
            callbackHandler.handle(new Callback[]{nameCallback, passwordCallback});
        } catch (Exception e) {
            throw new LoginException("Failed to handle authentication callbacks: " + e.getMessage());
        }

        String username = nameCallback.getName();
        char[] passwordChars = passwordCallback.getPassword();
        passwordCallback.clearPassword(); // Security best practice

        if (username == null || username.trim().isEmpty()) {
            throw new LoginException("Username is required");
        }
        if (passwordChars == null || passwordChars.length == 0) {
            throw new LoginException("Password is required");
        }

        String password = new String(passwordChars);

        if (debug) {
            LOG.info("[GreyLegacyLoginModule] Attempting authentication for user: " + username);
        }

        // ---- Validate credentials against database ----
        Connection conn = null;
        try {
            conn = getDataSource().getConnection();

            // Step 1: Lookup user and validate password
            if (!validateCredentials(conn, username, password)) {
                recordFailedAttempt(conn, username);
                throw new LoginException("Invalid username or password");
            }

            // Step 2: Load roles for the authenticated user
            loadRoles(conn, username);

            // Step 3: Record successful login
            recordSuccessfulLogin(conn, username);

            this.authenticatedUsername = username;
            this.loginSucceeded = true;

            if (debug) {
                LOG.info("[GreyLegacyLoginModule] Authentication succeeded for user: "
                        + username + ", roles: " + authenticatedRoles);
            }

            return true;

        } catch (SQLException e) {
            LOG.log(Level.SEVERE, "[GreyLegacyLoginModule] Database error during authentication", e);
            throw new LoginException("Database error during authentication: " + e.getMessage());
        } catch (NamingException e) {
            LOG.log(Level.SEVERE, "[GreyLegacyLoginModule] JNDI lookup failed for: " + dsJndiName, e);
            throw new LoginException("JNDI lookup failed: " + e.getMessage());
        } finally {
            closeQuietly(conn);
        }
    }

    /**
     * Phase 2: Commit the authentication.
     *
     * <p>Called by the JAAS framework if the overall login succeeds (all required
     * LoginModules in the stack passed). Adds Principals to the Subject.</p>
     *
     * @return true if principals were added
     * @throws LoginException if commit fails
     */
    @Override
    public boolean commit() throws LoginException {
        if (!loginSucceeded) {
            return false;
        }

        // Create and add user principal
        userPrincipal = new GreyLegacyUserPrincipal(authenticatedUsername);
        subject.getPrincipals().add(userPrincipal);

        // Create and add role principals
        for (String role : authenticatedRoles) {
            GreyLegacyRolePrincipal rolePrincipal = new GreyLegacyRolePrincipal(role);
            rolePrincipals.add(rolePrincipal);
            subject.getPrincipals().add(rolePrincipal);
        }

        commitSucceeded = true;

        if (debug) {
            LOG.info("[GreyLegacyLoginModule] Committed principals for: "
                    + authenticatedUsername + " (" + rolePrincipals.size() + " roles)");
        }

        return true;
    }

    /**
     * Abort the authentication.
     *
     * <p>Called if the overall login fails (e.g., another required LoginModule
     * in the stack failed). Cleans up any state from login() and commit().</p>
     *
     * @return true if abort performed cleanup
     * @throws LoginException if abort fails
     */
    @Override
    public boolean abort() throws LoginException {
        if (!loginSucceeded) {
            return false;
        }

        if (commitSucceeded) {
            // We committed principals — must remove them
            removePrincipals();
        }

        clearState();
        return true;
    }

    /**
     * Logout the user.
     *
     * <p>Removes all Principals that were added during {@link #commit()}.</p>
     *
     * @return true if logout succeeded
     * @throws LoginException if logout fails
     */
    @Override
    public boolean logout() throws LoginException {
        removePrincipals();
        clearState();

        if (debug) {
            LOG.info("[GreyLegacyLoginModule] Logged out user: " + authenticatedUsername);
        }

        return true;
    }

    // -----------------------------------------------------------------------
    // Private helper methods
    // -----------------------------------------------------------------------

    /**
     * Validates user credentials against the database.
     *
     * @return true if the username exists, is enabled, not locked, and the
     *         password hash matches
     */
    private boolean validateCredentials(Connection conn, String username, String password)
            throws SQLException, LoginException {

        try (PreparedStatement ps = conn.prepareStatement(userQuery)) {
            ps.setString(1, username);
            try (ResultSet rs = ps.executeQuery()) {
                if (!rs.next()) {
                    if (debug) {
                        LOG.info("[GreyLegacyLoginModule] User not found: " + username);
                    }
                    return false;
                }

                String storedHash = rs.getString("PASSWORD");
                boolean enabled = rs.getBoolean("ENABLED");
                boolean locked = rs.getBoolean("LOCKED");

                // Check account status before validating password
                if (!enabled) {
                    throw new LoginException("Account is disabled: " + username);
                }
                if (locked) {
                    throw new LoginException("Account is locked: " + username
                            + ". Contact administrator.");
                }

                // Hash the provided password and compare
                String inputHash = hashPassword(password);
                return inputHash.equals(storedHash);
            }
        }
    }

    /**
     * Loads granted roles for the authenticated user.
     */
    private void loadRoles(Connection conn, String username) throws SQLException {
        authenticatedRoles.clear();
        try (PreparedStatement ps = conn.prepareStatement(roleQuery)) {
            ps.setString(1, username);
            try (ResultSet rs = ps.executeQuery()) {
                while (rs.next()) {
                    authenticatedRoles.add(rs.getString("ROLE_NAME"));
                }
            }
        }
    }

    /**
     * Records a successful login (updates last login timestamp, resets failed attempts).
     */
    private void recordSuccessfulLogin(Connection conn, String username) {
        try (PreparedStatement ps = conn.prepareStatement(UPDATE_LAST_LOGIN_SQL)) {
            ps.setString(1, username);
            ps.executeUpdate();
        } catch (SQLException e) {
            // Non-fatal — log but don't fail the login
            LOG.log(Level.WARNING, "Failed to update last login for: " + username, e);
        }
    }

    /**
     * Records a failed login attempt and locks the account if threshold exceeded.
     */
    private void recordFailedAttempt(Connection conn, String username) {
        try {
            // Increment attempt counter
            try (PreparedStatement ps = conn.prepareStatement(INCREMENT_FAILED_SQL)) {
                ps.setString(1, username);
                ps.executeUpdate();
            }
            // Lock account if max attempts exceeded
            try (PreparedStatement ps = conn.prepareStatement(LOCK_ACCOUNT_SQL)) {
                ps.setString(1, username);
                int locked = ps.executeUpdate();
                if (locked > 0) {
                    LOG.warning("[GreyLegacyLoginModule] Account locked after "
                            + MAX_FAILED_ATTEMPTS + " failed attempts: " + username);
                }
            }
        } catch (SQLException e) {
            LOG.log(Level.WARNING, "Failed to record failed attempt for: " + username, e);
        }
    }

    /**
     * Hashes a password using the configured algorithm.
     * Returns the hex-encoded hash string.
     */
    private String hashPassword(String password) throws LoginException {
        try {
            MessageDigest md = MessageDigest.getInstance(hashAlgorithm);
            byte[] hash = md.digest(password.getBytes(java.nio.charset.StandardCharsets.UTF_8));
            return bytesToHex(hash);
        } catch (NoSuchAlgorithmException e) {
            throw new LoginException("Unsupported hash algorithm: " + hashAlgorithm);
        }
    }

    /**
     * Converts a byte array to a lowercase hex string.
     */
    private static String bytesToHex(byte[] bytes) {
        StringBuilder sb = new StringBuilder(bytes.length * 2);
        for (byte b : bytes) {
            sb.append(String.format("%02x", b));
        }
        return sb.toString();
    }

    /**
     * Obtains a DataSource via JNDI lookup.
     *
     * <p>Tries multiple JNDI prefixes to support Tomcat, JBoss, WebLogic,
     * and WebSphere containers.</p>
     */
    private DataSource getDataSource() throws NamingException {
        InitialContext ctx = new InitialContext();

        // Try the configured JNDI name first
        try {
            return (DataSource) ctx.lookup(dsJndiName);
        } catch (NamingException e) {
            if (debug) {
                LOG.fine("[GreyLegacyLoginModule] JNDI lookup failed for: " + dsJndiName
                        + ", trying fallback prefixes");
            }
        }

        // Fallback: try common container-specific prefixes
        String[] prefixes = {
                "java:comp/env/",   // Tomcat standard
                "java:jboss/",      // JBoss/WildFly
                "java:/",           // JBoss alternate
                ""                  // WebLogic/WebSphere (bare name)
        };

        String baseName = dsJndiName;
        // Strip known prefixes to get the base name
        for (String prefix : prefixes) {
            if (baseName.startsWith(prefix)) {
                baseName = baseName.substring(prefix.length());
                break;
            }
        }

        for (String prefix : prefixes) {
            try {
                return (DataSource) ctx.lookup(prefix + baseName);
            } catch (NamingException ignored) {
                // Try next prefix
            }
        }

        throw new NamingException("DataSource not found under any JNDI prefix for: " + dsJndiName);
    }

    /**
     * Gets a module option value, falling back to a default.
     */
    private String getOption(String key, String defaultValue) {
        if (options != null && options.containsKey(key)) {
            Object value = options.get(key);
            return value != null ? value.toString() : defaultValue;
        }
        return defaultValue;
    }

    /**
     * Removes all Principals that this module added to the Subject.
     */
    private void removePrincipals() {
        if (userPrincipal != null) {
            subject.getPrincipals().remove(userPrincipal);
        }
        for (GreyLegacyRolePrincipal rp : rolePrincipals) {
            subject.getPrincipals().remove(rp);
        }
    }

    /**
     * Clears all authentication state for reuse.
     */
    private void clearState() {
        loginSucceeded = false;
        commitSucceeded = false;
        authenticatedUsername = null;
        authenticatedRoles.clear();
        userPrincipal = null;
        rolePrincipals.clear();
    }

    /**
     * Quietly closes a JDBC Connection, logging but not throwing on error.
     */
    private void closeQuietly(Connection conn) {
        if (conn != null) {
            try {
                conn.close();
            } catch (SQLException e) {
                LOG.log(Level.FINE, "Failed to close connection", e);
            }
        }
    }

    // -----------------------------------------------------------------------
    // Inner classes: Principal implementations
    // -----------------------------------------------------------------------

    /**
     * Principal representing an authenticated user identity.
     *
     * <p>Implements {@link java.security.Principal} for JAAS Subject population.
     * The container uses this to identify the authenticated user in
     * {@code HttpServletRequest.getUserPrincipal()}.</p>
     */
    public static class GreyLegacyUserPrincipal implements Principal, java.io.Serializable {
        private static final long serialVersionUID = 1L;
        private final String name;

        public GreyLegacyUserPrincipal(String name) {
            this.name = name;
        }

        @Override
        public String getName() {
            return name;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            return name.equals(((GreyLegacyUserPrincipal) o).name);
        }

        @Override
        public int hashCode() {
            return name.hashCode();
        }

        @Override
        public String toString() {
            return "GreyLegacyUserPrincipal[" + name + "]";
        }
    }

    /**
     * Principal representing an assigned security role.
     *
     * <p>The container uses these to evaluate role-based access in
     * {@code HttpServletRequest.isUserInRole()} and
     * {@code <security-constraint>} elements in web.xml.</p>
     */
    public static class GreyLegacyRolePrincipal implements Principal, java.io.Serializable {
        private static final long serialVersionUID = 1L;
        private final String name;

        public GreyLegacyRolePrincipal(String name) {
            this.name = name;
        }

        @Override
        public String getName() {
            return name;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            return name.equals(((GreyLegacyRolePrincipal) o).name);
        }

        @Override
        public int hashCode() {
            return name.hashCode();
        }

        @Override
        public String toString() {
            return "GreyLegacyRolePrincipal[" + name + "]";
        }
    }
}
