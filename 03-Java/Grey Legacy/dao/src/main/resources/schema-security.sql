-- ============================================================================
-- Grey Legacy - Security & Audit Schema
-- ============================================================================
-- Target:      H2 (development) / PostgreSQL (production)
-- Purpose:     JAAS user/role tables, audit trail, compliance support
-- Depends on:  schema.sql (base tables)
-- Author:      Grey Legacy Security Engineering
-- ============================================================================
--
-- TABLE OF CONTENTS:
--   1. Application User Table (APP_USER)
--   2. Application Role Mapping (APP_USER_ROLE)
--   3. Login Audit Trail (LOGIN_AUDIT)
--   4. Data Access Audit (DATA_ACCESS_AUDIT)
--   5. Security Configuration (SECURITY_CONFIG)
--   6. Seed Data — Default Users and Roles
--   7. Indexes for Audit Query Performance
-- ============================================================================


-- ============================================================================
-- 1. APPLICATION USER TABLE
-- ============================================================================
-- Used by the JAAS GreyLegacyLoginModule for authentication.
-- Passwords are stored as SHA-256 hex-encoded hashes.
--
-- To generate a SHA-256 hash for a password:
--   echo -n "password123" | sha256sum
--   => ef92b778bafe771e89245b89ecbc08a44a4e166c06659911881f383d4473e94f
-- ============================================================================

CREATE TABLE IF NOT EXISTS APP_USER (
    USERNAME        VARCHAR(50)     NOT NULL,
    PASSWORD        VARCHAR(128)    NOT NULL,
    FIRST_NAME      VARCHAR(50),
    LAST_NAME       VARCHAR(50),
    EMAIL           VARCHAR(100),
    ENABLED         BOOLEAN         NOT NULL DEFAULT TRUE,
    LOCKED          BOOLEAN         NOT NULL DEFAULT FALSE,
    FAILED_ATTEMPTS INTEGER         NOT NULL DEFAULT 0,
    LAST_LOGIN      TIMESTAMP,
    PASSWORD_CHANGED TIMESTAMP      DEFAULT CURRENT_TIMESTAMP,
    ACCOUNT_EXPIRES TIMESTAMP,
    CREATED_BY      VARCHAR(50)     DEFAULT 'SYSTEM',
    CREATED_DATE    TIMESTAMP       DEFAULT CURRENT_TIMESTAMP,
    UPDATED_BY      VARCHAR(50),
    UPDATED_DATE    TIMESTAMP,
    CONSTRAINT PK_APP_USER PRIMARY KEY (USERNAME)
);

-- ============================================================================
-- 2. APPLICATION ROLE MAPPING
-- ============================================================================
-- Maps users to security roles. These roles are loaded by the JAAS
-- LoginModule and used by the container to evaluate <security-constraint>
-- and HttpServletRequest.isUserInRole() checks.
--
-- Role hierarchy (informational, enforced at application level):
--   READONLY         — View dashboards and search results
--   CLAIMS_ADJUSTER  — READONLY + process/investigate claims
--   CLAIMS_MANAGER   — CLAIMS_ADJUSTER + approve/deny/reassign
--   ADMIN            — All permissions + system configuration
--   SYSTEM_BATCH     — Batch job execution only (no UI)
--   WS_CLIENT        — SOAP web service access (service accounts)
-- ============================================================================

CREATE TABLE IF NOT EXISTS APP_USER_ROLE (
    USERNAME        VARCHAR(50)     NOT NULL,
    ROLE_NAME       VARCHAR(50)     NOT NULL,
    GRANTED_BY      VARCHAR(50)     DEFAULT 'SYSTEM',
    GRANTED_DATE    TIMESTAMP       DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT PK_APP_USER_ROLE PRIMARY KEY (USERNAME, ROLE_NAME),
    CONSTRAINT FK_AUR_USER FOREIGN KEY (USERNAME)
        REFERENCES APP_USER(USERNAME)
);

CREATE INDEX IF NOT EXISTS IDX_AUR_ROLE ON APP_USER_ROLE(ROLE_NAME);


-- ============================================================================
-- 3. LOGIN AUDIT TRAIL
-- ============================================================================
-- Immutable log of all authentication events. Required for SOX compliance,
-- security incident investigation, and regulatory audit.
--
-- This table is APPEND-ONLY — no updates or deletes are permitted.
-- In production, enforce via database permissions and trigger guards.
--
-- Retention: 7 years per insurance industry regulatory requirements
-- (state-dependent; CA requires 5 years, NY requires 7 years).
-- ============================================================================

CREATE SEQUENCE IF NOT EXISTS LOGIN_AUDIT_SEQ START WITH 1 INCREMENT BY 1;

CREATE TABLE IF NOT EXISTS LOGIN_AUDIT (
    ID              BIGINT          NOT NULL DEFAULT NEXT VALUE FOR LOGIN_AUDIT_SEQ,
    EVENT_TIMESTAMP TIMESTAMP       NOT NULL DEFAULT CURRENT_TIMESTAMP,
    USERNAME        VARCHAR(50)     NOT NULL,
    EVENT_TYPE      VARCHAR(30)     NOT NULL,
    -- LOGIN_SUCCESS, LOGIN_FAILURE, LOGOUT, ACCOUNT_LOCKED,
    -- ACCOUNT_UNLOCKED, PASSWORD_CHANGED, SESSION_EXPIRED
    SOURCE_IP       VARCHAR(45),    -- IPv4 or IPv6
    USER_AGENT      VARCHAR(500),
    SESSION_ID      VARCHAR(100),
    FAILURE_REASON  VARCHAR(500),
    -- BAD_PASSWORD, ACCOUNT_DISABLED, ACCOUNT_LOCKED,
    -- ACCOUNT_EXPIRED, INVALID_TOKEN
    DETAILS         VARCHAR(2000),
    CONSTRAINT PK_LOGIN_AUDIT PRIMARY KEY (ID)
);

CREATE INDEX IF NOT EXISTS IDX_LA_USER ON LOGIN_AUDIT(USERNAME, EVENT_TIMESTAMP);
CREATE INDEX IF NOT EXISTS IDX_LA_EVENT ON LOGIN_AUDIT(EVENT_TYPE, EVENT_TIMESTAMP);
CREATE INDEX IF NOT EXISTS IDX_LA_TIMESTAMP ON LOGIN_AUDIT(EVENT_TIMESTAMP);


-- ============================================================================
-- 4. DATA ACCESS AUDIT
-- ============================================================================
-- Tracks access to sensitive data (PII, PHI, financial records).
-- Required for compliance with state insurance data privacy laws,
-- HIPAA (when health claims are involved), and internal audit policy.
--
-- This supplements the CLAIM_AUDIT table (business event audit) with
-- a technical data access audit for security purposes.
--
-- Retention: Same as LOGIN_AUDIT (7 years)
-- ============================================================================

CREATE SEQUENCE IF NOT EXISTS DATA_ACCESS_AUDIT_SEQ START WITH 1 INCREMENT BY 1;

CREATE TABLE IF NOT EXISTS DATA_ACCESS_AUDIT (
    ID              BIGINT          NOT NULL DEFAULT NEXT VALUE FOR DATA_ACCESS_AUDIT_SEQ,
    EVENT_TIMESTAMP TIMESTAMP       NOT NULL DEFAULT CURRENT_TIMESTAMP,
    USERNAME        VARCHAR(50)     NOT NULL,
    ACTION          VARCHAR(20)     NOT NULL,
    -- VIEW, EXPORT, PRINT, MODIFY, DELETE, SEARCH
    RESOURCE_TYPE   VARCHAR(50)     NOT NULL,
    -- POLICY, CLAIM, CLAIM_PAYMENT, CLAIMANT_PII, POLICY_HOLDER_SSN
    RESOURCE_ID     VARCHAR(50),
    -- e.g., claim number, policy number
    FIELD_ACCESSED  VARCHAR(100),
    -- e.g., HOLDER_SSN, CLAIMANT_PHONE (PII fields)
    SOURCE_IP       VARCHAR(45),
    SESSION_ID      VARCHAR(100),
    CORRELATION_ID  VARCHAR(100),
    DETAILS         VARCHAR(2000),
    CONSTRAINT PK_DATA_ACCESS_AUDIT PRIMARY KEY (ID)
);

CREATE INDEX IF NOT EXISTS IDX_DAA_USER ON DATA_ACCESS_AUDIT(USERNAME, EVENT_TIMESTAMP);
CREATE INDEX IF NOT EXISTS IDX_DAA_RESOURCE ON DATA_ACCESS_AUDIT(RESOURCE_TYPE, RESOURCE_ID);
CREATE INDEX IF NOT EXISTS IDX_DAA_TIMESTAMP ON DATA_ACCESS_AUDIT(EVENT_TIMESTAMP);


-- ============================================================================
-- 5. SECURITY CONFIGURATION TABLE
-- ============================================================================
-- Runtime security settings stored in the database for hot-reload.
-- Avoids requiring application restarts for security parameter changes.
-- ============================================================================

CREATE TABLE IF NOT EXISTS SECURITY_CONFIG (
    CONFIG_KEY      VARCHAR(100)    NOT NULL,
    CONFIG_VALUE    VARCHAR(500)    NOT NULL,
    DESCRIPTION     VARCHAR(1000),
    CATEGORY        VARCHAR(50)     DEFAULT 'GENERAL',
    LAST_MODIFIED   TIMESTAMP       DEFAULT CURRENT_TIMESTAMP,
    MODIFIED_BY     VARCHAR(50)     DEFAULT 'SYSTEM',
    CONSTRAINT PK_SECURITY_CONFIG PRIMARY KEY (CONFIG_KEY)
);

-- Seed security configuration
MERGE INTO SECURITY_CONFIG (CONFIG_KEY, CONFIG_VALUE, DESCRIPTION, CATEGORY) KEY(CONFIG_KEY) VALUES
('password.min.length',         '12',       'Minimum password length',                          'PASSWORD');
MERGE INTO SECURITY_CONFIG (CONFIG_KEY, CONFIG_VALUE, DESCRIPTION, CATEGORY) KEY(CONFIG_KEY) VALUES
('password.require.uppercase',  'true',     'Require at least one uppercase letter',            'PASSWORD');
MERGE INTO SECURITY_CONFIG (CONFIG_KEY, CONFIG_VALUE, DESCRIPTION, CATEGORY) KEY(CONFIG_KEY) VALUES
('password.require.digit',      'true',     'Require at least one digit',                       'PASSWORD');
MERGE INTO SECURITY_CONFIG (CONFIG_KEY, CONFIG_VALUE, DESCRIPTION, CATEGORY) KEY(CONFIG_KEY) VALUES
('password.require.special',    'true',     'Require at least one special character',            'PASSWORD');
MERGE INTO SECURITY_CONFIG (CONFIG_KEY, CONFIG_VALUE, DESCRIPTION, CATEGORY) KEY(CONFIG_KEY) VALUES
('password.max.age.days',       '90',       'Password expiration period in days',                'PASSWORD');
MERGE INTO SECURITY_CONFIG (CONFIG_KEY, CONFIG_VALUE, DESCRIPTION, CATEGORY) KEY(CONFIG_KEY) VALUES
('password.history.count',      '12',       'Number of previous passwords to remember',          'PASSWORD');
MERGE INTO SECURITY_CONFIG (CONFIG_KEY, CONFIG_VALUE, DESCRIPTION, CATEGORY) KEY(CONFIG_KEY) VALUES
('login.max.failed.attempts',   '5',        'Failed attempts before account lockout',            'LOCKOUT');
MERGE INTO SECURITY_CONFIG (CONFIG_KEY, CONFIG_VALUE, DESCRIPTION, CATEGORY) KEY(CONFIG_KEY) VALUES
('login.lockout.duration.min',  '30',       'Account lockout duration in minutes',               'LOCKOUT');
MERGE INTO SECURITY_CONFIG (CONFIG_KEY, CONFIG_VALUE, DESCRIPTION, CATEGORY) KEY(CONFIG_KEY) VALUES
('session.timeout.minutes',     '30',       'HTTP session idle timeout',                         'SESSION');
MERGE INTO SECURITY_CONFIG (CONFIG_KEY, CONFIG_VALUE, DESCRIPTION, CATEGORY) KEY(CONFIG_KEY) VALUES
('session.max.concurrent',      '1',        'Max concurrent sessions per user',                  'SESSION');
MERGE INTO SECURITY_CONFIG (CONFIG_KEY, CONFIG_VALUE, DESCRIPTION, CATEGORY) KEY(CONFIG_KEY) VALUES
('audit.retention.years',       '7',        'Years to retain audit records',                     'AUDIT');
MERGE INTO SECURITY_CONFIG (CONFIG_KEY, CONFIG_VALUE, DESCRIPTION, CATEGORY) KEY(CONFIG_KEY) VALUES
('pii.masking.enabled',         'true',     'Mask PII in logs and non-privileged views',         'PRIVACY');
MERGE INTO SECURITY_CONFIG (CONFIG_KEY, CONFIG_VALUE, DESCRIPTION, CATEGORY) KEY(CONFIG_KEY) VALUES
('ssl.required',                'true',     'Require HTTPS for all requests',                    'TRANSPORT');


-- ============================================================================
-- 6. SEED DATA — Default Users and Roles
-- ============================================================================
-- Development/test users with pre-computed SHA-256 password hashes.
--
-- IMPORTANT: These are development-only credentials.
-- Production deployments MUST:
--   1. Delete all seed users
--   2. Create users via the admin interface or provisioning script
--   3. Use a secure password hashing scheme (bcrypt/scrypt/PBKDF2)
--      rather than bare SHA-256
--   4. Enforce password complexity requirements
--
-- Password hashes below:
--   "adjuster123"  => SHA-256 hex
--   "manager456"   => SHA-256 hex
--   "admin789"     => SHA-256 hex
--   "readonly000"  => SHA-256 hex
--   "batch_svc"    => SHA-256 hex
--   "ws_client"    => SHA-256 hex
-- ============================================================================

-- Users
MERGE INTO APP_USER (USERNAME, PASSWORD, FIRST_NAME, LAST_NAME, EMAIL) KEY(USERNAME) VALUES
('adjuster1',   'ecd71870d1963316a97e3ac3408c9835ad8cf0f3c1bc703527c30265534f75ae',
 'Sarah', 'Chen', 'sarah.chen@greylegacy.com');
MERGE INTO APP_USER (USERNAME, PASSWORD, FIRST_NAME, LAST_NAME, EMAIL) KEY(USERNAME) VALUES
('adjuster2',   'ecd71870d1963316a97e3ac3408c9835ad8cf0f3c1bc703527c30265534f75ae',
 'James', 'Wilson', 'james.wilson@greylegacy.com');
MERGE INTO APP_USER (USERNAME, PASSWORD, FIRST_NAME, LAST_NAME, EMAIL) KEY(USERNAME) VALUES
('manager1',    '5994471abb01112afcc18159f6cc74b4f511b99806da59b3caf5a9c173cacfc5',
 'Maria', 'Rodriguez', 'maria.rodriguez@greylegacy.com');
MERGE INTO APP_USER (USERNAME, PASSWORD, FIRST_NAME, LAST_NAME, EMAIL) KEY(USERNAME) VALUES
('admin',       '15e2b0d3c33891ebb0f1ef609ec419420c20e320ce94c65fbc8c3312448eb225',
 'System', 'Administrator', 'admin@greylegacy.com');
MERGE INTO APP_USER (USERNAME, PASSWORD, FIRST_NAME, LAST_NAME, EMAIL) KEY(USERNAME) VALUES
('auditor',     '7c211433f02024a5b5903e1684c1d320bf49a35b708a24db1e06e284d4e94053',
 'Robert', 'Kim', 'robert.kim@greylegacy.com');
MERGE INTO APP_USER (USERNAME, PASSWORD, FIRST_NAME, LAST_NAME, EMAIL) KEY(USERNAME) VALUES
('batch_svc',   'b5bea41b6c623f7c09f1bf24dcae58ebab3c0cdd90ad966bc43a45b44867e12b',
 'Batch', 'Service', 'batch@greylegacy.com');
MERGE INTO APP_USER (USERNAME, PASSWORD, FIRST_NAME, LAST_NAME, EMAIL) KEY(USERNAME) VALUES
('ws_client',   'd7a8fbb307d7809469ca9abcb0082e4f8d5651e46d3cdb762d02d0bf37c9e592',
 'WS', 'Client', 'ws-client@greylegacy.com');

-- Role assignments
MERGE INTO APP_USER_ROLE (USERNAME, ROLE_NAME) KEY(USERNAME, ROLE_NAME) VALUES ('adjuster1', 'CLAIMS_ADJUSTER');
MERGE INTO APP_USER_ROLE (USERNAME, ROLE_NAME) KEY(USERNAME, ROLE_NAME) VALUES ('adjuster1', 'READONLY');
MERGE INTO APP_USER_ROLE (USERNAME, ROLE_NAME) KEY(USERNAME, ROLE_NAME) VALUES ('adjuster2', 'CLAIMS_ADJUSTER');
MERGE INTO APP_USER_ROLE (USERNAME, ROLE_NAME) KEY(USERNAME, ROLE_NAME) VALUES ('adjuster2', 'READONLY');
MERGE INTO APP_USER_ROLE (USERNAME, ROLE_NAME) KEY(USERNAME, ROLE_NAME) VALUES ('manager1',  'CLAIMS_MANAGER');
MERGE INTO APP_USER_ROLE (USERNAME, ROLE_NAME) KEY(USERNAME, ROLE_NAME) VALUES ('manager1',  'CLAIMS_ADJUSTER');
MERGE INTO APP_USER_ROLE (USERNAME, ROLE_NAME) KEY(USERNAME, ROLE_NAME) VALUES ('manager1',  'READONLY');
MERGE INTO APP_USER_ROLE (USERNAME, ROLE_NAME) KEY(USERNAME, ROLE_NAME) VALUES ('admin',     'ADMIN');
MERGE INTO APP_USER_ROLE (USERNAME, ROLE_NAME) KEY(USERNAME, ROLE_NAME) VALUES ('admin',     'CLAIMS_MANAGER');
MERGE INTO APP_USER_ROLE (USERNAME, ROLE_NAME) KEY(USERNAME, ROLE_NAME) VALUES ('admin',     'CLAIMS_ADJUSTER');
MERGE INTO APP_USER_ROLE (USERNAME, ROLE_NAME) KEY(USERNAME, ROLE_NAME) VALUES ('admin',     'READONLY');
MERGE INTO APP_USER_ROLE (USERNAME, ROLE_NAME) KEY(USERNAME, ROLE_NAME) VALUES ('auditor',   'READONLY');
MERGE INTO APP_USER_ROLE (USERNAME, ROLE_NAME) KEY(USERNAME, ROLE_NAME) VALUES ('batch_svc', 'SYSTEM_BATCH');
MERGE INTO APP_USER_ROLE (USERNAME, ROLE_NAME) KEY(USERNAME, ROLE_NAME) VALUES ('ws_client', 'WS_CLIENT');
MERGE INTO APP_USER_ROLE (USERNAME, ROLE_NAME) KEY(USERNAME, ROLE_NAME) VALUES ('ws_client', 'CLAIMS_ADJUSTER');


-- ============================================================================
-- 7. AUDIT QUERY PERFORMANCE VIEWS
-- ============================================================================

-- Failed login attempts in the last 24 hours (security monitoring)
CREATE OR REPLACE VIEW V_RECENT_FAILED_LOGINS AS
SELECT
    USERNAME,
    COUNT(*)            AS FAILURE_COUNT,
    MAX(EVENT_TIMESTAMP) AS LAST_FAILURE,
    MIN(EVENT_TIMESTAMP) AS FIRST_FAILURE
FROM LOGIN_AUDIT
WHERE EVENT_TYPE = 'LOGIN_FAILURE'
  AND EVENT_TIMESTAMP > DATEADD('HOUR', -24, CURRENT_TIMESTAMP)
GROUP BY USERNAME
ORDER BY FAILURE_COUNT DESC;

-- PII access log for compliance reporting
CREATE OR REPLACE VIEW V_PII_ACCESS_LOG AS
SELECT
    da.EVENT_TIMESTAMP,
    da.USERNAME,
    da.ACTION,
    da.RESOURCE_TYPE,
    da.RESOURCE_ID,
    da.FIELD_ACCESSED,
    da.SOURCE_IP
FROM DATA_ACCESS_AUDIT da
WHERE da.RESOURCE_TYPE IN ('CLAIMANT_PII', 'POLICY_HOLDER_SSN')
ORDER BY da.EVENT_TIMESTAMP DESC;

-- Active user sessions summary
CREATE OR REPLACE VIEW V_USER_ACTIVITY_SUMMARY AS
SELECT
    u.USERNAME,
    u.FIRST_NAME || ' ' || u.LAST_NAME AS FULL_NAME,
    u.LAST_LOGIN,
    u.FAILED_ATTEMPTS,
    u.LOCKED,
    u.ENABLED,
    STRING_AGG(r.ROLE_NAME, ', ') AS ROLES
FROM APP_USER u
LEFT JOIN APP_USER_ROLE r ON r.USERNAME = u.USERNAME
GROUP BY u.USERNAME, u.FIRST_NAME, u.LAST_NAME,
         u.LAST_LOGIN, u.FAILED_ATTEMPTS, u.LOCKED, u.ENABLED;


-- ============================================================================
-- END OF schema-security.sql
-- ============================================================================
