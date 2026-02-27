-- ============================================================================
-- Grey Legacy - Insurance Claims Processing System
-- Extended Schema: Stored Procedures, Triggers, Functions & Reference Data
-- ============================================================================
-- Target:      H2 (development) / PostgreSQL (production)
-- Depends on:  schema.sql (base tables must exist first)
-- Author:      Grey Legacy DBA Team
-- Created:     2026-02-27
-- ============================================================================
--
-- COMPATIBILITY NOTES:
--   - H2 supports CREATE ALIAS for Java/SQL stored procedures.
--   - PostgreSQL uses CREATE OR REPLACE FUNCTION ... LANGUAGE plpgsql.
--   - Triggers use H2-compatible syntax with PostgreSQL equivalents in
--     comment blocks.
--   - Standard SQL is used wherever both engines agree.
--
-- TABLE OF CONTENTS:
--   1.  Sequences for new tables
--   2.  Reference / Lookup Tables
--   3.  SCD Type 2 History Table (POLICY_HISTORY)
--   4.  Soft-Delete Column Additions & Active Views
--   5.  Composite Key Join Table (CLAIM_COVERAGE_LINK)
--   6.  Schema Alterations for Procedure Support
--   7.  Stored Procedures
--   8.  Triggers
--   9.  Functions
--  10.  Reference Data Population (Batch Inserts)
--  11.  Pessimistic Locking Procedure
--  12.  Optimistic Locking Commentary
-- ============================================================================


-- ============================================================================
-- 1. SEQUENCES FOR NEW TABLES
-- ============================================================================

CREATE SEQUENCE IF NOT EXISTS POLICY_HISTORY_SEQ START WITH 1 INCREMENT BY 1;
CREATE SEQUENCE IF NOT EXISTS CLAIM_STATUS_REF_SEQ START WITH 1 INCREMENT BY 1;
CREATE SEQUENCE IF NOT EXISTS COVERAGE_TYPE_REF_SEQ START WITH 1 INCREMENT BY 1;
CREATE SEQUENCE IF NOT EXISTS STATE_PROVINCE_REF_SEQ START WITH 1 INCREMENT BY 1;
CREATE SEQUENCE IF NOT EXISTS CLAIM_NUMBER_SEQ START WITH 1 INCREMENT BY 1;


-- ============================================================================
-- 2. REFERENCE / LOOKUP TABLES
-- ============================================================================

-- ----------------------------------------------------------------------------
-- 2a. CLAIM_STATUS_REF
--     Canonical list of every claim status the system recognises.
--     TERMINAL_STATE = TRUE means the claim cannot transition out of this
--     status (e.g. CLOSED, DENIED, WITHDRAWN).
-- ----------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS CLAIM_STATUS_REF (
    ID              BIGINT          NOT NULL DEFAULT NEXT VALUE FOR CLAIM_STATUS_REF_SEQ,
    CODE            VARCHAR(30)     NOT NULL,
    DISPLAY_NAME    VARCHAR(100)    NOT NULL,
    DESCRIPTION     VARCHAR(500),
    SORT_ORDER      INTEGER         NOT NULL DEFAULT 0,
    ACTIVE          BOOLEAN         NOT NULL DEFAULT TRUE,
    TERMINAL_STATE  BOOLEAN         NOT NULL DEFAULT FALSE,
    CREATED_DATE    TIMESTAMP       DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT PK_CLAIM_STATUS_REF PRIMARY KEY (ID),
    CONSTRAINT UK_CLAIM_STATUS_CODE UNIQUE (CODE)
);

CREATE INDEX IF NOT EXISTS IDX_CLAIM_STATUS_REF_ACTIVE ON CLAIM_STATUS_REF(ACTIVE);

-- ----------------------------------------------------------------------------
-- 2b. COVERAGE_TYPE_REF
--     Defines available coverage categories and their default financial terms.
-- ----------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS COVERAGE_TYPE_REF (
    ID                  BIGINT          NOT NULL DEFAULT NEXT VALUE FOR COVERAGE_TYPE_REF_SEQ,
    CODE                VARCHAR(30)     NOT NULL,
    DESCRIPTION         VARCHAR(500)    NOT NULL,
    LINE_OF_BUSINESS    VARCHAR(50),
    DEFAULT_DEDUCTIBLE  DECIMAL(10,2)   NOT NULL DEFAULT 500.00,
    DEFAULT_LIMIT       DECIMAL(14,2)   NOT NULL DEFAULT 100000.00,
    ACTIVE              BOOLEAN         NOT NULL DEFAULT TRUE,
    CREATED_DATE        TIMESTAMP       DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT PK_COVERAGE_TYPE_REF PRIMARY KEY (ID),
    CONSTRAINT UK_COVERAGE_TYPE_CODE UNIQUE (CODE)
);

-- ----------------------------------------------------------------------------
-- 2c. STATE_PROVINCE_REF
--     Jurisdiction reference for regulatory and tax calculations.
-- ----------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS STATE_PROVINCE_REF (
    ID                  BIGINT          NOT NULL DEFAULT NEXT VALUE FOR STATE_PROVINCE_REF_SEQ,
    CODE                VARCHAR(5)      NOT NULL,
    NAME                VARCHAR(100)    NOT NULL,
    COUNTRY             VARCHAR(3)      NOT NULL DEFAULT 'US',
    TAX_RATE            DECIMAL(5,4)    NOT NULL DEFAULT 0.0000,
    REGULATORY_BODY     VARCHAR(200),
    FILING_DEADLINE_DAYS INTEGER        DEFAULT 30,
    ACTIVE              BOOLEAN         NOT NULL DEFAULT TRUE,
    CREATED_DATE        TIMESTAMP       DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT PK_STATE_PROVINCE_REF PRIMARY KEY (ID),
    CONSTRAINT UK_STATE_CODE UNIQUE (CODE)
);


-- ============================================================================
-- 3. SCD TYPE 2 HISTORY TABLE - POLICY_HISTORY
-- ============================================================================
-- Slowly Changing Dimension Type 2 implementation for the POLICY table.
-- Every time a tracked attribute changes, the old "current" row is expired
-- (EFFECTIVE_TO set, IS_CURRENT = FALSE) and a new row is inserted with
-- IS_CURRENT = TRUE and EFFECTIVE_TO = NULL.
-- ============================================================================

CREATE TABLE IF NOT EXISTS POLICY_HISTORY (
    HISTORY_ID          BIGINT          NOT NULL DEFAULT NEXT VALUE FOR POLICY_HISTORY_SEQ,
    POLICY_ID           BIGINT          NOT NULL,
    POLICY_NUMBER       VARCHAR(20)     NOT NULL,
    STATUS              VARCHAR(20)     NOT NULL,
    POLICY_TYPE         VARCHAR(30)     NOT NULL,
    HOLDER_FIRST_NAME   VARCHAR(50)     NOT NULL,
    HOLDER_LAST_NAME    VARCHAR(50)     NOT NULL,
    PREMIUM_AMOUNT      DECIMAL(12,2),
    COVERAGE_LIMIT      DECIMAL(14,2),
    DEDUCTIBLE          DECIMAL(10,2),
    EFFECTIVE_FROM      TIMESTAMP       NOT NULL DEFAULT CURRENT_TIMESTAMP,
    EFFECTIVE_TO        TIMESTAMP,       -- NULL means this is the current record
    IS_CURRENT          BOOLEAN         NOT NULL DEFAULT TRUE,
    CHANGE_REASON       VARCHAR(500),
    CHANGED_BY          VARCHAR(50)     NOT NULL DEFAULT 'SYSTEM',
    CREATED_DATE        TIMESTAMP       DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT PK_POLICY_HISTORY PRIMARY KEY (HISTORY_ID),
    CONSTRAINT FK_POLICY_HISTORY_POLICY FOREIGN KEY (POLICY_ID) REFERENCES POLICY(ID)
);

-- Composite index for fast lookup of the current version of a given policy
CREATE INDEX IF NOT EXISTS IDX_POLICY_HIST_CURRENT ON POLICY_HISTORY(POLICY_ID, IS_CURRENT);
-- Supporting index for temporal queries
CREATE INDEX IF NOT EXISTS IDX_POLICY_HIST_EFF ON POLICY_HISTORY(POLICY_ID, EFFECTIVE_FROM, EFFECTIVE_TO);


-- ============================================================================
-- 4. SOFT-DELETE SUPPORT
-- ============================================================================
-- Rather than physically removing rows, we flag them as deleted and record
-- the deletion timestamp.  Application queries should use the views below
-- to automatically exclude soft-deleted records.
-- ============================================================================

-- 4a. Add soft-delete columns to existing tables
ALTER TABLE POLICY   ADD COLUMN IF NOT EXISTS DELETED      BOOLEAN   DEFAULT FALSE;
ALTER TABLE POLICY   ADD COLUMN IF NOT EXISTS DELETED_DATE TIMESTAMP;

ALTER TABLE CLAIM    ADD COLUMN IF NOT EXISTS DELETED      BOOLEAN   DEFAULT FALSE;
ALTER TABLE CLAIM    ADD COLUMN IF NOT EXISTS DELETED_DATE TIMESTAMP;

ALTER TABLE ADJUSTER ADD COLUMN IF NOT EXISTS DELETED      BOOLEAN   DEFAULT FALSE;
ALTER TABLE ADJUSTER ADD COLUMN IF NOT EXISTS DELETED_DATE TIMESTAMP;

-- 4b. Filtered views that exclude soft-deleted rows
-- These views should be used by the application layer instead of direct
-- table access wherever possible.

CREATE OR REPLACE VIEW V_ACTIVE_POLICIES AS
SELECT
    ID, VERSION, POLICY_NUMBER, STATUS, POLICY_TYPE,
    HOLDER_FIRST_NAME, HOLDER_LAST_NAME, HOLDER_SSN,
    HOLDER_EMAIL, HOLDER_PHONE,
    EFFECTIVE_DATE, EXPIRATION_DATE,
    PREMIUM_AMOUNT, COVERAGE_LIMIT, DEDUCTIBLE,
    AGENT_CODE, UNDERWRITER_CODE,
    CREATED_BY, CREATED_DATE, UPDATED_BY, UPDATED_DATE
FROM POLICY
WHERE DELETED = FALSE;

CREATE OR REPLACE VIEW V_ACTIVE_CLAIMS AS
SELECT
    c.ID, c.VERSION, c.CLAIM_NUMBER, c.POLICY_ID, c.STATUS,
    c.CLAIM_TYPE, c.LOSS_DATE, c.REPORTED_DATE,
    c.LOSS_DESCRIPTION, c.LOSS_LOCATION,
    c.CLAIMANT_FIRST_NAME, c.CLAIMANT_LAST_NAME,
    c.CLAIMANT_PHONE, c.CLAIMANT_EMAIL,
    c.ADJUSTER_CODE, c.ESTIMATED_LOSS, c.APPROVED_AMOUNT,
    c.DEDUCTIBLE_APPLIED, c.FRAUD_RISK_LEVEL, c.FRAUD_SCORE,
    c.FRAUD_SCORED_DATE, c.CLOSED_DATE, c.CLOSED_REASON,
    c.CREATED_BY, c.CREATED_DATE, c.UPDATED_BY, c.UPDATED_DATE
FROM CLAIM c
WHERE c.DELETED = FALSE;

-- Convenience view: active adjusters with caseload summary
CREATE OR REPLACE VIEW V_ACTIVE_ADJUSTERS AS
SELECT
    ID, ADJUSTER_CODE, FIRST_NAME, LAST_NAME,
    EMAIL, PHONE, SPECIALIZATION,
    MAX_CASELOAD, CURRENT_CASELOAD,
    CREATED_DATE
FROM ADJUSTER
WHERE DELETED = FALSE
  AND ACTIVE = TRUE;


-- ============================================================================
-- 5. COMPOSITE KEY JOIN TABLE - CLAIM_COVERAGE_LINK
-- ============================================================================
-- Associates a claim with one or more coverage types and tracks how much
-- of the claim amount is allocated to each coverage bucket.
-- ============================================================================

CREATE TABLE IF NOT EXISTS CLAIM_COVERAGE_LINK (
    CLAIM_ID            BIGINT          NOT NULL,
    COVERAGE_TYPE_CODE  VARCHAR(30)     NOT NULL,
    ALLOCATED_AMOUNT    DECIMAL(14,2)   NOT NULL DEFAULT 0.00,
    NOTES               VARCHAR(1000),
    CREATED_BY          VARCHAR(50)     DEFAULT 'SYSTEM',
    CREATED_DATE        TIMESTAMP       DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT PK_CLAIM_COVERAGE PRIMARY KEY (CLAIM_ID, COVERAGE_TYPE_CODE),
    CONSTRAINT FK_CCL_CLAIM FOREIGN KEY (CLAIM_ID) REFERENCES CLAIM(ID),
    CONSTRAINT FK_CCL_COVERAGE FOREIGN KEY (COVERAGE_TYPE_CODE)
        REFERENCES COVERAGE_TYPE_REF(CODE)
);

CREATE INDEX IF NOT EXISTS IDX_CCL_COVERAGE ON CLAIM_COVERAGE_LINK(COVERAGE_TYPE_CODE);


-- ============================================================================
-- 6. SCHEMA ALTERATIONS FOR PROCEDURE SUPPORT
-- ============================================================================

-- RESERVE_AMOUNT: the calculated reserve held against a claim.
-- Updated by SP_CALCULATE_CLAIM_RESERVE.
ALTER TABLE CLAIM ADD COLUMN IF NOT EXISTS RESERVE_AMOUNT DECIMAL(14,2) DEFAULT 0.00;


-- ============================================================================
-- 7. STORED PROCEDURES
-- ============================================================================
-- H2 uses CREATE ALIAS to define stored procedures as SQL or Java methods.
-- PostgreSQL equivalents are provided in comment blocks.
--
-- For each procedure the H2 version is active; uncomment the PostgreSQL
-- block and comment the H2 block when deploying to production.
-- ============================================================================

-- ----------------------------------------------------------------------------
-- 7a. SP_CALCULATE_CLAIM_RESERVE
--     Calculates the outstanding reserve for a claim.
--     Reserve = ESTIMATED_LOSS - DEDUCTIBLE_APPLIED - SUM(payments made)
--     Result is clamped to zero (never negative).
-- ----------------------------------------------------------------------------

/*
 * --- PostgreSQL Version ---
 * CREATE OR REPLACE FUNCTION SP_CALCULATE_CLAIM_RESERVE(p_claim_id BIGINT)
 * RETURNS DECIMAL(14,2) AS $$
 * DECLARE
 *     v_estimated_loss   DECIMAL(14,2);
 *     v_deductible       DECIMAL(14,2);
 *     v_total_payments   DECIMAL(14,2);
 *     v_reserve          DECIMAL(14,2);
 * BEGIN
 *     SELECT COALESCE(ESTIMATED_LOSS, 0), COALESCE(DEDUCTIBLE_APPLIED, 0)
 *       INTO v_estimated_loss, v_deductible
 *       FROM CLAIM
 *      WHERE ID = p_claim_id;
 *
 *     IF NOT FOUND THEN
 *         RAISE EXCEPTION 'Claim % not found', p_claim_id;
 *     END IF;
 *
 *     SELECT COALESCE(SUM(AMOUNT), 0)
 *       INTO v_total_payments
 *       FROM CLAIM_PAYMENT
 *      WHERE CLAIM_ID = p_claim_id
 *        AND PAYMENT_STATUS IN ('APPROVED', 'PROCESSED', 'ISSUED');
 *
 *     v_reserve := GREATEST(v_estimated_loss - v_deductible - v_total_payments, 0);
 *
 *     UPDATE CLAIM
 *        SET RESERVE_AMOUNT = v_reserve,
 *            UPDATED_DATE   = CURRENT_TIMESTAMP,
 *            UPDATED_BY     = 'SP_CALC_RESERVE'
 *      WHERE ID = p_claim_id;
 *
 *     RETURN v_reserve;
 * END;
 * $$ LANGUAGE plpgsql;
 */

-- H2 Version (CREATE ALIAS with inline SQL)
CREATE ALIAS IF NOT EXISTS SP_CALCULATE_CLAIM_RESERVE AS $$
import java.sql.*;
@CODE
BigDecimal spCalculateClaimReserve(Connection conn, long claimId) throws SQLException {
    // Fetch estimated loss and deductible
    BigDecimal estimatedLoss = BigDecimal.ZERO;
    BigDecimal deductible = BigDecimal.ZERO;
    try (PreparedStatement ps = conn.prepareStatement(
            "SELECT COALESCE(ESTIMATED_LOSS, 0), COALESCE(DEDUCTIBLE_APPLIED, 0) FROM CLAIM WHERE ID = ?")) {
        ps.setLong(1, claimId);
        try (ResultSet rs = ps.executeQuery()) {
            if (rs.next()) {
                estimatedLoss = rs.getBigDecimal(1);
                deductible = rs.getBigDecimal(2);
            } else {
                throw new SQLException("Claim " + claimId + " not found");
            }
        }
    }
    // Sum all approved/processed/issued payments
    BigDecimal totalPayments = BigDecimal.ZERO;
    try (PreparedStatement ps = conn.prepareStatement(
            "SELECT COALESCE(SUM(AMOUNT), 0) FROM CLAIM_PAYMENT " +
            "WHERE CLAIM_ID = ? AND PAYMENT_STATUS IN ('APPROVED','PROCESSED','ISSUED')")) {
        ps.setLong(1, claimId);
        try (ResultSet rs = ps.executeQuery()) {
            if (rs.next()) {
                totalPayments = rs.getBigDecimal(1);
            }
        }
    }
    // Reserve = max(estimated - deductible - payments, 0)
    BigDecimal reserve = estimatedLoss.subtract(deductible).subtract(totalPayments);
    if (reserve.compareTo(BigDecimal.ZERO) < 0) {
        reserve = BigDecimal.ZERO;
    }
    // Update the claim row
    try (PreparedStatement ps = conn.prepareStatement(
            "UPDATE CLAIM SET RESERVE_AMOUNT = ?, UPDATED_DATE = CURRENT_TIMESTAMP, " +
            "UPDATED_BY = 'SP_CALC_RESERVE' WHERE ID = ?")) {
        ps.setBigDecimal(1, reserve);
        ps.setLong(2, claimId);
        ps.executeUpdate();
    }
    return reserve;
}
$$;

-- ----------------------------------------------------------------------------
-- 7b. SP_CLOSE_AGED_CLAIMS
--     Finds all claims that are older than the specified number of days
--     (measured from LOSS_DATE) and are NOT in a terminal status.
--     Moves them to 'CLOSED' with reason 'AUTO_AGED_CLOSURE' and creates
--     an audit trail entry for each.
--
--     Parameters:
--       p_age_threshold_days  - claims older than this are closed
--
--     Returns the count of claims closed.
-- ----------------------------------------------------------------------------

/*
 * --- PostgreSQL Version ---
 * CREATE OR REPLACE FUNCTION SP_CLOSE_AGED_CLAIMS(p_age_threshold_days INTEGER)
 * RETURNS INTEGER AS $$
 * DECLARE
 *     v_count       INTEGER := 0;
 *     v_claim_rec   RECORD;
 * BEGIN
 *     FOR v_claim_rec IN
 *         SELECT c.ID, c.STATUS
 *           FROM CLAIM c
 *          WHERE c.LOSS_DATE < CURRENT_DATE - p_age_threshold_days
 *            AND c.STATUS NOT IN (
 *                SELECT CODE FROM CLAIM_STATUS_REF WHERE TERMINAL_STATE = TRUE
 *            )
 *            AND (c.DELETED = FALSE OR c.DELETED IS NULL)
 *     LOOP
 *         UPDATE CLAIM
 *            SET STATUS        = 'CLOSED',
 *                CLOSED_DATE   = CURRENT_TIMESTAMP,
 *                CLOSED_REASON = 'AUTO_AGED_CLOSURE',
 *                UPDATED_BY    = 'SP_CLOSE_AGED',
 *                UPDATED_DATE  = CURRENT_TIMESTAMP
 *          WHERE ID = v_claim_rec.ID;
 *
 *         INSERT INTO CLAIM_AUDIT (ID, CLAIM_ID, EVENT_TIMESTAMP, EVENT_TYPE,
 *                     PREVIOUS_VALUE, NEW_VALUE, PERFORMED_BY, DESCRIPTION,
 *                     CREATED_BY, CREATED_DATE)
 *         VALUES (NEXTVAL('AUDIT_SEQ'), v_claim_rec.ID, CURRENT_TIMESTAMP,
 *                 'STATUS_CHANGE', v_claim_rec.STATUS, 'CLOSED',
 *                 'SP_CLOSE_AGED',
 *                 'Automatic closure: claim exceeded ' || p_age_threshold_days || ' day threshold',
 *                 'SYSTEM', CURRENT_TIMESTAMP);
 *
 *         v_count := v_count + 1;
 *     END LOOP;
 *
 *     RETURN v_count;
 * END;
 * $$ LANGUAGE plpgsql;
 */

-- H2 Version
CREATE ALIAS IF NOT EXISTS SP_CLOSE_AGED_CLAIMS AS $$
import java.sql.*;
@CODE
int spCloseAgedClaims(Connection conn, int ageThresholdDays) throws SQLException {
    int count = 0;
    // Find claims past the age threshold that are not in terminal status
    String selectSql =
        "SELECT c.ID, c.STATUS FROM CLAIM c " +
        "WHERE c.LOSS_DATE < DATEADD('DAY', -?, CURRENT_DATE) " +
        "AND c.STATUS NOT IN (" +
        "    SELECT CODE FROM CLAIM_STATUS_REF WHERE TERMINAL_STATE = TRUE" +
        ") " +
        "AND (c.DELETED = FALSE OR c.DELETED IS NULL)";
    try (PreparedStatement sel = conn.prepareStatement(selectSql)) {
        sel.setInt(1, ageThresholdDays);
        try (ResultSet rs = sel.executeQuery()) {
            while (rs.next()) {
                long claimId = rs.getLong("ID");
                String oldStatus = rs.getString("STATUS");

                // Close the claim
                try (PreparedStatement upd = conn.prepareStatement(
                        "UPDATE CLAIM SET STATUS = 'CLOSED', CLOSED_DATE = CURRENT_TIMESTAMP, " +
                        "CLOSED_REASON = 'AUTO_AGED_CLOSURE', UPDATED_BY = 'SP_CLOSE_AGED', " +
                        "UPDATED_DATE = CURRENT_TIMESTAMP WHERE ID = ?")) {
                    upd.setLong(1, claimId);
                    upd.executeUpdate();
                }

                // Audit entry
                try (PreparedStatement ins = conn.prepareStatement(
                        "INSERT INTO CLAIM_AUDIT (ID, CLAIM_ID, EVENT_TIMESTAMP, EVENT_TYPE, " +
                        "PREVIOUS_VALUE, NEW_VALUE, PERFORMED_BY, DESCRIPTION, " +
                        "CREATED_BY, CREATED_DATE) " +
                        "VALUES (NEXT VALUE FOR AUDIT_SEQ, ?, CURRENT_TIMESTAMP, 'STATUS_CHANGE', " +
                        "?, 'CLOSED', 'SP_CLOSE_AGED', " +
                        "'Automatic closure: claim exceeded ' || CAST(? AS VARCHAR) || ' day threshold', " +
                        "'SYSTEM', CURRENT_TIMESTAMP)")) {
                    ins.setLong(1, claimId);
                    ins.setString(2, oldStatus);
                    ins.setInt(3, ageThresholdDays);
                    ins.executeUpdate();
                }
                count++;
            }
        }
    }
    return count;
}
$$;

-- ----------------------------------------------------------------------------
-- 7c. SP_POLICY_SCD_SNAPSHOT
--     Captures the current state of a policy into the POLICY_HISTORY table.
--     The previous "current" history row is expired (EFFECTIVE_TO set,
--     IS_CURRENT = FALSE) before the new snapshot is inserted.
--
--     Parameters:
--       p_policy_id    - the ID of the policy to snapshot
--       p_change_reason - free-text reason for the change
--       p_changed_by   - user performing the change
-- ----------------------------------------------------------------------------

/*
 * --- PostgreSQL Version ---
 * CREATE OR REPLACE FUNCTION SP_POLICY_SCD_SNAPSHOT(
 *     p_policy_id    BIGINT,
 *     p_change_reason VARCHAR(500),
 *     p_changed_by   VARCHAR(50)
 * ) RETURNS VOID AS $$
 * BEGIN
 *     -- Expire the current record
 *     UPDATE POLICY_HISTORY
 *        SET EFFECTIVE_TO = CURRENT_TIMESTAMP,
 *            IS_CURRENT   = FALSE
 *      WHERE POLICY_ID  = p_policy_id
 *        AND IS_CURRENT  = TRUE;
 *
 *     -- Insert new current snapshot
 *     INSERT INTO POLICY_HISTORY (
 *         HISTORY_ID, POLICY_ID, POLICY_NUMBER, STATUS, POLICY_TYPE,
 *         HOLDER_FIRST_NAME, HOLDER_LAST_NAME,
 *         PREMIUM_AMOUNT, COVERAGE_LIMIT, DEDUCTIBLE,
 *         EFFECTIVE_FROM, EFFECTIVE_TO, IS_CURRENT,
 *         CHANGE_REASON, CHANGED_BY, CREATED_DATE
 *     )
 *     SELECT
 *         NEXTVAL('POLICY_HISTORY_SEQ'), p.ID, p.POLICY_NUMBER, p.STATUS, p.POLICY_TYPE,
 *         p.HOLDER_FIRST_NAME, p.HOLDER_LAST_NAME,
 *         p.PREMIUM_AMOUNT, p.COVERAGE_LIMIT, p.DEDUCTIBLE,
 *         CURRENT_TIMESTAMP, NULL, TRUE,
 *         p_change_reason, p_changed_by, CURRENT_TIMESTAMP
 *       FROM POLICY p
 *      WHERE p.ID = p_policy_id;
 *
 *     IF NOT FOUND THEN
 *         RAISE EXCEPTION 'Policy % not found', p_policy_id;
 *     END IF;
 * END;
 * $$ LANGUAGE plpgsql;
 */

-- H2 Version
CREATE ALIAS IF NOT EXISTS SP_POLICY_SCD_SNAPSHOT AS $$
import java.sql.*;
@CODE
void spPolicyScdSnapshot(Connection conn, long policyId, String changeReason, String changedBy) throws SQLException {
    // Expire the current history row for this policy
    try (PreparedStatement ps = conn.prepareStatement(
            "UPDATE POLICY_HISTORY SET EFFECTIVE_TO = CURRENT_TIMESTAMP, IS_CURRENT = FALSE " +
            "WHERE POLICY_ID = ? AND IS_CURRENT = TRUE")) {
        ps.setLong(1, policyId);
        ps.executeUpdate();
    }
    // Insert a new current snapshot from the live POLICY row
    try (PreparedStatement ps = conn.prepareStatement(
            "INSERT INTO POLICY_HISTORY (" +
            "  HISTORY_ID, POLICY_ID, POLICY_NUMBER, STATUS, POLICY_TYPE, " +
            "  HOLDER_FIRST_NAME, HOLDER_LAST_NAME, " +
            "  PREMIUM_AMOUNT, COVERAGE_LIMIT, DEDUCTIBLE, " +
            "  EFFECTIVE_FROM, EFFECTIVE_TO, IS_CURRENT, " +
            "  CHANGE_REASON, CHANGED_BY, CREATED_DATE" +
            ") SELECT " +
            "  NEXT VALUE FOR POLICY_HISTORY_SEQ, ID, POLICY_NUMBER, STATUS, POLICY_TYPE, " +
            "  HOLDER_FIRST_NAME, HOLDER_LAST_NAME, " +
            "  PREMIUM_AMOUNT, COVERAGE_LIMIT, DEDUCTIBLE, " +
            "  CURRENT_TIMESTAMP, NULL, TRUE, " +
            "  ?, ?, CURRENT_TIMESTAMP " +
            "FROM POLICY WHERE ID = ?")) {
        ps.setString(1, changeReason);
        ps.setString(2, changedBy);
        ps.setLong(3, policyId);
        int rows = ps.executeUpdate();
        if (rows == 0) {
            throw new SQLException("Policy " + policyId + " not found");
        }
    }
}
$$;

-- ----------------------------------------------------------------------------
-- 7d. SP_RECONCILE_PAYMENTS
--     Compares the sum of all issued/processed payments against the
--     APPROVED_AMOUNT for every claim.  Returns a result set of claims
--     where the totals do not match (i.e. discrepancies).
--
--     Columns returned:
--       CLAIM_ID, CLAIM_NUMBER, APPROVED_AMOUNT, TOTAL_PAID,
--       DISCREPANCY (approved - paid), DISCREPANCY_TYPE ('OVERPAID' / 'UNDERPAID')
-- ----------------------------------------------------------------------------

/*
 * --- PostgreSQL Version ---
 * CREATE OR REPLACE FUNCTION SP_RECONCILE_PAYMENTS()
 * RETURNS TABLE (
 *     CLAIM_ID         BIGINT,
 *     CLAIM_NUMBER     VARCHAR(20),
 *     APPROVED_AMOUNT  DECIMAL(14,2),
 *     TOTAL_PAID       DECIMAL(14,2),
 *     DISCREPANCY      DECIMAL(14,2),
 *     DISCREPANCY_TYPE VARCHAR(10)
 * ) AS $$
 * BEGIN
 *     RETURN QUERY
 *     SELECT
 *         c.ID                                         AS CLAIM_ID,
 *         c.CLAIM_NUMBER                               AS CLAIM_NUMBER,
 *         COALESCE(c.APPROVED_AMOUNT, 0)               AS APPROVED_AMOUNT,
 *         COALESCE(SUM(cp.AMOUNT), 0)                  AS TOTAL_PAID,
 *         COALESCE(c.APPROVED_AMOUNT, 0) - COALESCE(SUM(cp.AMOUNT), 0) AS DISCREPANCY,
 *         CASE
 *             WHEN COALESCE(SUM(cp.AMOUNT), 0) > COALESCE(c.APPROVED_AMOUNT, 0)
 *                 THEN 'OVERPAID'
 *             ELSE 'UNDERPAID'
 *         END                                          AS DISCREPANCY_TYPE
 *       FROM CLAIM c
 *       LEFT JOIN CLAIM_PAYMENT cp
 *         ON cp.CLAIM_ID = c.ID
 *        AND cp.PAYMENT_STATUS IN ('APPROVED', 'PROCESSED', 'ISSUED')
 *      WHERE c.APPROVED_AMOUNT IS NOT NULL
 *        AND (c.DELETED = FALSE OR c.DELETED IS NULL)
 *      GROUP BY c.ID, c.CLAIM_NUMBER, c.APPROVED_AMOUNT
 *     HAVING COALESCE(c.APPROVED_AMOUNT, 0) <> COALESCE(SUM(cp.AMOUNT), 0);
 * END;
 * $$ LANGUAGE plpgsql;
 */

-- H2 Version (returns ResultSet via ALIAS)
CREATE ALIAS IF NOT EXISTS SP_RECONCILE_PAYMENTS AS $$
import java.sql.*;
@CODE
ResultSet spReconcilePayments(Connection conn) throws SQLException {
    return conn.createStatement().executeQuery(
        "SELECT " +
        "  c.ID            AS CLAIM_ID, " +
        "  c.CLAIM_NUMBER  AS CLAIM_NUMBER, " +
        "  COALESCE(c.APPROVED_AMOUNT, 0) AS APPROVED_AMOUNT, " +
        "  COALESCE(SUM(cp.AMOUNT), 0)    AS TOTAL_PAID, " +
        "  COALESCE(c.APPROVED_AMOUNT, 0) - COALESCE(SUM(cp.AMOUNT), 0) AS DISCREPANCY, " +
        "  CASE " +
        "    WHEN COALESCE(SUM(cp.AMOUNT), 0) > COALESCE(c.APPROVED_AMOUNT, 0) " +
        "      THEN 'OVERPAID' " +
        "    ELSE 'UNDERPAID' " +
        "  END AS DISCREPANCY_TYPE " +
        "FROM CLAIM c " +
        "LEFT JOIN CLAIM_PAYMENT cp " +
        "  ON cp.CLAIM_ID = c.ID " +
        " AND cp.PAYMENT_STATUS IN ('APPROVED','PROCESSED','ISSUED') " +
        "WHERE c.APPROVED_AMOUNT IS NOT NULL " +
        "  AND (c.DELETED = FALSE OR c.DELETED IS NULL) " +
        "GROUP BY c.ID, c.CLAIM_NUMBER, c.APPROVED_AMOUNT " +
        "HAVING COALESCE(c.APPROVED_AMOUNT, 0) <> COALESCE(SUM(cp.AMOUNT), 0)"
    );
}
$$;

-- ----------------------------------------------------------------------------
-- 7e. SP_GENERATE_CLAIM_NUMBER
--     Generates the next claim number in the format 'CLM-YYYY-NNNNN'
--     where YYYY is the current four-digit year and NNNNN is a zero-padded
--     sequence number.
--
--     Uses CLAIM_NUMBER_SEQ to guarantee uniqueness under concurrent access.
-- ----------------------------------------------------------------------------

/*
 * --- PostgreSQL Version ---
 * CREATE OR REPLACE FUNCTION SP_GENERATE_CLAIM_NUMBER()
 * RETURNS VARCHAR(20) AS $$
 * DECLARE
 *     v_seq   BIGINT;
 *     v_year  VARCHAR(4);
 * BEGIN
 *     v_seq  := NEXTVAL('CLAIM_NUMBER_SEQ');
 *     v_year := TO_CHAR(CURRENT_DATE, 'YYYY');
 *     RETURN 'CLM-' || v_year || '-' || LPAD(v_seq::TEXT, 5, '0');
 * END;
 * $$ LANGUAGE plpgsql;
 */

-- H2 Version
CREATE ALIAS IF NOT EXISTS SP_GENERATE_CLAIM_NUMBER AS $$
import java.sql.*;
import java.time.Year;
@CODE
String spGenerateClaimNumber(Connection conn) throws SQLException {
    long seq;
    try (Statement st = conn.createStatement();
         ResultSet rs = st.executeQuery("SELECT NEXT VALUE FOR CLAIM_NUMBER_SEQ")) {
        rs.next();
        seq = rs.getLong(1);
    }
    String year = String.valueOf(Year.now().getValue());
    return String.format("CLM-%s-%05d", year, seq);
}
$$;


-- ============================================================================
-- 8. TRIGGERS
-- ============================================================================
-- H2 supports CREATE TRIGGER with source code or aliases.
-- PostgreSQL trigger syntax provided in comment blocks.
-- ============================================================================

-- ----------------------------------------------------------------------------
-- 8a. TRG_CLAIM_STATUS_CHANGE
--     AFTER UPDATE trigger on CLAIM.
--     When the STATUS column changes value, an audit row is inserted into
--     CLAIM_AUDIT capturing the old and new status plus a timestamp.
-- ----------------------------------------------------------------------------

/*
 * --- PostgreSQL Version ---
 * CREATE OR REPLACE FUNCTION fn_trg_claim_status_change()
 * RETURNS TRIGGER AS $$
 * BEGIN
 *     IF OLD.STATUS IS DISTINCT FROM NEW.STATUS THEN
 *         INSERT INTO CLAIM_AUDIT (
 *             ID, CLAIM_ID, EVENT_TIMESTAMP, EVENT_TYPE,
 *             PREVIOUS_VALUE, NEW_VALUE, PERFORMED_BY,
 *             DESCRIPTION, CREATED_BY, CREATED_DATE
 *         ) VALUES (
 *             NEXTVAL('AUDIT_SEQ'), NEW.ID, CURRENT_TIMESTAMP, 'STATUS_CHANGE',
 *             OLD.STATUS, NEW.STATUS, COALESCE(NEW.UPDATED_BY, 'SYSTEM'),
 *             'Status changed from ' || OLD.STATUS || ' to ' || NEW.STATUS,
 *             'TRG_STATUS', CURRENT_TIMESTAMP
 *         );
 *     END IF;
 *     RETURN NEW;
 * END;
 * $$ LANGUAGE plpgsql;
 *
 * CREATE TRIGGER TRG_CLAIM_STATUS_CHANGE
 *     AFTER UPDATE ON CLAIM
 *     FOR EACH ROW
 *     EXECUTE FUNCTION fn_trg_claim_status_change();
 */

-- H2 Version
CREATE TRIGGER IF NOT EXISTS TRG_CLAIM_STATUS_CHANGE
    AFTER UPDATE ON CLAIM
    FOR EACH ROW
    CALL "com.greylegacy.dao.trigger.ClaimStatusChangeTrigger";

-- NOTE: The H2 trigger above requires a Java class implementing
-- org.h2.api.Trigger. Below is the equivalent inline trigger for H2
-- if Java class triggers are not desired. Uncomment to use:
--
-- CREATE TRIGGER IF NOT EXISTS TRG_CLAIM_STATUS_CHANGE
--     AFTER UPDATE ON CLAIM
--     FOR EACH ROW
--     AS $$
--     import java.sql.*;
--     @CODE
--     void fire(Connection conn, Object[] oldRow, Object[] newRow) throws SQLException {
--         // Column indices depend on table definition order; STATUS is column 4 (0-based)
--         String oldStatus = (String) oldRow[4];
--         String newStatus = (String) newRow[4];
--         if (oldStatus != null && !oldStatus.equals(newStatus)) {
--             try (PreparedStatement ps = conn.prepareStatement(
--                 "INSERT INTO CLAIM_AUDIT (ID, CLAIM_ID, EVENT_TIMESTAMP, EVENT_TYPE, " +
--                 "PREVIOUS_VALUE, NEW_VALUE, PERFORMED_BY, DESCRIPTION, CREATED_BY, CREATED_DATE) " +
--                 "VALUES (NEXT VALUE FOR AUDIT_SEQ, ?, CURRENT_TIMESTAMP, 'STATUS_CHANGE', " +
--                 "?, ?, COALESCE(?, 'SYSTEM'), 'Status changed from ' || ? || ' to ' || ?, " +
--                 "'TRG_STATUS', CURRENT_TIMESTAMP)")) {
--                 ps.setLong(1, ((Number) newRow[0]).longValue()); // CLAIM.ID
--                 ps.setString(2, oldStatus);
--                 ps.setString(3, newStatus);
--                 ps.setString(4, (String) newRow[newRow.length - 2]); // UPDATED_BY
--                 ps.setString(5, oldStatus);
--                 ps.setString(6, newStatus);
--                 ps.executeUpdate();
--             }
--         }
--     }
--     $$;

-- ----------------------------------------------------------------------------
-- 8b. TRG_POLICY_PREMIUM_CHANGE
--     AFTER UPDATE trigger on POLICY.
--     When the PREMIUM_AMOUNT changes, captures a snapshot into
--     POLICY_HISTORY via SP_POLICY_SCD_SNAPSHOT.
-- ----------------------------------------------------------------------------

/*
 * --- PostgreSQL Version ---
 * CREATE OR REPLACE FUNCTION fn_trg_policy_premium_change()
 * RETURNS TRIGGER AS $$
 * BEGIN
 *     IF OLD.PREMIUM_AMOUNT IS DISTINCT FROM NEW.PREMIUM_AMOUNT THEN
 *         PERFORM SP_POLICY_SCD_SNAPSHOT(
 *             NEW.ID,
 *             'Premium changed from ' || COALESCE(OLD.PREMIUM_AMOUNT::TEXT, 'NULL')
 *                 || ' to ' || COALESCE(NEW.PREMIUM_AMOUNT::TEXT, 'NULL'),
 *             COALESCE(NEW.UPDATED_BY, 'SYSTEM')
 *         );
 *     END IF;
 *     RETURN NEW;
 * END;
 * $$ LANGUAGE plpgsql;
 *
 * CREATE TRIGGER TRG_POLICY_PREMIUM_CHANGE
 *     AFTER UPDATE ON POLICY
 *     FOR EACH ROW
 *     EXECUTE FUNCTION fn_trg_policy_premium_change();
 */

-- H2 Version
CREATE TRIGGER IF NOT EXISTS TRG_POLICY_PREMIUM_CHANGE
    AFTER UPDATE ON POLICY
    FOR EACH ROW
    CALL "com.greylegacy.dao.trigger.PolicyPremiumChangeTrigger";

-- NOTE: As with TRG_CLAIM_STATUS_CHANGE above, a Java Trigger class is
-- required for H2. The class should compare old and new PREMIUM_AMOUNT
-- values and call SP_POLICY_SCD_SNAPSHOT when they differ.

-- ----------------------------------------------------------------------------
-- 8c. TRG_PAYMENT_APPROVED_CHECK
--     BEFORE INSERT trigger on CLAIM_PAYMENT.
--     Validates that adding a new payment will not cause the total payments
--     for the claim to exceed the claim's APPROVED_AMOUNT.
--     If the total would be exceeded, the insert is rejected.
-- ----------------------------------------------------------------------------

/*
 * --- PostgreSQL Version ---
 * CREATE OR REPLACE FUNCTION fn_trg_payment_approved_check()
 * RETURNS TRIGGER AS $$
 * DECLARE
 *     v_approved     DECIMAL(14,2);
 *     v_total_paid   DECIMAL(14,2);
 * BEGIN
 *     SELECT COALESCE(APPROVED_AMOUNT, 0)
 *       INTO v_approved
 *       FROM CLAIM
 *      WHERE ID = NEW.CLAIM_ID;
 *
 *     IF NOT FOUND THEN
 *         RAISE EXCEPTION 'Claim % does not exist', NEW.CLAIM_ID;
 *     END IF;
 *
 *     SELECT COALESCE(SUM(AMOUNT), 0)
 *       INTO v_total_paid
 *       FROM CLAIM_PAYMENT
 *      WHERE CLAIM_ID = NEW.CLAIM_ID
 *        AND PAYMENT_STATUS IN ('APPROVED', 'PROCESSED', 'ISSUED');
 *
 *     IF (v_total_paid + NEW.AMOUNT) > v_approved THEN
 *         RAISE EXCEPTION 'Payment of % would exceed approved amount of %. '
 *             'Currently paid: %. Remaining: %.',
 *             NEW.AMOUNT, v_approved, v_total_paid,
 *             (v_approved - v_total_paid);
 *     END IF;
 *
 *     RETURN NEW;
 * END;
 * $$ LANGUAGE plpgsql;
 *
 * CREATE TRIGGER TRG_PAYMENT_APPROVED_CHECK
 *     BEFORE INSERT ON CLAIM_PAYMENT
 *     FOR EACH ROW
 *     EXECUTE FUNCTION fn_trg_payment_approved_check();
 */

-- H2 Version
CREATE TRIGGER IF NOT EXISTS TRG_PAYMENT_APPROVED_CHECK
    BEFORE INSERT ON CLAIM_PAYMENT
    FOR EACH ROW
    CALL "com.greylegacy.dao.trigger.PaymentApprovedCheckTrigger";

-- NOTE: The H2 Java trigger class should implement org.h2.api.Trigger and:
--   1. Query the CLAIM table for APPROVED_AMOUNT using the new row's CLAIM_ID.
--   2. Sum existing payments for the claim.
--   3. If (existing + new amount) > approved, throw a SQLException to abort.


-- ============================================================================
-- 9. FUNCTIONS
-- ============================================================================

-- ----------------------------------------------------------------------------
-- 9a. FN_CLAIM_AGE_DAYS
--     Returns the number of calendar days since the claim's LOSS_DATE.
--     Returns NULL if the claim does not exist.
-- ----------------------------------------------------------------------------

/*
 * --- PostgreSQL Version ---
 * CREATE OR REPLACE FUNCTION FN_CLAIM_AGE_DAYS(p_claim_id BIGINT)
 * RETURNS INTEGER AS $$
 * DECLARE
 *     v_loss_date DATE;
 * BEGIN
 *     SELECT LOSS_DATE INTO v_loss_date FROM CLAIM WHERE ID = p_claim_id;
 *     IF v_loss_date IS NULL THEN
 *         RETURN NULL;
 *     END IF;
 *     RETURN (CURRENT_DATE - v_loss_date);
 * END;
 * $$ LANGUAGE plpgsql;
 */

-- H2 Version
CREATE ALIAS IF NOT EXISTS FN_CLAIM_AGE_DAYS AS $$
import java.sql.*;
@CODE
Integer fnClaimAgeDays(Connection conn, long claimId) throws SQLException {
    try (PreparedStatement ps = conn.prepareStatement(
            "SELECT DATEDIFF('DAY', LOSS_DATE, CURRENT_DATE) FROM CLAIM WHERE ID = ?")) {
        ps.setLong(1, claimId);
        try (ResultSet rs = ps.executeQuery()) {
            if (rs.next()) {
                int days = rs.getInt(1);
                return rs.wasNull() ? null : days;
            }
        }
    }
    return null;
}
$$;

-- ----------------------------------------------------------------------------
-- 9b. FN_POLICY_CLAIMS_TOTAL
--     Returns the sum of APPROVED_AMOUNT across all non-deleted claims
--     for a given policy.  Returns 0.00 if no claims exist.
-- ----------------------------------------------------------------------------

/*
 * --- PostgreSQL Version ---
 * CREATE OR REPLACE FUNCTION FN_POLICY_CLAIMS_TOTAL(p_policy_id BIGINT)
 * RETURNS DECIMAL(14,2) AS $$
 * BEGIN
 *     RETURN (
 *         SELECT COALESCE(SUM(APPROVED_AMOUNT), 0)
 *           FROM CLAIM
 *          WHERE POLICY_ID = p_policy_id
 *            AND (DELETED = FALSE OR DELETED IS NULL)
 *     );
 * END;
 * $$ LANGUAGE plpgsql;
 */

-- H2 Version
CREATE ALIAS IF NOT EXISTS FN_POLICY_CLAIMS_TOTAL AS $$
import java.sql.*;
import java.math.BigDecimal;
@CODE
BigDecimal fnPolicyClaimsTotal(Connection conn, long policyId) throws SQLException {
    try (PreparedStatement ps = conn.prepareStatement(
            "SELECT COALESCE(SUM(APPROVED_AMOUNT), 0) FROM CLAIM " +
            "WHERE POLICY_ID = ? AND (DELETED = FALSE OR DELETED IS NULL)")) {
        ps.setLong(1, policyId);
        try (ResultSet rs = ps.executeQuery()) {
            if (rs.next()) {
                return rs.getBigDecimal(1);
            }
        }
    }
    return BigDecimal.ZERO;
}
$$;

-- ----------------------------------------------------------------------------
-- 9c. FN_CALCULATE_LOSS_RATIO
--     Loss Ratio = Total Paid Claims / Total Premiums Collected
--
--     For a given policy, sums all claim payments that are in PROCESSED or
--     ISSUED status, and divides by the policy's PREMIUM_AMOUNT.
--     Returns NULL if premium is zero or policy not found.
--
--     A loss ratio > 1.0 means the insurer is paying out more than
--     collecting — a key profitability metric.
-- ----------------------------------------------------------------------------

/*
 * --- PostgreSQL Version ---
 * CREATE OR REPLACE FUNCTION FN_CALCULATE_LOSS_RATIO(p_policy_id BIGINT)
 * RETURNS DECIMAL(10,4) AS $$
 * DECLARE
 *     v_premium     DECIMAL(12,2);
 *     v_total_paid  DECIMAL(14,2);
 * BEGIN
 *     SELECT COALESCE(PREMIUM_AMOUNT, 0)
 *       INTO v_premium
 *       FROM POLICY
 *      WHERE ID = p_policy_id;
 *
 *     IF v_premium IS NULL OR v_premium = 0 THEN
 *         RETURN NULL;
 *     END IF;
 *
 *     SELECT COALESCE(SUM(cp.AMOUNT), 0)
 *       INTO v_total_paid
 *       FROM CLAIM_PAYMENT cp
 *       JOIN CLAIM c ON c.ID = cp.CLAIM_ID
 *      WHERE c.POLICY_ID = p_policy_id
 *        AND cp.PAYMENT_STATUS IN ('PROCESSED', 'ISSUED')
 *        AND (c.DELETED = FALSE OR c.DELETED IS NULL);
 *
 *     RETURN v_total_paid / v_premium;
 * END;
 * $$ LANGUAGE plpgsql;
 */

-- H2 Version
CREATE ALIAS IF NOT EXISTS FN_CALCULATE_LOSS_RATIO AS $$
import java.sql.*;
import java.math.BigDecimal;
import java.math.RoundingMode;
@CODE
BigDecimal fnCalculateLossRatio(Connection conn, long policyId) throws SQLException {
    BigDecimal premium = BigDecimal.ZERO;
    // Get the policy premium
    try (PreparedStatement ps = conn.prepareStatement(
            "SELECT COALESCE(PREMIUM_AMOUNT, 0) FROM POLICY WHERE ID = ?")) {
        ps.setLong(1, policyId);
        try (ResultSet rs = ps.executeQuery()) {
            if (rs.next()) {
                premium = rs.getBigDecimal(1);
            } else {
                return null; // Policy not found
            }
        }
    }
    if (premium.compareTo(BigDecimal.ZERO) == 0) {
        return null; // Avoid division by zero
    }
    // Sum all processed/issued payments on claims for this policy
    BigDecimal totalPaid = BigDecimal.ZERO;
    try (PreparedStatement ps = conn.prepareStatement(
            "SELECT COALESCE(SUM(cp.AMOUNT), 0) FROM CLAIM_PAYMENT cp " +
            "JOIN CLAIM c ON c.ID = cp.CLAIM_ID " +
            "WHERE c.POLICY_ID = ? " +
            "AND cp.PAYMENT_STATUS IN ('PROCESSED','ISSUED') " +
            "AND (c.DELETED = FALSE OR c.DELETED IS NULL)")) {
        ps.setLong(1, policyId);
        try (ResultSet rs = ps.executeQuery()) {
            if (rs.next()) {
                totalPaid = rs.getBigDecimal(1);
            }
        }
    }
    return totalPaid.divide(premium, 4, RoundingMode.HALF_UP);
}
$$;


-- ============================================================================
-- 10. REFERENCE DATA POPULATION (Batch Inserts)
-- ============================================================================
-- Realistic seed data for all reference tables.
-- Idempotent: uses MERGE (H2) syntax.  For PostgreSQL, replace with
-- INSERT ... ON CONFLICT (CODE) DO NOTHING.
-- ============================================================================

-- ----------------------------------------------------------------------------
-- 10a. CLAIM_STATUS_REF - 12 statuses covering the full claim lifecycle
-- ----------------------------------------------------------------------------
MERGE INTO CLAIM_STATUS_REF (CODE, DISPLAY_NAME, DESCRIPTION, SORT_ORDER, ACTIVE, TERMINAL_STATE)
KEY(CODE) VALUES
('NEW',           'New',                  'Claim just reported, awaiting initial review',            10,  TRUE,  FALSE);
MERGE INTO CLAIM_STATUS_REF (CODE, DISPLAY_NAME, DESCRIPTION, SORT_ORDER, ACTIVE, TERMINAL_STATE)
KEY(CODE) VALUES
('OPEN',          'Open',                 'Claim opened and assigned to adjuster',                   20,  TRUE,  FALSE);
MERGE INTO CLAIM_STATUS_REF (CODE, DISPLAY_NAME, DESCRIPTION, SORT_ORDER, ACTIVE, TERMINAL_STATE)
KEY(CODE) VALUES
('UNDER_REVIEW',  'Under Review',         'Claim under active investigation',                       30,  TRUE,  FALSE);
MERGE INTO CLAIM_STATUS_REF (CODE, DISPLAY_NAME, DESCRIPTION, SORT_ORDER, ACTIVE, TERMINAL_STATE)
KEY(CODE) VALUES
('PENDING_INFO',  'Pending Information',  'Awaiting additional documentation from claimant',         35,  TRUE,  FALSE);
MERGE INTO CLAIM_STATUS_REF (CODE, DISPLAY_NAME, DESCRIPTION, SORT_ORDER, ACTIVE, TERMINAL_STATE)
KEY(CODE) VALUES
('APPROVED',      'Approved',             'Claim approved for payment',                              40,  TRUE,  FALSE);
MERGE INTO CLAIM_STATUS_REF (CODE, DISPLAY_NAME, DESCRIPTION, SORT_ORDER, ACTIVE, TERMINAL_STATE)
KEY(CODE) VALUES
('PARTIALLY_PAID','Partially Paid',       'Some payments issued but total not yet fully disbursed',  45,  TRUE,  FALSE);
MERGE INTO CLAIM_STATUS_REF (CODE, DISPLAY_NAME, DESCRIPTION, SORT_ORDER, ACTIVE, TERMINAL_STATE)
KEY(CODE) VALUES
('PAID',          'Paid',                 'All authorized payments have been issued',                50,  TRUE,  FALSE);
MERGE INTO CLAIM_STATUS_REF (CODE, DISPLAY_NAME, DESCRIPTION, SORT_ORDER, ACTIVE, TERMINAL_STATE)
KEY(CODE) VALUES
('CLOSED',        'Closed',               'Claim finalized and closed',                              60,  TRUE,  TRUE);
MERGE INTO CLAIM_STATUS_REF (CODE, DISPLAY_NAME, DESCRIPTION, SORT_ORDER, ACTIVE, TERMINAL_STATE)
KEY(CODE) VALUES
('DENIED',        'Denied',               'Claim denied — coverage does not apply or fraud detected',70,  TRUE,  TRUE);
MERGE INTO CLAIM_STATUS_REF (CODE, DISPLAY_NAME, DESCRIPTION, SORT_ORDER, ACTIVE, TERMINAL_STATE)
KEY(CODE) VALUES
('WITHDRAWN',     'Withdrawn',            'Claimant voluntarily withdrew the claim',                 75,  TRUE,  TRUE);
MERGE INTO CLAIM_STATUS_REF (CODE, DISPLAY_NAME, DESCRIPTION, SORT_ORDER, ACTIVE, TERMINAL_STATE)
KEY(CODE) VALUES
('REOPENED',      'Reopened',             'Previously closed claim reopened due to new evidence',     25,  TRUE,  FALSE);
MERGE INTO CLAIM_STATUS_REF (CODE, DISPLAY_NAME, DESCRIPTION, SORT_ORDER, ACTIVE, TERMINAL_STATE)
KEY(CODE) VALUES
('SIU_REFERRAL',  'SIU Referral',         'Referred to Special Investigations Unit for fraud review',32,  TRUE,  FALSE);

-- ----------------------------------------------------------------------------
-- 10b. COVERAGE_TYPE_REF - 10 coverage types across lines of business
-- ----------------------------------------------------------------------------
MERGE INTO COVERAGE_TYPE_REF (CODE, DESCRIPTION, LINE_OF_BUSINESS, DEFAULT_DEDUCTIBLE, DEFAULT_LIMIT)
KEY(CODE) VALUES
('COLLISION',     'Collision Coverage',                      'AUTO',         500.00,   50000.00);
MERGE INTO COVERAGE_TYPE_REF (CODE, DESCRIPTION, LINE_OF_BUSINESS, DEFAULT_DEDUCTIBLE, DEFAULT_LIMIT)
KEY(CODE) VALUES
('COMPREHENSIVE', 'Comprehensive Coverage',                  'AUTO',         250.00,   50000.00);
MERGE INTO COVERAGE_TYPE_REF (CODE, DESCRIPTION, LINE_OF_BUSINESS, DEFAULT_DEDUCTIBLE, DEFAULT_LIMIT)
KEY(CODE) VALUES
('LIABILITY',     'Liability Coverage',                      'AUTO',           0.00,  100000.00);
MERGE INTO COVERAGE_TYPE_REF (CODE, DESCRIPTION, LINE_OF_BUSINESS, DEFAULT_DEDUCTIBLE, DEFAULT_LIMIT)
KEY(CODE) VALUES
('DWELLING',      'Dwelling Coverage',                       'HOME',        1000.00,  350000.00);
MERGE INTO COVERAGE_TYPE_REF (CODE, DESCRIPTION, LINE_OF_BUSINESS, DEFAULT_DEDUCTIBLE, DEFAULT_LIMIT)
KEY(CODE) VALUES
('PERSONAL_PROP', 'Personal Property Coverage',              'HOME',         500.00,  175000.00);
MERGE INTO COVERAGE_TYPE_REF (CODE, DESCRIPTION, LINE_OF_BUSINESS, DEFAULT_DEDUCTIBLE, DEFAULT_LIMIT)
KEY(CODE) VALUES
('MED_PAY',       'Medical Payments Coverage',               'AUTO',           0.00,    5000.00);
MERGE INTO COVERAGE_TYPE_REF (CODE, DESCRIPTION, LINE_OF_BUSINESS, DEFAULT_DEDUCTIBLE, DEFAULT_LIMIT)
KEY(CODE) VALUES
('UNINSURED',     'Uninsured/Underinsured Motorist',         'AUTO',         250.00,   50000.00);
MERGE INTO COVERAGE_TYPE_REF (CODE, DESCRIPTION, LINE_OF_BUSINESS, DEFAULT_DEDUCTIBLE, DEFAULT_LIMIT)
KEY(CODE) VALUES
('GEN_LIABILITY', 'General Liability',                       'COMMERCIAL',  2500.00, 1000000.00);
MERGE INTO COVERAGE_TYPE_REF (CODE, DESCRIPTION, LINE_OF_BUSINESS, DEFAULT_DEDUCTIBLE, DEFAULT_LIMIT)
KEY(CODE) VALUES
('PROF_LIABILITY','Professional Liability (E&O)',            'COMMERCIAL',  5000.00, 2000000.00);
MERGE INTO COVERAGE_TYPE_REF (CODE, DESCRIPTION, LINE_OF_BUSINESS, DEFAULT_DEDUCTIBLE, DEFAULT_LIMIT)
KEY(CODE) VALUES
('WORKERS_COMP',  'Workers Compensation',                    'COMMERCIAL',     0.00,  500000.00);

-- ----------------------------------------------------------------------------
-- 10c. STATE_PROVINCE_REF - 12 US states with regulatory information
-- ----------------------------------------------------------------------------
MERGE INTO STATE_PROVINCE_REF (CODE, NAME, COUNTRY, TAX_RATE, REGULATORY_BODY, FILING_DEADLINE_DAYS)
KEY(CODE) VALUES
('CA', 'California',    'US', 0.0735, 'California Department of Insurance',             30);
MERGE INTO STATE_PROVINCE_REF (CODE, NAME, COUNTRY, TAX_RATE, REGULATORY_BODY, FILING_DEADLINE_DAYS)
KEY(CODE) VALUES
('TX', 'Texas',         'US', 0.0625, 'Texas Department of Insurance',                  60);
MERGE INTO STATE_PROVINCE_REF (CODE, NAME, COUNTRY, TAX_RATE, REGULATORY_BODY, FILING_DEADLINE_DAYS)
KEY(CODE) VALUES
('NY', 'New York',      'US', 0.0800, 'NY Department of Financial Services',            30);
MERGE INTO STATE_PROVINCE_REF (CODE, NAME, COUNTRY, TAX_RATE, REGULATORY_BODY, FILING_DEADLINE_DAYS)
KEY(CODE) VALUES
('FL', 'Florida',       'US', 0.0600, 'Florida Office of Insurance Regulation',         45);
MERGE INTO STATE_PROVINCE_REF (CODE, NAME, COUNTRY, TAX_RATE, REGULATORY_BODY, FILING_DEADLINE_DAYS)
KEY(CODE) VALUES
('IL', 'Illinois',      'US', 0.0625, 'Illinois Department of Insurance',               30);
MERGE INTO STATE_PROVINCE_REF (CODE, NAME, COUNTRY, TAX_RATE, REGULATORY_BODY, FILING_DEADLINE_DAYS)
KEY(CODE) VALUES
('PA', 'Pennsylvania',  'US', 0.0600, 'PA Insurance Department',                        30);
MERGE INTO STATE_PROVINCE_REF (CODE, NAME, COUNTRY, TAX_RATE, REGULATORY_BODY, FILING_DEADLINE_DAYS)
KEY(CODE) VALUES
('OH', 'Ohio',          'US', 0.0575, 'Ohio Department of Insurance',                   30);
MERGE INTO STATE_PROVINCE_REF (CODE, NAME, COUNTRY, TAX_RATE, REGULATORY_BODY, FILING_DEADLINE_DAYS)
KEY(CODE) VALUES
('GA', 'Georgia',       'US', 0.0400, 'Georgia Office of Insurance Commissioner',       60);
MERGE INTO STATE_PROVINCE_REF (CODE, NAME, COUNTRY, TAX_RATE, REGULATORY_BODY, FILING_DEADLINE_DAYS)
KEY(CODE) VALUES
('NC', 'North Carolina','US', 0.0475, 'NC Department of Insurance',                     30);
MERGE INTO STATE_PROVINCE_REF (CODE, NAME, COUNTRY, TAX_RATE, REGULATORY_BODY, FILING_DEADLINE_DAYS)
KEY(CODE) VALUES
('MI', 'Michigan',      'US', 0.0600, 'MI Dept of Insurance & Financial Services',      30);
MERGE INTO STATE_PROVINCE_REF (CODE, NAME, COUNTRY, TAX_RATE, REGULATORY_BODY, FILING_DEADLINE_DAYS)
KEY(CODE) VALUES
('NJ', 'New Jersey',    'US', 0.0663, 'NJ Department of Banking & Insurance',           30);
MERGE INTO STATE_PROVINCE_REF (CODE, NAME, COUNTRY, TAX_RATE, REGULATORY_BODY, FILING_DEADLINE_DAYS)
KEY(CODE) VALUES
('WA', 'Washington',    'US', 0.0650, 'WA Office of the Insurance Commissioner',        45);


-- ============================================================================
-- 11. PESSIMISTIC LOCKING PROCEDURE
-- ============================================================================
-- Uses SELECT ... FOR UPDATE to acquire an exclusive row-level lock on a
-- claim row.  The lock is held until the enclosing transaction commits or
-- rolls back.
--
-- This pattern is used when a process (e.g., payment issuance, adjuster
-- reassignment) must guarantee that no other session modifies the claim
-- while it is being processed.
--
-- IMPORTANT: The caller MUST be inside an explicit transaction
-- (SET AUTOCOMMIT FALSE / BEGIN) for the lock to be meaningful.
-- ============================================================================

/*
 * --- PostgreSQL Version ---
 * CREATE OR REPLACE FUNCTION SP_LOCK_CLAIM_FOR_PROCESSING(p_claim_id BIGINT)
 * RETURNS TABLE (
 *     ID              BIGINT,
 *     CLAIM_NUMBER    VARCHAR(20),
 *     STATUS          VARCHAR(20),
 *     APPROVED_AMOUNT DECIMAL(14,2),
 *     RESERVE_AMOUNT  DECIMAL(14,2)
 * ) AS $$
 * BEGIN
 *     -- SELECT ... FOR UPDATE acquires an exclusive row-level lock.
 *     -- Any concurrent session attempting to lock the same row will block
 *     -- until this transaction completes.
 *     RETURN QUERY
 *     SELECT c.ID, c.CLAIM_NUMBER, c.STATUS, c.APPROVED_AMOUNT, c.RESERVE_AMOUNT
 *       FROM CLAIM c
 *      WHERE c.ID = p_claim_id
 *        AND (c.DELETED = FALSE OR c.DELETED IS NULL)
 *        FOR UPDATE;
 *
 *     -- NOWAIT variant (throws error instead of blocking):
 *     -- FOR UPDATE NOWAIT;
 *
 *     -- SKIP LOCKED variant (useful for worker queues):
 *     -- FOR UPDATE SKIP LOCKED;
 *
 *     IF NOT FOUND THEN
 *         RAISE EXCEPTION 'Claim % not found or has been deleted', p_claim_id;
 *     END IF;
 * END;
 * $$ LANGUAGE plpgsql;
 */

-- H2 Version
CREATE ALIAS IF NOT EXISTS SP_LOCK_CLAIM_FOR_PROCESSING AS $$
import java.sql.*;
@CODE
ResultSet spLockClaimForProcessing(Connection conn, long claimId) throws SQLException {
    // SELECT ... FOR UPDATE acquires an exclusive row-level lock in H2.
    // The lock is held for the duration of the current transaction.
    // Caller MUST have autocommit disabled for this to be effective.
    PreparedStatement ps = conn.prepareStatement(
        "SELECT ID, CLAIM_NUMBER, STATUS, APPROVED_AMOUNT, RESERVE_AMOUNT " +
        "FROM CLAIM " +
        "WHERE ID = ? " +
        "AND (DELETED = FALSE OR DELETED IS NULL) " +
        "FOR UPDATE"
    );
    ps.setLong(1, claimId);
    ResultSet rs = ps.executeQuery();
    if (!rs.isBeforeFirst()) {
        throw new SQLException("Claim " + claimId + " not found or has been deleted");
    }
    return rs;
}
$$;


-- ============================================================================
-- 12. OPTIMISTIC LOCKING COMMENTARY
-- ============================================================================
/*
 * OPTIMISTIC LOCKING STRATEGY
 * ===========================
 *
 * All core entity tables in Grey Legacy include a VERSION column
 * (INTEGER DEFAULT 0).  This column serves as the optimistic locking
 * mechanism, coordinated by Hibernate / JPA at the application layer.
 *
 * How it works:
 *   1. When an entity is read from the database, Hibernate stores the
 *      current VERSION value in the persistence context.
 *
 *   2. When the entity is flushed (UPDATE), Hibernate appends a WHERE
 *      clause that includes the original VERSION value:
 *
 *          UPDATE CLAIM
 *             SET STATUS = 'APPROVED',
 *                 VERSION = VERSION + 1,
 *                 ...
 *           WHERE ID = :id
 *             AND VERSION = :expectedVersion
 *
 *   3. If the update affects zero rows, Hibernate throws
 *      org.hibernate.StaleObjectStateException (JPA:
 *      javax.persistence.OptimisticLockException), indicating that
 *      another transaction modified this row after it was read.
 *
 *   4. The application catches this exception and either:
 *        a) Retries the operation with a fresh read, or
 *        b) Returns a conflict error to the user.
 *
 * Tables using optimistic locking (VERSION column):
 *   - POLICY
 *   - CLAIM
 *   - CLAIM_PAYMENT
 *   - CLAIM_AUDIT
 *   - CLAIM_NOTE
 *   - CLAIM_SNAPSHOT
 *   - POLICY_ENDORSEMENT
 *   - ADJUSTER
 *
 * Contrast with Pessimistic Locking:
 *   Optimistic locking is preferred for web-facing operations where
 *   contention is low and holding database locks across HTTP requests
 *   would be impractical.
 *
 *   Pessimistic locking (SELECT ... FOR UPDATE, see Section 11) is
 *   reserved for backend batch processes and critical financial
 *   operations where absolute serialised access is required during
 *   a single short-lived database transaction.
 *
 * Domain Mapping Reference (Hibernate XML):
 *   <version name="version" column="VERSION" type="integer"
 *            unsaved-value="null" />
 *
 * Domain Mapping Reference (JPA Annotation):
 *   @Version
 *   @Column(name = "VERSION")
 *   private Integer version;
 */


-- ============================================================================
-- ADDITIONAL UTILITY VIEWS
-- ============================================================================

-- V_CLAIM_SUMMARY: Denormalised view joining claims with policy and adjuster
-- info for dashboard queries and reporting.
CREATE OR REPLACE VIEW V_CLAIM_SUMMARY AS
SELECT
    c.ID                AS CLAIM_ID,
    c.CLAIM_NUMBER,
    c.STATUS            AS CLAIM_STATUS,
    c.CLAIM_TYPE,
    c.LOSS_DATE,
    c.REPORTED_DATE,
    c.ESTIMATED_LOSS,
    c.APPROVED_AMOUNT,
    c.RESERVE_AMOUNT,
    c.FRAUD_RISK_LEVEL,
    c.FRAUD_SCORE,
    p.POLICY_NUMBER,
    p.POLICY_TYPE,
    p.HOLDER_FIRST_NAME || ' ' || p.HOLDER_LAST_NAME AS POLICY_HOLDER,
    p.PREMIUM_AMOUNT,
    p.COVERAGE_LIMIT    AS POLICY_LIMIT,
    a.FIRST_NAME || ' ' || a.LAST_NAME               AS ADJUSTER_NAME,
    a.SPECIALIZATION    AS ADJUSTER_SPECIALIZATION,
    sr.DISPLAY_NAME     AS STATUS_DISPLAY,
    sr.TERMINAL_STATE   AS IS_TERMINAL
FROM CLAIM c
JOIN POLICY p ON p.ID = c.POLICY_ID
LEFT JOIN ADJUSTER a ON a.ADJUSTER_CODE = c.ADJUSTER_CODE
LEFT JOIN CLAIM_STATUS_REF sr ON sr.CODE = c.STATUS
WHERE (c.DELETED = FALSE OR c.DELETED IS NULL);

-- V_PAYMENT_RECONCILIATION: Quick view for finance teams to spot discrepancies
CREATE OR REPLACE VIEW V_PAYMENT_RECONCILIATION AS
SELECT
    c.ID                AS CLAIM_ID,
    c.CLAIM_NUMBER,
    c.STATUS,
    COALESCE(c.APPROVED_AMOUNT, 0)             AS APPROVED_AMOUNT,
    COALESCE(pay.TOTAL_PAID, 0)                AS TOTAL_PAID,
    COALESCE(c.APPROVED_AMOUNT, 0)
        - COALESCE(pay.TOTAL_PAID, 0)          AS REMAINING_BALANCE,
    CASE
        WHEN COALESCE(pay.TOTAL_PAID, 0) > COALESCE(c.APPROVED_AMOUNT, 0)
            THEN 'OVERPAID'
        WHEN COALESCE(pay.TOTAL_PAID, 0) = COALESCE(c.APPROVED_AMOUNT, 0)
            THEN 'BALANCED'
        ELSE 'UNDERPAID'
    END                                        AS RECONCILIATION_STATUS,
    pay.PAYMENT_COUNT
FROM CLAIM c
LEFT JOIN (
    SELECT
        CLAIM_ID,
        SUM(AMOUNT)    AS TOTAL_PAID,
        COUNT(*)       AS PAYMENT_COUNT
    FROM CLAIM_PAYMENT
    WHERE PAYMENT_STATUS IN ('APPROVED','PROCESSED','ISSUED')
    GROUP BY CLAIM_ID
) pay ON pay.CLAIM_ID = c.ID
WHERE c.APPROVED_AMOUNT IS NOT NULL
  AND (c.DELETED = FALSE OR c.DELETED IS NULL);


-- ============================================================================
-- END OF schema-procedures.sql
-- ============================================================================
-- Post-deployment checklist:
--   [ ] Verify all sequences exist (run schema.sql first)
--   [ ] Verify reference data loaded (SELECT COUNT(*) FROM CLAIM_STATUS_REF)
--   [ ] Verify triggers fire correctly on test updates
--   [ ] For PostgreSQL: replace H2 ALIAS blocks with PL/pgSQL equivalents
--   [ ] For PostgreSQL: replace MERGE with INSERT ... ON CONFLICT DO NOTHING
--   [ ] Grant execute permissions on procedures/functions to app role
-- ============================================================================
