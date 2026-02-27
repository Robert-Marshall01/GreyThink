-- ============================================================================
-- Grey Legacy - Insurance Claims Processing System
-- ETL Schema: Staging Tables, Import Procedures, Data Quality & Reconciliation
-- ============================================================================
-- Target:      H2 (development) / PostgreSQL (production)
-- Depends on:  schema.sql (base tables), schema-procedures.sql (reference data)
-- Author:      Grey Legacy Data Engineering Team
-- Created:     2026-03-15
-- ============================================================================
--
-- PURPOSE:
--   This schema supports the file-based ETL pipelines used to exchange data
--   with partner systems (reinsurers, TPAs, actuarial platforms, regulatory
--   bodies).  The pattern is:
--
--     1. External files (CSV, XML) land in an inbound directory
--     2. FilePollingClaimImporter / Spring Batch reads them into STAGING tables
--     3. Data quality validation procedures flag bad records
--     4. Clean records are merged into production tables
--     5. Rejected records remain in staging for manual review
--     6. Reconciliation queries verify import completeness
--
-- TABLE OF CONTENTS:
--   1.  Sequences
--   2.  ETL_BATCH_LOG - Import batch tracking
--   3.  STG_CLAIM_IMPORT - Claims staging table
--   4.  STG_POLICY_IMPORT - Policy staging table
--   5.  STG_PAYMENT_IMPORT - Payment staging table
--   6.  SP_VALIDATE_STAGED_CLAIMS - Data quality checks
--   7.  SP_MERGE_STAGED_CLAIMS - Stage → Production merge
--   8.  SP_VALIDATE_STAGED_POLICIES - Policy data quality
--   9.  SP_MERGE_STAGED_POLICIES - Policy stage → Production
--  10.  SP_ETL_RECONCILIATION - Cross-system count/sum reconciliation
--  11.  Reconciliation & Monitoring Views
--  12.  Data Retention / Purge Procedure
-- ============================================================================


-- ============================================================================
-- 1. SEQUENCES
-- ============================================================================

CREATE SEQUENCE IF NOT EXISTS ETL_BATCH_SEQ START WITH 1 INCREMENT BY 1;
CREATE SEQUENCE IF NOT EXISTS STG_CLAIM_SEQ START WITH 1 INCREMENT BY 1;
CREATE SEQUENCE IF NOT EXISTS STG_POLICY_SEQ START WITH 1 INCREMENT BY 1;
CREATE SEQUENCE IF NOT EXISTS STG_PAYMENT_SEQ START WITH 1 INCREMENT BY 1;


-- ============================================================================
-- 2. ETL_BATCH_LOG - Tracks every import batch execution
-- ============================================================================
-- Each run of the ETL import (shell script or Spring Batch job) creates one
-- row here.  The batch ID is referenced by all staged records so we can
-- trace every imported row back to the file and run that created it.
-- ============================================================================

CREATE TABLE IF NOT EXISTS ETL_BATCH_LOG (
    BATCH_ID            BIGINT          NOT NULL DEFAULT NEXT VALUE FOR ETL_BATCH_SEQ,
    BATCH_TYPE          VARCHAR(30)     NOT NULL,     -- CLAIM_IMPORT, POLICY_IMPORT, PAYMENT_IMPORT
    SOURCE_FILENAME     VARCHAR(500)    NOT NULL,
    SOURCE_SYSTEM       VARCHAR(50)     DEFAULT 'EXTERNAL',
    STATUS              VARCHAR(20)     NOT NULL DEFAULT 'STARTED',  -- STARTED, VALIDATING, MERGING, COMPLETED, FAILED
    TOTAL_ROWS          INTEGER         DEFAULT 0,
    VALID_ROWS          INTEGER         DEFAULT 0,
    REJECTED_ROWS       INTEGER         DEFAULT 0,
    MERGED_ROWS         INTEGER         DEFAULT 0,
    DUPLICATE_ROWS      INTEGER         DEFAULT 0,
    ERROR_MESSAGE       VARCHAR(2000),
    STARTED_AT          TIMESTAMP       NOT NULL DEFAULT CURRENT_TIMESTAMP,
    COMPLETED_AT        TIMESTAMP,
    EXECUTED_BY         VARCHAR(50)     DEFAULT 'ETL_SERVICE',
    CONSTRAINT PK_ETL_BATCH_LOG PRIMARY KEY (BATCH_ID)
);

CREATE INDEX IF NOT EXISTS IDX_ETL_BATCH_TYPE ON ETL_BATCH_LOG(BATCH_TYPE, STATUS);
CREATE INDEX IF NOT EXISTS IDX_ETL_BATCH_DATE ON ETL_BATCH_LOG(STARTED_AT);


-- ============================================================================
-- 3. STG_CLAIM_IMPORT - Claims staging table
-- ============================================================================
-- Mirrors the structure of the inbound CSV (sample-claims-import.csv).
-- All columns are VARCHAR to accept any input; validation procedures
-- check data types, ranges, and referential integrity before merging.
-- ============================================================================

CREATE TABLE IF NOT EXISTS STG_CLAIM_IMPORT (
    STG_ID              BIGINT          NOT NULL DEFAULT NEXT VALUE FOR STG_CLAIM_SEQ,
    BATCH_ID            BIGINT          NOT NULL,
    LINE_NUMBER         INTEGER,                      -- Source file line number for error reporting
    -- Raw import fields (all VARCHAR to accept anything)
    CLAIM_NUMBER        VARCHAR(50),
    POLICY_NUMBER       VARCHAR(50),
    CLAIMANT_FIRST_NAME VARCHAR(100),
    CLAIMANT_LAST_NAME  VARCHAR(100),
    CLAIM_TYPE          VARCHAR(50),
    LOSS_DATE           VARCHAR(50),                  -- Will be validated as date
    LOSS_DESCRIPTION    VARCHAR(2000),
    ESTIMATED_LOSS      VARCHAR(50),                  -- Will be validated as numeric
    LOSS_LOCATION       VARCHAR(500),
    -- Validation metadata
    VALIDATION_STATUS   VARCHAR(20)     DEFAULT 'PENDING',  -- PENDING, VALID, REJECTED, DUPLICATE, MERGED
    VALIDATION_ERRORS   VARCHAR(2000),                       -- Semicolon-separated error descriptions
    VALIDATED_AT        TIMESTAMP,
    MERGED_AT           TIMESTAMP,
    CONSTRAINT PK_STG_CLAIM PRIMARY KEY (STG_ID),
    CONSTRAINT FK_STG_CLAIM_BATCH FOREIGN KEY (BATCH_ID) REFERENCES ETL_BATCH_LOG(BATCH_ID)
);

CREATE INDEX IF NOT EXISTS IDX_STG_CLAIM_BATCH ON STG_CLAIM_IMPORT(BATCH_ID);
CREATE INDEX IF NOT EXISTS IDX_STG_CLAIM_STATUS ON STG_CLAIM_IMPORT(VALIDATION_STATUS);
CREATE INDEX IF NOT EXISTS IDX_STG_CLAIM_NUMBER ON STG_CLAIM_IMPORT(CLAIM_NUMBER);


-- ============================================================================
-- 4. STG_POLICY_IMPORT - Policy staging table
-- ============================================================================

CREATE TABLE IF NOT EXISTS STG_POLICY_IMPORT (
    STG_ID              BIGINT          NOT NULL DEFAULT NEXT VALUE FOR STG_POLICY_SEQ,
    BATCH_ID            BIGINT          NOT NULL,
    LINE_NUMBER         INTEGER,
    -- Raw import fields
    POLICY_NUMBER       VARCHAR(50),
    POLICY_TYPE         VARCHAR(50),
    STATUS              VARCHAR(50),
    HOLDER_FIRST_NAME   VARCHAR(100),
    HOLDER_LAST_NAME    VARCHAR(100),
    HOLDER_EMAIL        VARCHAR(200),
    HOLDER_PHONE        VARCHAR(50),
    EFFECTIVE_DATE      VARCHAR(50),
    EXPIRATION_DATE     VARCHAR(50),
    PREMIUM_AMOUNT      VARCHAR(50),
    COVERAGE_LIMIT      VARCHAR(50),
    DEDUCTIBLE          VARCHAR(50),
    AGENT_CODE          VARCHAR(50),
    -- Validation metadata
    VALIDATION_STATUS   VARCHAR(20)     DEFAULT 'PENDING',
    VALIDATION_ERRORS   VARCHAR(2000),
    VALIDATED_AT        TIMESTAMP,
    MERGED_AT           TIMESTAMP,
    CONSTRAINT PK_STG_POLICY PRIMARY KEY (STG_ID),
    CONSTRAINT FK_STG_POLICY_BATCH FOREIGN KEY (BATCH_ID) REFERENCES ETL_BATCH_LOG(BATCH_ID)
);

CREATE INDEX IF NOT EXISTS IDX_STG_POLICY_BATCH ON STG_POLICY_IMPORT(BATCH_ID);
CREATE INDEX IF NOT EXISTS IDX_STG_POLICY_STATUS ON STG_POLICY_IMPORT(VALIDATION_STATUS);


-- ============================================================================
-- 5. STG_PAYMENT_IMPORT - Payment staging table
-- ============================================================================

CREATE TABLE IF NOT EXISTS STG_PAYMENT_IMPORT (
    STG_ID              BIGINT          NOT NULL DEFAULT NEXT VALUE FOR STG_PAYMENT_SEQ,
    BATCH_ID            BIGINT          NOT NULL,
    LINE_NUMBER         INTEGER,
    -- Raw import fields
    CLAIM_NUMBER        VARCHAR(50),
    PAYMENT_TYPE        VARCHAR(50),
    AMOUNT              VARCHAR(50),
    CHECK_NUMBER        VARCHAR(50),
    PAYEE_NAME          VARCHAR(200),
    PAYMENT_DATE        VARCHAR(50),
    -- Validation metadata
    VALIDATION_STATUS   VARCHAR(20)     DEFAULT 'PENDING',
    VALIDATION_ERRORS   VARCHAR(2000),
    VALIDATED_AT        TIMESTAMP,
    MERGED_AT           TIMESTAMP,
    CONSTRAINT PK_STG_PAYMENT PRIMARY KEY (STG_ID),
    CONSTRAINT FK_STG_PAYMENT_BATCH FOREIGN KEY (BATCH_ID) REFERENCES ETL_BATCH_LOG(BATCH_ID)
);


-- ============================================================================
-- 6. SP_VALIDATE_STAGED_CLAIMS - Data quality checks on staged claims
-- ============================================================================
-- Validates every PENDING row in a given batch:
--   - Required fields present (claim_number, policy_number, loss_date)
--   - Claim number format (CLM-YYYY-NNNNN)
--   - Loss date is a valid date and not in the future
--   - Estimated loss is a positive number
--   - Policy exists in POLICY table (referential integrity)
--   - No duplicate claim number already in production
-- Rows are updated to VALID or REJECTED with error descriptions.
-- ============================================================================

/*
 * --- PostgreSQL Version ---
 * CREATE OR REPLACE FUNCTION SP_VALIDATE_STAGED_CLAIMS(p_batch_id BIGINT)
 * RETURNS VOID AS $$
 * DECLARE
 *     v_rec RECORD;
 *     v_errors TEXT;
 *     v_loss_date DATE;
 *     v_estimated DECIMAL;
 *     v_valid INTEGER := 0;
 *     v_rejected INTEGER := 0;
 * BEGIN
 *     FOR v_rec IN SELECT * FROM STG_CLAIM_IMPORT
 *                  WHERE BATCH_ID = p_batch_id AND VALIDATION_STATUS = 'PENDING'
 *     LOOP
 *         v_errors := '';
 *
 *         -- Required fields
 *         IF v_rec.CLAIM_NUMBER IS NULL OR TRIM(v_rec.CLAIM_NUMBER) = '' THEN
 *             v_errors := v_errors || 'CLAIM_NUMBER is required; ';
 *         END IF;
 *         IF v_rec.POLICY_NUMBER IS NULL OR TRIM(v_rec.POLICY_NUMBER) = '' THEN
 *             v_errors := v_errors || 'POLICY_NUMBER is required; ';
 *         END IF;
 *         IF v_rec.LOSS_DATE IS NULL THEN
 *             v_errors := v_errors || 'LOSS_DATE is required; ';
 *         END IF;
 *
 *         -- Claim number format
 *         IF v_rec.CLAIM_NUMBER IS NOT NULL
 *            AND v_rec.CLAIM_NUMBER !~ '^CLM-\d{4}-\d{5}$' THEN
 *             v_errors := v_errors || 'CLAIM_NUMBER format invalid (expected CLM-YYYY-NNNNN); ';
 *         END IF;
 *
 *         -- Date validation
 *         BEGIN
 *             v_loss_date := v_rec.LOSS_DATE::DATE;
 *             IF v_loss_date > CURRENT_DATE THEN
 *                 v_errors := v_errors || 'LOSS_DATE is in the future; ';
 *             END IF;
 *         EXCEPTION WHEN OTHERS THEN
 *             v_errors := v_errors || 'LOSS_DATE is not a valid date; ';
 *         END;
 *
 *         -- Numeric validation
 *         BEGIN
 *             IF v_rec.ESTIMATED_LOSS IS NOT NULL THEN
 *                 v_estimated := v_rec.ESTIMATED_LOSS::DECIMAL;
 *                 IF v_estimated < 0 THEN
 *                     v_errors := v_errors || 'ESTIMATED_LOSS must be positive; ';
 *                 END IF;
 *             END IF;
 *         EXCEPTION WHEN OTHERS THEN
 *             v_errors := v_errors || 'ESTIMATED_LOSS is not a valid number; ';
 *         END;
 *
 *         -- Referential integrity: policy must exist
 *         IF v_rec.POLICY_NUMBER IS NOT NULL THEN
 *             IF NOT EXISTS (SELECT 1 FROM POLICY WHERE POLICY_NUMBER = v_rec.POLICY_NUMBER
 *                            AND (DELETED = FALSE OR DELETED IS NULL)) THEN
 *                 v_errors := v_errors || 'POLICY_NUMBER not found in POLICY table; ';
 *             END IF;
 *         END IF;
 *
 *         -- Duplicate check
 *         IF v_rec.CLAIM_NUMBER IS NOT NULL THEN
 *             IF EXISTS (SELECT 1 FROM CLAIM WHERE CLAIM_NUMBER = v_rec.CLAIM_NUMBER) THEN
 *                 v_errors := v_errors || 'CLAIM_NUMBER already exists in production; ';
 *                 UPDATE STG_CLAIM_IMPORT SET VALIDATION_STATUS = 'DUPLICATE',
 *                     VALIDATION_ERRORS = v_errors, VALIDATED_AT = CURRENT_TIMESTAMP
 *                 WHERE STG_ID = v_rec.STG_ID;
 *                 CONTINUE;
 *             END IF;
 *         END IF;
 *
 *         -- Set status
 *         IF v_errors = '' THEN
 *             UPDATE STG_CLAIM_IMPORT SET VALIDATION_STATUS = 'VALID',
 *                 VALIDATION_ERRORS = NULL, VALIDATED_AT = CURRENT_TIMESTAMP
 *             WHERE STG_ID = v_rec.STG_ID;
 *             v_valid := v_valid + 1;
 *         ELSE
 *             UPDATE STG_CLAIM_IMPORT SET VALIDATION_STATUS = 'REJECTED',
 *                 VALIDATION_ERRORS = v_errors, VALIDATED_AT = CURRENT_TIMESTAMP
 *             WHERE STG_ID = v_rec.STG_ID;
 *             v_rejected := v_rejected + 1;
 *         END IF;
 *     END LOOP;
 *
 *     -- Update batch log
 *     UPDATE ETL_BATCH_LOG SET STATUS = 'VALIDATING',
 *         VALID_ROWS = v_valid, REJECTED_ROWS = v_rejected
 *     WHERE BATCH_ID = p_batch_id;
 * END;
 * $$ LANGUAGE plpgsql;
 */

-- H2 Version
CREATE ALIAS IF NOT EXISTS SP_VALIDATE_STAGED_CLAIMS AS $$
import java.sql.*;
import java.time.LocalDate;
import java.time.format.DateTimeParseException;
@CODE
void spValidateStagedClaims(Connection conn, long batchId) throws SQLException {
    int valid = 0, rejected = 0, duplicates = 0;

    try (PreparedStatement sel = conn.prepareStatement(
            "SELECT STG_ID, CLAIM_NUMBER, POLICY_NUMBER, LOSS_DATE, ESTIMATED_LOSS " +
            "FROM STG_CLAIM_IMPORT WHERE BATCH_ID = ? AND VALIDATION_STATUS = 'PENDING'")) {
        sel.setLong(1, batchId);
        try (ResultSet rs = sel.executeQuery()) {
            while (rs.next()) {
                long stgId = rs.getLong("STG_ID");
                String claimNum = rs.getString("CLAIM_NUMBER");
                String policyNum = rs.getString("POLICY_NUMBER");
                String lossDateStr = rs.getString("LOSS_DATE");
                String estLossStr = rs.getString("ESTIMATED_LOSS");
                StringBuilder errors = new StringBuilder();

                // Required fields
                if (claimNum == null || claimNum.trim().isEmpty())
                    errors.append("CLAIM_NUMBER is required; ");
                if (policyNum == null || policyNum.trim().isEmpty())
                    errors.append("POLICY_NUMBER is required; ");
                if (lossDateStr == null || lossDateStr.trim().isEmpty())
                    errors.append("LOSS_DATE is required; ");

                // Claim number format
                if (claimNum != null && !claimNum.matches("^CLM-\\d{4}-\\d{5}$"))
                    errors.append("CLAIM_NUMBER format invalid (expected CLM-YYYY-NNNNN); ");

                // Date validation
                if (lossDateStr != null && !lossDateStr.trim().isEmpty()) {
                    try {
                        LocalDate ld = LocalDate.parse(lossDateStr.trim());
                        if (ld.isAfter(LocalDate.now()))
                            errors.append("LOSS_DATE is in the future; ");
                    } catch (DateTimeParseException e) {
                        errors.append("LOSS_DATE is not a valid date; ");
                    }
                }

                // Numeric validation
                if (estLossStr != null && !estLossStr.trim().isEmpty()) {
                    try {
                        double est = Double.parseDouble(estLossStr.trim());
                        if (est < 0) errors.append("ESTIMATED_LOSS must be positive; ");
                    } catch (NumberFormatException e) {
                        errors.append("ESTIMATED_LOSS is not a valid number; ");
                    }
                }

                // Referential integrity
                if (policyNum != null && !policyNum.trim().isEmpty()) {
                    try (PreparedStatement ps = conn.prepareStatement(
                            "SELECT 1 FROM POLICY WHERE POLICY_NUMBER = ? AND (DELETED = FALSE OR DELETED IS NULL)")) {
                        ps.setString(1, policyNum.trim());
                        try (ResultSet r2 = ps.executeQuery()) {
                            if (!r2.next())
                                errors.append("POLICY_NUMBER not found in POLICY table; ");
                        }
                    }
                }

                // Duplicate check
                boolean isDuplicate = false;
                if (claimNum != null && !claimNum.trim().isEmpty()) {
                    try (PreparedStatement ps = conn.prepareStatement(
                            "SELECT 1 FROM CLAIM WHERE CLAIM_NUMBER = ?")) {
                        ps.setString(1, claimNum.trim());
                        try (ResultSet r2 = ps.executeQuery()) {
                            if (r2.next()) {
                                errors.append("CLAIM_NUMBER already exists in production; ");
                                isDuplicate = true;
                            }
                        }
                    }
                }

                // Update staging row
                String status = isDuplicate ? "DUPLICATE" : (errors.length() == 0 ? "VALID" : "REJECTED");
                try (PreparedStatement upd = conn.prepareStatement(
                        "UPDATE STG_CLAIM_IMPORT SET VALIDATION_STATUS = ?, " +
                        "VALIDATION_ERRORS = ?, VALIDATED_AT = CURRENT_TIMESTAMP WHERE STG_ID = ?")) {
                    upd.setString(1, status);
                    upd.setString(2, errors.length() == 0 ? null : errors.toString());
                    upd.setLong(3, stgId);
                    upd.executeUpdate();
                }

                if (isDuplicate) duplicates++;
                else if (errors.length() == 0) valid++;
                else rejected++;
            }
        }
    }

    // Update batch log
    try (PreparedStatement upd = conn.prepareStatement(
            "UPDATE ETL_BATCH_LOG SET STATUS = 'VALIDATING', " +
            "VALID_ROWS = ?, REJECTED_ROWS = ?, DUPLICATE_ROWS = ? WHERE BATCH_ID = ?")) {
        upd.setInt(1, valid);
        upd.setInt(2, rejected);
        upd.setInt(3, duplicates);
        upd.setLong(4, batchId);
        upd.executeUpdate();
    }
}
$$;


-- ============================================================================
-- 7. SP_MERGE_STAGED_CLAIMS - Move valid staged claims into production
-- ============================================================================
-- Inserts all VALID staged claims into the CLAIM table, sets their staging
-- status to MERGED, and updates the batch log with the merged count.
-- Uses SP_GENERATE_CLAIM_NUMBER if the claim number collision occurs.
-- ============================================================================

/*
 * --- PostgreSQL Version ---
 * CREATE OR REPLACE FUNCTION SP_MERGE_STAGED_CLAIMS(p_batch_id BIGINT)
 * RETURNS INTEGER AS $$
 * DECLARE
 *     v_merged INTEGER := 0;
 *     v_rec RECORD;
 *     v_policy_id BIGINT;
 * BEGIN
 *     FOR v_rec IN SELECT * FROM STG_CLAIM_IMPORT
 *                  WHERE BATCH_ID = p_batch_id AND VALIDATION_STATUS = 'VALID'
 *     LOOP
 *         SELECT ID INTO v_policy_id FROM POLICY
 *         WHERE POLICY_NUMBER = v_rec.POLICY_NUMBER LIMIT 1;
 *
 *         INSERT INTO CLAIM (
 *             ID, VERSION, CLAIM_NUMBER, POLICY_ID, STATUS, CLAIM_TYPE,
 *             LOSS_DATE, REPORTED_DATE, LOSS_DESCRIPTION, LOSS_LOCATION,
 *             CLAIMANT_FIRST_NAME, CLAIMANT_LAST_NAME,
 *             ESTIMATED_LOSS, CREATED_BY, CREATED_DATE
 *         ) VALUES (
 *             NEXTVAL('CLAIM_SEQ'), 0, v_rec.CLAIM_NUMBER, v_policy_id,
 *             'NEW', v_rec.CLAIM_TYPE,
 *             v_rec.LOSS_DATE::DATE, CURRENT_TIMESTAMP,
 *             v_rec.LOSS_DESCRIPTION, v_rec.LOSS_LOCATION,
 *             v_rec.CLAIMANT_FIRST_NAME, v_rec.CLAIMANT_LAST_NAME,
 *             v_rec.ESTIMATED_LOSS::DECIMAL,
 *             'ETL_IMPORT', CURRENT_TIMESTAMP
 *         );
 *
 *         UPDATE STG_CLAIM_IMPORT SET VALIDATION_STATUS = 'MERGED',
 *             MERGED_AT = CURRENT_TIMESTAMP WHERE STG_ID = v_rec.STG_ID;
 *         v_merged := v_merged + 1;
 *     END LOOP;
 *
 *     UPDATE ETL_BATCH_LOG SET STATUS = 'COMPLETED',
 *         MERGED_ROWS = v_merged, COMPLETED_AT = CURRENT_TIMESTAMP
 *     WHERE BATCH_ID = p_batch_id;
 *
 *     RETURN v_merged;
 * END;
 * $$ LANGUAGE plpgsql;
 */

-- H2 Version
CREATE ALIAS IF NOT EXISTS SP_MERGE_STAGED_CLAIMS AS $$
import java.sql.*;
@CODE
int spMergeStagedClaims(Connection conn, long batchId) throws SQLException {
    int merged = 0;
    try (PreparedStatement sel = conn.prepareStatement(
            "SELECT STG_ID, CLAIM_NUMBER, POLICY_NUMBER, CLAIMANT_FIRST_NAME, " +
            "CLAIMANT_LAST_NAME, CLAIM_TYPE, LOSS_DATE, LOSS_DESCRIPTION, " +
            "ESTIMATED_LOSS, LOSS_LOCATION " +
            "FROM STG_CLAIM_IMPORT WHERE BATCH_ID = ? AND VALIDATION_STATUS = 'VALID'")) {
        sel.setLong(1, batchId);
        try (ResultSet rs = sel.executeQuery()) {
            while (rs.next()) {
                long stgId = rs.getLong("STG_ID");
                String policyNumber = rs.getString("POLICY_NUMBER");

                // Look up policy ID
                Long policyId = null;
                try (PreparedStatement ps = conn.prepareStatement(
                        "SELECT ID FROM POLICY WHERE POLICY_NUMBER = ? LIMIT 1")) {
                    ps.setString(1, policyNumber);
                    try (ResultSet r2 = ps.executeQuery()) {
                        if (r2.next()) policyId = r2.getLong(1);
                    }
                }

                if (policyId == null) continue; // Should not happen after validation

                // Insert into production CLAIM table
                try (PreparedStatement ins = conn.prepareStatement(
                        "INSERT INTO CLAIM (ID, VERSION, CLAIM_NUMBER, POLICY_ID, STATUS, " +
                        "CLAIM_TYPE, LOSS_DATE, REPORTED_DATE, LOSS_DESCRIPTION, LOSS_LOCATION, " +
                        "CLAIMANT_FIRST_NAME, CLAIMANT_LAST_NAME, ESTIMATED_LOSS, " +
                        "CREATED_BY, CREATED_DATE) " +
                        "VALUES (NEXT VALUE FOR CLAIM_SEQ, 0, ?, ?, 'NEW', ?, " +
                        "PARSEDATETIME(?, 'yyyy-MM-dd'), CURRENT_TIMESTAMP, ?, ?, " +
                        "?, ?, CAST(? AS DECIMAL(14,2)), 'ETL_IMPORT', CURRENT_TIMESTAMP)")) {
                    ins.setString(1, rs.getString("CLAIM_NUMBER"));
                    ins.setLong(2, policyId);
                    ins.setString(3, rs.getString("CLAIM_TYPE"));
                    ins.setString(4, rs.getString("LOSS_DATE"));
                    ins.setString(5, rs.getString("LOSS_DESCRIPTION"));
                    ins.setString(6, rs.getString("LOSS_LOCATION"));
                    ins.setString(7, rs.getString("CLAIMANT_FIRST_NAME"));
                    ins.setString(8, rs.getString("CLAIMANT_LAST_NAME"));
                    ins.setString(9, rs.getString("ESTIMATED_LOSS"));
                    ins.executeUpdate();
                }

                // Mark as merged
                try (PreparedStatement upd = conn.prepareStatement(
                        "UPDATE STG_CLAIM_IMPORT SET VALIDATION_STATUS = 'MERGED', " +
                        "MERGED_AT = CURRENT_TIMESTAMP WHERE STG_ID = ?")) {
                    upd.setLong(1, stgId);
                    upd.executeUpdate();
                }
                merged++;
            }
        }
    }

    // Update batch log
    try (PreparedStatement upd = conn.prepareStatement(
            "UPDATE ETL_BATCH_LOG SET STATUS = 'COMPLETED', MERGED_ROWS = ?, " +
            "COMPLETED_AT = CURRENT_TIMESTAMP WHERE BATCH_ID = ?")) {
        upd.setInt(1, merged);
        upd.setLong(2, batchId);
        upd.executeUpdate();
    }
    return merged;
}
$$;


-- ============================================================================
-- 8. SP_VALIDATE_STAGED_POLICIES
-- ============================================================================

/*
 * --- PostgreSQL Version ---
 * CREATE OR REPLACE FUNCTION SP_VALIDATE_STAGED_POLICIES(p_batch_id BIGINT)
 * RETURNS VOID AS $$ ... $$ LANGUAGE plpgsql;
 */

-- H2 Version
CREATE ALIAS IF NOT EXISTS SP_VALIDATE_STAGED_POLICIES AS $$
import java.sql.*;
import java.time.LocalDate;
import java.time.format.DateTimeParseException;
@CODE
void spValidateStagedPolicies(Connection conn, long batchId) throws SQLException {
    int valid = 0, rejected = 0, duplicates = 0;

    try (PreparedStatement sel = conn.prepareStatement(
            "SELECT STG_ID, POLICY_NUMBER, POLICY_TYPE, STATUS, " +
            "HOLDER_FIRST_NAME, HOLDER_LAST_NAME, EFFECTIVE_DATE, " +
            "EXPIRATION_DATE, PREMIUM_AMOUNT, COVERAGE_LIMIT, DEDUCTIBLE " +
            "FROM STG_POLICY_IMPORT WHERE BATCH_ID = ? AND VALIDATION_STATUS = 'PENDING'")) {
        sel.setLong(1, batchId);
        try (ResultSet rs = sel.executeQuery()) {
            while (rs.next()) {
                long stgId = rs.getLong("STG_ID");
                StringBuilder errors = new StringBuilder();

                String polNum = rs.getString("POLICY_NUMBER");
                String effDate = rs.getString("EFFECTIVE_DATE");
                String expDate = rs.getString("EXPIRATION_DATE");
                String premium = rs.getString("PREMIUM_AMOUNT");

                // Required fields
                if (polNum == null || polNum.trim().isEmpty())
                    errors.append("POLICY_NUMBER is required; ");
                if (rs.getString("HOLDER_LAST_NAME") == null)
                    errors.append("HOLDER_LAST_NAME is required; ");
                if (effDate == null)
                    errors.append("EFFECTIVE_DATE is required; ");

                // Policy number format (POL-TYPE-NNNNN)
                if (polNum != null && !polNum.matches("^POL-[A-Z]+-\\d{5}$"))
                    errors.append("POLICY_NUMBER format invalid (expected POL-TYPE-NNNNN); ");

                // Date validation
                LocalDate eff = null, exp = null;
                if (effDate != null) {
                    try { eff = LocalDate.parse(effDate.trim()); }
                    catch (DateTimeParseException e) { errors.append("EFFECTIVE_DATE is not valid; "); }
                }
                if (expDate != null) {
                    try { exp = LocalDate.parse(expDate.trim()); }
                    catch (DateTimeParseException e) { errors.append("EXPIRATION_DATE is not valid; "); }
                }
                if (eff != null && exp != null && !exp.isAfter(eff))
                    errors.append("EXPIRATION_DATE must be after EFFECTIVE_DATE; ");

                // Premium validation
                if (premium != null && !premium.trim().isEmpty()) {
                    try {
                        double p = Double.parseDouble(premium.trim());
                        if (p < 0) errors.append("PREMIUM_AMOUNT must be positive; ");
                    } catch (NumberFormatException e) {
                        errors.append("PREMIUM_AMOUNT is not a valid number; ");
                    }
                }

                // Duplicate check
                boolean isDuplicate = false;
                if (polNum != null && !polNum.trim().isEmpty()) {
                    try (PreparedStatement ps = conn.prepareStatement(
                            "SELECT 1 FROM POLICY WHERE POLICY_NUMBER = ?")) {
                        ps.setString(1, polNum.trim());
                        try (ResultSet r2 = ps.executeQuery()) {
                            if (r2.next()) {
                                errors.append("POLICY_NUMBER already exists; ");
                                isDuplicate = true;
                            }
                        }
                    }
                }

                String status = isDuplicate ? "DUPLICATE" : (errors.length() == 0 ? "VALID" : "REJECTED");
                try (PreparedStatement upd = conn.prepareStatement(
                        "UPDATE STG_POLICY_IMPORT SET VALIDATION_STATUS = ?, " +
                        "VALIDATION_ERRORS = ?, VALIDATED_AT = CURRENT_TIMESTAMP WHERE STG_ID = ?")) {
                    upd.setString(1, status);
                    upd.setString(2, errors.length() == 0 ? null : errors.toString());
                    upd.setLong(3, stgId);
                    upd.executeUpdate();
                }

                if (isDuplicate) duplicates++;
                else if (errors.length() == 0) valid++;
                else rejected++;
            }
        }
    }

    try (PreparedStatement upd = conn.prepareStatement(
            "UPDATE ETL_BATCH_LOG SET STATUS = 'VALIDATING', " +
            "VALID_ROWS = ?, REJECTED_ROWS = ?, DUPLICATE_ROWS = ? WHERE BATCH_ID = ?")) {
        upd.setInt(1, valid);
        upd.setInt(2, rejected);
        upd.setInt(3, duplicates);
        upd.setLong(4, batchId);
        upd.executeUpdate();
    }
}
$$;


-- ============================================================================
-- 9. SP_MERGE_STAGED_POLICIES
-- ============================================================================

CREATE ALIAS IF NOT EXISTS SP_MERGE_STAGED_POLICIES AS $$
import java.sql.*;
@CODE
int spMergeStagedPolicies(Connection conn, long batchId) throws SQLException {
    int merged = 0;
    try (PreparedStatement sel = conn.prepareStatement(
            "SELECT * FROM STG_POLICY_IMPORT WHERE BATCH_ID = ? AND VALIDATION_STATUS = 'VALID'")) {
        sel.setLong(1, batchId);
        try (ResultSet rs = sel.executeQuery()) {
            while (rs.next()) {
                long stgId = rs.getLong("STG_ID");
                try (PreparedStatement ins = conn.prepareStatement(
                        "INSERT INTO POLICY (ID, VERSION, POLICY_NUMBER, STATUS, POLICY_TYPE, " +
                        "HOLDER_FIRST_NAME, HOLDER_LAST_NAME, HOLDER_EMAIL, HOLDER_PHONE, " +
                        "EFFECTIVE_DATE, EXPIRATION_DATE, PREMIUM_AMOUNT, COVERAGE_LIMIT, " +
                        "DEDUCTIBLE, AGENT_CODE, CREATED_BY, CREATED_DATE) " +
                        "VALUES (NEXT VALUE FOR POLICY_SEQ, 0, ?, ?, ?, ?, ?, ?, ?, " +
                        "PARSEDATETIME(?, 'yyyy-MM-dd'), PARSEDATETIME(?, 'yyyy-MM-dd'), " +
                        "CAST(? AS DECIMAL(12,2)), CAST(? AS DECIMAL(14,2)), " +
                        "CAST(? AS DECIMAL(10,2)), ?, 'ETL_IMPORT', CURRENT_TIMESTAMP)")) {
                    ins.setString(1, rs.getString("POLICY_NUMBER"));
                    ins.setString(2, rs.getString("STATUS"));
                    ins.setString(3, rs.getString("POLICY_TYPE"));
                    ins.setString(4, rs.getString("HOLDER_FIRST_NAME"));
                    ins.setString(5, rs.getString("HOLDER_LAST_NAME"));
                    ins.setString(6, rs.getString("HOLDER_EMAIL"));
                    ins.setString(7, rs.getString("HOLDER_PHONE"));
                    ins.setString(8, rs.getString("EFFECTIVE_DATE"));
                    ins.setString(9, rs.getString("EXPIRATION_DATE"));
                    ins.setString(10, rs.getString("PREMIUM_AMOUNT"));
                    ins.setString(11, rs.getString("COVERAGE_LIMIT"));
                    ins.setString(12, rs.getString("DEDUCTIBLE"));
                    ins.setString(13, rs.getString("AGENT_CODE"));
                    ins.executeUpdate();
                }
                try (PreparedStatement upd = conn.prepareStatement(
                        "UPDATE STG_POLICY_IMPORT SET VALIDATION_STATUS = 'MERGED', " +
                        "MERGED_AT = CURRENT_TIMESTAMP WHERE STG_ID = ?")) {
                    upd.setLong(1, stgId);
                    upd.executeUpdate();
                }
                merged++;
            }
        }
    }

    try (PreparedStatement upd = conn.prepareStatement(
            "UPDATE ETL_BATCH_LOG SET STATUS = 'COMPLETED', MERGED_ROWS = ?, " +
            "COMPLETED_AT = CURRENT_TIMESTAMP WHERE BATCH_ID = ?")) {
        upd.setInt(1, merged);
        upd.setLong(2, batchId);
        upd.executeUpdate();
    }
    return merged;
}
$$;


-- ============================================================================
-- 10. SP_ETL_RECONCILIATION - Cross-system count/sum verification
-- ============================================================================
-- After an ETL cycle completes, this procedure compares:
--   - Count of records in staging vs production
--   - Sum of financial amounts in staging vs production
-- Returns a result set with the reconciliation results.
--
-- Used by the nightly health check and by auditors to verify data integrity.
-- ============================================================================

CREATE ALIAS IF NOT EXISTS SP_ETL_RECONCILIATION AS $$
import java.sql.*;
@CODE
ResultSet spEtlReconciliation(Connection conn, long batchId) throws SQLException {
    return conn.createStatement().executeQuery(
        "SELECT " +
        "  b.BATCH_ID, " +
        "  b.BATCH_TYPE, " +
        "  b.SOURCE_FILENAME, " +
        "  b.TOTAL_ROWS AS BATCH_TOTAL, " +
        "  b.VALID_ROWS AS BATCH_VALID, " +
        "  b.REJECTED_ROWS AS BATCH_REJECTED, " +
        "  b.DUPLICATE_ROWS AS BATCH_DUPLICATES, " +
        "  b.MERGED_ROWS AS BATCH_MERGED, " +
        "  COALESCE(stg.STG_COUNT, 0) AS ACTUAL_STG_ROWS, " +
        "  COALESCE(stg.STG_VALID, 0) AS ACTUAL_VALID_ROWS, " +
        "  COALESCE(stg.STG_MERGED, 0) AS ACTUAL_MERGED_ROWS, " +
        "  CASE WHEN b.MERGED_ROWS = COALESCE(stg.STG_MERGED, 0) " +
        "    THEN 'RECONCILED' ELSE 'DISCREPANCY' END AS RECON_STATUS, " +
        "  b.STARTED_AT, " +
        "  b.COMPLETED_AT " +
        "FROM ETL_BATCH_LOG b " +
        "LEFT JOIN (" +
        "  SELECT BATCH_ID, " +
        "    COUNT(*) AS STG_COUNT, " +
        "    SUM(CASE WHEN VALIDATION_STATUS = 'VALID' THEN 1 ELSE 0 END) AS STG_VALID, " +
        "    SUM(CASE WHEN VALIDATION_STATUS = 'MERGED' THEN 1 ELSE 0 END) AS STG_MERGED " +
        "  FROM STG_CLAIM_IMPORT GROUP BY BATCH_ID " +
        "  UNION ALL " +
        "  SELECT BATCH_ID, " +
        "    COUNT(*) AS STG_COUNT, " +
        "    SUM(CASE WHEN VALIDATION_STATUS = 'VALID' THEN 1 ELSE 0 END) AS STG_VALID, " +
        "    SUM(CASE WHEN VALIDATION_STATUS = 'MERGED' THEN 1 ELSE 0 END) AS STG_MERGED " +
        "  FROM STG_POLICY_IMPORT GROUP BY BATCH_ID " +
        ") stg ON stg.BATCH_ID = b.BATCH_ID " +
        "WHERE b.BATCH_ID = " + batchId
    );
}
$$;


-- ============================================================================
-- 11. RECONCILIATION & MONITORING VIEWS
-- ============================================================================

-- V_ETL_BATCH_SUMMARY: Dashboard view of recent ETL runs
CREATE OR REPLACE VIEW V_ETL_BATCH_SUMMARY AS
SELECT
    BATCH_ID,
    BATCH_TYPE,
    SOURCE_FILENAME,
    STATUS,
    TOTAL_ROWS,
    VALID_ROWS,
    REJECTED_ROWS,
    DUPLICATE_ROWS,
    MERGED_ROWS,
    CASE
        WHEN TOTAL_ROWS > 0
        THEN CAST(VALID_ROWS AS DECIMAL(5,2)) / TOTAL_ROWS * 100
        ELSE 0
    END AS VALID_PERCENT,
    CASE
        WHEN TOTAL_ROWS > 0
        THEN CAST(REJECTED_ROWS AS DECIMAL(5,2)) / TOTAL_ROWS * 100
        ELSE 0
    END AS REJECT_PERCENT,
    STARTED_AT,
    COMPLETED_AT,
    CASE
        WHEN COMPLETED_AT IS NOT NULL
        THEN DATEDIFF('SECOND', STARTED_AT, COMPLETED_AT)
        ELSE NULL
    END AS DURATION_SECONDS,
    EXECUTED_BY
FROM ETL_BATCH_LOG
ORDER BY STARTED_AT DESC;

-- V_STAGED_CLAIM_ERRORS: Shows all rejected/errored staged claims for review
CREATE OR REPLACE VIEW V_STAGED_CLAIM_ERRORS AS
SELECT
    s.STG_ID,
    s.BATCH_ID,
    b.SOURCE_FILENAME,
    s.LINE_NUMBER,
    s.CLAIM_NUMBER,
    s.POLICY_NUMBER,
    s.VALIDATION_STATUS,
    s.VALIDATION_ERRORS,
    s.VALIDATED_AT
FROM STG_CLAIM_IMPORT s
JOIN ETL_BATCH_LOG b ON b.BATCH_ID = s.BATCH_ID
WHERE s.VALIDATION_STATUS IN ('REJECTED', 'DUPLICATE')
ORDER BY s.BATCH_ID DESC, s.LINE_NUMBER;

-- V_ETL_DAILY_STATS: Aggregated daily import statistics
CREATE OR REPLACE VIEW V_ETL_DAILY_STATS AS
SELECT
    CAST(STARTED_AT AS DATE) AS IMPORT_DATE,
    BATCH_TYPE,
    COUNT(*)                 AS BATCH_COUNT,
    SUM(TOTAL_ROWS)          AS TOTAL_RECORDS,
    SUM(MERGED_ROWS)         AS TOTAL_MERGED,
    SUM(REJECTED_ROWS)       AS TOTAL_REJECTED,
    SUM(DUPLICATE_ROWS)      AS TOTAL_DUPLICATES,
    SUM(CASE WHEN STATUS = 'COMPLETED' THEN 1 ELSE 0 END) AS SUCCESSFUL_BATCHES,
    SUM(CASE WHEN STATUS = 'FAILED' THEN 1 ELSE 0 END)    AS FAILED_BATCHES
FROM ETL_BATCH_LOG
GROUP BY CAST(STARTED_AT AS DATE), BATCH_TYPE
ORDER BY IMPORT_DATE DESC, BATCH_TYPE;

-- V_DATA_QUALITY_SCORECARD: Overall data quality metrics across recent imports
CREATE OR REPLACE VIEW V_DATA_QUALITY_SCORECARD AS
SELECT
    BATCH_TYPE,
    COUNT(DISTINCT BATCH_ID)  AS TOTAL_BATCHES,
    SUM(TOTAL_ROWS)           AS TOTAL_RECORDS_PROCESSED,
    SUM(VALID_ROWS)           AS TOTAL_VALID,
    SUM(REJECTED_ROWS)        AS TOTAL_REJECTED,
    SUM(DUPLICATE_ROWS)       AS TOTAL_DUPLICATES,
    CASE
        WHEN SUM(TOTAL_ROWS) > 0
        THEN CAST(SUM(VALID_ROWS) AS DECIMAL(7,2)) / SUM(TOTAL_ROWS) * 100
        ELSE 0
    END                       AS OVERALL_QUALITY_PERCENT,
    MAX(STARTED_AT)           AS LAST_RUN
FROM ETL_BATCH_LOG
WHERE STARTED_AT >= DATEADD('DAY', -30, CURRENT_TIMESTAMP)
GROUP BY BATCH_TYPE;


-- ============================================================================
-- 12. DATA RETENTION / PURGE PROCEDURE
-- ============================================================================
-- Purges staging table data older than the specified retention period.
-- Production data is NEVER deleted by this procedure.
-- ETL_BATCH_LOG rows are kept for audit purposes (only staging rows purged).
--
-- Retention policy:
--   - MERGED rows:   Purge after 90 days (data is safely in production)
--   - REJECTED rows: Purge after 180 days (keep longer for investigation)
--   - DUPLICATE rows: Purge after 90 days
-- ============================================================================

CREATE ALIAS IF NOT EXISTS SP_PURGE_STAGING_DATA AS $$
import java.sql.*;
@CODE
String spPurgeStagingData(Connection conn, int mergedRetentionDays, int rejectedRetentionDays) throws SQLException {
    int claimsPurged = 0, policiesPurged = 0, paymentsPurged = 0;

    // Purge merged/duplicate claim staging rows
    try (PreparedStatement ps = conn.prepareStatement(
            "DELETE FROM STG_CLAIM_IMPORT " +
            "WHERE VALIDATION_STATUS IN ('MERGED', 'DUPLICATE') " +
            "AND MERGED_AT < DATEADD('DAY', -?, CURRENT_TIMESTAMP)")) {
        ps.setInt(1, mergedRetentionDays);
        claimsPurged += ps.executeUpdate();
    }
    // Purge rejected claim staging rows (longer retention)
    try (PreparedStatement ps = conn.prepareStatement(
            "DELETE FROM STG_CLAIM_IMPORT " +
            "WHERE VALIDATION_STATUS = 'REJECTED' " +
            "AND VALIDATED_AT < DATEADD('DAY', -?, CURRENT_TIMESTAMP)")) {
        ps.setInt(1, rejectedRetentionDays);
        claimsPurged += ps.executeUpdate();
    }

    // Same for policies
    try (PreparedStatement ps = conn.prepareStatement(
            "DELETE FROM STG_POLICY_IMPORT " +
            "WHERE VALIDATION_STATUS IN ('MERGED', 'DUPLICATE') " +
            "AND MERGED_AT < DATEADD('DAY', -?, CURRENT_TIMESTAMP)")) {
        ps.setInt(1, mergedRetentionDays);
        policiesPurged += ps.executeUpdate();
    }
    try (PreparedStatement ps = conn.prepareStatement(
            "DELETE FROM STG_POLICY_IMPORT " +
            "WHERE VALIDATION_STATUS = 'REJECTED' " +
            "AND VALIDATED_AT < DATEADD('DAY', -?, CURRENT_TIMESTAMP)")) {
        ps.setInt(1, rejectedRetentionDays);
        policiesPurged += ps.executeUpdate();
    }

    // Same for payments
    try (PreparedStatement ps = conn.prepareStatement(
            "DELETE FROM STG_PAYMENT_IMPORT " +
            "WHERE VALIDATION_STATUS IN ('MERGED', 'DUPLICATE') " +
            "AND MERGED_AT < DATEADD('DAY', -?, CURRENT_TIMESTAMP)")) {
        ps.setInt(1, mergedRetentionDays);
        paymentsPurged += ps.executeUpdate();
    }
    try (PreparedStatement ps = conn.prepareStatement(
            "DELETE FROM STG_PAYMENT_IMPORT " +
            "WHERE VALIDATION_STATUS = 'REJECTED' " +
            "AND VALIDATED_AT < DATEADD('DAY', -?, CURRENT_TIMESTAMP)")) {
        ps.setInt(1, rejectedRetentionDays);
        paymentsPurged += ps.executeUpdate();
    }

    return "Purged staging data: claims=" + claimsPurged +
           ", policies=" + policiesPurged +
           ", payments=" + paymentsPurged;
}
$$;


-- ============================================================================
-- END OF schema-etl.sql
-- ============================================================================
-- Post-deployment checklist:
--   [ ] Run after schema.sql and schema-procedures.sql
--   [ ] Verify all staging tables created
--   [ ] Test SP_VALIDATE_STAGED_CLAIMS with sample-claims-import.csv data
--   [ ] Grant execute permissions to ETL service account
--   [ ] Configure retention parameters in scheduled purge job
--   [ ] For PostgreSQL: replace H2 ALIAS blocks with PL/pgSQL equivalents
-- ============================================================================
