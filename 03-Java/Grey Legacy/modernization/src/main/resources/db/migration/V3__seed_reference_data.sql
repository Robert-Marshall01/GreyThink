-- ============================================================================
-- Flyway Migration V3: Seed Reference Data and Stored Procedures
-- ============================================================================
-- Seeds the reference/lookup data that was previously loaded by
-- ApplicationContext init scripts. Also recreates stored procedures
-- under Flyway version control (previously managed by DBA ad-hoc scripts).
-- ============================================================================

-- ==========================================
-- Seed Adjusters (reference data)
-- ==========================================
INSERT INTO ADJUSTER (ID, ADJUSTER_CODE, FIRST_NAME, LAST_NAME, EMAIL, PHONE, SPECIALIZATION, ACTIVE, MAX_CASELOAD, CURRENT_CASELOAD, CREATED_BY, CREATED_DATE)
VALUES (NEXT VALUE FOR ADJUSTER_SEQ, 'ADJ001', 'Robert', 'Chen',     'rchen@greylegacy.com',     '555-0101', 'AUTO',     TRUE, 40, 0,  'SYSTEM', CURRENT_TIMESTAMP);
INSERT INTO ADJUSTER (ID, ADJUSTER_CODE, FIRST_NAME, LAST_NAME, EMAIL, PHONE, SPECIALIZATION, ACTIVE, MAX_CASELOAD, CURRENT_CASELOAD, CREATED_BY, CREATED_DATE)
VALUES (NEXT VALUE FOR ADJUSTER_SEQ, 'ADJ002', 'Maria', 'Gonzalez', 'mgonzalez@greylegacy.com', '555-0102', 'PROPERTY', TRUE, 35, 0,  'SYSTEM', CURRENT_TIMESTAMP);
INSERT INTO ADJUSTER (ID, ADJUSTER_CODE, FIRST_NAME, LAST_NAME, EMAIL, PHONE, SPECIALIZATION, ACTIVE, MAX_CASELOAD, CURRENT_CASELOAD, CREATED_BY, CREATED_DATE)
VALUES (NEXT VALUE FOR ADJUSTER_SEQ, 'ADJ003', 'James', 'Williams', 'jwilliams@greylegacy.com', '555-0103', 'LIABILITY', TRUE, 30, 0, 'SYSTEM', CURRENT_TIMESTAMP);
INSERT INTO ADJUSTER (ID, ADJUSTER_CODE, FIRST_NAME, LAST_NAME, EMAIL, PHONE, SPECIALIZATION, ACTIVE, MAX_CASELOAD, CURRENT_CASELOAD, CREATED_BY, CREATED_DATE)
VALUES (NEXT VALUE FOR ADJUSTER_SEQ, 'ADJ004', 'Sarah', 'Johnson',  'sjohnson@greylegacy.com',  '555-0104', 'WORKERS_COMP', TRUE, 25, 0, 'SYSTEM', CURRENT_TIMESTAMP);
INSERT INTO ADJUSTER (ID, ADJUSTER_CODE, FIRST_NAME, LAST_NAME, EMAIL, PHONE, SPECIALIZATION, ACTIVE, MAX_CASELOAD, CURRENT_CASELOAD, CREATED_BY, CREATED_DATE)
VALUES (NEXT VALUE FOR ADJUSTER_SEQ, 'ADJ005', 'David', 'Kim',      'dkim@greylegacy.com',      '555-0105', 'MEDICAL',   TRUE, 45, 0,  'SYSTEM', CURRENT_TIMESTAMP);

-- ==========================================
-- Seed Sample Policies (dev/staging)
-- ==========================================
INSERT INTO POLICY (ID, POLICY_NUMBER, STATUS, POLICY_TYPE, HOLDER_FIRST_NAME, HOLDER_LAST_NAME, HOLDER_SSN, HOLDER_EMAIL, EFFECTIVE_DATE, EXPIRATION_DATE, PREMIUM_AMOUNT, COVERAGE_LIMIT, DEDUCTIBLE, AGENT_CODE, CREATED_BY, CREATED_DATE)
VALUES (NEXT VALUE FOR POLICY_SEQ, 'POL-2024-00001', 'ACTIVE', 'AUTO',     'John',    'Smith',    '***-**-1234', 'jsmith@example.com',    '2024-01-01', '2025-01-01', 1200.00, 100000.00, 500.00,  'AGT001', 'SYSTEM', CURRENT_TIMESTAMP);
INSERT INTO POLICY (ID, POLICY_NUMBER, STATUS, POLICY_TYPE, HOLDER_FIRST_NAME, HOLDER_LAST_NAME, HOLDER_SSN, HOLDER_EMAIL, EFFECTIVE_DATE, EXPIRATION_DATE, PREMIUM_AMOUNT, COVERAGE_LIMIT, DEDUCTIBLE, AGENT_CODE, CREATED_BY, CREATED_DATE)
VALUES (NEXT VALUE FOR POLICY_SEQ, 'POL-2024-00002', 'ACTIVE', 'HOMEOWNER','Jane',    'Doe',      '***-**-5678', 'jdoe@example.com',      '2024-03-15', '2025-03-15', 2400.00, 500000.00, 1000.00, 'AGT002', 'SYSTEM', CURRENT_TIMESTAMP);
INSERT INTO POLICY (ID, POLICY_NUMBER, STATUS, POLICY_TYPE, HOLDER_FIRST_NAME, HOLDER_LAST_NAME, HOLDER_SSN, HOLDER_EMAIL, EFFECTIVE_DATE, EXPIRATION_DATE, PREMIUM_AMOUNT, COVERAGE_LIMIT, DEDUCTIBLE, AGENT_CODE, CREATED_BY, CREATED_DATE)
VALUES (NEXT VALUE FOR POLICY_SEQ, 'POL-2024-00003', 'ACTIVE', 'COMMERCIAL_PROPERTY', 'Acme',  'Corp',     NULL,          'insurance@acme.com',   '2024-06-01', '2025-06-01', 8500.00, 2000000.00, 5000.00, 'AGT001', 'SYSTEM', CURRENT_TIMESTAMP);
INSERT INTO POLICY (ID, POLICY_NUMBER, STATUS, POLICY_TYPE, HOLDER_FIRST_NAME, HOLDER_LAST_NAME, HOLDER_SSN, HOLDER_EMAIL, EFFECTIVE_DATE, EXPIRATION_DATE, PREMIUM_AMOUNT, COVERAGE_LIMIT, DEDUCTIBLE, AGENT_CODE, CREATED_BY, CREATED_DATE)
VALUES (NEXT VALUE FOR POLICY_SEQ, 'POL-2023-00010', 'EXPIRED', 'AUTO',    'Michael', 'Brown',    '***-**-9012', 'mbrown@example.com',    '2023-01-01', '2024-01-01', 1100.00, 75000.00,  500.00,  'AGT003', 'SYSTEM', CURRENT_TIMESTAMP);

-- ==========================================
-- Stored Procedures (migrated from schema-procedures.sql)
-- ==========================================

-- Calculate aged claim statistics by status bucket
-- Used by ClaimDashboardAction and management reports
CREATE ALIAS IF NOT EXISTS SP_CALCULATE_CLAIM_AGING FOR "com.greylegacy.dao.jdbc.StoredProcedureDao.calculateClaimAging";

-- Auto-assign claims to adjusters based on specialization and caseload
-- Called by FnolAction after FNOL submission to route to best available adjuster
CREATE ALIAS IF NOT EXISTS SP_AUTO_ASSIGN_ADJUSTER FOR "com.greylegacy.dao.jdbc.StoredProcedureDao.autoAssignAdjuster";

-- Monthly premium reconciliation procedure
-- Invoked by PremiumRecalculationJob at end of each billing cycle
CREATE ALIAS IF NOT EXISTS SP_RECONCILE_PREMIUMS FOR "com.greylegacy.dao.jdbc.StoredProcedureDao.reconcilePremiums";
