--------------------------------------------------------------
-- PostgreSQL Schema for Compliance Auditing System
-- 
-- This script creates the database schema and seeds baseline
-- compliance rules for infrastructure compliance checking.
--
-- Tables:
--   1. compliance_rules - Stores compliance rule definitions
--   2. resource_audit   - Logs compliance check results
--
-- Author: DevSecOps Team
-- Version: 1.0.0
--------------------------------------------------------------

-- Enable required extensions
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";
CREATE EXTENSION IF NOT EXISTS "pgcrypto";

--------------------------------------------------------------
-- SCHEMA CREATION
--------------------------------------------------------------

-- Create schema for compliance objects
CREATE SCHEMA IF NOT EXISTS compliance;

-- Set search path
SET search_path TO compliance, public;

--------------------------------------------------------------
-- TABLE: compliance_rules
-- 
-- Stores all compliance rules that are checked against
-- infrastructure resources. Each rule contains:
-- - Unique identifier
-- - Human-readable name and description
-- - Severity level for prioritization
-- - SQL check query or reference
-- - Metadata for categorization and tracking
--------------------------------------------------------------

CREATE TABLE IF NOT EXISTS compliance.compliance_rules (
    -- Primary key using UUID for distributed systems compatibility
    rule_id         UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    
    -- Rule identification
    rule_name       VARCHAR(255) NOT NULL,
    rule_code       VARCHAR(50) UNIQUE NOT NULL,
    description     TEXT,
    
    -- Categorization
    category        VARCHAR(100) NOT NULL DEFAULT 'general',
    cloud_provider  VARCHAR(50) DEFAULT 'aws',
    resource_type   VARCHAR(100),
    
    -- Severity classification
    -- CRITICAL: Immediate action required, security risk
    -- HIGH: Action required within 24 hours
    -- MEDIUM: Action required within 7 days
    -- LOW: Best practice, no immediate action required
    severity        VARCHAR(20) NOT NULL CHECK (severity IN ('CRITICAL', 'HIGH', 'MEDIUM', 'LOW')),
    
    -- Compliance check definition
    -- SQL query or script reference for compliance verification
    sql_check       TEXT NOT NULL,
    
    -- Remediation guidance
    remediation     TEXT,
    documentation_url VARCHAR(500),
    
    -- Rule status
    is_active       BOOLEAN DEFAULT TRUE,
    is_automated    BOOLEAN DEFAULT TRUE,
    
    -- Audit fields
    created_at      TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    updated_at      TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    created_by      VARCHAR(100) DEFAULT 'system',
    
    -- Framework mappings (e.g., CIS, SOC2, PCI-DSS)
    frameworks      JSONB DEFAULT '[]'::jsonb,
    
    -- Additional metadata
    metadata        JSONB DEFAULT '{}'::jsonb
);

-- Create indexes for performance
CREATE INDEX IF NOT EXISTS idx_rules_category ON compliance.compliance_rules(category);
CREATE INDEX IF NOT EXISTS idx_rules_severity ON compliance.compliance_rules(severity);
CREATE INDEX IF NOT EXISTS idx_rules_cloud_provider ON compliance.compliance_rules(cloud_provider);
CREATE INDEX IF NOT EXISTS idx_rules_resource_type ON compliance.compliance_rules(resource_type);
CREATE INDEX IF NOT EXISTS idx_rules_active ON compliance.compliance_rules(is_active) WHERE is_active = TRUE;

-- Add comments
COMMENT ON TABLE compliance.compliance_rules IS 'Stores compliance rule definitions for infrastructure auditing';
COMMENT ON COLUMN compliance.compliance_rules.sql_check IS 'SQL query or script reference used to verify compliance';
COMMENT ON COLUMN compliance.compliance_rules.severity IS 'Risk severity: CRITICAL, HIGH, MEDIUM, or LOW';

--------------------------------------------------------------
-- TABLE: resource_audit
-- 
-- Logs the results of compliance checks performed against
-- infrastructure resources. Provides audit trail for:
-- - Which resources were checked
-- - When they were checked
-- - Compliance status
-- - Evidence and details
--------------------------------------------------------------

CREATE TABLE IF NOT EXISTS compliance.resource_audit (
    -- Primary key
    audit_id            UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    
    -- Resource identification
    resource_id         VARCHAR(500) NOT NULL,
    resource_name       VARCHAR(255),
    resource_type       VARCHAR(100) NOT NULL,
    resource_arn        VARCHAR(500),
    cloud_provider      VARCHAR(50) DEFAULT 'aws',
    region              VARCHAR(50),
    account_id          VARCHAR(50),
    
    -- Compliance check reference
    rule_id             UUID REFERENCES compliance.compliance_rules(rule_id),
    rule_code           VARCHAR(50),
    
    -- Compliance status
    -- COMPLIANT: Resource meets the rule requirements
    -- NON_COMPLIANT: Resource violates the rule
    -- NOT_APPLICABLE: Rule doesn't apply to this resource
    -- ERROR: Check failed due to an error
    -- PENDING: Check scheduled but not yet executed
    compliance_status   VARCHAR(20) NOT NULL CHECK (
        compliance_status IN ('COMPLIANT', 'NON_COMPLIANT', 'NOT_APPLICABLE', 'ERROR', 'PENDING')
    ),
    
    -- Check details
    checked_at          TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    check_duration_ms   INTEGER,
    
    -- Evidence and details
    evidence            JSONB DEFAULT '{}'::jsonb,
    error_message       TEXT,
    remediation_status  VARCHAR(50) DEFAULT 'PENDING',
    remediation_notes   TEXT,
    
    -- Scanner information
    scanner_version     VARCHAR(50),
    scan_id             UUID,
    
    -- Audit fields
    created_at          TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    created_by          VARCHAR(100) DEFAULT 'system'
);

-- Create indexes for common queries
CREATE INDEX IF NOT EXISTS idx_audit_resource_id ON compliance.resource_audit(resource_id);
CREATE INDEX IF NOT EXISTS idx_audit_resource_type ON compliance.resource_audit(resource_type);
CREATE INDEX IF NOT EXISTS idx_audit_status ON compliance.resource_audit(compliance_status);
CREATE INDEX IF NOT EXISTS idx_audit_checked_at ON compliance.resource_audit(checked_at DESC);
CREATE INDEX IF NOT EXISTS idx_audit_rule_id ON compliance.resource_audit(rule_id);
CREATE INDEX IF NOT EXISTS idx_audit_account_region ON compliance.resource_audit(account_id, region);
CREATE INDEX IF NOT EXISTS idx_audit_non_compliant ON compliance.resource_audit(compliance_status) 
    WHERE compliance_status = 'NON_COMPLIANT';

-- Composite index for common dashboard queries
CREATE INDEX IF NOT EXISTS idx_audit_dashboard ON compliance.resource_audit(
    cloud_provider, resource_type, compliance_status, checked_at DESC
);

-- Add comments
COMMENT ON TABLE compliance.resource_audit IS 'Logs compliance check results for infrastructure resources';
COMMENT ON COLUMN compliance.resource_audit.evidence IS 'JSON evidence collected during compliance check';
COMMENT ON COLUMN compliance.resource_audit.compliance_status IS 'Status: COMPLIANT, NON_COMPLIANT, NOT_APPLICABLE, ERROR, PENDING';

--------------------------------------------------------------
-- TRIGGER: Update timestamp on rule modification
--------------------------------------------------------------

CREATE OR REPLACE FUNCTION compliance.update_modified_timestamp()
RETURNS TRIGGER AS $$
BEGIN
    NEW.updated_at = CURRENT_TIMESTAMP;
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

DROP TRIGGER IF EXISTS trigger_rules_updated_at ON compliance.compliance_rules;
CREATE TRIGGER trigger_rules_updated_at
    BEFORE UPDATE ON compliance.compliance_rules
    FOR EACH ROW
    EXECUTE FUNCTION compliance.update_modified_timestamp();

--------------------------------------------------------------
-- SEED DATA: Baseline Compliance Rules
-- 
-- These rules cover common AWS infrastructure compliance
-- requirements for security and best practices.
--------------------------------------------------------------

INSERT INTO compliance.compliance_rules (
    rule_code, rule_name, description, category, cloud_provider, 
    resource_type, severity, sql_check, remediation, documentation_url, frameworks
) VALUES

-- S3 Bucket Security Rules
(
    'S3-001',
    'All S3 buckets must be encrypted',
    'Ensures that all S3 buckets have server-side encryption enabled to protect data at rest.',
    'data-protection',
    'aws',
    'aws_s3_bucket',
    'HIGH',
    'SELECT bucket_name FROM aws_s3_bucket WHERE server_side_encryption_configuration IS NULL OR server_side_encryption_configuration = ''{}''',
    'Enable server-side encryption using AWS KMS (SSE-KMS) or AES-256 (SSE-S3). Use aws_s3_bucket_server_side_encryption_configuration resource in Terraform.',
    'https://docs.aws.amazon.com/AmazonS3/latest/userguide/bucket-encryption.html',
    '["CIS-AWS-1.4", "SOC2-CC6.1", "PCI-DSS-3.4"]'::jsonb
),
(
    'S3-002',
    'S3 buckets must block public access',
    'Ensures that S3 buckets have public access blocked to prevent unauthorized data exposure.',
    'access-control',
    'aws',
    'aws_s3_bucket',
    'CRITICAL',
    'SELECT bucket_name FROM aws_s3_bucket b LEFT JOIN aws_s3_bucket_public_access_block p ON b.bucket_name = p.bucket_name WHERE p.block_public_acls IS NOT TRUE OR p.block_public_policy IS NOT TRUE',
    'Enable S3 Block Public Access settings at the bucket or account level. Use aws_s3_bucket_public_access_block resource in Terraform.',
    'https://docs.aws.amazon.com/AmazonS3/latest/userguide/access-control-block-public-access.html',
    '["CIS-AWS-2.1.5", "SOC2-CC6.6", "NIST-AC-3"]'::jsonb
),
(
    'S3-003',
    'S3 buckets must have versioning enabled',
    'Ensures S3 buckets have versioning enabled for data recovery and protection against accidental deletion.',
    'data-protection',
    'aws',
    'aws_s3_bucket',
    'MEDIUM',
    'SELECT bucket_name FROM aws_s3_bucket WHERE versioning_status IS NULL OR versioning_status != ''Enabled''',
    'Enable versioning on S3 buckets using aws_s3_bucket_versioning resource in Terraform.',
    'https://docs.aws.amazon.com/AmazonS3/latest/userguide/Versioning.html',
    '["CIS-AWS-2.1.3", "SOC2-CC7.2"]'::jsonb
),
(
    'S3-004',
    'S3 buckets must have access logging enabled',
    'Ensures S3 buckets have server access logging enabled for audit and security monitoring.',
    'logging',
    'aws',
    'aws_s3_bucket',
    'MEDIUM',
    'SELECT bucket_name FROM aws_s3_bucket WHERE logging_target_bucket IS NULL',
    'Enable server access logging for S3 buckets. Configure a target bucket for log delivery.',
    'https://docs.aws.amazon.com/AmazonS3/latest/userguide/ServerLogs.html',
    '["CIS-AWS-3.6", "SOC2-CC7.1"]'::jsonb
),

-- EC2 Instance Security Rules
(
    'EC2-001',
    'EC2 instances must use encrypted EBS volumes',
    'Ensures all EBS volumes attached to EC2 instances are encrypted to protect data at rest.',
    'data-protection',
    'aws',
    'aws_instance',
    'HIGH',
    'SELECT instance_id FROM aws_ec2_instance i JOIN aws_ebs_volume v ON i.instance_id = ANY(v.attachments) WHERE v.encrypted = FALSE',
    'Enable EBS encryption by default in the account settings or specify encryption when creating volumes.',
    'https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html',
    '["CIS-AWS-2.2.1", "SOC2-CC6.1", "PCI-DSS-3.4"]'::jsonb
),
(
    'EC2-002',
    'EC2 instances must not have public IP addresses',
    'Ensures EC2 instances in private subnets do not have public IP addresses assigned.',
    'network-security',
    'aws',
    'aws_instance',
    'HIGH',
    'SELECT instance_id FROM aws_ec2_instance WHERE public_ip_address IS NOT NULL AND subnet_type = ''private''',
    'Remove public IP addresses from instances in private subnets. Use NAT Gateway for outbound internet access.',
    'https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-instance-addressing.html',
    '["CIS-AWS-4.1", "NIST-SC-7"]'::jsonb
),
(
    'EC2-003',
    'EC2 instances must use IMDSv2',
    'Ensures EC2 instances require Instance Metadata Service Version 2 (IMDSv2) to prevent SSRF attacks.',
    'access-control',
    'aws',
    'aws_instance',
    'MEDIUM',
    'SELECT instance_id FROM aws_ec2_instance WHERE metadata_options_http_tokens != ''required''',
    'Configure instances to require IMDSv2 by setting http_tokens to required in metadata_options.',
    'https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/configuring-instance-metadata-service.html',
    '["CIS-AWS-5.6", "AWS-Best-Practices"]'::jsonb
),

-- RDS Database Security Rules
(
    'RDS-001',
    'RDS instances must be encrypted at rest',
    'Ensures all RDS database instances have encryption at rest enabled.',
    'data-protection',
    'aws',
    'aws_db_instance',
    'CRITICAL',
    'SELECT db_instance_identifier FROM aws_rds_db_instance WHERE storage_encrypted = FALSE',
    'Enable encryption when creating RDS instances. Note: Encryption cannot be enabled on existing unencrypted instances.',
    'https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Overview.Encryption.html',
    '["CIS-AWS-2.3.1", "SOC2-CC6.1", "PCI-DSS-3.4", "HIPAA"]'::jsonb
),
(
    'RDS-002',
    'RDS instances must not be publicly accessible',
    'Ensures RDS instances are not accessible from the public internet.',
    'network-security',
    'aws',
    'aws_db_instance',
    'CRITICAL',
    'SELECT db_instance_identifier FROM aws_rds_db_instance WHERE publicly_accessible = TRUE',
    'Set publicly_accessible to false and use VPC security groups to control access.',
    'https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_VPC.WorkingWithRDSInstanceinaVPC.html',
    '["CIS-AWS-4.3", "SOC2-CC6.6", "PCI-DSS-1.3"]'::jsonb
),
(
    'RDS-003',
    'RDS instances must have automated backups enabled',
    'Ensures RDS instances have automated backups configured with appropriate retention.',
    'data-protection',
    'aws',
    'aws_db_instance',
    'HIGH',
    'SELECT db_instance_identifier FROM aws_rds_db_instance WHERE backup_retention_period < 7',
    'Configure backup_retention_period to at least 7 days. Consider 35 days for production workloads.',
    'https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_WorkingWithAutomatedBackups.html',
    '["CIS-AWS-2.3.2", "SOC2-CC7.2"]'::jsonb
),
(
    'RDS-004',
    'RDS instances must enforce SSL connections',
    'Ensures all connections to RDS instances use SSL/TLS encryption.',
    'data-protection',
    'aws',
    'aws_db_instance',
    'HIGH',
    'SELECT db_instance_identifier FROM aws_rds_db_instance WHERE ssl_enforcement = FALSE OR ca_certificate_identifier IS NULL',
    'Enable SSL enforcement in the RDS parameter group. Configure application connection strings to use SSL.',
    'https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.SSL.html',
    '["CIS-AWS-2.3.3", "SOC2-CC6.1", "PCI-DSS-4.1"]'::jsonb
),

-- IAM Security Rules
(
    'IAM-001',
    'IAM policies must not allow full "*" admin access',
    'Ensures IAM policies follow least privilege principle and do not grant unrestricted access.',
    'access-control',
    'aws',
    'aws_iam_policy',
    'CRITICAL',
    'SELECT policy_name FROM aws_iam_policy WHERE policy_document::text LIKE ''%"Action": "*"%'' AND policy_document::text LIKE ''%"Resource": "*"%''',
    'Review and restrict IAM policies to specific actions and resources required for the role.',
    'https://docs.aws.amazon.com/IAM/latest/UserGuide/best-practices.html#grant-least-privilege',
    '["CIS-AWS-1.16", "SOC2-CC6.3", "NIST-AC-6"]'::jsonb
),
(
    'IAM-002',
    'IAM users must have MFA enabled',
    'Ensures all IAM users have multi-factor authentication enabled.',
    'access-control',
    'aws',
    'aws_iam_user',
    'HIGH',
    'SELECT user_name FROM aws_iam_user WHERE mfa_enabled = FALSE AND console_access = TRUE',
    'Enable MFA for all IAM users with console access. Use virtual or hardware MFA devices.',
    'https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_mfa.html',
    '["CIS-AWS-1.10", "SOC2-CC6.1", "PCI-DSS-8.3"]'::jsonb
),
(
    'IAM-003',
    'IAM access keys must be rotated within 90 days',
    'Ensures IAM access keys are rotated regularly to limit exposure of compromised credentials.',
    'access-control',
    'aws',
    'aws_iam_access_key',
    'MEDIUM',
    'SELECT access_key_id, user_name FROM aws_iam_access_key WHERE create_date < NOW() - INTERVAL ''90 days'' AND status = ''Active''',
    'Implement access key rotation policy. Use AWS Secrets Manager for automated rotation.',
    'https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_access-keys.html',
    '["CIS-AWS-1.14", "SOC2-CC6.1", "NIST-IA-5"]'::jsonb
),

-- VPC and Network Security Rules
(
    'VPC-001',
    'Security groups must not allow unrestricted SSH access',
    'Ensures security groups do not allow SSH (port 22) access from 0.0.0.0/0.',
    'network-security',
    'aws',
    'aws_security_group',
    'CRITICAL',
    'SELECT group_id, group_name FROM aws_security_group_rule WHERE from_port <= 22 AND to_port >= 22 AND cidr_blocks @> ''["0.0.0.0/0"]''::jsonb',
    'Restrict SSH access to specific IP ranges or use AWS Systems Manager Session Manager.',
    'https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/security-group-rules-reference.html',
    '["CIS-AWS-5.2", "SOC2-CC6.6", "PCI-DSS-1.3"]'::jsonb
),
(
    'VPC-002',
    'Security groups must not allow unrestricted RDP access',
    'Ensures security groups do not allow RDP (port 3389) access from 0.0.0.0/0.',
    'network-security',
    'aws',
    'aws_security_group',
    'CRITICAL',
    'SELECT group_id, group_name FROM aws_security_group_rule WHERE from_port <= 3389 AND to_port >= 3389 AND cidr_blocks @> ''["0.0.0.0/0"]''::jsonb',
    'Restrict RDP access to specific IP ranges or use AWS Systems Manager Session Manager.',
    'https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/security-group-rules-reference.html',
    '["CIS-AWS-5.3", "SOC2-CC6.6", "PCI-DSS-1.3"]'::jsonb
),
(
    'VPC-003',
    'VPC flow logs must be enabled',
    'Ensures VPC flow logs are enabled for network traffic monitoring and security analysis.',
    'logging',
    'aws',
    'aws_vpc',
    'HIGH',
    'SELECT vpc_id FROM aws_vpc v LEFT JOIN aws_vpc_flow_log f ON v.vpc_id = f.vpc_id WHERE f.flow_log_id IS NULL',
    'Enable VPC flow logs and send them to CloudWatch Logs or S3 for analysis.',
    'https://docs.aws.amazon.com/vpc/latest/userguide/flow-logs.html',
    '["CIS-AWS-3.9", "SOC2-CC7.1", "NIST-AU-12"]'::jsonb
),

-- CloudTrail and Logging Rules
(
    'LOG-001',
    'CloudTrail must be enabled in all regions',
    'Ensures CloudTrail is enabled across all AWS regions for comprehensive audit logging.',
    'logging',
    'aws',
    'aws_cloudtrail',
    'CRITICAL',
    'SELECT trail_name FROM aws_cloudtrail WHERE is_multi_region_trail = FALSE OR is_logging = FALSE',
    'Configure CloudTrail with multi-region logging enabled. Store logs in encrypted S3 bucket.',
    'https://docs.aws.amazon.com/awscloudtrail/latest/userguide/cloudtrail-create-and-update-a-trail.html',
    '["CIS-AWS-3.1", "SOC2-CC7.1", "PCI-DSS-10.1"]'::jsonb
),
(
    'LOG-002',
    'CloudTrail logs must be encrypted',
    'Ensures CloudTrail logs are encrypted using AWS KMS for data protection.',
    'data-protection',
    'aws',
    'aws_cloudtrail',
    'HIGH',
    'SELECT trail_name FROM aws_cloudtrail WHERE kms_key_id IS NULL',
    'Enable SSE-KMS encryption for CloudTrail logs. Create a dedicated KMS key for CloudTrail.',
    'https://docs.aws.amazon.com/awscloudtrail/latest/userguide/encrypting-cloudtrail-log-files-with-aws-kms.html',
    '["CIS-AWS-3.7", "SOC2-CC6.1", "PCI-DSS-3.4"]'::jsonb
),

-- KMS Key Management Rules
(
    'KMS-001',
    'KMS keys must have key rotation enabled',
    'Ensures AWS KMS customer managed keys have automatic rotation enabled.',
    'data-protection',
    'aws',
    'aws_kms_key',
    'MEDIUM',
    'SELECT key_id FROM aws_kms_key WHERE key_rotation_enabled = FALSE AND key_manager = ''CUSTOMER''',
    'Enable automatic key rotation for customer managed KMS keys in the key policy.',
    'https://docs.aws.amazon.com/kms/latest/developerguide/rotate-keys.html',
    '["CIS-AWS-3.8", "SOC2-CC6.1", "NIST-SC-12"]'::jsonb
),

-- Lambda Security Rules
(
    'LAMBDA-001',
    'Lambda functions must use latest runtime versions',
    'Ensures Lambda functions use supported runtime versions for security patches.',
    'vulnerability-management',
    'aws',
    'aws_lambda_function',
    'MEDIUM',
    'SELECT function_name, runtime FROM aws_lambda_function WHERE runtime IN (''python2.7'', ''nodejs10.x'', ''nodejs8.10'', ''dotnetcore2.1'')',
    'Update Lambda functions to use the latest supported runtime versions.',
    'https://docs.aws.amazon.com/lambda/latest/dg/lambda-runtimes.html',
    '["AWS-Best-Practices", "SOC2-CC7.1"]'::jsonb
),

-- Azure-Specific Rules
(
    'AZ-STORAGE-001',
    'Azure Storage accounts must enforce HTTPS',
    'Ensures Azure Storage accounts only accept secure HTTPS connections.',
    'data-protection',
    'azure',
    'azurerm_storage_account',
    'HIGH',
    'SELECT name FROM azurerm_storage_account WHERE enable_https_traffic_only = FALSE',
    'Enable HTTPS-only access in the storage account configuration.',
    'https://docs.microsoft.com/en-us/azure/storage/common/storage-require-secure-transfer',
    '["CIS-Azure-3.1", "SOC2-CC6.1"]'::jsonb
),
(
    'AZ-SQL-001',
    'Azure SQL must have Transparent Data Encryption enabled',
    'Ensures Azure SQL databases have TDE enabled for encryption at rest.',
    'data-protection',
    'azure',
    'azurerm_mssql_database',
    'CRITICAL',
    'SELECT name FROM azurerm_mssql_database WHERE transparent_data_encryption_enabled = FALSE',
    'Enable Transparent Data Encryption on the Azure SQL database.',
    'https://docs.microsoft.com/en-us/azure/azure-sql/database/transparent-data-encryption-tde-overview',
    '["CIS-Azure-4.1.1", "SOC2-CC6.1", "PCI-DSS-3.4"]'::jsonb
)

ON CONFLICT (rule_code) DO UPDATE SET
    rule_name = EXCLUDED.rule_name,
    description = EXCLUDED.description,
    severity = EXCLUDED.severity,
    sql_check = EXCLUDED.sql_check,
    remediation = EXCLUDED.remediation,
    documentation_url = EXCLUDED.documentation_url,
    frameworks = EXCLUDED.frameworks,
    updated_at = CURRENT_TIMESTAMP;

--------------------------------------------------------------
-- VIEWS: Compliance Reporting
--------------------------------------------------------------

-- View: Active rules summary
CREATE OR REPLACE VIEW compliance.v_active_rules_summary AS
SELECT 
    category,
    cloud_provider,
    severity,
    COUNT(*) as rule_count
FROM compliance.compliance_rules
WHERE is_active = TRUE
GROUP BY category, cloud_provider, severity
ORDER BY 
    CASE severity 
        WHEN 'CRITICAL' THEN 1 
        WHEN 'HIGH' THEN 2 
        WHEN 'MEDIUM' THEN 3 
        WHEN 'LOW' THEN 4 
    END;

-- View: Recent audit results
CREATE OR REPLACE VIEW compliance.v_recent_audit_results AS
SELECT 
    ra.audit_id,
    ra.resource_id,
    ra.resource_type,
    ra.compliance_status,
    ra.checked_at,
    cr.rule_name,
    cr.severity,
    cr.category
FROM compliance.resource_audit ra
LEFT JOIN compliance.compliance_rules cr ON ra.rule_id = cr.rule_id
WHERE ra.checked_at >= NOW() - INTERVAL '24 hours'
ORDER BY ra.checked_at DESC;

-- View: Compliance dashboard metrics
CREATE OR REPLACE VIEW compliance.v_compliance_dashboard AS
SELECT 
    cloud_provider,
    resource_type,
    compliance_status,
    COUNT(*) as count,
    DATE_TRUNC('hour', checked_at) as check_hour
FROM compliance.resource_audit
WHERE checked_at >= NOW() - INTERVAL '7 days'
GROUP BY cloud_provider, resource_type, compliance_status, DATE_TRUNC('hour', checked_at);

--------------------------------------------------------------
-- GRANT PERMISSIONS
--------------------------------------------------------------

-- Create a read-only role for compliance dashboards
DO $$
BEGIN
    IF NOT EXISTS (SELECT FROM pg_catalog.pg_roles WHERE rolname = 'compliance_reader') THEN
        CREATE ROLE compliance_reader;
    END IF;
END $$;

GRANT USAGE ON SCHEMA compliance TO compliance_reader;
GRANT SELECT ON ALL TABLES IN SCHEMA compliance TO compliance_reader;
ALTER DEFAULT PRIVILEGES IN SCHEMA compliance GRANT SELECT ON TABLES TO compliance_reader;

--------------------------------------------------------------
-- Verification: Display summary
--------------------------------------------------------------

DO $$
DECLARE
    rules_count INTEGER;
BEGIN
    SELECT COUNT(*) INTO rules_count FROM compliance.compliance_rules;
    RAISE NOTICE 'Schema creation completed successfully!';
    RAISE NOTICE 'Total compliance rules loaded: %', rules_count;
END $$;

-- Display rule summary
SELECT 
    'Rules Summary' as info,
    cloud_provider,
    severity,
    COUNT(*) as count
FROM compliance.compliance_rules
WHERE is_active = TRUE
GROUP BY cloud_provider, severity
ORDER BY cloud_provider, 
    CASE severity 
        WHEN 'CRITICAL' THEN 1 
        WHEN 'HIGH' THEN 2 
        WHEN 'MEDIUM' THEN 3 
        WHEN 'LOW' THEN 4 
    END;
