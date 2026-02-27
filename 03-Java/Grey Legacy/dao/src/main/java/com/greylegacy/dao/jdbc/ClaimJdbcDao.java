package com.greylegacy.dao.jdbc;

import com.greylegacy.domain.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import javax.sql.DataSource;
import java.math.BigDecimal;
import java.sql.*;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

/**
 * Fallback JDBC-based DAO for claim queries.
 * Demonstrates manual JDBC operations without ORM abstraction.
 * Used for:
 *   - Complex reporting queries that don't map well to ORM
 *   - Batch operations requiring fine-grained control
 *   - Performance-critical reads bypassing Hibernate session cache
 *   - Manual transaction management scenarios
 */
@Repository("claimJdbcDao")
public class ClaimJdbcDao {

    private static final Logger log = LoggerFactory.getLogger(ClaimJdbcDao.class);

    private static final String FIND_CLAIMS_BY_STATUS_SQL =
        "SELECT c.ID, c.CLAIM_NUMBER, c.STATUS, c.CLAIM_TYPE, c.LOSS_DATE, " +
        "       c.REPORTED_DATE, c.CLAIMANT_FIRST_NAME, c.CLAIMANT_LAST_NAME, " +
        "       c.ESTIMATED_LOSS, c.APPROVED_AMOUNT, c.ADJUSTER_CODE, " +
        "       c.FRAUD_SCORE, c.FRAUD_RISK_LEVEL, c.POLICY_ID " +
        "FROM CLAIM c WHERE c.STATUS = ?";

    private static final String FIND_CLAIMS_SUMMARY_SQL =
        "SELECT c.CLAIM_NUMBER, c.STATUS, c.CLAIM_TYPE, c.LOSS_DATE, " +
        "       c.CLAIMANT_LAST_NAME, c.ESTIMATED_LOSS, c.APPROVED_AMOUNT, " +
        "       p.POLICY_NUMBER, p.HOLDER_LAST_NAME " +
        "FROM CLAIM c " +
        "INNER JOIN POLICY p ON c.POLICY_ID = p.ID " +
        "WHERE c.STATUS IN (?, ?) " +
        "ORDER BY c.LOSS_DATE DESC";

    private static final String COUNT_CLAIMS_BY_STATUS_SQL =
        "SELECT c.STATUS, COUNT(*) as CNT, SUM(c.ESTIMATED_LOSS) as TOTAL_ESTIMATED " +
        "FROM CLAIM c GROUP BY c.STATUS ORDER BY CNT DESC";

    private static final String BATCH_UPDATE_FRAUD_SCORE_SQL =
        "UPDATE CLAIM SET FRAUD_SCORE = ?, FRAUD_RISK_LEVEL = ?, " +
        "FRAUD_SCORED_DATE = ?, UPDATED_DATE = ?, UPDATED_BY = ? " +
        "WHERE ID = ?";

    private static final String INSERT_AUDIT_ENTRY_SQL =
        "INSERT INTO CLAIM_AUDIT (ID, CLAIM_ID, EVENT_TIMESTAMP, EVENT_TYPE, " +
        "PREVIOUS_VALUE, NEW_VALUE, PERFORMED_BY, DESCRIPTION, IP_ADDRESS, " +
        "CREATED_BY, CREATED_DATE, UPDATED_BY, UPDATED_DATE, VERSION) " +
        "VALUES (NEXT VALUE FOR AUDIT_SEQ, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, 0)";

    private static final String AGING_REPORT_SQL =
        "SELECT c.CLAIM_NUMBER, c.STATUS, c.LOSS_DATE, c.REPORTED_DATE, " +
        "       DATEDIFF(DAY, c.REPORTED_DATE, CURRENT_TIMESTAMP) as DAYS_OPEN, " +
        "       c.ADJUSTER_CODE, c.ESTIMATED_LOSS " +
        "FROM CLAIM c " +
        "WHERE c.STATUS NOT IN ('CLOSED', 'DENIED', 'SETTLED') " +
        "  AND DATEDIFF(DAY, c.REPORTED_DATE, CURRENT_TIMESTAMP) > ? " +
        "ORDER BY DAYS_OPEN DESC";

    @Autowired
    private DataSource dataSource;

    /**
     * Find claims by status using raw JDBC.
     * Demonstrates manual ResultSet mapping and resource management.
     */
    public List<Claim> findClaimsByStatus(String status) {
        List<Claim> claims = new ArrayList<>();
        Connection conn = null;
        PreparedStatement ps = null;
        ResultSet rs = null;

        try {
            conn = dataSource.getConnection();
            ps = conn.prepareStatement(FIND_CLAIMS_BY_STATUS_SQL);
            ps.setString(1, status);
            rs = ps.executeQuery();

            while (rs.next()) {
                claims.add(mapResultSetToClaim(rs));
            }

            log.info("Found {} claims with status {}", claims.size(), status);
        } catch (SQLException e) {
            log.error("Error querying claims by status: {}", status, e);
            throw new RuntimeException("Failed to query claims by status", e);
        } finally {
            closeQuietly(rs);
            closeQuietly(ps);
            closeQuietly(conn);
        }

        return claims;
    }

    /**
     * Execute a claim summary report joining claims with policies.
     * Demonstrates multi-table JDBC joins.
     */
    public List<ClaimSummaryRow> findClaimsSummary(String status1, String status2) {
        List<ClaimSummaryRow> results = new ArrayList<>();
        Connection conn = null;
        PreparedStatement ps = null;
        ResultSet rs = null;

        try {
            conn = dataSource.getConnection();
            ps = conn.prepareStatement(FIND_CLAIMS_SUMMARY_SQL);
            ps.setString(1, status1);
            ps.setString(2, status2);
            rs = ps.executeQuery();

            while (rs.next()) {
                ClaimSummaryRow row = new ClaimSummaryRow();
                row.setClaimNumber(rs.getString("CLAIM_NUMBER"));
                row.setStatus(rs.getString("STATUS"));
                row.setClaimType(rs.getString("CLAIM_TYPE"));
                row.setLossDate(rs.getDate("LOSS_DATE"));
                row.setClaimantLastName(rs.getString("CLAIMANT_LAST_NAME"));
                row.setEstimatedLoss(rs.getBigDecimal("ESTIMATED_LOSS"));
                row.setApprovedAmount(rs.getBigDecimal("APPROVED_AMOUNT"));
                row.setPolicyNumber(rs.getString("POLICY_NUMBER"));
                row.setHolderLastName(rs.getString("HOLDER_LAST_NAME"));
                results.add(row);
            }

        } catch (SQLException e) {
            log.error("Error querying claim summary", e);
            throw new RuntimeException("Failed to query claim summary", e);
        } finally {
            closeQuietly(rs);
            closeQuietly(ps);
            closeQuietly(conn);
        }

        return results;
    }

    /**
     * Batch update fraud scores using JDBC batch operations.
     * Demonstrates manual transaction management with commit/rollback.
     */
    public int batchUpdateFraudScores(List<FraudScoreUpdate> updates) {
        Connection conn = null;
        PreparedStatement ps = null;
        int totalUpdated = 0;

        try {
            conn = dataSource.getConnection();
            conn.setAutoCommit(false); // Manual transaction management

            ps = conn.prepareStatement(BATCH_UPDATE_FRAUD_SCORE_SQL);
            Timestamp now = new Timestamp(System.currentTimeMillis());

            int batchCount = 0;
            for (FraudScoreUpdate update : updates) {
                ps.setInt(1, update.getFraudScore());
                ps.setString(2, update.getFraudRiskLevel());
                ps.setTimestamp(3, now);
                ps.setTimestamp(4, now);
                ps.setString(5, "BATCH_FRAUD_SCORER");
                ps.setLong(6, update.getClaimId());
                ps.addBatch();

                batchCount++;
                if (batchCount % 100 == 0) {
                    int[] results = ps.executeBatch();
                    totalUpdated += sumBatchResults(results);
                    log.debug("Executed batch of {} fraud score updates", batchCount);
                }
            }

            // Execute remaining
            if (batchCount % 100 != 0) {
                int[] results = ps.executeBatch();
                totalUpdated += sumBatchResults(results);
            }

            conn.commit();
            log.info("Batch updated {} fraud scores", totalUpdated);

        } catch (SQLException e) {
            log.error("Error in batch fraud score update, rolling back", e);
            rollbackQuietly(conn);
            throw new RuntimeException("Failed to batch update fraud scores", e);
        } finally {
            closeQuietly(ps);
            closeQuietly(conn);
        }

        return totalUpdated;
    }

    /**
     * Insert audit entries using JDBC batch — bypasses Hibernate for performance.
     */
    public int batchInsertAuditEntries(List<AuditEntryInput> entries) {
        Connection conn = null;
        PreparedStatement ps = null;
        int totalInserted = 0;

        try {
            conn = dataSource.getConnection();
            conn.setAutoCommit(false);

            ps = conn.prepareStatement(INSERT_AUDIT_ENTRY_SQL);
            Timestamp now = new Timestamp(System.currentTimeMillis());

            for (AuditEntryInput entry : entries) {
                ps.setLong(1, entry.getClaimId());
                ps.setTimestamp(2, now);
                ps.setString(3, entry.getEventType());
                ps.setString(4, entry.getPreviousValue());
                ps.setString(5, entry.getNewValue());
                ps.setString(6, entry.getPerformedBy());
                ps.setString(7, entry.getDescription());
                ps.setString(8, entry.getIpAddress());
                ps.setString(9, "SYSTEM");
                ps.setTimestamp(10, now);
                ps.setString(11, "SYSTEM");
                ps.setTimestamp(12, now);
                ps.addBatch();
            }

            int[] results = ps.executeBatch();
            totalInserted = sumBatchResults(results);
            conn.commit();

            log.info("Batch inserted {} audit entries", totalInserted);
        } catch (SQLException e) {
            log.error("Error in batch audit entry insert", e);
            rollbackQuietly(conn);
            throw new RuntimeException("Failed to batch insert audit entries", e);
        } finally {
            closeQuietly(ps);
            closeQuietly(conn);
        }

        return totalInserted;
    }

    /**
     * Aging report query — finds claims open longer than specified days.
     */
    public List<AgingReportRow> getAgingReport(int daysThreshold) {
        List<AgingReportRow> results = new ArrayList<>();
        Connection conn = null;
        PreparedStatement ps = null;
        ResultSet rs = null;

        try {
            conn = dataSource.getConnection();
            ps = conn.prepareStatement(AGING_REPORT_SQL);
            ps.setInt(1, daysThreshold);
            rs = ps.executeQuery();

            while (rs.next()) {
                AgingReportRow row = new AgingReportRow();
                row.setClaimNumber(rs.getString("CLAIM_NUMBER"));
                row.setStatus(rs.getString("STATUS"));
                row.setLossDate(rs.getDate("LOSS_DATE"));
                row.setReportedDate(rs.getDate("REPORTED_DATE"));
                row.setDaysOpen(rs.getInt("DAYS_OPEN"));
                row.setAdjusterCode(rs.getString("ADJUSTER_CODE"));
                row.setEstimatedLoss(rs.getBigDecimal("ESTIMATED_LOSS"));
                results.add(row);
            }

        } catch (SQLException e) {
            log.error("Error generating aging report", e);
            throw new RuntimeException("Failed to generate aging report", e);
        } finally {
            closeQuietly(rs);
            closeQuietly(ps);
            closeQuietly(conn);
        }

        return results;
    }

    // --- Private helpers ---

    private Claim mapResultSetToClaim(ResultSet rs) throws SQLException {
        Claim claim = new Claim();
        claim.setId(rs.getLong("ID"));
        claim.setClaimNumber(rs.getString("CLAIM_NUMBER"));
        claim.setClaimType(ClaimType.valueOf(rs.getString("CLAIM_TYPE")));
        claim.setLossDate(rs.getDate("LOSS_DATE"));
        claim.setReportedDate(rs.getTimestamp("REPORTED_DATE"));
        claim.setClaimantFirstName(rs.getString("CLAIMANT_FIRST_NAME"));
        claim.setClaimantLastName(rs.getString("CLAIMANT_LAST_NAME"));
        claim.setEstimatedLoss(rs.getBigDecimal("ESTIMATED_LOSS"));
        claim.setApprovedAmount(rs.getBigDecimal("APPROVED_AMOUNT"));
        claim.setAdjusterCode(rs.getString("ADJUSTER_CODE"));

        int fraudScore = rs.getInt("FRAUD_SCORE");
        if (!rs.wasNull()) {
            claim.setFraudScore(fraudScore);
        }

        String riskLevel = rs.getString("FRAUD_RISK_LEVEL");
        if (riskLevel != null) {
            claim.setFraudRiskLevel(FraudRiskLevel.valueOf(riskLevel));
        }

        String status = rs.getString("STATUS");
        if (status != null) {
            claim.setStatus(ClaimStatus.valueOf(status));
        }

        return claim;
    }

    private int sumBatchResults(int[] results) {
        int sum = 0;
        for (int r : results) {
            if (r >= 0) {
                sum += r;
            } else if (r == Statement.SUCCESS_NO_INFO) {
                sum += 1;
            }
        }
        return sum;
    }

    private void rollbackQuietly(Connection conn) {
        if (conn != null) {
            try {
                conn.rollback();
            } catch (SQLException e) {
                log.warn("Failed to rollback connection", e);
            }
        }
    }

    private void closeQuietly(AutoCloseable resource) {
        if (resource != null) {
            try {
                resource.close();
            } catch (Exception e) {
                log.warn("Failed to close resource", e);
            }
        }
    }

    // --- Inner DTOs for JDBC results ---

    public static class ClaimSummaryRow {
        private String claimNumber;
        private String status;
        private String claimType;
        private Date lossDate;
        private String claimantLastName;
        private BigDecimal estimatedLoss;
        private BigDecimal approvedAmount;
        private String policyNumber;
        private String holderLastName;

        public String getClaimNumber() { return claimNumber; }
        public void setClaimNumber(String claimNumber) { this.claimNumber = claimNumber; }
        public String getStatus() { return status; }
        public void setStatus(String status) { this.status = status; }
        public String getClaimType() { return claimType; }
        public void setClaimType(String claimType) { this.claimType = claimType; }
        public Date getLossDate() { return lossDate; }
        public void setLossDate(Date lossDate) { this.lossDate = lossDate; }
        public String getClaimantLastName() { return claimantLastName; }
        public void setClaimantLastName(String claimantLastName) { this.claimantLastName = claimantLastName; }
        public BigDecimal getEstimatedLoss() { return estimatedLoss; }
        public void setEstimatedLoss(BigDecimal estimatedLoss) { this.estimatedLoss = estimatedLoss; }
        public BigDecimal getApprovedAmount() { return approvedAmount; }
        public void setApprovedAmount(BigDecimal approvedAmount) { this.approvedAmount = approvedAmount; }
        public String getPolicyNumber() { return policyNumber; }
        public void setPolicyNumber(String policyNumber) { this.policyNumber = policyNumber; }
        public String getHolderLastName() { return holderLastName; }
        public void setHolderLastName(String holderLastName) { this.holderLastName = holderLastName; }
    }

    public static class FraudScoreUpdate {
        private Long claimId;
        private int fraudScore;
        private String fraudRiskLevel;

        public FraudScoreUpdate(Long claimId, int fraudScore, String fraudRiskLevel) {
            this.claimId = claimId;
            this.fraudScore = fraudScore;
            this.fraudRiskLevel = fraudRiskLevel;
        }

        public Long getClaimId() { return claimId; }
        public int getFraudScore() { return fraudScore; }
        public String getFraudRiskLevel() { return fraudRiskLevel; }
    }

    public static class AuditEntryInput {
        private Long claimId;
        private String eventType;
        private String previousValue;
        private String newValue;
        private String performedBy;
        private String description;
        private String ipAddress;

        public Long getClaimId() { return claimId; }
        public void setClaimId(Long claimId) { this.claimId = claimId; }
        public String getEventType() { return eventType; }
        public void setEventType(String eventType) { this.eventType = eventType; }
        public String getPreviousValue() { return previousValue; }
        public void setPreviousValue(String previousValue) { this.previousValue = previousValue; }
        public String getNewValue() { return newValue; }
        public void setNewValue(String newValue) { this.newValue = newValue; }
        public String getPerformedBy() { return performedBy; }
        public void setPerformedBy(String performedBy) { this.performedBy = performedBy; }
        public String getDescription() { return description; }
        public void setDescription(String description) { this.description = description; }
        public String getIpAddress() { return ipAddress; }
        public void setIpAddress(String ipAddress) { this.ipAddress = ipAddress; }
    }

    public static class AgingReportRow {
        private String claimNumber;
        private String status;
        private Date lossDate;
        private Date reportedDate;
        private int daysOpen;
        private String adjusterCode;
        private BigDecimal estimatedLoss;

        public String getClaimNumber() { return claimNumber; }
        public void setClaimNumber(String claimNumber) { this.claimNumber = claimNumber; }
        public String getStatus() { return status; }
        public void setStatus(String status) { this.status = status; }
        public Date getLossDate() { return lossDate; }
        public void setLossDate(Date lossDate) { this.lossDate = lossDate; }
        public Date getReportedDate() { return reportedDate; }
        public void setReportedDate(Date reportedDate) { this.reportedDate = reportedDate; }
        public int getDaysOpen() { return daysOpen; }
        public void setDaysOpen(int daysOpen) { this.daysOpen = daysOpen; }
        public String getAdjusterCode() { return adjusterCode; }
        public void setAdjusterCode(String adjusterCode) { this.adjusterCode = adjusterCode; }
        public BigDecimal getEstimatedLoss() { return estimatedLoss; }
        public void setEstimatedLoss(BigDecimal estimatedLoss) { this.estimatedLoss = estimatedLoss; }
    }
}
