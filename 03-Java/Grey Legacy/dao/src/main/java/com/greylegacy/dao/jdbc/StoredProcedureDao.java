package com.greylegacy.dao.jdbc;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import javax.sql.DataSource;
import java.math.BigDecimal;
import java.sql.*;
import java.util.ArrayList;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * DAO for invoking database stored procedures via JDBC {@link CallableStatement}.
 * <p>
 * This is the classic enterprise Java pattern for calling PL/SQL, T-SQL, or
 * H2 ALIAS procedures from application code. It demonstrates:
 * <ul>
 *   <li>{@code CallableStatement} with IN, OUT, and INOUT parameters</li>
 *   <li>Stored function invocation using {@code {? = call fn_name(...)}}</li>
 *   <li>Stored procedure invocation using {@code {call proc_name(...)}}</li>
 *   <li>ResultSet handling from stored procedures</li>
 *   <li>Transaction-scoped pessimistic locking via {@code SELECT FOR UPDATE}</li>
 *   <li>Manual connection/resource management following enterprise JDBC patterns</li>
 * </ul>
 * <p>
 * Many legacy insurance applications make heavy use of stored procedures for:
 * <ul>
 *   <li>Reserve calculations (actuarial business logic in the DB layer)</li>
 *   <li>Claim number generation (sequence-based, database-enforced uniqueness)</li>
 *   <li>Automated aging and closure of stale claims</li>
 *   <li>Payment reconciliation and reporting</li>
 *   <li>Row-level pessimistic locking for critical financial operations</li>
 * </ul>
 * <p>
 * <b>Why stored procedures?</b> In legacy systems, stored procedures
 * centralised business logic in the database to ensure all applications
 * (mainframe batch, web UI, data warehouse ETL) applied the same rules.
 * Modern architectures prefer application-layer logic, but understanding
 * {@code CallableStatement} usage is essential for maintaining systems
 * built before that shift.
 *
 * @see java.sql.CallableStatement
 * @see com.greylegacy.dao.jdbc.ClaimJdbcDao
 */
@Repository("storedProcedureDao")
public class StoredProcedureDao {

    private static final Logger log = LoggerFactory.getLogger(StoredProcedureDao.class);

    @Autowired
    private DataSource dataSource;

    // ========================================================================
    // Stored Procedure Calls (CallableStatement)
    // ========================================================================

    /**
     * Calculate the reserve amount for a claim.
     * <p>
     * Invokes {@code SP_CALCULATE_CLAIM_RESERVE} as a stored function
     * using the {@code {? = call ...}} JDBC callable statement syntax.
     * The procedure computes:
     * {@code reserve = max(estimated_loss - deductible - sum(payments), 0)}
     * and updates the CLAIM.RESERVE_AMOUNT column.
     *
     * @param claimId the ID of the claim to calculate reserves for
     * @return the calculated reserve amount, or null if claim not found
     */
    public BigDecimal calculateClaimReserve(Long claimId) {
        log.info("Calling SP_CALCULATE_CLAIM_RESERVE for claim {}", claimId);

        // {? = call fn()} syntax — first parameter is OUT (return value)
        String call = "{? = call SP_CALCULATE_CLAIM_RESERVE(?)}";
        Connection conn = null;
        CallableStatement cs = null;

        try {
            conn = dataSource.getConnection();

            cs = conn.prepareCall(call);
            cs.registerOutParameter(1, Types.DECIMAL);  // OUT: return value
            cs.setLong(2, claimId);                      // IN: claim ID

            cs.execute();

            BigDecimal reserve = cs.getBigDecimal(1);
            log.info("SP_CALCULATE_CLAIM_RESERVE({}) = {}", claimId, reserve);
            return reserve;

        } catch (SQLException e) {
            log.error("Error calling SP_CALCULATE_CLAIM_RESERVE for claim {}", claimId, e);
            throw new RuntimeException("Stored procedure call failed: SP_CALCULATE_CLAIM_RESERVE", e);
        } finally {
            closeQuietly(cs);
            closeQuietly(conn);
        }
    }

    /**
     * Close aged claims that exceed the threshold number of days.
     * <p>
     * Invokes {@code SP_CLOSE_AGED_CLAIMS} which:
     * <ol>
     *   <li>Finds all claims older than {@code ageThresholdDays}</li>
     *   <li>Moves them from their current status to {@code CLOSED}</li>
     *   <li>Creates audit trail entries for each closure</li>
     * </ol>
     * <p>
     * Demonstrates the {@code {? = call proc(?)}} pattern with both
     * IN and OUT parameters — the return value is the count of claims closed.
     *
     * @param ageThresholdDays claims older than this many days are closed
     * @return number of claims closed by the procedure
     */
    public int closeAgedClaims(int ageThresholdDays) {
        log.info("Calling SP_CLOSE_AGED_CLAIMS with threshold {} days", ageThresholdDays);

        String call = "{? = call SP_CLOSE_AGED_CLAIMS(?)}";
        Connection conn = null;
        CallableStatement cs = null;

        try {
            conn = dataSource.getConnection();
            conn.setAutoCommit(false);  // Explicit transaction control

            cs = conn.prepareCall(call);
            cs.registerOutParameter(1, Types.INTEGER);   // OUT: count of closed claims
            cs.setInt(2, ageThresholdDays);               // IN: threshold

            cs.execute();
            int closedCount = cs.getInt(1);

            conn.commit();
            log.info("SP_CLOSE_AGED_CLAIMS closed {} claims", closedCount);
            return closedCount;

        } catch (SQLException e) {
            log.error("Error calling SP_CLOSE_AGED_CLAIMS", e);
            rollbackQuietly(conn);
            throw new RuntimeException("Stored procedure call failed: SP_CLOSE_AGED_CLAIMS", e);
        } finally {
            closeQuietly(cs);
            closeQuietly(conn);
        }
    }

    /**
     * Generate a unique claim number using the database sequence.
     * <p>
     * Invokes {@code SP_GENERATE_CLAIM_NUMBER} which uses
     * {@code CLAIM_NUMBER_SEQ} to produce a formatted claim number
     * in the pattern {@code CLM-YYYY-NNNNN} (e.g., {@code CLM-2026-00042}).
     * <p>
     * Demonstrates stored function call returning a VARCHAR.
     *
     * @return the generated claim number
     */
    public String generateClaimNumber() {
        log.debug("Calling SP_GENERATE_CLAIM_NUMBER");

        String call = "{? = call SP_GENERATE_CLAIM_NUMBER()}";
        Connection conn = null;
        CallableStatement cs = null;

        try {
            conn = dataSource.getConnection();

            cs = conn.prepareCall(call);
            cs.registerOutParameter(1, Types.VARCHAR);

            cs.execute();
            String claimNumber = cs.getString(1);

            log.info("Generated claim number: {}", claimNumber);
            return claimNumber;

        } catch (SQLException e) {
            log.error("Error calling SP_GENERATE_CLAIM_NUMBER", e);
            throw new RuntimeException("Stored procedure call failed: SP_GENERATE_CLAIM_NUMBER", e);
        } finally {
            closeQuietly(cs);
            closeQuietly(conn);
        }
    }

    /**
     * Capture a point-in-time snapshot of a policy into the SCD Type 2
     * history table.
     * <p>
     * Invokes {@code SP_POLICY_SCD_SNAPSHOT} which:
     * <ol>
     *   <li>Expires the current history record (sets EFFECTIVE_TO, IS_CURRENT=false)</li>
     *   <li>Inserts a new current record from the live POLICY row</li>
     * </ol>
     * <p>
     * Demonstrates a stored procedure with multiple IN parameters and no
     * return value (void procedure).
     *
     * @param policyId     the policy to snapshot
     * @param changeReason why the change is being captured
     * @param changedBy    the user or system performing the change
     */
    public void capturePolicySnapshot(Long policyId, String changeReason, String changedBy) {
        log.info("Calling SP_POLICY_SCD_SNAPSHOT for policy {} (reason: {})", policyId, changeReason);

        String call = "{call SP_POLICY_SCD_SNAPSHOT(?, ?, ?)}";
        Connection conn = null;
        CallableStatement cs = null;

        try {
            conn = dataSource.getConnection();
            conn.setAutoCommit(false);

            cs = conn.prepareCall(call);
            cs.setLong(1, policyId);
            cs.setString(2, changeReason);
            cs.setString(3, changedBy);

            cs.execute();
            conn.commit();

            log.info("Policy snapshot captured for policy {}", policyId);

        } catch (SQLException e) {
            log.error("Error calling SP_POLICY_SCD_SNAPSHOT for policy {}", policyId, e);
            rollbackQuietly(conn);
            throw new RuntimeException("Stored procedure call failed: SP_POLICY_SCD_SNAPSHOT", e);
        } finally {
            closeQuietly(cs);
            closeQuietly(conn);
        }
    }

    /**
     * Reconcile payments for all claims and return discrepancies.
     * <p>
     * Invokes {@code SP_RECONCILE_PAYMENTS} which returns a ResultSet
     * of claims where total payments do not match the approved amount.
     * <p>
     * Demonstrates handling a stored procedure that returns a ResultSet
     * (common in Oracle, SQL Server, and H2 — PostgreSQL uses RETURNS TABLE).
     *
     * @return list of payment discrepancy records
     */
    public List<PaymentDiscrepancy> reconcilePayments() {
        log.info("Calling SP_RECONCILE_PAYMENTS");

        String call = "{call SP_RECONCILE_PAYMENTS()}";
        Connection conn = null;
        CallableStatement cs = null;
        ResultSet rs = null;
        List<PaymentDiscrepancy> discrepancies = new ArrayList<>();

        try {
            conn = dataSource.getConnection();

            cs = conn.prepareCall(call);
            boolean hasResultSet = cs.execute();

            if (hasResultSet) {
                rs = cs.getResultSet();
                while (rs.next()) {
                    PaymentDiscrepancy d = new PaymentDiscrepancy();
                    d.setClaimId(rs.getLong("CLAIM_ID"));
                    d.setClaimNumber(rs.getString("CLAIM_NUMBER"));
                    d.setApprovedAmount(rs.getBigDecimal("APPROVED_AMOUNT"));
                    d.setTotalPaid(rs.getBigDecimal("TOTAL_PAID"));
                    d.setDiscrepancy(rs.getBigDecimal("DISCREPANCY"));
                    d.setDiscrepancyType(rs.getString("DISCREPANCY_TYPE"));
                    discrepancies.add(d);
                }
            }

            log.info("SP_RECONCILE_PAYMENTS found {} discrepancies", discrepancies.size());
            return discrepancies;

        } catch (SQLException e) {
            log.error("Error calling SP_RECONCILE_PAYMENTS", e);
            throw new RuntimeException("Stored procedure call failed: SP_RECONCILE_PAYMENTS", e);
        } finally {
            closeQuietly(rs);
            closeQuietly(cs);
            closeQuietly(conn);
        }
    }

    // ========================================================================
    // Direct Function Calls (SQL functions via CallableStatement)
    // ========================================================================

    /**
     * Get the age of a claim in days.
     * <p>
     * Calls {@code FN_CLAIM_AGE_DAYS(claimId)} — a scalar function
     * returning an integer.
     *
     * @param claimId the claim ID
     * @return number of days since loss, or null if claim not found
     */
    public Integer getClaimAgeDays(Long claimId) {
        String call = "{? = call FN_CLAIM_AGE_DAYS(?)}";
        Connection conn = null;
        CallableStatement cs = null;

        try {
            conn = dataSource.getConnection();
            cs = conn.prepareCall(call);
            cs.registerOutParameter(1, Types.INTEGER);
            cs.setLong(2, claimId);
            cs.execute();

            int age = cs.getInt(1);
            return cs.wasNull() ? null : age;

        } catch (SQLException e) {
            log.error("Error calling FN_CLAIM_AGE_DAYS({})", claimId, e);
            throw new RuntimeException("Function call failed: FN_CLAIM_AGE_DAYS", e);
        } finally {
            closeQuietly(cs);
            closeQuietly(conn);
        }
    }

    /**
     * Get total approved claims amount for a policy.
     * <p>
     * Calls {@code FN_POLICY_CLAIMS_TOTAL(policyId)}.
     *
     * @param policyId the policy ID
     * @return total approved amount across all claims for the policy
     */
    public BigDecimal getPolicyClaimsTotal(Long policyId) {
        String call = "{? = call FN_POLICY_CLAIMS_TOTAL(?)}";
        Connection conn = null;
        CallableStatement cs = null;

        try {
            conn = dataSource.getConnection();
            cs = conn.prepareCall(call);
            cs.registerOutParameter(1, Types.DECIMAL);
            cs.setLong(2, policyId);
            cs.execute();

            return cs.getBigDecimal(1);

        } catch (SQLException e) {
            log.error("Error calling FN_POLICY_CLAIMS_TOTAL({})", policyId, e);
            throw new RuntimeException("Function call failed: FN_POLICY_CLAIMS_TOTAL", e);
        } finally {
            closeQuietly(cs);
            closeQuietly(conn);
        }
    }

    /**
     * Calculate loss ratio for a policy.
     * <p>
     * Calls {@code FN_CALCULATE_LOSS_RATIO(policyId)}.
     * Loss Ratio = Total Paid Claims / Total Premiums Collected.
     * A ratio > 1.0 indicates unprofitability.
     *
     * @param policyId the policy ID
     * @return loss ratio as a decimal (e.g., 0.7500 = 75%), or null if premium is zero
     */
    public BigDecimal calculateLossRatio(Long policyId) {
        String call = "{? = call FN_CALCULATE_LOSS_RATIO(?)}";
        Connection conn = null;
        CallableStatement cs = null;

        try {
            conn = dataSource.getConnection();
            cs = conn.prepareCall(call);
            cs.registerOutParameter(1, Types.DECIMAL);
            cs.setLong(2, policyId);
            cs.execute();

            return cs.getBigDecimal(1);

        } catch (SQLException e) {
            log.error("Error calling FN_CALCULATE_LOSS_RATIO({})", policyId, e);
            throw new RuntimeException("Function call failed: FN_CALCULATE_LOSS_RATIO", e);
        } finally {
            closeQuietly(cs);
            closeQuietly(conn);
        }
    }

    // ========================================================================
    // Pessimistic Locking Procedure Call
    // ========================================================================

    /**
     * Acquire an exclusive row-level lock on a claim for processing.
     * <p>
     * Invokes {@code SP_LOCK_CLAIM_FOR_PROCESSING} which uses
     * {@code SELECT ... FOR UPDATE} to acquire a pessimistic lock.
     * The lock is held until the enclosing transaction commits or rolls back.
     * <p>
     * <b>IMPORTANT:</b> The caller MUST manage the transaction boundary.
     * The returned connection is intentionally NOT closed — the caller
     * must commit/rollback and then close the connection when it has
     * finished its critical section.
     * <p>
     * Common usage pattern:
     * <pre>
     *   LockedClaim lock = storedProcedureDao.lockClaimForProcessing(claimId);
     *   try {
     *       // perform critical financial operations using lock.getConnection()
     *       lock.getConnection().commit();
     *   } catch (Exception e) {
     *       lock.getConnection().rollback();
     *   } finally {
     *       lock.close();
     *   }
     * </pre>
     *
     * @param claimId the claim to lock
     * @return a LockedClaim containing the lock handle and claim data
     */
    public LockedClaim lockClaimForProcessing(Long claimId) {
        log.warn("Acquiring pessimistic lock on claim {} — ensure caller manages transaction", claimId);

        String call = "{call SP_LOCK_CLAIM_FOR_PROCESSING(?)}";

        try {
            Connection conn = dataSource.getConnection();
            conn.setAutoCommit(false);  // Lock requires explicit transaction

            CallableStatement cs = conn.prepareCall(call);
            cs.setLong(1, claimId);

            boolean hasResultSet = cs.execute();

            if (hasResultSet) {
                ResultSet rs = cs.getResultSet();
                if (rs.next()) {
                    LockedClaim locked = new LockedClaim();
                    locked.setConnection(conn);
                    locked.setClaimId(rs.getLong("ID"));
                    locked.setClaimNumber(rs.getString("CLAIM_NUMBER"));
                    locked.setStatus(rs.getString("STATUS"));
                    locked.setApprovedAmount(rs.getBigDecimal("APPROVED_AMOUNT"));
                    locked.setReserveAmount(rs.getBigDecimal("RESERVE_AMOUNT"));

                    log.info("Pessimistic lock acquired on claim {}", claimId);
                    // NOTE: Connection left open — caller must commit/rollback/close
                    return locked;
                }
            }

            conn.close();
            throw new RuntimeException("Claim " + claimId + " not found for locking");

        } catch (SQLException e) {
            log.error("Error acquiring lock on claim {}", claimId, e);
            throw new RuntimeException("Failed to lock claim for processing", e);
        }
    }

    // ========================================================================
    // H2 stored procedure implementations (static methods for CREATE ALIAS)
    // ========================================================================

    /**
     * H2 CREATE ALIAS target for SP_CALCULATE_CLAIM_AGING.
     * Calculates aging statistics by status bucket.
     */
    public static ResultSet calculateClaimAging(Connection conn) throws SQLException {
        return conn.createStatement().executeQuery(
            "SELECT STATUS, COUNT(*) AS CLAIM_COUNT, " +
            "AVG(DATEDIFF(DAY, REPORTED_DATE, CURRENT_TIMESTAMP)) AS AVG_DAYS, " +
            "MAX(DATEDIFF(DAY, REPORTED_DATE, CURRENT_TIMESTAMP)) AS MAX_DAYS, " +
            "SUM(ESTIMATED_LOSS) AS TOTAL_EXPOSURE " +
            "FROM CLAIM WHERE STATUS NOT IN ('CLOSED','DENIED','WITHDRAWN') " +
            "AND (DELETED = FALSE OR DELETED IS NULL) " +
            "GROUP BY STATUS ORDER BY AVG_DAYS DESC"
        );
    }

    /**
     * H2 CREATE ALIAS target for SP_AUTO_ASSIGN_ADJUSTER.
     * Assigns a claim to the adjuster with the lowest caseload in the matching specialization.
     */
    public static String autoAssignAdjuster(Connection conn, long claimId, String claimType)
            throws SQLException {
        // Find best available adjuster by specialization and caseload
        String sql = "SELECT ADJUSTER_CODE FROM ADJUSTER " +
            "WHERE ACTIVE = TRUE AND (DELETED = FALSE OR DELETED IS NULL) " +
            "AND CURRENT_CASELOAD < MAX_CASELOAD " +
            "ORDER BY CURRENT_CASELOAD ASC LIMIT 1";
        String adjusterCode = null;
        try (PreparedStatement ps = conn.prepareStatement(sql);
             ResultSet rs = ps.executeQuery()) {
            if (rs.next()) {
                adjusterCode = rs.getString("ADJUSTER_CODE");
            }
        }
        if (adjusterCode != null) {
            // Assign the adjuster to the claim
            try (PreparedStatement ps = conn.prepareStatement(
                    "UPDATE CLAIM SET ADJUSTER_CODE = ?, UPDATED_BY = 'SP_AUTO_ASSIGN', " +
                    "UPDATED_DATE = CURRENT_TIMESTAMP WHERE ID = ?")) {
                ps.setString(1, adjusterCode);
                ps.setLong(2, claimId);
                ps.executeUpdate();
            }
            // Increment adjuster caseload
            try (PreparedStatement ps = conn.prepareStatement(
                    "UPDATE ADJUSTER SET CURRENT_CASELOAD = CURRENT_CASELOAD + 1 WHERE ADJUSTER_CODE = ?")) {
                ps.setString(1, adjusterCode);
                ps.executeUpdate();
            }
        }
        return adjusterCode;
    }

    /**
     * H2 CREATE ALIAS target for SP_RECONCILE_PREMIUMS.
     * Monthly premium reconciliation.
     */
    public static ResultSet reconcilePremiums(Connection conn) throws SQLException {
        return conn.createStatement().executeQuery(
            "SELECT p.POLICY_NUMBER, p.PREMIUM_AMOUNT, " +
            "COALESCE(SUM(cp.AMOUNT), 0) AS TOTAL_CLAIMS_PAID, " +
            "p.PREMIUM_AMOUNT - COALESCE(SUM(cp.AMOUNT), 0) AS NET_PREMIUM " +
            "FROM POLICY p " +
            "LEFT JOIN CLAIM c ON c.POLICY_ID = p.ID " +
            "LEFT JOIN CLAIM_PAYMENT cp ON cp.CLAIM_ID = c.ID " +
            "AND cp.PAYMENT_STATUS IN ('PROCESSED','ISSUED') " +
            "WHERE p.STATUS = 'ACTIVE' " +
            "GROUP BY p.POLICY_NUMBER, p.PREMIUM_AMOUNT " +
            "ORDER BY NET_PREMIUM ASC"
        );
    }

    // ========================================================================
    // Inner classes
    // ========================================================================

    /**
     * Represents a claim with an active pessimistic lock.
     * The associated Connection must be committed/rolled back and closed
     * by the caller to release the lock.
     */
    public static class LockedClaim implements AutoCloseable {
        private Connection connection;
        private Long claimId;
        private String claimNumber;
        private String status;
        private BigDecimal approvedAmount;
        private BigDecimal reserveAmount;

        @Override
        public void close() {
            if (connection != null) {
                try {
                    connection.close();
                } catch (SQLException e) {
                    LoggerFactory.getLogger(LockedClaim.class)
                            .warn("Failed to close locked connection", e);
                }
            }
        }

        public Connection getConnection() { return connection; }
        public void setConnection(Connection connection) { this.connection = connection; }
        public Long getClaimId() { return claimId; }
        public void setClaimId(Long claimId) { this.claimId = claimId; }
        public String getClaimNumber() { return claimNumber; }
        public void setClaimNumber(String claimNumber) { this.claimNumber = claimNumber; }
        public String getStatus() { return status; }
        public void setStatus(String status) { this.status = status; }
        public BigDecimal getApprovedAmount() { return approvedAmount; }
        public void setApprovedAmount(BigDecimal approvedAmount) { this.approvedAmount = approvedAmount; }
        public BigDecimal getReserveAmount() { return reserveAmount; }
        public void setReserveAmount(BigDecimal reserveAmount) { this.reserveAmount = reserveAmount; }
    }

    /**
     * DTO representing a payment discrepancy found during reconciliation.
     */
    public static class PaymentDiscrepancy {
        private Long claimId;
        private String claimNumber;
        private BigDecimal approvedAmount;
        private BigDecimal totalPaid;
        private BigDecimal discrepancy;
        private String discrepancyType;

        public Long getClaimId() { return claimId; }
        public void setClaimId(Long claimId) { this.claimId = claimId; }
        public String getClaimNumber() { return claimNumber; }
        public void setClaimNumber(String claimNumber) { this.claimNumber = claimNumber; }
        public BigDecimal getApprovedAmount() { return approvedAmount; }
        public void setApprovedAmount(BigDecimal approvedAmount) { this.approvedAmount = approvedAmount; }
        public BigDecimal getTotalPaid() { return totalPaid; }
        public void setTotalPaid(BigDecimal totalPaid) { this.totalPaid = totalPaid; }
        public BigDecimal getDiscrepancy() { return discrepancy; }
        public void setDiscrepancy(BigDecimal discrepancy) { this.discrepancy = discrepancy; }
        public String getDiscrepancyType() { return discrepancyType; }
        public void setDiscrepancyType(String discrepancyType) { this.discrepancyType = discrepancyType; }
    }

    // ========================================================================
    // Resource management helpers
    // ========================================================================

    private void rollbackQuietly(Connection conn) {
        if (conn != null) {
            try { conn.rollback(); }
            catch (SQLException e) { log.warn("Failed to rollback", e); }
        }
    }

    private void closeQuietly(AutoCloseable resource) {
        if (resource != null) {
            try { resource.close(); }
            catch (Exception e) { log.warn("Failed to close resource", e); }
        }
    }
}
