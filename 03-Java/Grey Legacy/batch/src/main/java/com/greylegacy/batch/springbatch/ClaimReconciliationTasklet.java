package com.greylegacy.batch.springbatch;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.core.StepContribution;
import org.springframework.batch.core.scope.context.ChunkContext;
import org.springframework.batch.core.step.tasklet.Tasklet;
import org.springframework.batch.repeat.RepeatStatus;

import javax.sql.DataSource;
import java.math.BigDecimal;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.Statement;
import java.sql.Timestamp;
import java.util.Date;

/**
 * Spring Batch {@link Tasklet} that reconciles claim payment totals against
 * approved amounts.
 *
 * <p>For every claim that has an {@code APPROVED_AMOUNT}, this tasklet sums
 * all {@code CLAIM_PAYMENT.AMOUNT} rows and compares the result. Any
 * discrepancy is recorded in the {@code RECONCILIATION_REPORT} table and
 * logged at WARN level.</p>
 *
 * <p>Uses raw JDBC via an injected {@link DataSource} to avoid Hibernate
 * overhead for these aggregate queries.</p>
 *
 * @author Grey Legacy Batch Team
 * @since 1.0.0
 */
public class ClaimReconciliationTasklet implements Tasklet {

    private static final Logger log = LoggerFactory.getLogger(ClaimReconciliationTasklet.class);

    /** SQL to find claims where total payments differ from approved amount. */
    private static final String DISCREPANCY_SQL =
            "SELECT c.ID AS CLAIM_ID, c.CLAIM_NUMBER, c.APPROVED_AMOUNT, " +
            "       COALESCE(SUM(p.AMOUNT), 0) AS TOTAL_PAID " +
            "FROM CLAIM c " +
            "LEFT JOIN CLAIM_PAYMENT p ON p.CLAIM_ID = c.ID " +
            "WHERE c.APPROVED_AMOUNT IS NOT NULL " +
            "GROUP BY c.ID, c.CLAIM_NUMBER, c.APPROVED_AMOUNT " +
            "HAVING c.APPROVED_AMOUNT <> COALESCE(SUM(p.AMOUNT), 0)";

    /** SQL to insert a discrepancy row into the reconciliation report table. */
    private static final String INSERT_REPORT_SQL =
            "INSERT INTO RECONCILIATION_REPORT " +
            "(CLAIM_ID, CLAIM_NUMBER, APPROVED_AMOUNT, TOTAL_PAID, DISCREPANCY, REPORT_DATE) " +
            "VALUES (?, ?, ?, ?, ?, ?)";

    private DataSource dataSource;

    // -------------------------------------------------------------------------
    // Configuration
    // -------------------------------------------------------------------------

    /**
     * Sets the JDBC {@link DataSource} used for reconciliation queries.
     *
     * @param dataSource the data source
     */
    public void setDataSource(DataSource dataSource) {
        this.dataSource = dataSource;
    }

    // -------------------------------------------------------------------------
    // Tasklet implementation
    // -------------------------------------------------------------------------

    /**
     * {@inheritDoc}
     *
     * Executes the reconciliation, writing all discrepancies to the
     * {@code RECONCILIATION_REPORT} table, then returns
     * {@link RepeatStatus#FINISHED}.
     */
    @Override
    public RepeatStatus execute(StepContribution contribution, ChunkContext chunkContext)
            throws Exception {

        log.info("=== CLAIM RECONCILIATION TASKLET STARTED ===");
        long startTime = System.currentTimeMillis();
        int discrepancyCount = 0;

        Connection conn = null;
        Statement stmt = null;
        ResultSet rs = null;
        PreparedStatement insertStmt = null;

        try {
            conn = dataSource.getConnection();
            stmt = conn.createStatement();
            rs = stmt.executeQuery(DISCREPANCY_SQL);

            insertStmt = conn.prepareStatement(INSERT_REPORT_SQL);
            Timestamp reportDate = new Timestamp(new Date().getTime());

            while (rs.next()) {
                long claimId = rs.getLong("CLAIM_ID");
                String claimNumber = rs.getString("CLAIM_NUMBER");
                BigDecimal approved = rs.getBigDecimal("APPROVED_AMOUNT");
                BigDecimal totalPaid = rs.getBigDecimal("TOTAL_PAID");
                BigDecimal discrepancy = approved.subtract(totalPaid);

                log.warn("Discrepancy on claim [{}]: approved={}, paid={}, diff={}",
                        claimNumber, approved, totalPaid, discrepancy);

                insertStmt.setLong(1, claimId);
                insertStmt.setString(2, claimNumber);
                insertStmt.setBigDecimal(3, approved);
                insertStmt.setBigDecimal(4, totalPaid);
                insertStmt.setBigDecimal(5, discrepancy);
                insertStmt.setTimestamp(6, reportDate);
                insertStmt.addBatch();

                discrepancyCount++;
            }

            if (discrepancyCount > 0) {
                insertStmt.executeBatch();
            }

        } finally {
            closeQuietly(rs);
            closeQuietly(stmt);
            closeQuietly(insertStmt);
            closeQuietly(conn);
        }

        long duration = System.currentTimeMillis() - startTime;
        log.info("=== CLAIM RECONCILIATION COMPLETED in {}ms. Discrepancies found: {} ===",
                duration, discrepancyCount);

        return RepeatStatus.FINISHED;
    }

    // -------------------------------------------------------------------------
    // Resource cleanup helpers
    // -------------------------------------------------------------------------

    private void closeQuietly(AutoCloseable resource) {
        if (resource != null) {
            try {
                resource.close();
            } catch (Exception e) {
                log.debug("Error closing resource", e);
            }
        }
    }
}
