package com.greylegacy.batch.springbatch;

import com.greylegacy.domain.ClaimStatus;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.core.partition.support.Partitioner;
import org.springframework.batch.item.ExecutionContext;

import javax.sql.DataSource;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.util.HashMap;
import java.util.Map;

/**
 * Spring Batch {@link Partitioner} that splits the claim export workload
 * across multiple step execution threads by dividing the claim table
 * into contiguous ID ranges.
 *
 * <h3>Partitioning Strategy</h3>
 * <p>Queries the database for the {@code MIN(ID)} and {@code MAX(ID)} of
 * claims matching the configured status, then divides the range into
 * {@code gridSize} roughly equal partitions. Each partition receives an
 * {@link ExecutionContext} with {@code minId} and {@code maxId} keys
 * that the partitioned reader uses to constrain its query.</p>
 *
 * <h3>Why Range Partitioning?</h3>
 * <ul>
 *   <li>Works with any database — no vendor-specific SQL</li>
 *   <li>Each slave step reads a non-overlapping range → no duplicates</li>
 *   <li>Avoids OFFSET/LIMIT pagination (O(n) performance on large tables)</li>
 *   <li>Survives restarts: each partition's checkpoint is independent</li>
 * </ul>
 *
 * <h3>Usage</h3>
 * <pre>
 * &lt;batch:step id="partitionedExportStep"&gt;
 *   &lt;batch:partition step="exportSlaveStep"
 *                     partitioner="claimExportPartitioner"&gt;
 *     &lt;batch:handler grid-size="4"
 *                    task-executor="batchTaskExecutor"/&gt;
 *   &lt;/batch:partition&gt;
 * &lt;/batch:step&gt;
 * </pre>
 *
 * @author Grey Legacy Batch Team
 * @since 1.0.0
 */
public class ClaimExportPartitioner implements Partitioner {

    private static final Logger log = LoggerFactory.getLogger(ClaimExportPartitioner.class);

    private static final String RANGE_SQL =
            "SELECT MIN(ID) AS MIN_ID, MAX(ID) AS MAX_ID FROM CLAIM WHERE STATUS = ?";

    private DataSource dataSource;
    private String claimStatus = ClaimStatus.APPROVED.name();

    // -------------------------------------------------------------------------
    // Configuration
    // -------------------------------------------------------------------------

    /**
     * Sets the JDBC {@link DataSource} for querying claim ID ranges.
     *
     * @param dataSource the data source
     */
    public void setDataSource(DataSource dataSource) {
        this.dataSource = dataSource;
    }

    /**
     * Sets the claim status to partition on.
     *
     * @param claimStatus the status string (e.g. "APPROVED")
     */
    public void setClaimStatus(String claimStatus) {
        this.claimStatus = claimStatus;
    }

    // -------------------------------------------------------------------------
    // Partitioner implementation
    // -------------------------------------------------------------------------

    /**
     * {@inheritDoc}
     *
     * Creates a map of partition name → {@link ExecutionContext}, where each
     * context contains {@code minId} and {@code maxId} for a contiguous
     * range of claim IDs.
     *
     * @param gridSize the desired number of partitions
     * @return map of partition contexts, keyed by {@code "partition0"..N}
     */
    @Override
    public Map<String, ExecutionContext> partition(int gridSize) {
        log.info("Partitioning claims (status={}) into {} partitions", claimStatus, gridSize);

        long minId = 0;
        long maxId = 0;

        Connection conn = null;
        PreparedStatement ps = null;
        ResultSet rs = null;

        try {
            conn = dataSource.getConnection();
            ps = conn.prepareStatement(RANGE_SQL);
            ps.setString(1, claimStatus);
            rs = ps.executeQuery();

            if (rs.next()) {
                minId = rs.getLong("MIN_ID");
                maxId = rs.getLong("MAX_ID");
            }
        } catch (Exception e) {
            log.error("Failed to query ID range for partitioning", e);
            // Fall back to a single partition covering all IDs
            minId = 0;
            maxId = Long.MAX_VALUE;
        } finally {
            closeQuietly(rs);
            closeQuietly(ps);
            closeQuietly(conn);
        }

        log.info("Claim ID range: [{}, {}]", minId, maxId);

        Map<String, ExecutionContext> partitions = new HashMap<>();

        long totalRange = maxId - minId + 1;
        long partitionSize = Math.max(totalRange / gridSize, 1);

        long start = minId;
        for (int i = 0; i < gridSize; i++) {
            ExecutionContext context = new ExecutionContext();

            long end = (i == gridSize - 1) ? maxId : start + partitionSize - 1;
            context.putLong("minId", start);
            context.putLong("maxId", end);
            context.putString("partitionName", "partition" + i);

            String partitionKey = "partition" + i;
            partitions.put(partitionKey, context);

            log.info("  {} → [{}, {}] (est. {} records)",
                    partitionKey, start, end, end - start + 1);

            start = end + 1;
        }

        return partitions;
    }

    // -------------------------------------------------------------------------
    // Cleanup helpers
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
