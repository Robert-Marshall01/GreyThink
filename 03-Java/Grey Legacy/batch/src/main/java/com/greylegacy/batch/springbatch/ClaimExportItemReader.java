package com.greylegacy.batch.springbatch;

import com.greylegacy.domain.Claim;
import com.greylegacy.domain.ClaimStatus;
import org.hibernate.ScrollMode;
import org.hibernate.ScrollableResults;
import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.item.ExecutionContext;
import org.springframework.batch.item.ItemReader;
import org.springframework.batch.item.ItemStream;
import org.springframework.batch.item.ItemStreamException;

/**
 * Spring Batch {@link ItemReader} that reads {@link Claim} entities from the
 * database using a Hibernate {@link ScrollableResults} cursor for
 * memory-efficient processing of large result sets.
 *
 * <p>Claims are filtered by a configurable {@link ClaimStatus}. The reader
 * implements {@link ItemStream} so that Spring Batch automatically manages
 * the Hibernate session lifecycle through {@link #open}, {@link #update},
 * and {@link #close}.</p>
 *
 * <h3>Restartability</h3>
 * <p>On {@link #update}, the current read count is persisted to the
 * {@link ExecutionContext}. On a restart, {@link #open} reads this count
 * and fast-forwards the cursor to the last checkpoint, enabling the job
 * to resume from where it left off rather than re-processing records.</p>
 *
 * <p>This implementation follows the HibernateCursorItemReader pattern from
 * Spring Batch but is implemented manually for compatibility with the
 * existing Spring 4.3 / Hibernate 5.2 stack.</p>
 *
 * @author Grey Legacy Batch Team
 * @since 1.0.0
 */
public class ClaimExportItemReader implements ItemReader<Claim>, ItemStream {

    private static final Logger log = LoggerFactory.getLogger(ClaimExportItemReader.class);

    private static final String READ_COUNT_KEY = "claim.export.read.count";

    private static final String QUERY_BY_STATUS =
            "SELECT c FROM Claim c LEFT JOIN FETCH c.policy WHERE c.status = :status ORDER BY c.id";

    private SessionFactory sessionFactory;
    private ClaimStatus claimStatus;

    private Session session;
    private ScrollableResults scrollableResults;
    private int readCount;

    // -------------------------------------------------------------------------
    // Configuration setters
    // -------------------------------------------------------------------------

    /**
     * Sets the Hibernate {@link SessionFactory} used to create the cursor session.
     *
     * @param sessionFactory the session factory
     */
    public void setSessionFactory(SessionFactory sessionFactory) {
        this.sessionFactory = sessionFactory;
    }

    /**
     * Sets the {@link ClaimStatus} used to filter which claims are read.
     *
     * @param claimStatus the status to filter on
     */
    public void setClaimStatus(ClaimStatus claimStatus) {
        this.claimStatus = claimStatus;
    }

    // -------------------------------------------------------------------------
    // ItemStream lifecycle
    // -------------------------------------------------------------------------

    /**
     * Opens a new Hibernate session, creates a forward-only scrollable
     * cursor, and fast-forwards past previously read records on restart.
     *
     * @param executionContext the execution context for restart state
     * @throws ItemStreamException if the session or cursor cannot be opened
     */
    @Override
    public void open(ExecutionContext executionContext) throws ItemStreamException {
        log.info("Opening ClaimExportItemReader for status [{}]", claimStatus);

        int itemsToSkip = 0;
        if (executionContext.containsKey(READ_COUNT_KEY)) {
            itemsToSkip = executionContext.getInt(READ_COUNT_KEY);
            log.info("Restart detected — fast-forwarding past {} previously read records", itemsToSkip);
        }

        readCount = 0;

        try {
            session = sessionFactory.openSession();
            scrollableResults = session.createQuery(QUERY_BY_STATUS)
                    .setParameter("status", claimStatus)
                    .setReadOnly(true)
                    .setFetchSize(100)
                    .scroll(ScrollMode.FORWARD_ONLY);
        } catch (Exception e) {
            throw new ItemStreamException("Failed to open Hibernate cursor", e);
        }

        // Fast-forward on restart
        for (int i = 0; i < itemsToSkip; i++) {
            if (!scrollableResults.next()) {
                log.warn("Fewer records available ({}) than checkpoint ({}). "
                        + "Data may have changed since last run.", i, itemsToSkip);
                break;
            }
            readCount++;
        }

        log.debug("Scrollable cursor opened successfully. Position: {}", readCount);
    }

    /**
     * Persists the current read count to the execution context so the job
     * can resume from this checkpoint on restart.
     *
     * @param executionContext the execution context to update
     * @throws ItemStreamException if the update fails
     */
    @Override
    public void update(ExecutionContext executionContext) throws ItemStreamException {
        executionContext.putInt(READ_COUNT_KEY, readCount);
        log.trace("Checkpointed read count: {}", readCount);
    }

    /**
     * Closes the scrollable cursor and the Hibernate session, releasing all
     * database resources.
     *
     * @throws ItemStreamException if resources cannot be released
     */
    @Override
    public void close() throws ItemStreamException {
        log.info("Closing ClaimExportItemReader. Total claims read: {}", readCount);

        if (scrollableResults != null) {
            try {
                scrollableResults.close();
            } catch (Exception e) {
                log.warn("Error closing ScrollableResults", e);
            }
        }
        if (session != null && session.isOpen()) {
            try {
                session.close();
            } catch (Exception e) {
                log.warn("Error closing Hibernate session", e);
            }
        }
    }

    // -------------------------------------------------------------------------
    // ItemReader implementation
    // -------------------------------------------------------------------------

    /**
     * {@inheritDoc}
     *
     * Returns the next {@link Claim} from the scrollable cursor, or
     * {@code null} when all matching claims have been read.
     */
    @Override
    public Claim read() {
        if (scrollableResults == null) {
            throw new IllegalStateException(
                    "Reader has not been opened. Ensure it implements ItemStream and is "
                    + "registered as a stream on the step.");
        }

        if (scrollableResults.next()) {
            Claim claim = (Claim) scrollableResults.get(0);
            readCount++;

            if (readCount % 500 == 0) {
                log.debug("Read {} claims so far", readCount);
            }
            return claim;
        }

        log.debug("No more claims to read. Returning null to signal end of data.");
        return null;
    }
}
