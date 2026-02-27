package com.greylegacy.batch.springbatch;

import com.greylegacy.domain.Policy;
import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.item.ItemWriter;

import java.util.List;

/**
 * Spring Batch {@link ItemWriter} that persists imported {@link Policy}
 * entities to the database via Hibernate.
 *
 * <h3>Persistence Strategy</h3>
 * <p>Uses Hibernate's {@code saveOrUpdate} so that:</p>
 * <ul>
 *   <li>New policies (no existing ID) are inserted</li>
 *   <li>Existing policies (matched by policy number) can be updated
 *       if the DAO layer supports natural-key lookup</li>
 * </ul>
 *
 * <h3>Session Management</h3>
 * <p>The writer piggybacks on the step's transactional session. After
 * writing each chunk, the Hibernate session is flushed and cleared to
 * avoid first-level cache bloat during large imports.</p>
 *
 * <h3>Duplicate Handling</h3>
 * <p>Before persisting, the writer checks whether a policy with the same
 * policy number already exists. If so, it updates the existing record
 * rather than inserting a duplicate, providing idempotent behaviour
 * useful for restartable jobs.</p>
 *
 * @author Grey Legacy Batch Team
 * @since 1.0.0
 */
public class PolicyImportWriter implements ItemWriter<Policy> {

    private static final Logger log = LoggerFactory.getLogger(PolicyImportWriter.class);

    private static final String FIND_BY_POLICY_NUMBER_HQL =
            "SELECT p FROM Policy p WHERE p.policyNumber = :policyNumber";

    private SessionFactory sessionFactory;
    private int totalInserted;
    private int totalUpdated;

    // -------------------------------------------------------------------------
    // Configuration
    // -------------------------------------------------------------------------

    /**
     * Sets the Hibernate {@link SessionFactory} for persistence operations.
     *
     * @param sessionFactory the session factory
     */
    public void setSessionFactory(SessionFactory sessionFactory) {
        this.sessionFactory = sessionFactory;
    }

    // -------------------------------------------------------------------------
    // ItemWriter implementation
    // -------------------------------------------------------------------------

    /**
     * {@inheritDoc}
     *
     * Writes a chunk of {@link Policy} entities to the database. For each
     * policy, checks if a record with the same policy number exists:
     * <ul>
     *   <li>If yes, merges the incoming data into the existing record</li>
     *   <li>If no, saves a new record</li>
     * </ul>
     *
     * After processing the chunk, the session is flushed and cleared to
     * free memory.
     */
    @Override
    public void write(List<? extends Policy> items) throws Exception {
        Session session = sessionFactory.getCurrentSession();

        for (Policy incoming : items) {
            Policy existing = findByPolicyNumber(session, incoming.getPolicyNumber());

            if (existing != null) {
                // Update existing policy with incoming data
                mergePolicy(existing, incoming);
                session.update(existing);
                totalUpdated++;
                log.debug("Updated existing policy [{}]", incoming.getPolicyNumber());
            } else {
                // Insert new policy
                session.save(incoming);
                totalInserted++;
                log.debug("Inserted new policy [{}]", incoming.getPolicyNumber());
            }
        }

        // Flush and clear session to prevent L1 cache bloat on large imports
        session.flush();
        session.clear();

        log.debug("Wrote chunk of {} policies (total inserted: {}, updated: {})",
                items.size(), totalInserted, totalUpdated);
    }

    // -------------------------------------------------------------------------
    // Internal helpers
    // -------------------------------------------------------------------------

    /**
     * Looks up an existing policy by policy number. Returns {@code null} if
     * no match is found.
     */
    @SuppressWarnings("unchecked")
    private Policy findByPolicyNumber(Session session, String policyNumber) {
        List<Policy> results = session.createQuery(FIND_BY_POLICY_NUMBER_HQL)
                .setParameter("policyNumber", policyNumber)
                .setMaxResults(1)
                .list();
        return results.isEmpty() ? null : results.get(0);
    }

    /**
     * Merges incoming field values onto the existing persistent entity.
     * Only non-null incoming fields overwrite existing values.
     */
    private void mergePolicy(Policy existing, Policy incoming) {
        if (incoming.getHolderFirstName() != null) {
            existing.setHolderFirstName(incoming.getHolderFirstName());
        }
        if (incoming.getHolderLastName() != null) {
            existing.setHolderLastName(incoming.getHolderLastName());
        }
        if (incoming.getPolicyType() != null) {
            existing.setPolicyType(incoming.getPolicyType());
        }
        if (incoming.getEffectiveDate() != null) {
            existing.setEffectiveDate(incoming.getEffectiveDate());
        }
        if (incoming.getExpirationDate() != null) {
            existing.setExpirationDate(incoming.getExpirationDate());
        }
        if (incoming.getPremiumAmount() != null) {
            existing.setPremiumAmount(incoming.getPremiumAmount());
        }
        if (incoming.getCoverageLimit() != null) {
            existing.setCoverageLimit(incoming.getCoverageLimit());
        }
        if (incoming.getDeductible() != null) {
            existing.setDeductible(incoming.getDeductible());
        }
        if (incoming.getStatus() != null) {
            existing.setStatus(incoming.getStatus());
        }
    }

    // -------------------------------------------------------------------------
    // Metrics
    // -------------------------------------------------------------------------

    public int getTotalInserted() {
        return totalInserted;
    }

    public int getTotalUpdated() {
        return totalUpdated;
    }
}
