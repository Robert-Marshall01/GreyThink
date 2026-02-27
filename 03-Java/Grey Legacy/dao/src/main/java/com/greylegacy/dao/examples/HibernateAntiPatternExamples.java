package com.greylegacy.dao.examples;

import com.greylegacy.domain.*;

import org.hibernate.*;
import org.hibernate.query.Query;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import javax.persistence.*;
import javax.persistence.criteria.*;
import java.math.BigDecimal;
import java.util.List;
import java.util.Set;

/**
 * Documented reference of common Hibernate/JPA anti-patterns encountered in the
 * Grey Legacy insurance claims system. Each static method demonstrates a specific
 * anti-pattern with inline comments showing the generated SQL and explaining the
 * performance or correctness impact.
 *
 * <p><strong>This class is NOT intended to be executed.</strong> It serves as a
 * training reference and code review checklist for the development team.</p>
 *
 * <p>Domain model relationships referenced:</p>
 * <pre>
 *   Policy (1) ──→ (*) Claim ──→ (*) ClaimPayment
 *                                 ──→ (*) ClaimAuditEntry
 *                                 ──→ (*) ClaimNote
 *   Policy (1) ──→ (*) PolicyEndorsement
 * </pre>
 *
 * @see com.greylegacy.domain.Policy
 * @see com.greylegacy.domain.Claim
 * @see com.greylegacy.domain.ClaimPayment
 */
public class HibernateAntiPatternExamples {

    // These would be injected in real code; declared here for illustration only
    private static SessionFactory sessionFactory;
    private static EntityManager entityManager;

    // =========================================================================
    // 1. N+1 Query Problem
    // =========================================================================

    /**
     * Demonstrates the N+1 query problem when iterating over a parent collection
     * and accessing a lazy-loaded child collection for each parent.
     *
     * <p>If there are 100 active policies, this produces <strong>101 SQL queries</strong>:
     * 1 to load all policies, then 1 per policy to load its claims.</p>
     *
     * <p>In production, this has been observed turning a sub-second dashboard load
     * into a 12-second page render when the policy count exceeded 500.</p>
     */
    public static void nPlusOneQueryProblem() {
        Session session = sessionFactory.getCurrentSession();

        // Query 1: Load all active policies
        // SQL: SELECT p.* FROM POLICY p WHERE p.STATUS = 'ACTIVE'
        List<Policy> policies = session.createQuery(
                "FROM Policy p WHERE p.status = :status", Policy.class)
                .setParameter("status", PolicyStatus.ACTIVE)
                .getResultList();

        for (Policy policy : policies) {
            // For EACH policy, Hibernate fires a separate query to load claims.
            // SQL (repeated N times):
            //   SELECT c.* FROM CLAIM c WHERE c.POLICY_ID = ?
            //
            // With 100 policies, this loop generates 100 additional queries.
            List<Claim> claims = policy.getClaims();
            for (Claim claim : claims) {
                System.out.println(policy.getPolicyNumber()
                        + " -> " + claim.getClaimNumber()
                        + " [" + claim.getStatus() + "]");
            }
        }
        // Total queries: 1 + N  (where N = number of active policies)
        // With 100 policies: 101 queries. With 1000 policies: 1001 queries.
    }

    // =========================================================================
    // 2. N+1 Query Fix
    // =========================================================================

    /**
     * Fixes the N+1 problem using an HQL fetch join. All data is loaded in a
     * single SQL query using a LEFT JOIN.
     *
     * <p>The {@code DISTINCT} keyword is required because the join produces
     * duplicate parent rows (one per child). Hibernate deduplicates in memory.</p>
     */
    public static void nPlusOneQueryFix() {
        Session session = sessionFactory.getCurrentSession();

        // Single query with JOIN FETCH:
        // SQL: SELECT DISTINCT p.*, c.*
        //      FROM POLICY p
        //      LEFT JOIN CLAIM c ON c.POLICY_ID = p.ID
        //      WHERE p.STATUS = 'ACTIVE'
        List<Policy> policies = session.createQuery(
                "SELECT DISTINCT p FROM Policy p "
                        + "LEFT JOIN FETCH p.claims c "
                        + "WHERE p.status = :status", Policy.class)
                .setParameter("status", PolicyStatus.ACTIVE)
                .getResultList();

        // No additional queries — claims are already loaded
        for (Policy policy : policies) {
            for (Claim claim : policy.getClaims()) {
                System.out.println(policy.getPolicyNumber()
                        + " -> " + claim.getClaimNumber());
            }
        }
        // Total queries: 1 (down from 101+)
    }

    // =========================================================================
    // 3. LazyInitializationException Outside Session
    // =========================================================================

    /**
     * Demonstrates {@link org.hibernate.LazyInitializationException} by accessing
     * a lazy collection after the Hibernate Session has been closed.
     *
     * <p>This is the single most common Hibernate error in the Grey Legacy
     * codebase, typically occurring when a Struts Action or JSP accesses
     * claim.getPayments() after the service-layer transaction has committed.</p>
     *
     * @throws org.hibernate.LazyInitializationException always
     */
    public static void lazyInitializationOutsideSession() {
        Claim claim;

        // --- Transaction / Session boundary START ---
        Session session = sessionFactory.openSession();
        Transaction tx = session.beginTransaction();

        // Load claim — payments collection is a lazy proxy, NOT yet loaded
        // SQL: SELECT c.* FROM CLAIM c WHERE c.ID = 1042
        claim = session.get(Claim.class, 1042L);

        tx.commit();
        session.close();
        // --- Transaction / Session boundary END ---

        // Attempting to access the lazy collection AFTER session is closed:
        // This throws: org.hibernate.LazyInitializationException:
        //   failed to lazily initialize a collection of role:
        //   com.greylegacy.domain.Claim.payments,
        //   could not initialize proxy - no Session
        List<ClaimPayment> payments = claim.getPayments();
        payments.size();  // BOOM! No session to load the data from.
    }

    // =========================================================================
    // 4. LazyInitializationException Fix
    // =========================================================================

    /**
     * Fixes the LazyInitializationException by explicitly initializing lazy
     * collections within the active session, and by using fetch joins.
     *
     * <p>Two strategies shown:</p>
     * <ul>
     *   <li>{@code Hibernate.initialize()} — forces proxy initialization</li>
     *   <li>Fetch join HQL — loads everything in one query</li>
     * </ul>
     *
     * <p>A third option (OpenSessionInViewFilter) keeps the session open through
     * the HTTP request but is considered an anti-pattern because it hides N+1
     * issues and can cause unexpected DB access during view rendering.</p>
     */
    public static void lazyInitializationFix() {
        Session session = sessionFactory.openSession();
        Transaction tx = session.beginTransaction();

        // Strategy 1: Explicit initialization
        Claim claim = session.get(Claim.class, 1042L);
        Hibernate.initialize(claim.getPayments());    // Forces SQL now
        Hibernate.initialize(claim.getAuditEntries()); // Forces SQL now

        tx.commit();
        session.close();

        // Safe to access — data is already in memory
        int paymentCount = claim.getPayments().size();  // No exception


        // Strategy 2: Fetch join (preferred — single query)
        Session session2 = sessionFactory.openSession();
        Transaction tx2 = session2.beginTransaction();

        // SQL: SELECT c.*, p.* FROM CLAIM c
        //      LEFT JOIN CLAIM_PAYMENT p ON p.CLAIM_ID = c.ID
        //      WHERE c.ID = 1042
        Claim claimWithPayments = session2.createQuery(
                "SELECT c FROM Claim c LEFT JOIN FETCH c.payments "
                        + "WHERE c.id = :id", Claim.class)
                .setParameter("id", 1042L)
                .uniqueResult();

        tx2.commit();
        session2.close();

        // Payments already loaded via the join — safe to access
        claimWithPayments.getPayments().forEach(
                p -> System.out.println(p.getPaymentNumber()));
    }

    // =========================================================================
    // 5. Cartesian Product Problem
    // =========================================================================

    /**
     * Demonstrates the cartesian product problem when multiple collections on
     * the same entity are fetched eagerly or via multiple fetch joins.
     *
     * <p>If a Claim has 5 payments and 10 audit entries, a single query
     * joining both produces 5 × 10 = 50 rows. With 3 collections this
     * grows explosively. Hibernate also throws a
     * {@code MultipleBagFetchException} when two {@code List} collections
     * are fetch-joined simultaneously.</p>
     */
    public static void cartesianProductProblem() {
        Session session = sessionFactory.getCurrentSession();

        // BAD: Fetch-joining two List collections simultaneously.
        // Hibernate will throw:
        //   org.hibernate.loader.MultipleBagFetchException:
        //   cannot simultaneously fetch multiple bags:
        //   [com.greylegacy.domain.Claim.payments,
        //    com.greylegacy.domain.Claim.auditEntries]
        //
        // Even if it worked (using Set instead of List), the resulting SQL
        // would be a cartesian product:
        //
        // SQL: SELECT c.*, p.*, a.*
        //      FROM CLAIM c
        //      LEFT JOIN CLAIM_PAYMENT p ON p.CLAIM_ID = c.ID
        //      LEFT JOIN CLAIM_AUDIT a ON a.CLAIM_ID = c.ID
        //      WHERE c.ID = 1042
        //
        // If claim 1042 has 5 payments and 10 audits:
        //   Result set: 5 × 10 = 50 rows  (should be at most 15)
        //   Duplicate data transferred over the wire.
        //   Hibernate deduplicates in memory — but the DB did extra work.
        try {
            Claim claim = session.createQuery(
                    "SELECT c FROM Claim c "
                            + "LEFT JOIN FETCH c.payments "
                            + "LEFT JOIN FETCH c.auditEntries "
                            + "WHERE c.id = :id", Claim.class)
                    .setParameter("id", 1042L)
                    .uniqueResult();
        } catch (Exception e) {
            // MultipleBagFetchException if using List (the Grey Legacy default)
            System.err.println("Cannot fetch multiple bags: " + e.getMessage());
        }
    }

    // =========================================================================
    // 6. Cartesian Product Fix
    // =========================================================================

    /**
     * Fixes the cartesian product problem using two strategies:
     * <ul>
     *   <li>Separate queries for each collection (avoids the join explosion)</li>
     *   <li>{@code @BatchSize} for lazy batch-loading</li>
     * </ul>
     *
     * <p>Alternatively, changing {@code List} to {@code Set} on the entity
     * avoids MultipleBagFetchException but still produces a cartesian product.
     * The separate-query approach is preferred.</p>
     */
    public static void cartesianProductFix() {
        Session session = sessionFactory.getCurrentSession();

        // Strategy 1: Two separate fetch-join queries
        // Query 1 — fetch payments
        // SQL: SELECT c.*, p.* FROM CLAIM c
        //      LEFT JOIN CLAIM_PAYMENT p ON p.CLAIM_ID = c.ID
        //      WHERE c.ID = 1042
        Claim claim = session.createQuery(
                "SELECT c FROM Claim c LEFT JOIN FETCH c.payments "
                        + "WHERE c.id = :id", Claim.class)
                .setParameter("id", 1042L)
                .uniqueResult();

        // Query 2 — fetch audit entries (claim already in session, gets merged)
        // SQL: SELECT c.*, a.* FROM CLAIM c
        //      LEFT JOIN CLAIM_AUDIT a ON a.CLAIM_ID = c.ID
        //      WHERE c.ID = 1042
        session.createQuery(
                "SELECT c FROM Claim c LEFT JOIN FETCH c.auditEntries "
                        + "WHERE c.id = :id", Claim.class)
                .setParameter("id", 1042L)
                .uniqueResult();

        // Total: 2 queries, NO cartesian product, NO MultipleBagFetchException
        // claim now has both payments and auditEntries initialized

        // Strategy 2: Use @BatchSize on the entity mapping
        // @OneToMany(mappedBy = "claim")
        // @BatchSize(size = 25)
        // private List<ClaimPayment> payments;
        //
        // This tells Hibernate to initialize up to 25 collections at a time
        // using an IN clause, avoiding both cartesian products and N+1.
    }

    // =========================================================================
    // 7. Session Cache (First-Level Cache) Overflow
    // =========================================================================

    /**
     * Demonstrates session cache overflow when loading a large number of entities
     * without clearing the session. The Hibernate first-level cache holds every
     * entity loaded in the session — with 100K claims, this causes
     * {@link OutOfMemoryError}.
     *
     * <p>This pattern was discovered during the nightly claims aging batch job
     * which processes all open claims.</p>
     */
    public static void sessionCacheOverflow() {
        Session session = sessionFactory.getCurrentSession();

        // BAD: Loading 100,000 claims into a single session.
        // Each Claim entity (~2 KB in memory) × 100,000 = ~200 MB in the
        // first-level cache alone. Plus lazy proxies, dirty-checking snapshots,
        // and collection wrappers — easily 500 MB+.
        //
        // SQL: SELECT c.* FROM CLAIM c WHERE c.STATUS = 'OPEN'
        List<Claim> allOpenClaims = session.createQuery(
                "FROM Claim c WHERE c.status = :status", Claim.class)
                .setParameter("status", ClaimStatus.OPEN)
                .getResultList();

        for (Claim claim : allOpenClaims) {
            // Process each claim — all 100K entities remain in session cache
            claim.setFraudScore(calculateFraudScore(claim));
        }

        // Flush writes all 100K dirty entities at once:
        // - Dirty checking compares all 100K original snapshots
        // - Generates 100K UPDATE statements
        // - Likely causes OutOfMemoryError or extreme GC pressure
        session.flush();
    }

    // =========================================================================
    // 8. Session Cache Overflow Fix
    // =========================================================================

    /**
     * Fixes the session cache overflow by processing entities in batches with
     * periodic {@code flush()} and {@code clear()} calls.
     *
     * <p>Alternative: use {@link StatelessSession} which has no first-level
     * cache at all — ideal for batch processing.</p>
     */
    public static void sessionCacheOverflowFix() {
        final int BATCH_SIZE = 50;

        // Strategy 1: Periodic flush/clear
        Session session = sessionFactory.getCurrentSession();

        ScrollableResults scroll = session.createQuery(
                "FROM Claim c WHERE c.status = :status")
                .setParameter("status", ClaimStatus.OPEN)
                .scroll(ScrollMode.FORWARD_ONLY);

        int count = 0;
        while (scroll.next()) {
            Claim claim = (Claim) scroll.get(0);
            claim.setFraudScore(calculateFraudScore(claim));
            count++;

            if (count % BATCH_SIZE == 0) {
                session.flush();   // Write pending changes to DB
                session.clear();   // Evict all entities from session cache
                // Now only ~50 entities in memory at any time
            }
        }
        scroll.close();

        // Strategy 2: StatelessSession (no first-level cache)
        StatelessSession stateless = sessionFactory.openStatelessSession();
        Transaction tx = stateless.beginTransaction();

        ScrollableResults scroll2 = stateless.createQuery(
                "FROM Claim c WHERE c.status = :status")
                .setParameter("status", ClaimStatus.OPEN)
                .scroll(ScrollMode.FORWARD_ONLY);

        while (scroll2.next()) {
            Claim claim = (Claim) scroll2.get(0);
            claim.setFraudScore(calculateFraudScore(claim));
            stateless.update(claim);  // Immediate UPDATE, no cache
            // SQL: UPDATE CLAIM SET FRAUD_SCORE = ? WHERE ID = ? AND VERSION = ?
        }

        scroll2.close();
        tx.commit();
        stateless.close();
    }

    // =========================================================================
    // 9. Optimistic Locking Conflict
    // =========================================================================

    /**
     * Demonstrates {@link org.hibernate.StaleObjectStateException} caused by
     * optimistic locking with the {@code @Version} column in
     * {@link com.greylegacy.domain.BaseEntity}.
     *
     * <p>Scenario: Two adjusters edit the same claim simultaneously. Both load
     * version 3. The first adjuster saves (version becomes 4). The second
     * adjuster's save fails because the version check doesn't match.</p>
     *
     * <p>Generated SQL includes a version check in the WHERE clause:</p>
     * <pre>
     * UPDATE CLAIM SET STATUS = ?, VERSION = 4, ...
     * WHERE ID = 1042 AND VERSION = 3
     * -- Returns 0 rows updated → StaleObjectStateException
     * </pre>
     */
    public static void optimisticLockingConflict() {
        // Adjuster A's session
        Session sessionA = sessionFactory.openSession();
        Transaction txA = sessionA.beginTransaction();
        Claim claimA = sessionA.get(Claim.class, 1042L);  // version = 3

        // Adjuster B's session (concurrent)
        Session sessionB = sessionFactory.openSession();
        Transaction txB = sessionB.beginTransaction();
        Claim claimB = sessionB.get(Claim.class, 1042L);  // version = 3

        // Adjuster A approves the claim
        claimA.setStatus(ClaimStatus.APPROVED);
        txA.commit();  // SUCCESS — version updated to 4
        sessionA.close();
        // SQL: UPDATE CLAIM SET STATUS='APPROVED', VERSION=4 WHERE ID=1042 AND VERSION=3

        // Adjuster B changes the estimated loss
        claimB.setEstimatedLoss(new BigDecimal("75000.00"));
        try {
            txB.commit();  // FAILS — version is now 4, not 3
            // SQL: UPDATE CLAIM SET ESTIMATED_LOSS=75000.00, VERSION=4
            //      WHERE ID=1042 AND VERSION=3
            //      --> 0 rows updated --> StaleObjectStateException
        } catch (Exception e) {
            // org.hibernate.StaleObjectStateException:
            //   Row was updated or deleted by another transaction
            //   (or unsaved-value mapping was incorrect):
            //   [com.greylegacy.domain.Claim#1042]
            txB.rollback();
            System.err.println("Optimistic lock failure: " + e.getMessage());
        } finally {
            sessionB.close();
        }
    }

    // =========================================================================
    // 10. Pessimistic Locking Example
    // =========================================================================

    /**
     * Demonstrates pessimistic locking using {@code LockMode.PESSIMISTIC_WRITE}
     * to prevent concurrent modifications at the database level.
     *
     * <p>This issues a {@code SELECT ... FOR UPDATE} which holds a row-level
     * lock until the transaction commits. Other transactions trying to read
     * the same row with FOR UPDATE will block.</p>
     *
     * <p>Use sparingly — pessimistic locks reduce concurrency and can cause
     * deadlocks if multiple rows are locked in different orders.</p>
     *
     * <p>Appropriate for Grey Legacy scenarios like payment processing where
     * double-payment must be absolutely prevented.</p>
     */
    public static void pessimisticLockingExample() {
        Session session = sessionFactory.getCurrentSession();

        // Acquire a write lock on the claim row.
        // SQL: SELECT c.* FROM CLAIM c WHERE c.ID = 1042 FOR UPDATE
        //
        // This blocks any other transaction from modifying (or locking) this row
        // until our transaction commits or rolls back.
        Claim claim = session.get(Claim.class, 1042L,
                LockMode.PESSIMISTIC_WRITE);

        // Safe to modify — no other transaction can touch this row
        claim.setStatus(ClaimStatus.APPROVED);
        claim.setApprovedAmount(new BigDecimal("50000.00"));

        // With a timeout to avoid indefinite blocking:
        // session.buildLockRequest(new LockOptions(LockMode.PESSIMISTIC_WRITE)
        //         .setTimeOut(5000))  // Wait max 5 seconds for the lock
        //         .lock(claim);

        // Lock released when transaction commits
    }

    // =========================================================================
    // 11. Transaction Propagation Pitfall
    // =========================================================================

    /**
     * Demonstrates the pitfall of using {@code REQUIRES_NEW} inside a
     * {@code REQUIRED} transaction with rollback.
     *
     * <p><strong>Scenario:</strong> Updating a claim (REQUIRED) should always
     * write an audit entry (REQUIRES_NEW). If the claim update fails and rolls
     * back, the audit entry is STILL committed — because it ran in its own
     * independent transaction.</p>
     *
     * <p>This can be a feature (audit trail survives failures) or a bug
     * (orphaned audit entries for changes that never happened), depending on
     * business requirements.</p>
     *
     * <p><strong>Additional pitfall:</strong> If both methods are in the same
     * class, Spring's proxy-based AOP will NOT apply @Transactional to the
     * inner call (self-invocation bypass). The REQUIRES_NEW has no effect.</p>
     */
    @Transactional(propagation = Propagation.REQUIRED)
    public static void transactionPropagationPitfall() {
        Session session = sessionFactory.getCurrentSession();

        Claim claim = session.get(Claim.class, 1042L);
        ClaimStatus oldStatus = claim.getStatus();
        claim.setStatus(ClaimStatus.APPROVED);

        // This call runs in a NEW transaction (REQUIRES_NEW).
        // The new transaction commits independently.
        writeAuditEntry(claim, oldStatus, ClaimStatus.APPROVED);

        // Now suppose an exception occurs AFTER the audit was written:
        if (claim.getEstimatedLoss().compareTo(claim.getPolicy().getCoverageLimit()) > 0) {
            // This rolls back the OUTER transaction (claim update),
            // but the audit entry is ALREADY committed in its own transaction.
            // Result: DB shows an audit entry for a status change that never
            // actually persisted. Data inconsistency!
            throw new RuntimeException("Claim exceeds coverage limit");
        }
    }

    /**
     * Writes an audit entry in a REQUIRES_NEW transaction. This transaction
     * commits independently of the caller's transaction.
     */
    @Transactional(propagation = Propagation.REQUIRES_NEW)
    private static void writeAuditEntry(Claim claim, ClaimStatus from, ClaimStatus to) {
        Session session = sessionFactory.getCurrentSession();
        ClaimAuditEntry audit = new ClaimAuditEntry(
                claim,
                new java.util.Date(),
                "STATUS_CHANGE",
                from.name(),
                to.name(),
                "SYSTEM",
                "Status changed from " + from + " to " + to,
                "127.0.0.1"
        );
        session.save(audit);
        // This commits when the method returns — BEFORE the outer TX completes.
    }

    // =========================================================================
    // 12. Wrong CascadeType on ManyToOne
    // =========================================================================

    /**
     * Demonstrates the danger of using {@code CascadeType.ALL} on a
     * {@code @ManyToOne} relationship. When the child (Claim) is deleted,
     * the cascade causes the parent (Policy) to be deleted too — which then
     * cascade-deletes ALL other claims on that policy.
     *
     * <p><strong>Root cause:</strong> {@code CascadeType.ALL} includes
     * {@code CascadeType.REMOVE}. On a ManyToOne, REMOVE propagates from
     * child → parent, which is almost never the desired behavior.</p>
     *
     * <p>In the Grey Legacy domain model, the correct cascade is defined on
     * {@link Policy#getClaims()} (parent → children), NOT on
     * {@link Claim#getPolicy()} (child → parent).</p>
     */
    public static void wrongCascadeType() {
        // WRONG entity mapping:
        // @ManyToOne(cascade = CascadeType.ALL)  // <-- DANGEROUS!
        // @JoinColumn(name = "POLICY_ID")
        // private Policy policy;

        Session session = sessionFactory.getCurrentSession();

        // Load a claim
        Claim claim = session.get(Claim.class, 1042L);

        // Developer intends to delete just this one claim.
        session.delete(claim);
        // SQL generated: DELETE FROM CLAIM WHERE ID = 1042

        // BUT with CascadeType.ALL on the @ManyToOne, Hibernate also does:
        //   DELETE FROM POLICY WHERE ID = 501     <-- Deletes the parent policy!
        //
        // And since Policy has CascadeType.ALL on its @OneToMany claims:
        //   DELETE FROM CLAIM WHERE POLICY_ID = 501  <-- Deletes ALL sibling claims!
        //   DELETE FROM POLICY_ENDORSEMENT WHERE POLICY_ID = 501
        //
        // One claim deletion just wiped out an entire policy and all its claims.

        // CORRECT mapping (what Grey Legacy actually uses):
        // @ManyToOne(fetch = FetchType.LAZY)   // No cascade on ManyToOne!
        // @JoinColumn(name = "POLICY_ID")
        // private Policy policy;
        //
        // CascadeType.ALL belongs on the @OneToMany side (Policy → Claims).
    }

    // =========================================================================
    // Helper methods (stubs for compilation)
    // =========================================================================

    private static int calculateFraudScore(Claim claim) {
        // Stub — real implementation in FraudScoringJob
        return 0;
    }
}
