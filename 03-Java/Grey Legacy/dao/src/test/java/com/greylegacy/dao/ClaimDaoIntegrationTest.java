package com.greylegacy.dao;

import com.greylegacy.domain.*;
import org.hibernate.SessionFactory;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.util.Calendar;
import java.util.Date;
import java.util.List;

import static org.junit.Assert.*;

/**
 * JUnit 4 integration test for ClaimDao.
 * Demonstrates legacy Spring test patterns:
 * - SpringJUnit4ClassRunner (pre-SpringRunner shortcut)
 * - XML-based context configuration
 * - @Transactional for automatic rollback
 * - Direct SessionFactory injection for test setup
 *
 * Uses H2 in-memory database for isolation.
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = {
    "classpath:applicationContext-dao.xml",
    "classpath:test-applicationContext.xml"
})
@Transactional
public class ClaimDaoIntegrationTest {

    @Autowired
    private ClaimDao claimDao;

    @Autowired
    private PolicyDao policyDao;

    @Autowired
    private SessionFactory sessionFactory;

    private Policy testPolicy;
    private Claim testClaim;

    @Before
    public void setUp() {
        // Create test policy
        testPolicy = new Policy();
        testPolicy.setPolicyNumber("POL-IT-000001");
        testPolicy.setStatus(PolicyStatus.ACTIVE);
        testPolicy.setPolicyType(PolicyType.AUTO);
        testPolicy.setHolderFirstName("Integration");
        testPolicy.setHolderLastName("Test");
        testPolicy.setEffectiveDate(daysAgo(365));
        testPolicy.setExpirationDate(daysFromNow(365));
        testPolicy.setPremiumAmount(new BigDecimal("1500.00"));
        testPolicy.setCoverageLimit(new BigDecimal("100000.00"));
        testPolicy.setDeductible(new BigDecimal("500.00"));
        policyDao.save(testPolicy);

        // Create test claim
        testClaim = new Claim();
        testClaim.setClaimNumber("CLM-IT-00001");
        testClaim.setPolicy(testPolicy);
        testClaim.setStatus(ClaimStatus.FNOL_RECEIVED);
        testClaim.setClaimType(ClaimType.COLLISION);
        testClaim.setLossDate(daysAgo(5));
        testClaim.setReportedDate(new Date());
        testClaim.setClaimantFirstName("Test");
        testClaim.setClaimantLastName("Claimant");
        testClaim.setEstimatedLoss(new BigDecimal("10000.00"));
        claimDao.save(testClaim);

        // Flush to ensure DB consistency
        sessionFactory.getCurrentSession().flush();
        sessionFactory.getCurrentSession().clear();
    }

    @After
    public void tearDown() {
        // @Transactional ensures rollback; explicit cleanup not needed
    }

    // -----------------------------------------------------------------------
    // CRUD Tests
    // -----------------------------------------------------------------------

    @Test
    public void testSaveAndFindById() {
        Claim found = claimDao.findById(testClaim.getId());
        assertNotNull("Saved claim should be retrievable by ID", found);
        assertEquals("CLM-IT-00001", found.getClaimNumber());
    }

    @Test
    public void testUpdate() {
        testClaim.setStatus(ClaimStatus.UNDER_REVIEW);
        claimDao.update(testClaim);
        sessionFactory.getCurrentSession().flush();
        sessionFactory.getCurrentSession().clear();

        Claim updated = claimDao.findById(testClaim.getId());
        assertEquals(ClaimStatus.UNDER_REVIEW, updated.getStatus());
    }

    @Test
    public void testDelete() {
        Long id = testClaim.getId();
        claimDao.delete(testClaim);
        sessionFactory.getCurrentSession().flush();
        sessionFactory.getCurrentSession().clear();

        Claim deleted = claimDao.findById(id);
        assertNull("Deleted claim should not be found", deleted);
    }

    @Test
    public void testFindAll() {
        List<Claim> all = claimDao.findAll();
        assertNotNull(all);
        assertTrue("Should find at least one claim", all.size() >= 1);
    }

    @Test
    public void testCount() {
        long count = claimDao.count();
        assertTrue("Count should be at least 1", count >= 1);
    }

    // -----------------------------------------------------------------------
    // Named Query Tests (exercises JPA @NamedQuery + HQL)
    // -----------------------------------------------------------------------

    @Test
    public void testFindByClaimNumber() {
        Claim found = claimDao.findByClaimNumber("CLM-IT-00001");
        assertNotNull("Should find claim by claim number", found);
        assertEquals(testClaim.getId(), found.getId());
    }

    @Test
    public void testFindByClaimNumber_NotFound() {
        Claim found = claimDao.findByClaimNumber("CLM-NONEXISTENT");
        assertNull("Should return null for non-existent claim number", found);
    }

    @Test
    public void testFindByStatus() {
        List<Claim> fnolClaims = claimDao.findByStatus(ClaimStatus.FNOL_RECEIVED);
        assertNotNull(fnolClaims);
        assertFalse("Should find at least one FNOL_RECEIVED claim", fnolClaims.isEmpty());

        for (Claim c : fnolClaims) {
            assertEquals("All returned claims should have FNOL_RECEIVED status",
                    ClaimStatus.FNOL_RECEIVED, c.getStatus());
        }
    }

    @Test
    public void testFindByPolicyId() {
        List<Claim> policyClaims = claimDao.findByPolicyId(testPolicy.getId());
        assertNotNull(policyClaims);
        assertEquals("Should find exactly one claim for test policy", 1, policyClaims.size());
    }

    @Test
    public void testFindOpenClaims() {
        List<Claim> openClaims = claimDao.findOpenClaims();
        assertNotNull(openClaims);
        for (Claim c : openClaims) {
            assertTrue("Open claims should not be CLOSED, DENIED, or SETTLED",
                    c.getStatus() != ClaimStatus.CLOSED &&
                    c.getStatus() != ClaimStatus.DENIED &&
                    c.getStatus() != ClaimStatus.SETTLED);
        }
    }

    // -----------------------------------------------------------------------
    // Criteria API Tests (exercises legacy Hibernate Criteria)
    // -----------------------------------------------------------------------

    @Test
    public void testFindClaimsByAdjuster() {
        testClaim.setAdjusterCode("ADJ-0001");
        claimDao.update(testClaim);
        sessionFactory.getCurrentSession().flush();
        sessionFactory.getCurrentSession().clear();

        List<Claim> adjusterClaims = claimDao.findClaimsByAdjuster("ADJ-0001");
        assertNotNull(adjusterClaims);
        assertFalse(adjusterClaims.isEmpty());
        assertEquals("ADJ-0001", adjusterClaims.get(0).getAdjusterCode());
    }

    // -----------------------------------------------------------------------
    // Fetch Join Tests (exercises N+1 prevention)
    // -----------------------------------------------------------------------

    @Test
    public void testFindClaimsWithPaymentsFetchJoin() {
        // Add a payment to the test claim
        ClaimPayment payment = new ClaimPayment();
        payment.setPaymentNumber("PAY-IT-00001");
        payment.setPaymentStatus(PaymentStatus.SCHEDULED);
        payment.setAmount(new BigDecimal("5000.00"));
        payment.setPayeeName("Test Payee");
        payment.setScheduledDate(new Date());
        testClaim.addPayment(payment);
        claimDao.update(testClaim);
        sessionFactory.getCurrentSession().flush();
        sessionFactory.getCurrentSession().clear();

        List<Claim> claimsWithPayments =
                claimDao.findClaimsWithPaymentsFetchJoin(testClaim.getId());
        assertNotNull(claimsWithPayments);
        assertFalse(claimsWithPayments.isEmpty());

        // Payments should be eagerly loaded (no LazyInitializationException)
        Claim loaded = claimsWithPayments.get(0);
        assertNotNull(loaded.getPayments());
        assertEquals("Should have one payment", 1, loaded.getPayments().size());
    }

    // -----------------------------------------------------------------------
    // Bulk Update Tests (exercises HQL UPDATE)
    // -----------------------------------------------------------------------

    @Test
    public void testFindUnscored() {
        List<Claim> unscored = claimDao.findUnscored();
        assertNotNull(unscored);
        for (Claim c : unscored) {
            assertNull("Unscored claims should have null fraudScore", c.getFraudScore());
        }
    }

    // -----------------------------------------------------------------------
    // Helpers
    // -----------------------------------------------------------------------

    private Date daysAgo(int days) {
        Calendar cal = Calendar.getInstance();
        cal.add(Calendar.DAY_OF_YEAR, -days);
        return cal.getTime();
    }

    private Date daysFromNow(int days) {
        Calendar cal = Calendar.getInstance();
        cal.add(Calendar.DAY_OF_YEAR, days);
        return cal.getTime();
    }
}
