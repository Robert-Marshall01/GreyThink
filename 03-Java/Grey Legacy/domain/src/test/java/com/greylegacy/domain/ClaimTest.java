package com.greylegacy.domain;

import junit.framework.TestCase;
import junit.framework.TestSuite;

import java.math.BigDecimal;
import java.util.Calendar;
import java.util.Date;

/**
 * JUnit 3 style test case for the Claim domain entity.
 * Demonstrates legacy testing patterns prior to annotation-based JUnit 4.
 */
public class ClaimTest extends TestCase {

    private Claim claim;
    private Policy policy;

    protected void setUp() throws Exception {
        super.setUp();

        policy = new Policy();
        policy.setId(1L);
        policy.setPolicyNumber("POL-AU-100001");
        policy.setStatus(PolicyStatus.ACTIVE);
        policy.setCoverageLimit(new BigDecimal("100000.00"));
        policy.setDeductible(new BigDecimal("500.00"));

        claim = new Claim();
        claim.setId(100L);
        claim.setClaimNumber("CLM-00005001");
        claim.setPolicy(policy);
        claim.setStatus(ClaimStatus.FNOL_RECEIVED);
        claim.setClaimType(ClaimType.COLLISION);
        claim.setLossDate(new Date());
        claim.setReportedDate(new Date());
        claim.setClaimantFirstName("Jane");
        claim.setClaimantLastName("Doe");
        claim.setClaimantPhone("555-123-4567");
        claim.setEstimatedLoss(new BigDecimal("15000.00"));
    }

    protected void tearDown() throws Exception {
        claim = null;
        policy = null;
        super.tearDown();
    }

    public static TestSuite suite() {
        return new TestSuite(ClaimTest.class);
    }

    // -----------------------------------------------------------------------
    // Basic Property Tests
    // -----------------------------------------------------------------------

    public void testClaimNumber() {
        assertEquals("CLM-00005001", claim.getClaimNumber());
    }

    public void testClaimStatus() {
        assertEquals(ClaimStatus.FNOL_RECEIVED, claim.getStatus());
    }

    public void testClaimType() {
        assertEquals(ClaimType.COLLISION, claim.getClaimType());
    }

    public void testPolicyAssociation() {
        assertNotNull(claim.getPolicy());
        assertEquals("POL-AU-100001", claim.getPolicy().getPolicyNumber());
    }

    // -----------------------------------------------------------------------
    // Business Logic Tests
    // -----------------------------------------------------------------------

    public void testIsOpen_WhenFnolReceived_ReturnsTrue() {
        claim.setStatus(ClaimStatus.FNOL_RECEIVED);
        assertTrue(claim.isOpen());
    }

    public void testIsOpen_WhenUnderReview_ReturnsTrue() {
        claim.setStatus(ClaimStatus.UNDER_REVIEW);
        assertTrue(claim.isOpen());
    }

    public void testIsOpen_WhenApproved_ReturnsTrue() {
        claim.setStatus(ClaimStatus.APPROVED);
        assertTrue(claim.isOpen());
    }

    public void testIsOpen_WhenClosed_ReturnsFalse() {
        claim.setStatus(ClaimStatus.CLOSED);
        assertFalse(claim.isOpen());
    }

    public void testIsOpen_WhenDenied_ReturnsFalse() {
        claim.setStatus(ClaimStatus.DENIED);
        assertFalse(claim.isOpen());
    }

    public void testIsOpen_WhenSettled_ReturnsFalse() {
        claim.setStatus(ClaimStatus.SETTLED);
        assertFalse(claim.isOpen());
    }

    public void testIsFraudSuspected_WhenStatusFraudSuspected() {
        claim.setStatus(ClaimStatus.FRAUD_SUSPECTED);
        assertTrue(claim.isFraudSuspected());
    }

    public void testIsFraudSuspected_WhenHighRiskLevel() {
        claim.setFraudRiskLevel(FraudRiskLevel.HIGH);
        assertTrue(claim.isFraudSuspected());
    }

    public void testIsFraudSuspected_WhenCriticalRiskLevel() {
        claim.setFraudRiskLevel(FraudRiskLevel.CRITICAL);
        assertTrue(claim.isFraudSuspected());
    }

    public void testIsFraudSuspected_WhenLowRiskLevel() {
        claim.setFraudRiskLevel(FraudRiskLevel.LOW);
        assertFalse(claim.isFraudSuspected());
    }

    // -----------------------------------------------------------------------
    // Collection Tests
    // -----------------------------------------------------------------------

    public void testAddPayment_SetsBidirectionalRelationship() {
        ClaimPayment payment = new ClaimPayment();
        payment.setPaymentNumber("PAY-00010001");
        payment.setAmount(new BigDecimal("5000.00"));

        claim.addPayment(payment);

        assertEquals(1, claim.getPayments().size());
        assertSame(claim, payment.getClaim());
    }

    public void testAddAuditEntry_SetsBidirectionalRelationship() {
        ClaimAuditEntry audit = new ClaimAuditEntry(
                claim, new Date(), "STATUS_CHANGE",
                "FNOL_RECEIVED", "UNDER_REVIEW",
                "SYSTEM", "Status changed", "127.0.0.1");

        claim.addAuditEntry(audit);

        assertEquals(1, claim.getAuditEntries().size());
    }

    public void testAddNote_SetsBidirectionalRelationship() {
        ClaimNote note = new ClaimNote();
        note.setSubject("Initial Review");
        note.setContent("Claim received and logged.");
        note.setAuthorName("adjuster1");

        claim.addNote(note);

        assertEquals(1, claim.getNotes().size());
        assertSame(claim, note.getClaim());
    }

    // -----------------------------------------------------------------------
    // Fraud Score Tests
    // -----------------------------------------------------------------------

    public void testFraudScore_InitiallyNull() {
        assertNull(claim.getFraudScore());
    }

    public void testFraudScore_SetAndGet() {
        claim.setFraudScore(75);
        assertEquals(Integer.valueOf(75), claim.getFraudScore());
    }

    public void testFraudScoredDate_SetAndGet() {
        Date now = new Date();
        claim.setFraudScoredDate(now);
        assertEquals(now, claim.getFraudScoredDate());
    }

    // -----------------------------------------------------------------------
    // Equality Tests
    // -----------------------------------------------------------------------

    public void testEquals_SameId() {
        Claim other = new Claim();
        other.setId(100L);
        assertEquals(claim, other);
    }

    public void testEquals_DifferentId() {
        Claim other = new Claim();
        other.setId(999L);
        assertFalse(claim.equals(other));
    }

    public void testEquals_NullObject() {
        assertFalse(claim.equals(null));
    }

    public void testEquals_DifferentType() {
        assertFalse(claim.equals("not a claim"));
    }
}
