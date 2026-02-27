package com.greylegacy.domain;

import junit.framework.TestCase;
import junit.framework.TestSuite;

import java.math.BigDecimal;
import java.util.Calendar;
import java.util.Date;

/**
 * JUnit 3 style test case for the Policy domain entity.
 * Demonstrates legacy testing patterns: extending TestCase,
 * setUp/tearDown lifecycle, and TestSuite construction.
 *
 * JUnit 3 was standard in Java 1.4 / early Java 5 codebases.
 * Many legacy insurance systems still use this pattern.
 */
public class PolicyTest extends TestCase {

    private Policy policy;
    private Date today;
    private Date oneYearAgo;
    private Date oneYearFromNow;

    /**
     * JUnit 3 setUp — called before each test method.
     * No @Before annotation — lifecycle driven by method name.
     */
    protected void setUp() throws Exception {
        super.setUp();

        Calendar cal = Calendar.getInstance();
        today = cal.getTime();

        cal.add(Calendar.YEAR, -1);
        oneYearAgo = cal.getTime();

        cal.add(Calendar.YEAR, 2);
        oneYearFromNow = cal.getTime();

        policy = new Policy();
        policy.setId(1L);
        policy.setPolicyNumber("POL-AU-100001");
        policy.setStatus(PolicyStatus.ACTIVE);
        policy.setPolicyType(PolicyType.AUTO);
        policy.setHolderFirstName("John");
        policy.setHolderLastName("Smith");
        policy.setHolderSsn("123-45-6789");
        policy.setHolderEmail("john.smith@example.com");
        policy.setEffectiveDate(oneYearAgo);
        policy.setExpirationDate(oneYearFromNow);
        policy.setPremiumAmount(new BigDecimal("1200.00"));
        policy.setCoverageLimit(new BigDecimal("100000.00"));
        policy.setDeductible(new BigDecimal("500.00"));
        policy.setAgentCode("AGT-001");
    }

    /**
     * JUnit 3 tearDown — called after each test method.
     */
    protected void tearDown() throws Exception {
        policy = null;
        super.tearDown();
    }

    /**
     * JUnit 3 pattern: static suite() method for TestRunner.
     */
    public static TestSuite suite() {
        return new TestSuite(PolicyTest.class);
    }

    // -----------------------------------------------------------------------
    // Basic Property Tests
    // -----------------------------------------------------------------------

    public void testPolicyNumberNotNull() {
        assertNotNull("Policy number should not be null", policy.getPolicyNumber());
        assertEquals("POL-AU-100001", policy.getPolicyNumber());
    }

    public void testPolicyStatus() {
        assertEquals(PolicyStatus.ACTIVE, policy.getStatus());
    }

    public void testPolicyType() {
        assertEquals(PolicyType.AUTO, policy.getPolicyType());
    }

    public void testHolderName() {
        assertEquals("John", policy.getHolderFirstName());
        assertEquals("Smith", policy.getHolderLastName());
    }

    public void testPremiumAmount() {
        assertEquals(new BigDecimal("1200.00"), policy.getPremiumAmount());
    }

    // -----------------------------------------------------------------------
    // Business Logic Tests
    // -----------------------------------------------------------------------

    public void testIsActive_WhenStatusActive_ReturnsTrue() {
        policy.setStatus(PolicyStatus.ACTIVE);
        assertTrue("Active policy should return true for isActive()", policy.isActive());
    }

    public void testIsActive_WhenStatusCancelled_ReturnsFalse() {
        policy.setStatus(PolicyStatus.CANCELLED);
        assertFalse("Cancelled policy should return false for isActive()", policy.isActive());
    }

    public void testIsActive_WhenStatusExpired_ReturnsFalse() {
        policy.setStatus(PolicyStatus.EXPIRED);
        assertFalse("Expired policy should return false for isActive()", policy.isActive());
    }

    public void testIsActive_WhenStatusSuspended_ReturnsFalse() {
        policy.setStatus(PolicyStatus.SUSPENDED);
        assertFalse(policy.isActive());
    }

    public void testIsCoverageValid_WhenActiveAndWithinDates_ReturnsTrue() {
        assertTrue("Coverage should be valid for today", policy.isCoverageValid(today));
    }

    public void testIsCoverageValid_WhenCancelled_ReturnsFalse() {
        policy.setStatus(PolicyStatus.CANCELLED);
        assertFalse("Cancelled policy should have invalid coverage", policy.isCoverageValid(today));
    }

    public void testIsCoverageValid_WhenDateBeforeEffective_ReturnsFalse() {
        Calendar cal = Calendar.getInstance();
        cal.add(Calendar.YEAR, -2);
        Date twoYearsAgo = cal.getTime();
        assertFalse("Coverage should be invalid before effective date",
                policy.isCoverageValid(twoYearsAgo));
    }

    public void testIsCoverageValid_WhenDateAfterExpiration_ReturnsFalse() {
        Calendar cal = Calendar.getInstance();
        cal.add(Calendar.YEAR, 3);
        Date threeYearsFromNow = cal.getTime();
        assertFalse("Coverage should be invalid after expiration date",
                policy.isCoverageValid(threeYearsFromNow));
    }

    // -----------------------------------------------------------------------
    // Collection / Association Tests
    // -----------------------------------------------------------------------

    public void testAddClaim_SetsBidirectionalRelationship() {
        Claim claim = new Claim();
        claim.setClaimNumber("CLM-00000001");
        claim.setStatus(ClaimStatus.FNOL_RECEIVED);

        policy.addClaim(claim);

        assertEquals("Policy should have one claim", 1, policy.getClaims().size());
        assertSame("Claim's policy reference should point back to this policy",
                policy, claim.getPolicy());
    }

    public void testRemoveClaim_ClearsBidirectionalRelationship() {
        Claim claim = new Claim();
        claim.setClaimNumber("CLM-00000001");
        policy.addClaim(claim);

        policy.removeClaim(claim);

        assertEquals("Policy should have no claims", 0, policy.getClaims().size());
        assertNull("Claim's policy reference should be null after removal", claim.getPolicy());
    }

    public void testAddEndorsement_SetsBidirectionalRelationship() {
        PolicyEndorsement endorsement = new PolicyEndorsement();
        endorsement.setEndorsementNumber("END-001");
        endorsement.setEndorsementType("PREMIUM_ADJUSTMENT");
        endorsement.setPremiumAdjustment(new BigDecimal("100.00"));

        policy.addEndorsement(endorsement);

        assertEquals(1, policy.getEndorsements().size());
        assertSame(policy, endorsement.getPolicy());
    }

    // -----------------------------------------------------------------------
    // BaseEntity (inherited) Tests
    // -----------------------------------------------------------------------

    public void testEquals_SameId_ReturnsTrue() {
        Policy other = new Policy();
        other.setId(1L);
        assertEquals("Policies with same ID should be equal", policy, other);
    }

    public void testEquals_DifferentId_ReturnsFalse() {
        Policy other = new Policy();
        other.setId(999L);
        assertFalse("Policies with different IDs should not be equal",
                policy.equals(other));
    }

    public void testEquals_NullId_ReturnsFalse() {
        Policy other = new Policy();
        other.setId(null);
        assertFalse("Policy with null ID should not equal policy with ID",
                policy.equals(other));
    }

    public void testHashCode_ConsistentWithEquals() {
        Policy other = new Policy();
        other.setId(1L);
        assertEquals("Equal policies must have same hashCode",
                policy.hashCode(), other.hashCode());
    }

    public void testToString_ContainsPolicyNumber() {
        String str = policy.toString();
        assertNotNull(str);
        assertTrue("toString should contain policy number",
                str.contains("POL-AU-100001"));
    }
}
