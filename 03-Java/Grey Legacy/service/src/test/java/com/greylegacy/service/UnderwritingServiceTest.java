package com.greylegacy.service;

import com.greylegacy.dao.ClaimDao;
import com.greylegacy.dao.PolicyDao;
import com.greylegacy.domain.*;
import com.greylegacy.service.impl.UnderwritingServiceImpl;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

import java.math.BigDecimal;
import java.util.*;

import static org.junit.Assert.*;
import static org.mockito.Mockito.*;

/**
 * JUnit 4 unit test for UnderwritingService.
 * Tests premium recalculation risk logic and policy lifecycle.
 */
@RunWith(MockitoJUnitRunner.class)
public class UnderwritingServiceTest {

    @Mock
    private PolicyDao policyDao;

    @Mock
    private ClaimDao claimDao;

    @InjectMocks
    private UnderwritingServiceImpl underwritingService;

    private Policy activePolicy;

    @Before
    public void setUp() {
        activePolicy = new Policy();
        activePolicy.setId(1L);
        activePolicy.setPolicyNumber("POL-HM-200001");
        activePolicy.setStatus(PolicyStatus.ACTIVE);
        activePolicy.setPolicyType(PolicyType.HOME);
        activePolicy.setHolderFirstName("Alice");
        activePolicy.setHolderLastName("Johnson");
        activePolicy.setPremiumAmount(new BigDecimal("2000.00"));
        activePolicy.setCoverageLimit(new BigDecimal("250000.00"));
        activePolicy.setDeductible(new BigDecimal("1000.00"));

        Calendar cal = Calendar.getInstance();
        cal.add(Calendar.YEAR, -1);
        activePolicy.setEffectiveDate(cal.getTime());
        cal.add(Calendar.YEAR, 2);
        activePolicy.setExpirationDate(cal.getTime());
    }

    // -----------------------------------------------------------------------
    // Policy Lookup
    // -----------------------------------------------------------------------

    @Test
    public void testFindByPolicyNumber_Found() {
        when(policyDao.findByPolicyNumber("POL-HM-200001")).thenReturn(activePolicy);

        Policy result = underwritingService.findByPolicyNumber("POL-HM-200001");

        assertNotNull(result);
        assertEquals("POL-HM-200001", result.getPolicyNumber());
    }

    @Test
    public void testFindByPolicyNumber_NotFound() {
        when(policyDao.findByPolicyNumber("POL-NONEXISTENT")).thenReturn(null);

        Policy result = underwritingService.findByPolicyNumber("POL-NONEXISTENT");

        assertNull(result);
    }

    // -----------------------------------------------------------------------
    // Cancel Policy
    // -----------------------------------------------------------------------

    @Test
    public void testCancelPolicy_Success() {
        when(policyDao.findByPolicyNumber("POL-HM-200001")).thenReturn(activePolicy);

        underwritingService.cancelPolicy("POL-HM-200001", "Non-payment", "system");

        assertEquals(PolicyStatus.CANCELLED, activePolicy.getStatus());
        verify(policyDao).update(activePolicy);
    }

    @Test(expected = IllegalStateException.class)
    public void testCancelPolicy_AlreadyCancelled() {
        activePolicy.setStatus(PolicyStatus.CANCELLED);
        when(policyDao.findByPolicyNumber("POL-HM-200001")).thenReturn(activePolicy);

        underwritingService.cancelPolicy("POL-HM-200001", "Duplicate", "system");
    }

    // -----------------------------------------------------------------------
    // Suspend Policy
    // -----------------------------------------------------------------------

    @Test
    public void testSuspendPolicy_Success() {
        when(policyDao.findByPolicyNumber("POL-HM-200001")).thenReturn(activePolicy);

        underwritingService.suspendPolicy("POL-HM-200001", "Under investigation", "underwriter1");

        assertEquals(PolicyStatus.SUSPENDED, activePolicy.getStatus());
        verify(policyDao).update(activePolicy);
    }

    @Test(expected = IllegalStateException.class)
    public void testSuspendPolicy_NotActive_ThrowsException() {
        activePolicy.setStatus(PolicyStatus.EXPIRED);
        when(policyDao.findByPolicyNumber("POL-HM-200001")).thenReturn(activePolicy);

        underwritingService.suspendPolicy("POL-HM-200001", "Investigation", "underwriter1");
    }

    // -----------------------------------------------------------------------
    // Premium Recalculation (Risk Logic)
    // -----------------------------------------------------------------------

    @Test
    public void testRecalculatePremium_ZeroClaims_Discount() {
        when(policyDao.findByPolicyNumber("POL-HM-200001")).thenReturn(activePolicy);
        when(claimDao.findByPolicyId(1L)).thenReturn(Collections.emptyList());

        BigDecimal newPremium = underwritingService.recalculatePremium("POL-HM-200001");

        assertNotNull(newPremium);
        // 0 claims → 0.95x discount
        assertTrue("Premium should decrease with zero claims",
                newPremium.compareTo(activePolicy.getPremiumAmount()) < 0);
    }

    @Test
    public void testRecalculatePremium_MultipleClaims_Surcharge() {
        when(policyDao.findByPolicyNumber("POL-HM-200001")).thenReturn(activePolicy);

        // Create 3 claims
        List<Claim> claims = new ArrayList<>();
        for (int i = 0; i < 3; i++) {
            Claim claim = new Claim();
            claim.setId((long) i + 1);
            claim.setStatus(ClaimStatus.CLOSED);
            claims.add(claim);
        }
        when(claimDao.findByPolicyId(1L)).thenReturn(claims);

        BigDecimal newPremium = underwritingService.recalculatePremium("POL-HM-200001");

        assertNotNull(newPremium);
        // 3-4 claims → 1.35x surcharge
        assertTrue("Premium should increase with 3 claims",
                newPremium.compareTo(activePolicy.getPremiumAmount()) > 0);
    }

    // -----------------------------------------------------------------------
    // Find Policies for Review
    // -----------------------------------------------------------------------

    @Test
    public void testFindPoliciesForReview() {
        activePolicy.setStatus(PolicyStatus.PENDING_REVIEW);
        when(policyDao.findByStatus(PolicyStatus.PENDING_REVIEW))
                .thenReturn(Collections.singletonList(activePolicy));

        List<Policy> result = underwritingService.findPoliciesForReview();

        assertNotNull(result);
        assertEquals(1, result.size());
        assertEquals(PolicyStatus.PENDING_REVIEW, result.get(0).getStatus());
    }
}
