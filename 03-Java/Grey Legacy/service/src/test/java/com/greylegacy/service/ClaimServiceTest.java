package com.greylegacy.service;

import com.greylegacy.dao.*;
import com.greylegacy.domain.*;
import com.greylegacy.service.impl.ClaimServiceImpl;
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
 * JUnit 4 unit test with Mockito for ClaimService.
 * Demonstrates modern legacy testing patterns:
 * - @RunWith(MockitoJUnitRunner.class) for automatic mock injection
 * - @Mock for dependency mocking
 * - @InjectMocks for subject-under-test construction
 * - verify() for interaction testing
 * - ArgumentCaptor for capturing method arguments
 * - when/thenReturn/thenThrow for stubbing
 */
@RunWith(MockitoJUnitRunner.class)
public class ClaimServiceTest {

    @Mock
    private ClaimDao claimDao;

    @Mock
    private PolicyDao policyDao;

    @Mock
    private AdjusterDao adjusterDao;

    @Mock
    private ClaimPaymentDao claimPaymentDao;

    @Mock
    private ClaimAuditDao claimAuditDao;

    @Mock
    private ClaimSnapshotDao claimSnapshotDao;

    @InjectMocks
    private ClaimServiceImpl claimService;

    private Policy activePolicy;
    private Claim existingClaim;
    private Adjuster availableAdjuster;

    @Before
    public void setUp() {
        // Set up active policy
        activePolicy = new Policy();
        activePolicy.setId(1L);
        activePolicy.setPolicyNumber("POL-AU-100001");
        activePolicy.setStatus(PolicyStatus.ACTIVE);
        activePolicy.setPolicyType(PolicyType.AUTO);
        activePolicy.setHolderFirstName("John");
        activePolicy.setHolderLastName("Smith");
        activePolicy.setCoverageLimit(new BigDecimal("100000.00"));
        activePolicy.setDeductible(new BigDecimal("500.00"));

        Calendar cal = Calendar.getInstance();
        cal.add(Calendar.YEAR, -1);
        activePolicy.setEffectiveDate(cal.getTime());
        cal.add(Calendar.YEAR, 2);
        activePolicy.setExpirationDate(cal.getTime());

        // Set up existing claim
        existingClaim = new Claim();
        existingClaim.setId(100L);
        existingClaim.setClaimNumber("CLM-00005001");
        existingClaim.setPolicy(activePolicy);
        existingClaim.setStatus(ClaimStatus.FNOL_RECEIVED);
        existingClaim.setClaimType(ClaimType.COLLISION);
        existingClaim.setLossDate(new Date());
        existingClaim.setReportedDate(new Date());
        existingClaim.setClaimantFirstName("Jane");
        existingClaim.setClaimantLastName("Doe");
        existingClaim.setEstimatedLoss(new BigDecimal("15000.00"));

        // Set up available adjuster
        availableAdjuster = new Adjuster();
        availableAdjuster.setId(10L);
        availableAdjuster.setAdjusterCode("ADJ-0001");
        availableAdjuster.setFirstName("Bob");
        availableAdjuster.setLastName("Adjuster");
        availableAdjuster.setSpecialization("AUTO");
        availableAdjuster.setActive(true);
        availableAdjuster.setMaxCaseload(50);
        availableAdjuster.setCurrentCaseload(10);
    }

    // -----------------------------------------------------------------------
    // FNOL Submission Tests
    // -----------------------------------------------------------------------

    @Test
    public void testSubmitFnol_Success() {
        FnolRequest request = createValidFnolRequest();

        when(policyDao.findByPolicyNumber("POL-AU-100001")).thenReturn(activePolicy);
        when(claimDao.save(any(Claim.class))).thenAnswer(invocation -> {
            Claim saved = invocation.getArgument(0);
            saved.setId(999L);
            return saved;
        });
        when(claimAuditDao.save(any(ClaimAuditEntry.class))).thenAnswer(invocation -> invocation.getArgument(0));
        when(claimSnapshotDao.save(any(ClaimSnapshot.class))).thenAnswer(invocation -> invocation.getArgument(0));

        Claim result = claimService.submitFnol(request);

        assertNotNull("FNOL should return a claim", result);
        assertEquals(ClaimStatus.FNOL_RECEIVED, result.getStatus());
        assertEquals("Jane", result.getClaimantFirstName());
        assertEquals("Doe", result.getClaimantLastName());
        assertNotNull("Claim number should be generated", result.getClaimNumber());

        verify(policyDao).findByPolicyNumber("POL-AU-100001");
        verify(claimDao).save(any(Claim.class));
    }

    @Test(expected = IllegalArgumentException.class)
    public void testSubmitFnol_NullPolicyNumber_ThrowsException() {
        FnolRequest request = createValidFnolRequest();
        request.setPolicyNumber(null);

        claimService.submitFnol(request);
    }

    @Test(expected = IllegalStateException.class)
    public void testSubmitFnol_PolicyNotFound_ThrowsException() {
        FnolRequest request = createValidFnolRequest();
        when(policyDao.findByPolicyNumber("POL-AU-100001")).thenReturn(null);

        claimService.submitFnol(request);
    }

    @Test(expected = IllegalStateException.class)
    public void testSubmitFnol_CancelledPolicy_ThrowsException() {
        FnolRequest request = createValidFnolRequest();
        activePolicy.setStatus(PolicyStatus.CANCELLED);
        when(policyDao.findByPolicyNumber("POL-AU-100001")).thenReturn(activePolicy);

        claimService.submitFnol(request);
    }

    // -----------------------------------------------------------------------
    // Claim Lookup Tests
    // -----------------------------------------------------------------------

    @Test
    public void testFindByClaimNumber_Success() {
        when(claimDao.findByClaimNumber("CLM-00005001")).thenReturn(existingClaim);

        Claim result = claimService.findByClaimNumber("CLM-00005001");

        assertNotNull(result);
        assertEquals("CLM-00005001", result.getClaimNumber());
        verify(claimDao).findByClaimNumber("CLM-00005001");
    }

    @Test
    public void testFindByClaimNumber_NotFound() {
        when(claimDao.findByClaimNumber("CLM-NONEXISTENT")).thenReturn(null);

        Claim result = claimService.findByClaimNumber("CLM-NONEXISTENT");

        assertNull(result);
    }

    @Test
    public void testFindOpenClaims() {
        List<Claim> openClaims = Arrays.asList(existingClaim);
        when(claimDao.findOpenClaims()).thenReturn(openClaims);

        List<Claim> result = claimService.findOpenClaims();

        assertNotNull(result);
        assertEquals(1, result.size());
        verify(claimDao).findOpenClaims();
    }

    @Test
    public void testFindClaimsByStatus() {
        List<Claim> claims = Arrays.asList(existingClaim);
        when(claimDao.findByStatus(ClaimStatus.FNOL_RECEIVED)).thenReturn(claims);

        List<Claim> result = claimService.findClaimsByStatus(ClaimStatus.FNOL_RECEIVED);

        assertNotNull(result);
        assertEquals(1, result.size());
    }

    // -----------------------------------------------------------------------
    // Claim Status Transition Tests
    // -----------------------------------------------------------------------

    @Test
    public void testApproveClaim() {
        when(claimDao.findById(100L)).thenReturn(existingClaim);
        existingClaim.setStatus(ClaimStatus.UNDER_REVIEW);

        claimService.approveClaim(100L, "manager1", new BigDecimal("12000.00"));

        assertEquals(ClaimStatus.APPROVED, existingClaim.getStatus());
        assertEquals(new BigDecimal("12000.00"), existingClaim.getApprovedAmount());
        verify(claimDao).update(existingClaim);
        verify(claimAuditDao).save(any(ClaimAuditEntry.class));
    }

    @Test
    public void testDenyClaim() {
        when(claimDao.findById(100L)).thenReturn(existingClaim);
        existingClaim.setStatus(ClaimStatus.UNDER_REVIEW);

        claimService.denyClaim(100L, "manager1", "Insufficient evidence");

        assertEquals(ClaimStatus.DENIED, existingClaim.getStatus());
        verify(claimDao).update(existingClaim);
    }

    @Test
    public void testCloseClaim() {
        when(claimDao.findById(100L)).thenReturn(existingClaim);
        existingClaim.setStatus(ClaimStatus.APPROVED);
        existingClaim.setAdjusterCode("ADJ-0001");
        when(adjusterDao.findByAdjusterCode("ADJ-0001")).thenReturn(availableAdjuster);

        claimService.closeClaim(100L, "manager1", "All payments processed");

        assertEquals(ClaimStatus.CLOSED, existingClaim.getStatus());
        assertNotNull(existingClaim.getClosedDate());
        // Adjuster caseload should be decremented
        verify(adjusterDao).findByAdjusterCode("ADJ-0001");
    }

    @Test
    public void testReopenClaim() {
        when(claimDao.findById(100L)).thenReturn(existingClaim);
        existingClaim.setStatus(ClaimStatus.CLOSED);

        claimService.reopenClaim(100L, "supervisor1", "New evidence discovered");

        assertEquals(ClaimStatus.REOPENED, existingClaim.getStatus());
        verify(claimDao).update(existingClaim);
        verify(claimAuditDao).save(any(ClaimAuditEntry.class));
    }

    // -----------------------------------------------------------------------
    // Payment Scheduling Tests
    // -----------------------------------------------------------------------

    @Test
    public void testSchedulePayment() {
        when(claimDao.findById(100L)).thenReturn(existingClaim);
        existingClaim.setStatus(ClaimStatus.APPROVED);
        existingClaim.setApprovedAmount(new BigDecimal("12000.00"));
        when(claimPaymentDao.save(any(ClaimPayment.class))).thenAnswer(invocation -> {
            ClaimPayment p = invocation.getArgument(0);
            p.setId(500L);
            return p;
        });

        PaymentRequest paymentRequest = new PaymentRequest();
        paymentRequest.setPayeeName("Jane Doe");
        paymentRequest.setAmount(new BigDecimal("5000.00"));
        paymentRequest.setPaymentMethod(PaymentMethod.CHECK);
        paymentRequest.setScheduledDate(new Date());
        paymentRequest.setApprovedBy("manager1");

        ClaimPayment result = claimService.schedulePayment(100L, paymentRequest);

        assertNotNull(result);
        assertEquals(PaymentStatus.SCHEDULED, result.getPaymentStatus());
        assertEquals(new BigDecimal("5000.00"), result.getAmount());
        verify(claimPaymentDao).save(any(ClaimPayment.class));
    }

    // -----------------------------------------------------------------------
    // Note Tests
    // -----------------------------------------------------------------------

    @Test
    public void testAddClaimNote() {
        when(claimDao.findById(100L)).thenReturn(existingClaim);

        claimService.addClaimNote(100L, "GENERAL", "Progress Update",
                "Investigation underway.", "adjuster1");

        assertEquals(1, existingClaim.getNotes().size());
        ClaimNote note = existingClaim.getNotes().get(0);
        assertEquals("Progress Update", note.getSubject());
        assertEquals("adjuster1", note.getAuthorName());
        verify(claimDao).update(existingClaim);
    }

    // -----------------------------------------------------------------------
    // Interaction Verification Tests
    // -----------------------------------------------------------------------

    @Test
    public void testSubmitFnol_CreatesAuditEntry() {
        FnolRequest request = createValidFnolRequest();
        when(policyDao.findByPolicyNumber("POL-AU-100001")).thenReturn(activePolicy);
        when(claimDao.save(any(Claim.class))).thenAnswer(invocation -> {
            Claim saved = invocation.getArgument(0);
            saved.setId(999L);
            return saved;
        });
        when(claimAuditDao.save(any(ClaimAuditEntry.class))).thenAnswer(invocation -> invocation.getArgument(0));
        when(claimSnapshotDao.save(any(ClaimSnapshot.class))).thenAnswer(invocation -> invocation.getArgument(0));

        claimService.submitFnol(request);

        // Verify audit entry and snapshot were created
        verify(claimAuditDao, atLeastOnce()).save(any(ClaimAuditEntry.class));
        verify(claimSnapshotDao, atLeastOnce()).save(any(ClaimSnapshot.class));
    }

    @Test
    public void testNoInteractionWithSnapshotDao_WhenFindingClaims() {
        when(claimDao.findByClaimNumber("CLM-00005001")).thenReturn(existingClaim);

        claimService.findByClaimNumber("CLM-00005001");

        verifyNoInteractions(claimSnapshotDao);
        verifyNoInteractions(claimPaymentDao);
    }

    // -----------------------------------------------------------------------
    // Helpers
    // -----------------------------------------------------------------------

    private FnolRequest createValidFnolRequest() {
        FnolRequest request = new FnolRequest();
        request.setPolicyNumber("POL-AU-100001");
        request.setClaimantFirstName("Jane");
        request.setClaimantLastName("Doe");
        request.setClaimantPhone("555-123-4567");
        request.setClaimantEmail("jane.doe@example.com");
        request.setClaimType(ClaimType.COLLISION);
        request.setLossDate(new Date());
        request.setLossDescription("Rear-end collision at intersection");
        request.setLossLocation("123 Main St, Springfield");
        request.setEstimatedLoss(new BigDecimal("15000.00"));
        request.setReportedBy("WEB_USER");
        return request;
    }
}
