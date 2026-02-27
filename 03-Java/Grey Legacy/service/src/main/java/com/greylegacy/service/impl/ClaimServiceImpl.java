package com.greylegacy.service.impl;

import com.greylegacy.dao.*;
import com.greylegacy.domain.*;
import com.greylegacy.service.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Isolation;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.*;

/**
 * Core claims processing service implementation.
 *
 * Transaction boundaries are managed via Spring @Transactional.
 * This class orchestrates the full claim lifecycle and demonstrates
 * long-lived business transactions, audit trail management, and
 * complex domain logic.
 */
@Service("claimService")
@Transactional(propagation = Propagation.REQUIRED, isolation = Isolation.READ_COMMITTED)
public class ClaimServiceImpl implements ClaimService {

    private static final Logger log = LoggerFactory.getLogger(ClaimServiceImpl.class);
    private static final String CLAIM_PREFIX = "CLM";
    private static final String PAYMENT_PREFIX = "PAY";

    @Autowired
    private ClaimDao claimDao;

    @Autowired
    private PolicyDao policyDao;

    @Autowired
    private AdjusterDao adjusterDao;

    @Autowired
    private ClaimPaymentDao claimPaymentDao;

    @Autowired
    private ClaimAuditDao claimAuditDao;

    @Autowired
    private ClaimSnapshotDao claimSnapshotDao;

    // ---- FNOL ----

    @Override
    @Transactional(propagation = Propagation.REQUIRES_NEW)
    public Claim submitFnol(FnolRequest request) {
        log.info("Submitting FNOL for policy: {}", request.getPolicyNumber());

        // 1. Validate policy exists
        Policy policy = policyDao.findByPolicyNumber(request.getPolicyNumber());
        if (policy == null) {
            throw new IllegalArgumentException("Policy not found: " + request.getPolicyNumber());
        }

        // 2. Validate policy is active
        if (!policy.isActive()) {
            throw new IllegalStateException("Policy is not active: " + policy.getStatus());
        }

        // 3. Validate coverage dates
        Date lossDate = request.getLossDate() != null ? request.getLossDate() : new Date();
        if (!policy.isCoverageValid(lossDate)) {
            throw new IllegalStateException("Loss date falls outside coverage period");
        }

        // 4. Create claim
        Claim claim = new Claim();
        claim.setClaimNumber(generateClaimNumber());
        claim.setClaimType(request.getClaimType());
        claim.setStatus(ClaimStatus.FNOL_RECEIVED);
        claim.setLossDate(lossDate);
        claim.setReportedDate(new Date());
        claim.setLossDescription(request.getLossDescription());
        claim.setLossLocation(request.getLossLocation());
        claim.setClaimantFirstName(request.getClaimantFirstName());
        claim.setClaimantLastName(request.getClaimantLastName());
        claim.setClaimantPhone(request.getClaimantPhone());
        claim.setClaimantEmail(request.getClaimantEmail());
        claim.setEstimatedLoss(request.getEstimatedLoss());

        // 5. Associate with policy
        policy.addClaim(claim);
        claimDao.save(claim);

        // 6. Create audit entry
        createAuditEntry(claim, "FNOL_SUBMITTED", null, ClaimStatus.FNOL_RECEIVED.name(),
                request.getReportedBy() != null ? request.getReportedBy() : "SYSTEM",
                "First Notice of Loss submitted for policy " + request.getPolicyNumber());

        // 7. Create initial snapshot
        ClaimSnapshot snapshot = new ClaimSnapshot(claim);
        claimSnapshotDao.save(snapshot);

        log.info("FNOL submitted successfully. Claim number: {}", claim.getClaimNumber());
        return claim;
    }

    // ---- ADJUDICATION ----

    @Override
    public AdjudicationResult adjudicateClaim(Long claimId) {
        log.info("Beginning adjudication for claim ID: {}", claimId);

        Claim claim = claimDao.findById(claimId);
        if (claim == null) {
            throw new IllegalArgumentException("Claim not found: " + claimId);
        }

        AdjudicationResult result = new AdjudicationResult();
        result.setClaimId(claimId);

        Policy policy = claim.getPolicy();

        // Step 1: Validate coverage
        boolean coverageValid = policy.isCoverageValid(claim.getLossDate());
        result.setCoverageValid(coverageValid);
        if (!coverageValid) {
            result.addNote("Coverage not valid for loss date: " + claim.getLossDate());
        }

        // Step 2: Check policy status
        boolean policyActive = policy.isActive();
        result.setPolicyActive(policyActive);
        if (!policyActive) {
            result.addNote("Policy is not active. Status: " + policy.getStatus());
        }

        // Step 3: Auto-deny if coverage invalid or policy inactive
        if (!coverageValid || !policyActive) {
            result.setAdjudicationStatus("DENIED");
            claim.setStatus(ClaimStatus.DENIED);
            claim.setClosedDate(new Date());
            claim.setClosedReason("Adjudication: coverage or policy validation failed");
            claimDao.update(claim);

            createAuditEntry(claim, "ADJUDICATION_DENIED", ClaimStatus.FNOL_RECEIVED.name(),
                    ClaimStatus.DENIED.name(), "SYSTEM", "Auto-denied during adjudication");

            return result;
        }

        // Step 4: Assign adjuster
        try {
            autoAssignAdjuster(claimId);
            Claim refreshed = claimDao.findById(claimId);
            result.setAdjusterAssigned(true);
            result.setAdjusterCode(refreshed.getAdjusterCode());
            result.addNote("Adjuster assigned: " + refreshed.getAdjusterCode());
        } catch (Exception e) {
            result.setAdjusterAssigned(false);
            result.addNote("No adjuster available: " + e.getMessage());
        }

        // Step 5: Calculate preliminary liability
        BigDecimal prelimLiability = calculatePreliminaryLiability(claim, policy);
        result.setPreliminaryLiability(prelimLiability);
        result.addNote("Preliminary liability: $" + prelimLiability);

        // Step 6: Update claim status
        claim.setStatus(ClaimStatus.UNDER_REVIEW);
        claimDao.update(claim);

        result.setAdjudicationStatus("PENDING_REVIEW");

        createAuditEntry(claim, "ADJUDICATION_COMPLETE", ClaimStatus.FNOL_RECEIVED.name(),
                ClaimStatus.UNDER_REVIEW.name(), "SYSTEM",
                "Adjudication complete. Preliminary liability: $" + prelimLiability);

        log.info("Adjudication complete for claim {}. Status: {}", claim.getClaimNumber(), result.getAdjudicationStatus());
        return result;
    }

    @Override
    public void assignAdjuster(Long claimId, String adjusterCode) {
        Claim claim = claimDao.findById(claimId);
        if (claim == null) throw new IllegalArgumentException("Claim not found: " + claimId);

        Adjuster adjuster = adjusterDao.findByAdjusterCode(adjusterCode);
        if (adjuster == null) throw new IllegalArgumentException("Adjuster not found: " + adjusterCode);
        if (!adjuster.isAvailable()) throw new IllegalStateException("Adjuster not available: " + adjusterCode);

        String previousAdjuster = claim.getAdjusterCode();
        claim.setAdjusterCode(adjusterCode);
        claim.setStatus(ClaimStatus.ADJUSTER_ASSIGNED);
        adjuster.setCurrentCaseload(adjuster.getCurrentCaseload() + 1);

        claimDao.update(claim);
        adjusterDao.update(adjuster);

        createAuditEntry(claim, "ADJUSTER_ASSIGNED", previousAdjuster, adjusterCode, "SYSTEM",
                "Adjuster " + adjuster.getFirstName() + " " + adjuster.getLastName() + " assigned");
    }

    @Override
    public void autoAssignAdjuster(Long claimId) {
        Claim claim = claimDao.findById(claimId);
        if (claim == null) throw new IllegalArgumentException("Claim not found: " + claimId);

        // Find adjuster by specialization matching claim type
        String specialization = mapClaimTypeToSpecialization(claim.getClaimType());
        List<Adjuster> available = adjusterDao.findBySpecialization(specialization);

        Adjuster selected = null;
        for (Adjuster adj : available) {
            if (adj.isAvailable()) {
                selected = adj;
                break;
            }
        }

        if (selected == null) {
            // Fallback: any available adjuster
            List<Adjuster> allAvailable = adjusterDao.findAvailableAdjusters();
            if (!allAvailable.isEmpty()) {
                selected = allAvailable.get(0);
            }
        }

        if (selected == null) {
            throw new IllegalStateException("No available adjusters found");
        }

        assignAdjuster(claimId, selected.getAdjusterCode());
    }

    // ---- ADJUSTMENT AND PAYOUT ----

    @Override
    public void submitAdjusterReview(Long claimId, AdjusterReviewRequest review) {
        log.info("Adjuster review submitted for claim: {}", claimId);

        Claim claim = claimDao.findById(claimId);
        if (claim == null) throw new IllegalArgumentException("Claim not found: " + claimId);

        String previousStatus = claim.getStatus().name();

        switch (review.getRecommendation()) {
            case "APPROVE":
                claim.setApprovedAmount(review.getRecommendedAmount());
                claim.setStatus(ClaimStatus.APPROVED);
                break;
            case "DENY":
                claim.setStatus(ClaimStatus.DENIED);
                claim.setClosedDate(new Date());
                claim.setClosedReason("Adjuster recommendation: DENY. " + review.getFindings());
                break;
            case "ESCALATE":
                claim.setStatus(ClaimStatus.INVESTIGATION);
                break;
            default:
                throw new IllegalArgumentException("Invalid recommendation: " + review.getRecommendation());
        }

        claimDao.update(claim);

        createAuditEntry(claim, "ADJUSTER_REVIEW", previousStatus, claim.getStatus().name(),
                review.getAdjusterCode(),
                "Recommendation: " + review.getRecommendation() + ". " + review.getFindings());

        // Snapshot the claim state after review
        ClaimSnapshot snapshot = new ClaimSnapshot(claim);
        claimSnapshotDao.save(snapshot);
    }

    @Override
    public BigDecimal calculatePayout(Long claimId) {
        Claim claim = claimDao.findById(claimId);
        if (claim == null) throw new IllegalArgumentException("Claim not found: " + claimId);

        Policy policy = claim.getPolicy();

        BigDecimal approvedAmount = claim.getApprovedAmount();
        if (approvedAmount == null) {
            throw new IllegalStateException("Claim has not been approved yet");
        }

        BigDecimal deductible = policy.getDeductible() != null ? policy.getDeductible() : BigDecimal.ZERO;
        BigDecimal coverageLimit = policy.getCoverageLimit() != null ? policy.getCoverageLimit() : approvedAmount;

        // Payout = min(approvedAmount - deductible, coverageLimit)
        BigDecimal afterDeductible = approvedAmount.subtract(deductible);
        if (afterDeductible.compareTo(BigDecimal.ZERO) < 0) {
            afterDeductible = BigDecimal.ZERO;
        }

        BigDecimal payout = afterDeductible.min(coverageLimit);

        claim.setDeductibleApplied(deductible);
        claimDao.update(claim);

        log.info("Payout calculated for claim {}: approved={}, deductible={}, payout={}",
                claim.getClaimNumber(), approvedAmount, deductible, payout);

        return payout.setScale(2, RoundingMode.HALF_UP);
    }

    @Override
    @Transactional(propagation = Propagation.REQUIRES_NEW)
    public ClaimPayment schedulePayment(Long claimId, PaymentRequest paymentRequest) {
        Claim claim = claimDao.findById(claimId);
        if (claim == null) throw new IllegalArgumentException("Claim not found: " + claimId);

        ClaimPayment payment = new ClaimPayment();
        payment.setPaymentNumber(generatePaymentNumber());
        payment.setPaymentStatus(PaymentStatus.SCHEDULED);
        payment.setPaymentMethod(paymentRequest.getPaymentMethod());
        payment.setAmount(paymentRequest.getAmount());
        payment.setScheduledDate(paymentRequest.getScheduledDate() != null ?
                paymentRequest.getScheduledDate() : new Date());
        payment.setPayeeName(paymentRequest.getPayeeName());
        payment.setPayeeAddress(paymentRequest.getPayeeAddress());
        payment.setMemo(paymentRequest.getMemo());
        payment.setApprovedBy(paymentRequest.getApprovedBy());

        claim.addPayment(payment);
        claimPaymentDao.save(payment);

        createAuditEntry(claim, "PAYMENT_SCHEDULED", null,
                "Amount: $" + paymentRequest.getAmount() + " via " + paymentRequest.getPaymentMethod(),
                paymentRequest.getApprovedBy(),
                "Payment scheduled to " + paymentRequest.getPayeeName());

        log.info("Payment {} scheduled for claim {}: ${}", payment.getPaymentNumber(),
                claim.getClaimNumber(), payment.getAmount());

        return payment;
    }

    // ---- STATUS MANAGEMENT ----

    @Override
    public void approveClaim(Long claimId, String approvedBy, BigDecimal approvedAmount) {
        Claim claim = claimDao.findById(claimId);
        if (claim == null) throw new IllegalArgumentException("Claim not found: " + claimId);

        String previousStatus = claim.getStatus().name();
        claim.setStatus(ClaimStatus.APPROVED);
        claim.setApprovedAmount(approvedAmount);
        claimDao.update(claim);

        createAuditEntry(claim, "STATUS_CHANGE", previousStatus, ClaimStatus.APPROVED.name(),
                approvedBy, "Claim approved. Amount: $" + approvedAmount);
    }

    @Override
    public void denyClaim(Long claimId, String deniedBy, String reason) {
        Claim claim = claimDao.findById(claimId);
        if (claim == null) throw new IllegalArgumentException("Claim not found: " + claimId);

        String previousStatus = claim.getStatus().name();
        claim.setStatus(ClaimStatus.DENIED);
        claim.setClosedDate(new Date());
        claim.setClosedReason(reason);
        claimDao.update(claim);

        createAuditEntry(claim, "STATUS_CHANGE", previousStatus, ClaimStatus.DENIED.name(),
                deniedBy, "Claim denied: " + reason);

        ClaimSnapshot snapshot = new ClaimSnapshot(claim);
        claimSnapshotDao.save(snapshot);
    }

    @Override
    public void closeClaim(Long claimId, String closedBy, String reason) {
        Claim claim = claimDao.findById(claimId);
        if (claim == null) throw new IllegalArgumentException("Claim not found: " + claimId);

        String previousStatus = claim.getStatus().name();
        claim.setStatus(ClaimStatus.CLOSED);
        claim.setClosedDate(new Date());
        claim.setClosedReason(reason);
        claimDao.update(claim);

        // Release adjuster caseload
        if (claim.getAdjusterCode() != null) {
            Adjuster adjuster = adjusterDao.findByAdjusterCode(claim.getAdjusterCode());
            if (adjuster != null && adjuster.getCurrentCaseload() > 0) {
                adjuster.setCurrentCaseload(adjuster.getCurrentCaseload() - 1);
                adjusterDao.update(adjuster);
            }
        }

        createAuditEntry(claim, "STATUS_CHANGE", previousStatus, ClaimStatus.CLOSED.name(),
                closedBy, "Claim closed: " + reason);

        ClaimSnapshot snapshot = new ClaimSnapshot(claim);
        claimSnapshotDao.save(snapshot);
    }

    @Override
    public void reopenClaim(Long claimId, String reopenedBy, String reason) {
        Claim claim = claimDao.findById(claimId);
        if (claim == null) throw new IllegalArgumentException("Claim not found: " + claimId);

        if (!ClaimStatus.CLOSED.equals(claim.getStatus()) && !ClaimStatus.DENIED.equals(claim.getStatus())) {
            throw new IllegalStateException("Can only reopen CLOSED or DENIED claims");
        }

        String previousStatus = claim.getStatus().name();
        claim.setStatus(ClaimStatus.REOPENED);
        claim.setClosedDate(null);
        claim.setClosedReason(null);
        claimDao.update(claim);

        createAuditEntry(claim, "STATUS_CHANGE", previousStatus, ClaimStatus.REOPENED.name(),
                reopenedBy, "Claim reopened: " + reason);
    }

    // ---- QUERIES ----

    @Override
    @Transactional(readOnly = true)
    public Claim findByClaimNumber(String claimNumber) {
        return claimDao.findByClaimNumber(claimNumber);
    }

    @Override
    @Transactional(readOnly = true)
    public List<Claim> findOpenClaims() {
        return claimDao.findOpenClaims();
    }

    @Override
    @Transactional(readOnly = true)
    public List<Claim> findClaimsByStatus(ClaimStatus status) {
        return claimDao.findByStatus(status);
    }

    @Override
    @Transactional(readOnly = true)
    public List<Claim> findClaimsByPolicy(String policyNumber) {
        Policy policy = policyDao.findByPolicyNumber(policyNumber);
        if (policy == null) return Collections.emptyList();
        return claimDao.findByPolicyId(policy.getId());
    }

    // ---- NOTES / AUDIT ----

    @Override
    public void addClaimNote(Long claimId, String noteType, String subject, String content, String author) {
        Claim claim = claimDao.findById(claimId);
        if (claim == null) throw new IllegalArgumentException("Claim not found: " + claimId);

        ClaimNote note = new ClaimNote();
        note.setNoteType(noteType);
        note.setSubject(subject);
        note.setContent(content);
        note.setAuthorName(author);
        claim.addNote(note);

        claimDao.update(claim);

        createAuditEntry(claim, "NOTE_ADDED", null, noteType + ": " + subject, author,
                "Note added to claim");
    }

    // ---- PRIVATE HELPERS ----

    private BigDecimal calculatePreliminaryLiability(Claim claim, Policy policy) {
        BigDecimal estimated = claim.getEstimatedLoss();
        if (estimated == null) return BigDecimal.ZERO;

        BigDecimal deductible = policy.getDeductible() != null ? policy.getDeductible() : BigDecimal.ZERO;
        BigDecimal coverageLimit = policy.getCoverageLimit() != null ? policy.getCoverageLimit() : estimated;

        BigDecimal liability = estimated.subtract(deductible);
        if (liability.compareTo(BigDecimal.ZERO) < 0) liability = BigDecimal.ZERO;
        if (liability.compareTo(coverageLimit) > 0) liability = coverageLimit;

        return liability.setScale(2, RoundingMode.HALF_UP);
    }

    private void createAuditEntry(Claim claim, String eventType, String previousValue,
                                   String newValue, String performedBy, String description) {
        ClaimAuditEntry audit = new ClaimAuditEntry(
                claim, new Date(), eventType, previousValue, newValue,
                performedBy, description, null);
        claim.addAuditEntry(audit);
    }

    private String generateClaimNumber() {
        return CLAIM_PREFIX + "-" + System.currentTimeMillis() + "-" + (int)(Math.random() * 1000);
    }

    private String generatePaymentNumber() {
        return PAYMENT_PREFIX + "-" + System.currentTimeMillis() + "-" + (int)(Math.random() * 1000);
    }

    private String mapClaimTypeToSpecialization(ClaimType claimType) {
        if (claimType == null) return "GENERAL";
        switch (claimType) {
            case COLLISION:
            case COMPREHENSIVE:
            case THEFT:
                return "AUTO";
            case PROPERTY_DAMAGE:
            case FIRE:
            case NATURAL_DISASTER:
                return "HOME";
            case BODILY_INJURY:
            case MEDICAL:
                return "MEDICAL";
            case LIABILITY:
                return "COMMERCIAL";
            default:
                return "GENERAL";
        }
    }
}
