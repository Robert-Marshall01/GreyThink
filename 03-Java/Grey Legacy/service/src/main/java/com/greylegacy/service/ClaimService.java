package com.greylegacy.service;

import com.greylegacy.domain.*;
import java.math.BigDecimal;
import java.util.Date;
import java.util.List;

/**
 * Core claims processing service.
 * Handles the full claim lifecycle: FNOL → adjudication → adjustment → payout → close.
 */
public interface ClaimService {

    // FNOL - First Notice of Loss
    Claim submitFnol(FnolRequest request);

    // Adjudication workflow
    AdjudicationResult adjudicateClaim(Long claimId);
    void assignAdjuster(Long claimId, String adjusterCode);
    void autoAssignAdjuster(Long claimId);

    // Adjustment and payout
    void submitAdjusterReview(Long claimId, AdjusterReviewRequest review);
    BigDecimal calculatePayout(Long claimId);
    ClaimPayment schedulePayment(Long claimId, PaymentRequest paymentRequest);

    // Status management
    void approveClaim(Long claimId, String approvedBy, BigDecimal approvedAmount);
    void denyClaim(Long claimId, String deniedBy, String reason);
    void closeClaim(Long claimId, String closedBy, String reason);
    void reopenClaim(Long claimId, String reopenedBy, String reason);

    // Queries
    Claim findByClaimNumber(String claimNumber);
    List<Claim> findOpenClaims();
    List<Claim> findClaimsByStatus(ClaimStatus status);
    List<Claim> findClaimsByPolicy(String policyNumber);

    // Audit
    void addClaimNote(Long claimId, String noteType, String subject, String content, String author);
}
