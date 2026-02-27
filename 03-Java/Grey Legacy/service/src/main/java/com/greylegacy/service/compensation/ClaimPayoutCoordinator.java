package com.greylegacy.service.compensation;

import com.greylegacy.dao.ClaimDao;
import com.greylegacy.domain.Claim;
import com.greylegacy.service.ClaimService;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.util.Date;
import java.util.List;

/**
 * Coordinates multi-step claim payout across systems using the
 * <b>Compensating Transaction</b> pattern (a.k.a. Saga, orchestration variant).
 *
 * <p>Insurance claim payouts involve multiple systems that cannot participate
 * in a single ACID transaction:</p>
 * <ol>
 *   <li>Database: Update claim reserve and status</li>
 *   <li>Payment Gateway: Schedule payment to claimant</li>
 *   <li>JMS: Send notification to claimant and internal audit queue</li>
 *   <li>SOAP: Report payout to reinsurance partner</li>
 * </ol>
 *
 * <p>If any step fails, preceding steps must be undone via compensating
 * actions (reverse operations). This is NOT a distributed XA transaction —
 * each step runs in its own local transaction. Compensations are
 * <b>best-effort</b>; if a compensation itself fails, the failure is
 * logged to the {@code COMPENSATION_AUDIT} table for manual resolution.</p>
 *
 * <h3>Why not XA/2PC?</h3>
 * <ul>
 *   <li>External SOAP endpoints don't support XA</li>
 *   <li>Payment gateways use HTTP APIs, not JTA resources</li>
 *   <li>XA coordination overhead is unacceptable for high-volume claims</li>
 *   <li>2PC has the "2PC doubt" problem — coordinator failure leaves
 *       resources locked indefinitely</li>
 * </ul>
 *
 * <h3>Transaction Semantics:</h3>
 * <p>Each step uses {@code REQUIRES_NEW} to isolate its DB changes.
 * This ensures that a failure in step N doesn't roll back the record
 * of steps 1..(N-1) completing, which is critical for the compensation
 * chain to know what needs to be undone.</p>
 *
 * @author Grey Legacy Architecture Team
 * @see CompensationChain
 */
public class ClaimPayoutCoordinator {

    private static final Logger log = LoggerFactory.getLogger(ClaimPayoutCoordinator.class);

    // -- Dependencies (injected via Spring XML applicationContext-service.xml) --

    private ClaimDao claimDao;
    private ClaimService claimService;

    // These would be real service references in production.
    // Declared as interfaces for testability.
    private ReserveService reserveService;
    private PaymentGatewayClient paymentGateway;
    private ClaimNotificationSender notificationSender;
    private ReinsurerSoapClient reinsurerClient;
    private CompensationAuditService auditService;

    /**
     * Executes a claim payout with full compensation support.
     *
     * <p>Each step registers a compensating action with the chain.
     * If any step throws an exception, all compensations are executed
     * in reverse order.</p>
     *
     * @param claimId the claim to pay out
     * @param amount  the payout amount
     * @return result containing success/failure status and any compensation failures
     * @throws IllegalArgumentException if claim not found or not in payable state
     */
    @Transactional(propagation = Propagation.REQUIRES_NEW,
                   rollbackFor = Exception.class)
    public PayoutResult executePayout(Long claimId, BigDecimal amount) {

        log.info("Initiating payout of {} for claim ID {}", amount, claimId);

        // Validate preconditions
        Claim claim = claimDao.findById(claimId);
        if (claim == null) {
            throw new IllegalArgumentException("Claim not found: " + claimId);
        }
        if (!"APPROVED".equals(claim.getStatus())) {
            throw new IllegalStateException(
                "Claim " + claim.getClaimNumber() + " is not in APPROVED status; " +
                "current status: " + claim.getStatus());
        }
        if (amount.compareTo(BigDecimal.ZERO) <= 0) {
            throw new IllegalArgumentException("Payout amount must be positive");
        }

        CompensationChain compensationChain = new CompensationChain();
        String previousStatus = claim.getStatus();

        try {
            // =================================================================
            // Step 1: Reserve funds against the claim's coverage
            // =================================================================
            log.info("Step 1: Reserving {} against claim {}", amount, claim.getClaimNumber());
            reserveService.reserveFunds(claim, amount);
            compensationChain.register(
                "Release reserved funds: " + amount,
                () -> reserveService.releaseFunds(claim, amount)
            );

            // =================================================================
            // Step 2: Update claim status to PROCESSING
            // =================================================================
            log.info("Step 2: Updating claim status to PROCESSING");
            claim.setStatus("PROCESSING");
            claim.setUpdatedBy("PAYOUT_COORDINATOR");
            claim.setUpdatedDate(new Date());
            claimDao.update(claim);
            compensationChain.register(
                "Revert claim status to " + previousStatus,
                () -> {
                    Claim freshClaim = claimDao.findById(claimId);
                    freshClaim.setStatus(previousStatus);
                    freshClaim.setUpdatedBy("COMPENSATION");
                    freshClaim.setUpdatedDate(new Date());
                    claimDao.update(freshClaim);
                }
            );

            // =================================================================
            // Step 3: Schedule payment via external payment gateway
            //
            // NOTE: This call leaves the local transaction boundary and hits an
            // external HTTP/SOAP service. If this succeeds but a later step
            // fails, we must call cancelPayment() to reverse the scheduled
            // payment before it is disbursed.
            // =================================================================
            log.info("Step 3: Scheduling payment of {} via payment gateway", amount);
            PaymentConfirmation confirmation = paymentGateway.schedulePayment(
                claim.getClaimNumber(),
                amount,
                claim.getClaimantFirstName() + " " + claim.getClaimantLastName()
            );
            log.info("Payment scheduled: txn={}", confirmation.getTransactionId());
            compensationChain.register(
                "Cancel payment: txn=" + confirmation.getTransactionId(),
                () -> paymentGateway.cancelPayment(confirmation.getTransactionId())
            );

            // =================================================================
            // Step 4: Send notification via JMS
            //
            // Notification is less critical — if it fails, the payout still
            // happened. But we still register a compensation to send a
            // correction notice if a LATER step causes full rollback.
            // =================================================================
            log.info("Step 4: Sending payout notification for claim {}", claim.getClaimNumber());
            notificationSender.sendPayoutNotification(claim, amount);
            compensationChain.register(
                "Send correction notice for claim " + claim.getClaimNumber(),
                () -> notificationSender.sendCorrectionNotice(claim,
                    "Previous payout notification of " + amount + " has been reversed. " +
                    "Please disregard the earlier notice.")
            );

            // =================================================================
            // Step 5: Report payout to reinsurance partner via SOAP
            //
            // SOAP call to the reinsurer's endpoint. The reinsurer's system
            // accepts correction reports, so compensation is a correction
            // rather than a cancellation.
            // =================================================================
            log.info("Step 5: Reporting payout to reinsurer for claim {}", claim.getClaimNumber());
            reinsurerClient.reportPayout(claim.getClaimNumber(), amount);
            // Reinsurer accepts corrections via a separate endpoint;
            // compensation is a correction report.
            compensationChain.register(
                "Send correction report to reinsurer for claim " + claim.getClaimNumber(),
                () -> reinsurerClient.reportPayoutCorrection(
                    claim.getClaimNumber(), amount, "Payout reversed due to processing failure")
            );

            // =================================================================
            // Step 6: Update final status
            // =================================================================
            log.info("Step 6: Marking claim as PAID");
            claim.setStatus("PAID");
            claim.setUpdatedBy("PAYOUT_COORDINATOR");
            claim.setUpdatedDate(new Date());
            claimDao.update(claim);
            // No compensation needed — earlier status revert handles this

            // =================================================================
            // All steps succeeded — discard compensation chain
            // =================================================================
            compensationChain.clear();
            log.info("Payout completed successfully for claim {} (txn={})",
                     claim.getClaimNumber(), confirmation.getTransactionId());

            // Record successful payout in audit trail
            auditService.recordPayoutSuccess(claimId, amount,
                confirmation.getTransactionId());

            return PayoutResult.success(confirmation);

        } catch (Exception e) {
            log.error("Payout FAILED for claim {} at step {}: {}",
                      claim.getClaimNumber(),
                      compensationChain.getStepsCompleted(),
                      e.getMessage(), e);

            // Execute compensations in reverse order
            List<String> compensationFailures = compensationChain.compensate();

            // Record the failure and any compensation issues
            auditService.recordPayoutFailure(claimId, amount,
                e.getMessage(), compensationFailures);

            if (!compensationFailures.isEmpty()) {
                // Some compensations failed — alert operations team
                log.error("CRITICAL: {} compensation(s) FAILED for claim {}. " +
                          "Manual intervention required. Failures: {}",
                          compensationFailures.size(),
                          claim.getClaimNumber(),
                          compensationFailures);
                // In production, this would page the on-call engineer
            }

            return PayoutResult.failed(e.getMessage(), compensationFailures);
        }
    }


    // =========================================================================
    // Inner interfaces — defined here for clarity; in production these would
    // be separate interface files in the service package.
    // =========================================================================

    /**
     * Manages fund reservation against claim coverage limits.
     */
    public interface ReserveService {
        void reserveFunds(Claim claim, BigDecimal amount);
        void releaseFunds(Claim claim, BigDecimal amount);
    }

    /**
     * Client for the external payment gateway (HTTP/SOAP).
     */
    public interface PaymentGatewayClient {
        PaymentConfirmation schedulePayment(String claimNumber, BigDecimal amount, String payeeName);
        void cancelPayment(String transactionId);
    }

    /**
     * Sends claim-related notifications via JMS.
     */
    public interface ClaimNotificationSender {
        void sendPayoutNotification(Claim claim, BigDecimal amount);
        void sendCorrectionNotice(Claim claim, String message);
    }

    /**
     * SOAP client for the reinsurance partner system.
     */
    public interface ReinsurerSoapClient {
        void reportPayout(String claimNumber, BigDecimal amount);
        void reportPayoutCorrection(String claimNumber, BigDecimal amount, String reason);
    }

    /**
     * Records payout and compensation events in the audit trail.
     */
    public interface CompensationAuditService {
        void recordPayoutSuccess(Long claimId, BigDecimal amount, String transactionId);
        void recordPayoutFailure(Long claimId, BigDecimal amount,
                                  String errorMessage, List<String> compensationFailures);
        void recordCompensationFailure(Long claimId, List<String> failures, String errorMessage);
    }


    // =========================================================================
    // Result object
    // =========================================================================

    /**
     * Immutable result of a payout execution.
     */
    public static class PayoutResult {
        private final boolean success;
        private final String message;
        private final PaymentConfirmation confirmation;
        private final List<String> compensationFailures;

        private PayoutResult(boolean success, String message,
                             PaymentConfirmation confirmation,
                             List<String> compensationFailures) {
            this.success = success;
            this.message = message;
            this.confirmation = confirmation;
            this.compensationFailures = compensationFailures;
        }

        public static PayoutResult success(PaymentConfirmation confirmation) {
            return new PayoutResult(true, "Payout completed successfully",
                                   confirmation, java.util.Collections.emptyList());
        }

        public static PayoutResult failed(String message, List<String> compensationFailures) {
            return new PayoutResult(false, message, null, compensationFailures);
        }

        public boolean isSuccess() { return success; }
        public String getMessage() { return message; }
        public PaymentConfirmation getConfirmation() { return confirmation; }
        public List<String> getCompensationFailures() { return compensationFailures; }
        public boolean hasCompensationFailures() {
            return compensationFailures != null && !compensationFailures.isEmpty();
        }
    }

    /**
     * Confirmation from the payment gateway.
     */
    public static class PaymentConfirmation {
        private final String transactionId;
        private final String status;

        public PaymentConfirmation(String transactionId, String status) {
            this.transactionId = transactionId;
            this.status = status;
        }

        public String getTransactionId() { return transactionId; }
        public String getStatus() { return status; }
    }


    // =========================================================================
    // Setters for Spring XML injection
    // =========================================================================

    public void setClaimDao(ClaimDao claimDao) {
        this.claimDao = claimDao;
    }

    public void setClaimService(ClaimService claimService) {
        this.claimService = claimService;
    }

    public void setReserveService(ReserveService reserveService) {
        this.reserveService = reserveService;
    }

    public void setPaymentGateway(PaymentGatewayClient paymentGateway) {
        this.paymentGateway = paymentGateway;
    }

    public void setNotificationSender(ClaimNotificationSender notificationSender) {
        this.notificationSender = notificationSender;
    }

    public void setReinsurerClient(ReinsurerSoapClient reinsurerClient) {
        this.reinsurerClient = reinsurerClient;
    }

    public void setAuditService(CompensationAuditService auditService) {
        this.auditService = auditService;
    }
}
