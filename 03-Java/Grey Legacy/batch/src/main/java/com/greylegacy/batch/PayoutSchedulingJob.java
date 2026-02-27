package com.greylegacy.batch;

import com.greylegacy.dao.ClaimDao;
import com.greylegacy.dao.ClaimPaymentDao;
import com.greylegacy.domain.Claim;
import com.greylegacy.domain.ClaimAuditEntry;
import com.greylegacy.domain.ClaimPayment;
import com.greylegacy.domain.PaymentStatus;
import org.quartz.Job;
import org.quartz.JobExecutionContext;
import org.quartz.JobExecutionException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import java.util.Date;
import java.util.List;

/**
 * Nightly batch job that processes scheduled payments.
 *
 * Finds all payments with status {@link PaymentStatus#SCHEDULED} and a
 * scheduledDate on or before today, transitions them through PROCESSING,
 * and marks them COMPLETED or FAILED based on processing outcome.
 *
 * Session management: flushes every 50 records to manage memory.
 *
 * Runs nightly at 3:00 AM via Quartz scheduler.
 */
@Component("payoutSchedulingJob")
public class PayoutSchedulingJob implements Job {

    private static final Logger log = LoggerFactory.getLogger(PayoutSchedulingJob.class);

    /** Flush interval to manage Hibernate session memory. */
    private static final int FLUSH_INTERVAL = 50;

    @Autowired
    private ClaimPaymentDao claimPaymentDao;

    @Autowired
    private ClaimDao claimDao;

    @Override
    @Transactional
    public void execute(JobExecutionContext context) throws JobExecutionException {
        log.info("=== PAYOUT SCHEDULING JOB STARTED ===");
        long startTime = System.currentTimeMillis();

        int totalProcessed = 0;
        int successCount = 0;
        int failureCount = 0;

        try {
            Date today = new Date();
            List<ClaimPayment> duePayments = claimPaymentDao.findScheduledPaymentsDue(today);
            log.info("Found {} scheduled payments due for processing", duePayments.size());

            if (duePayments.isEmpty()) {
                log.info("No scheduled payments due. Job complete.");
                return;
            }

            for (ClaimPayment payment : duePayments) {
                try {
                    // Transition to PROCESSING
                    payment.setPaymentStatus(PaymentStatus.PROCESSING);
                    claimPaymentDao.update(payment);

                    log.debug("Processing payment {} (amount={}, method={}, payee={})",
                            payment.getPaymentNumber(),
                            payment.getAmount(),
                            payment.getPaymentMethod(),
                            payment.getPayeeName());

                    // Simulate payment processing (ACH/wire/check)
                    boolean success = processPayment(payment);

                    if (success) {
                        payment.setPaymentStatus(PaymentStatus.COMPLETED);
                        payment.setProcessedDate(new Date());
                        claimPaymentDao.update(payment);

                        // Create success audit entry on the parent claim
                        createPaymentAuditEntry(payment, "PAYMENT_COMPLETED",
                                "Payment " + payment.getPaymentNumber()
                                        + " completed. Amount: " + payment.getAmount());

                        successCount++;
                        log.debug("Payment {} completed successfully", payment.getPaymentNumber());
                    } else {
                        payment.setPaymentStatus(PaymentStatus.FAILED);
                        claimPaymentDao.update(payment);

                        // Create failure audit entry on the parent claim
                        createPaymentAuditEntry(payment, "PAYMENT_FAILED",
                                "Payment " + payment.getPaymentNumber()
                                        + " failed during processing. Amount: " + payment.getAmount());

                        failureCount++;
                        log.warn("Payment {} failed during processing", payment.getPaymentNumber());
                    }

                    totalProcessed++;

                    // Flush periodically to manage session memory
                    if (totalProcessed % FLUSH_INTERVAL == 0) {
                        claimPaymentDao.flush();
                        log.debug("Flushed session after {} payments", totalProcessed);
                    }

                } catch (Exception e) {
                    log.error("Error processing payment {}: {}",
                            payment.getPaymentNumber(), e.getMessage(), e);

                    // Mark as FAILED on unexpected error
                    try {
                        payment.setPaymentStatus(PaymentStatus.FAILED);
                        claimPaymentDao.update(payment);

                        createPaymentAuditEntry(payment, "PAYMENT_FAILED",
                                "Payment " + payment.getPaymentNumber()
                                        + " failed with error: " + e.getMessage());
                    } catch (Exception inner) {
                        log.error("Failed to mark payment {} as FAILED",
                                payment.getPaymentNumber(), inner);
                    }

                    failureCount++;
                    totalProcessed++;
                }
            }

            // Final flush
            claimPaymentDao.flush();

        } catch (Exception e) {
            log.error("Payout scheduling job failed", e);
            throw new JobExecutionException("Payout scheduling job failed", e);
        }

        long duration = System.currentTimeMillis() - startTime;
        log.info("=== PAYOUT SCHEDULING JOB COMPLETED in {}ms ===", duration);
        log.info("Processed {} payments: {} completed, {} failed",
                totalProcessed, successCount, failureCount);
    }

    /**
     * Simulates payment processing. In production this would integrate with
     * a payment gateway, ACH processor, or check printing service.
     *
     * @param payment the payment to process
     * @return true if processing succeeded, false otherwise
     */
    private boolean processPayment(ClaimPayment payment) {
        // Simulate processing delay and occasional failures
        // In production, this would call an external payment service
        try {
            // Basic validation
            if (payment.getAmount() == null
                    || payment.getAmount().signum() <= 0) {
                log.warn("Payment {} has invalid amount: {}",
                        payment.getPaymentNumber(), payment.getAmount());
                return false;
            }

            if (payment.getPayeeName() == null
                    || payment.getPayeeName().trim().isEmpty()) {
                log.warn("Payment {} has no payee name", payment.getPaymentNumber());
                return false;
            }

            // Simulate successful processing
            return true;

        } catch (Exception e) {
            log.error("Payment processing error for {}", payment.getPaymentNumber(), e);
            return false;
        }
    }

    /**
     * Creates an audit entry on the parent claim for a payment event.
     */
    private void createPaymentAuditEntry(ClaimPayment payment, String eventType, String description) {
        Claim claim = payment.getClaim();
        if (claim != null) {
            ClaimAuditEntry audit = new ClaimAuditEntry(
                    claim, new Date(), eventType,
                    PaymentStatus.SCHEDULED.name(),
                    payment.getPaymentStatus().name(),
                    "BATCH_PAYOUT_JOB", description, null);
            claim.addAuditEntry(audit);
            claimDao.update(claim);
        }
    }
}
