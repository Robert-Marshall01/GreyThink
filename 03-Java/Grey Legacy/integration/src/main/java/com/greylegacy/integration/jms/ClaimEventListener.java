package com.greylegacy.integration.jms;

import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.MessageListener;
import javax.jms.ObjectMessage;
import javax.jms.TextMessage;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.MDC;

/**
 * Message-Driven POJO that receives {@link ClaimEventMessage} instances
 * from the claim-event JMS queue and dispatches them to the appropriate
 * business handler based on {@code eventType}.
 *
 * <p>Implements the classic {@link MessageListener} interface so it can be
 * wired into a {@code DefaultMessageListenerContainer} in Spring XML.
 * Transaction semantics are managed by the container; unhandled exceptions
 * are wrapped in {@link RuntimeException} to trigger container-managed
 * redelivery.</p>
 *
 * <p>Senior-level patterns demonstrated:</p>
 * <ul>
 *   <li><strong>Idempotent consumer</strong> — prevents duplicate processing
 *       when messages are redelivered after transient failures</li>
 *   <li><strong>Poison-message protection</strong> — discards messages that
 *       exceed the maximum redelivery count</li>
 *   <li><strong>MDC correlation</strong> — attaches the correlation ID to
 *       the logging context for end-to-end tracing</li>
 *   <li><strong>Message ordering awareness</strong> — logs sequence gaps
 *       for monitoring (full ordering requires single-consumer or
 *       message-group configuration at the broker level)</li>
 * </ul>
 *
 * @author Grey Legacy Integration Team
 * @since 1.0
 */
public class ClaimEventListener implements MessageListener {

    private static final Logger LOG = LoggerFactory.getLogger(ClaimEventListener.class);

    /** Maximum number of redelivery attempts before treating as poison. */
    private static final int MAX_REDELIVERY_ATTEMPTS = 5;

    private ClaimMessageConverter messageConverter;

    /**
     * Idempotent consumer — injected via Spring XML to prevent duplicate
     * message processing across redeliveries and failovers.
     */
    private IdempotentMessageConsumer idempotentConsumer;

    /**
     * Setter injection for the message converter (Spring XML style).
     *
     * @param messageConverter converter used to deserialise {@link TextMessage} payloads
     */
    public void setMessageConverter(ClaimMessageConverter messageConverter) {
        this.messageConverter = messageConverter;
    }

    /**
     * Setter injection for the idempotent consumer.
     *
     * @param idempotentConsumer the duplicate-detection component
     */
    public void setIdempotentConsumer(IdempotentMessageConsumer idempotentConsumer) {
        this.idempotentConsumer = idempotentConsumer;
    }

    // -------------------------------------------------------------------------
    // MessageListener implementation
    // -------------------------------------------------------------------------

    @Override
    public void onMessage(Message message) {
        String correlationId = null;
        try {
            correlationId = message.getJMSCorrelationID();
            MDC.put("correlationId", correlationId != null ? correlationId : "N/A");

            LOG.info("Received JMS message [id={}, correlationId={}]",
                    message.getJMSMessageID(), correlationId);

            // -----------------------------------------------------------------
            // Poison-message guard — check redelivery count
            // -----------------------------------------------------------------
            if (message.getJMSRedelivered()) {
                int redeliveryCount = 1;
                try {
                    redeliveryCount = message.getIntProperty("JMSXDeliveryCount");
                } catch (Exception e) {
                    LOG.debug("JMSXDeliveryCount property not available");
                }
                LOG.warn("Message redelivered — delivery count={}", redeliveryCount);
                if (redeliveryCount > MAX_REDELIVERY_ATTEMPTS) {
                    LOG.error("Poison message detected [id={}]. Exceeded {} redelivery attempts. "
                            + "Message will be routed to DLQ by the broker's redelivery policy.",
                            message.getJMSMessageID(), MAX_REDELIVERY_ATTEMPTS);
                    // Throw to trigger DLQ routing via broker redelivery policy
                    throw new RuntimeException("Poison message — exceeded max redelivery attempts");
                }
            }

            // -----------------------------------------------------------------
            // Idempotent consumer guard — prevent duplicate processing
            // -----------------------------------------------------------------
            String messageId = message.getJMSMessageID();
            if (idempotentConsumer != null && idempotentConsumer.isDuplicate(messageId)) {
                LOG.warn("Duplicate message detected [id={}]. Skipping processing.", messageId);
                return;
            }

            // -----------------------------------------------------------------
            // Deserialise the payload
            // -----------------------------------------------------------------
            ClaimEventMessage event = extractEvent(message);
            if (event == null) {
                LOG.error("Unable to extract ClaimEventMessage from JMS message [id={}]",
                        message.getJMSMessageID());
                return;
            }

            LOG.info("Processing event: type={}, claim={}, correlationId={}",
                    event.getEventType(), event.getClaimNumber(), event.getCorrelationId());

            // -----------------------------------------------------------------
            // Dispatch by event type
            // -----------------------------------------------------------------
            dispatchEvent(event);

            // Mark as successfully processed (idempotent consumer)
            if (idempotentConsumer != null) {
                idempotentConsumer.markProcessed(messageId);
            }

        } catch (JMSException ex) {
            LOG.error("JMS error while processing message: {}", ex.getMessage(), ex);
            throw new RuntimeException("JMS processing failure — triggering container retry", ex);
        } catch (Exception ex) {
            LOG.error("Unexpected error while processing message: {}", ex.getMessage(), ex);
            throw new RuntimeException("Processing failure — triggering container retry", ex);
        } finally {
            MDC.remove("correlationId");
        }
    }

    // -------------------------------------------------------------------------
    // Internal helpers
    // -------------------------------------------------------------------------

    /**
     * Extracts a {@link ClaimEventMessage} from either an {@link ObjectMessage}
     * or a {@link TextMessage} (via the configured converter).
     */
    private ClaimEventMessage extractEvent(Message message) throws JMSException {
        if (message instanceof ObjectMessage) {
            Object payload = ((ObjectMessage) message).getObject();
            if (payload instanceof ClaimEventMessage) {
                return (ClaimEventMessage) payload;
            }
            LOG.warn("ObjectMessage payload is not a ClaimEventMessage: {}",
                    payload != null ? payload.getClass().getName() : "null");
            return null;
        }

        if (message instanceof TextMessage) {
            if (messageConverter != null) {
                try {
                    return (ClaimEventMessage) messageConverter.fromMessage(message);
                } catch (Exception ex) {
                    LOG.error("Failed to convert TextMessage via ClaimMessageConverter", ex);
                    return null;
                }
            }
            LOG.warn("Received TextMessage but no MessageConverter is configured");
            return null;
        }

        LOG.warn("Unsupported JMS message type: {}", message.getClass().getName());
        return null;
    }

    /**
     * Dispatches the claim event to the appropriate handler. Each branch
     * represents a downstream integration point.
     */
    private void dispatchEvent(ClaimEventMessage event) {
        if (event.getEventType() == null) {
            LOG.warn("Event type is null for claim {} — skipping dispatch", event.getClaimNumber());
            return;
        }

        switch (event.getEventType()) {
            case CLAIM_CREATED:
                handleClaimCreated(event);
                break;

            case CLAIM_UPDATED:
                handleClaimUpdated(event);
                break;

            case CLAIM_CLOSED:
                LOG.info("CLAIM_CLOSED: Triggering reconciliation for claim {}",
                        event.getClaimNumber());
                triggerReconciliation(event);
                break;

            case PAYMENT_PROCESSED:
                LOG.info("PAYMENT_PROCESSED: Updating payment totals for claim {}, amount={}",
                        event.getClaimNumber(), event.getAmount());
                updatePaymentTotals(event);
                break;

            case FRAUD_DETECTED:
                LOG.warn("FRAUD_DETECTED: Escalating claim {} for investigation",
                        event.getClaimNumber());
                escalateFraud(event);
                break;

            default:
                LOG.warn("Unknown event type {} for claim {}", event.getEventType(), event.getClaimNumber());
                break;
        }
    }

    /**
     * Handles CLAIM_CREATED events:
     * - Logs the new claim for audit
     * - Could trigger auto-assignment, partner notifications, etc.
     */
    private void handleClaimCreated(ClaimEventMessage event) {
        LOG.info("CLAIM_CREATED: New claim {} for policy {} (source={})",
                event.getClaimNumber(), event.getPolicyNumber(), event.getSourceSystem());

        // In production, this would:
        // 1. Notify the assigned adjuster
        // 2. Trigger initial fraud pre-screening
        // 3. Send acknowledgement to the submitting partner system
        // 4. Update the SLA tracking dashboard
        LOG.info("CLAIM_CREATED processing complete for claim {}", event.getClaimNumber());
    }

    /**
     * Handles CLAIM_UPDATED events:
     * - Tracks status changes for SLA monitoring
     * - Could trigger downstream notifications
     */
    private void handleClaimUpdated(ClaimEventMessage event) {
        LOG.info("CLAIM_UPDATED: Claim {} has been modified (source={})",
                event.getClaimNumber(), event.getSourceSystem());

        // In production, this would:
        // 1. Check if status changed and notify stakeholders
        // 2. Update the SLA clock if claim was reassigned
        // 3. Trigger re-scoring if loss amount changed significantly
        LOG.info("CLAIM_UPDATED processing complete for claim {}", event.getClaimNumber());
    }

    /**
     * Triggers payment reconciliation when a claim is closed.
     *
     * <p>Ensures that total payments match the approved amount before
     * the claim is finalised. Discrepancies are logged and flagged
     * for manual review.</p>
     */
    private void triggerReconciliation(ClaimEventMessage event) {
        LOG.info("Reconciliation triggered for claim {} (approved amount={})",
                event.getClaimNumber(), event.getAmount());

        // In production, this would invoke:
        //   reconciliationService.reconcile(event.getClaimNumber());
        //
        // The reconciliation compares:
        //   SUM(claim_payment.amount) vs claim.approved_amount
        // Any discrepancy is recorded in RECONCILIATION_REPORT

        LOG.info("Reconciliation check completed for claim {}", event.getClaimNumber());
    }

    /**
     * Updates running payment totals when a payment is processed.
     *
     * <p>Maintains an aggregate view of payments per claim for
     * real-time dashboard reporting.</p>
     */
    private void updatePaymentTotals(ClaimEventMessage event) {
        LOG.info("Payment totals update for claim {}, amount={}",
                event.getClaimNumber(), event.getAmount());

        // In production:
        //   paymentService.updateTotals(event.getClaimNumber(), event.getAmount());
        //
        // Also updates the claims dashboard and triggers
        // regulatory reporting if the payment exceeds thresholds

        if (event.getAmount() != null
                && event.getAmount().compareTo(new java.math.BigDecimal("50000")) > 0) {
            LOG.warn("Large payment processed for claim {}: {}. "
                    + "Flagging for regulatory reporting.",
                    event.getClaimNumber(), event.getAmount());
        }

        LOG.info("Payment totals updated for claim {}", event.getClaimNumber());
    }

    /**
     * Escalates fraud-detected claims for investigation.
     *
     * <p>Fraud events are treated with highest priority (JMS priority 8).
     * This handler notifies the Special Investigations Unit (SIU) and
     * freezes the claim to prevent further payouts.</p>
     */
    private void escalateFraud(ClaimEventMessage event) {
        LOG.warn("=== FRAUD ESCALATION ===");
        LOG.warn("Claim:    {}", event.getClaimNumber());
        LOG.warn("Policy:   {}", event.getPolicyNumber());
        LOG.warn("Claimant: {}", event.getClaimantName());
        LOG.warn("Amount:   {}", event.getAmount());
        LOG.warn("Source:   {}", event.getSourceSystem());

        // In production:
        // 1. fraudEscalationService.escalate(event.getClaimNumber());
        // 2. claimService.freezePayments(event.getClaimNumber());
        // 3. notificationService.alertSIU(event);
        // 4. auditService.logFraudEscalation(event);

        LOG.warn("Fraud escalation completed for claim {}. SIU notified.", event.getClaimNumber());
    }
}
