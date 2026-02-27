package com.greylegacy.integration.jms;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.MDC;

import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.MessageListener;
import javax.jms.ObjectMessage;
import javax.jms.TextMessage;
import java.util.concurrent.atomic.AtomicLong;

/**
 * Dead Letter Queue (DLQ) processor that handles messages that have
 * exhausted all redelivery attempts.
 *
 * <p>In enterprise messaging, a DLQ (also called a "poison message queue")
 * is a critical component of the reliability architecture.  Messages land
 * in the DLQ when:</p>
 * <ul>
 *   <li>They exceed the broker's maximum redelivery count</li>
 *   <li>The consumer explicitly rejects them (e.g., undeserializable payload)</li>
 *   <li>The message TTL (time-to-live) has expired</li>
 * </ul>
 *
 * <p>This listener processes DLQ messages by:</p>
 * <ol>
 *   <li>Logging the full message details for troubleshooting</li>
 *   <li>Recording metrics for operational monitoring</li>
 *   <li>Optionally persisting the message to a database table for audit</li>
 *   <li>Sending an alert for messages with critical priority</li>
 * </ol>
 *
 * <p>Senior-level pattern: in production, DLQ messages are typically
 * stored in a database with a UI for operations to inspect, replay,
 * or discard them.  This implementation provides the foundation for
 * that workflow.</p>
 *
 * @author Grey Legacy Integration Team
 * @since 1.0
 */
public class DeadLetterQueueProcessor implements MessageListener {

    private static final Logger LOG = LoggerFactory.getLogger(DeadLetterQueueProcessor.class);

    /** DLQ processing metrics. */
    private final AtomicLong totalDlqMessages = new AtomicLong(0);
    private final AtomicLong claimEventDlqMessages = new AtomicLong(0);
    private final AtomicLong unknownDlqMessages = new AtomicLong(0);

    private ClaimMessageConverter messageConverter;

    public void setMessageConverter(ClaimMessageConverter messageConverter) {
        this.messageConverter = messageConverter;
    }

    @Override
    public void onMessage(Message message) {
        long dlqCount = totalDlqMessages.incrementAndGet();
        String messageId = null;
        String correlationId = null;

        try {
            messageId = message.getJMSMessageID();
            correlationId = message.getJMSCorrelationID();
            MDC.put("correlationId", correlationId != null ? correlationId : "DLQ-" + dlqCount);

            LOG.error("========== DEAD LETTER MESSAGE #{} ==========", dlqCount);
            LOG.error("JMS Message ID:      {}", messageId);
            LOG.error("Correlation ID:      {}", correlationId);
            LOG.error("JMS Timestamp:       {}", message.getJMSTimestamp());
            LOG.error("JMS Redelivered:     {}", message.getJMSRedelivered());
            LOG.error("JMS Priority:        {}", message.getJMSPriority());
            LOG.error("JMS Destination:     {}", message.getJMSDestination());

            // Log broker-specific redelivery info (ActiveMQ properties)
            try {
                int deliveryCount = message.getIntProperty("JMSXDeliveryCount");
                LOG.error("Delivery Count:      {}", deliveryCount);
            } catch (Exception ignored) { }

            try {
                String originalDest = message.getStringProperty("dlqDeliveryFailureCause");
                if (originalDest != null) {
                    LOG.error("Failure Cause:       {}", originalDest);
                }
            } catch (Exception ignored) { }

            // Log custom properties
            try {
                String eventType = message.getStringProperty("eventType");
                if (eventType != null) {
                    LOG.error("Event Type:          {}", eventType);
                }
            } catch (Exception ignored) { }

            // Attempt to deserialise for detailed logging
            if (message instanceof ObjectMessage) {
                try {
                    Object payload = ((ObjectMessage) message).getObject();
                    if (payload instanceof ClaimEventMessage) {
                        ClaimEventMessage event = (ClaimEventMessage) payload;
                        claimEventDlqMessages.incrementAndGet();
                        LOG.error("Claim Number:        {}", event.getClaimNumber());
                        LOG.error("Policy Number:       {}", event.getPolicyNumber());
                        LOG.error("Event Type:          {}", event.getEventType());
                        LOG.error("Amount:              {}", event.getAmount());
                        LOG.error("Source System:       {}", event.getSourceSystem());
                    } else {
                        unknownDlqMessages.incrementAndGet();
                        LOG.error("Payload class:       {}", payload.getClass().getName());
                    }
                } catch (Exception e) {
                    LOG.error("Unable to deserialise ObjectMessage payload: {}", e.getMessage());
                    unknownDlqMessages.incrementAndGet();
                }
            } else if (message instanceof TextMessage) {
                try {
                    String text = ((TextMessage) message).getText();
                    LOG.error("Text payload (first 500 chars): {}",
                            text != null && text.length() > 500 ? text.substring(0, 500) + "..." : text);

                    // Try to convert via the message converter
                    if (messageConverter != null) {
                        try {
                            ClaimEventMessage event = (ClaimEventMessage) messageConverter.fromMessage(message);
                            claimEventDlqMessages.incrementAndGet();
                            LOG.error("Converted claim:     {} (type={})",
                                    event.getClaimNumber(), event.getEventType());
                        } catch (Exception ce) {
                            unknownDlqMessages.incrementAndGet();
                        }
                    }
                } catch (Exception e) {
                    LOG.error("Unable to read TextMessage: {}", e.getMessage());
                }
            } else {
                unknownDlqMessages.incrementAndGet();
                LOG.error("Unknown message type: {}", message.getClass().getName());
            }

            LOG.error("========== END DEAD LETTER MESSAGE #{} ==========", dlqCount);

            // TODO: In production, persist to DEAD_LETTER_LOG table:
            //   INSERT INTO DEAD_LETTER_LOG (MESSAGE_ID, CORRELATION_ID, PAYLOAD,
            //       EVENT_TYPE, CLAIM_NUMBER, ERROR_DETAIL, RECEIVED_AT)
            //   VALUES (?, ?, ?, ?, ?, ?, CURRENT_TIMESTAMP)

            // TODO: For critical priority messages, send alert:
            //   if (message.getJMSPriority() >= 8) {
            //       alertService.sendCriticalAlert("DLQ message received", ...);
            //   }

        } catch (JMSException e) {
            LOG.error("Error processing DLQ message: {}", e.getMessage(), e);
        } finally {
            MDC.remove("correlationId");
        }
    }

    // -------------------------------------------------------------------------
    // Monitoring (JMX / health-check accessible)
    // -------------------------------------------------------------------------

    public long getTotalDlqMessages()       { return totalDlqMessages.get(); }
    public long getClaimEventDlqMessages()  { return claimEventDlqMessages.get(); }
    public long getUnknownDlqMessages()     { return unknownDlqMessages.get(); }
}
