package com.greylegacy.integration.jms;

import javax.jms.Destination;
import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.ObjectMessage;
import javax.jms.Session;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.jms.JmsException;
import org.springframework.jms.core.JmsTemplate;
import org.springframework.jms.core.MessageCreator;

/**
 * JMS message producer that publishes {@link ClaimEventMessage} instances
 * to the configured claim-event queue via Spring's {@link JmsTemplate}.
 *
 * <p>Messages are sent as {@link ObjectMessage} payloads.  The JMS priority
 * is elevated for {@code FRAUD_DETECTED} events so that downstream fraud
 * handlers process them ahead of normal traffic.</p>
 *
 * <p>Wired through Spring XML — setter injection for {@code JmsTemplate},
 * destination queue, and optional reply-to queue.</p>
 *
 * @author Grey Legacy Integration Team
 * @since 1.0
 */
public class ClaimNotificationSender {

    private static final Logger LOG = LoggerFactory.getLogger(ClaimNotificationSender.class);

    /** Default JMS priority (0-9). Normal messages. */
    private static final int PRIORITY_NORMAL = 4;

    /** Elevated JMS priority for fraud events. */
    private static final int PRIORITY_HIGH = 8;

    private JmsTemplate jmsTemplate;
    private Destination destination;
    private Destination replyToQueue;

    // -------------------------------------------------------------------------
    // Spring setter injection
    // -------------------------------------------------------------------------

    public void setJmsTemplate(JmsTemplate jmsTemplate) {
        this.jmsTemplate = jmsTemplate;
    }

    public void setDestination(Destination destination) {
        this.destination = destination;
    }

    public void setReplyToQueue(Destination replyToQueue) {
        this.replyToQueue = replyToQueue;
    }

    // -------------------------------------------------------------------------
    // Public API
    // -------------------------------------------------------------------------

    /**
     * Sends a {@link ClaimEventMessage} to the configured JMS destination.
     *
     * @param event the claim event to publish; must not be {@code null}
     */
    public void sendClaimEvent(final ClaimEventMessage event) {
        if (event == null) {
            LOG.warn("Attempted to send a null ClaimEventMessage — ignoring.");
            return;
        }

        final String correlationId = event.getCorrelationId();
        LOG.info("Sending claim event [correlationId={}] type={} claim={}",
                correlationId, event.getEventType(), event.getClaimNumber());

        try {
            jmsTemplate.send(destination, new MessageCreator() {
                @Override
                public Message createMessage(Session session) throws JMSException {
                    ObjectMessage objectMessage = session.createObjectMessage(event);

                    // Set correlation ID so consumers can trace the message
                    if (correlationId != null) {
                        objectMessage.setJMSCorrelationID(correlationId);
                    }

                    // Stamp the event type as a string property for selector-based routing
                    objectMessage.setStringProperty("eventType",
                            event.getEventType() != null ? event.getEventType().name() : "UNKNOWN");

                    // Elevate priority for fraud events
                    if (ClaimEventMessage.EventType.FRAUD_DETECTED.equals(event.getEventType())) {
                        objectMessage.setJMSPriority(PRIORITY_HIGH);
                    } else {
                        objectMessage.setJMSPriority(PRIORITY_NORMAL);
                    }

                    // Set reply-to queue if configured
                    if (replyToQueue != null) {
                        objectMessage.setJMSReplyTo(replyToQueue);
                    }

                    return objectMessage;
                }
            });

            LOG.info("Successfully sent claim event [correlationId={}]", correlationId);

        } catch (JmsException ex) {
            LOG.error("Failed to send claim event [correlationId={}]: {}", correlationId, ex.getMessage(), ex);
            throw ex; // propagate so callers can handle or rollback
        }
    }
}
