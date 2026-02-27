package com.greylegacy.integration.jms;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;

/**
 * Serializable JMS message POJO representing a claim lifecycle event.
 * Transported over JMS queues between the claims processing engine
 * and downstream consumers (notifications, reconciliation, fraud).
 *
 * <p>This class is intentionally kept framework-free so that it can
 * be serialised by the JVM's default mechanism ({@link ObjectMessage})
 * or manually marshalled to XML ({@link javax.jms.TextMessage}).</p>
 *
 * @author Grey Legacy Integration Team
 * @since 1.0
 */
public class ClaimEventMessage implements Serializable {

    private static final long serialVersionUID = 20260101L;

    /** Supported claim event types. */
    public enum EventType {
        CLAIM_CREATED,
        CLAIM_UPDATED,
        CLAIM_CLOSED,
        PAYMENT_PROCESSED,
        FRAUD_DETECTED
    }

    private String messageId;
    private String claimNumber;
    private EventType eventType;
    private String policyNumber;
    private String claimantName;
    private BigDecimal amount;
    private Date timestamp;
    private String correlationId;
    private String sourceSystem;

    /** No-arg constructor required for serialisation frameworks. */
    public ClaimEventMessage() {
    }

    /** All-arg constructor for programmatic convenience. */
    public ClaimEventMessage(String messageId, String claimNumber, EventType eventType,
                             String policyNumber, String claimantName, BigDecimal amount,
                             Date timestamp, String correlationId, String sourceSystem) {
        this.messageId = messageId;
        this.claimNumber = claimNumber;
        this.eventType = eventType;
        this.policyNumber = policyNumber;
        this.claimantName = claimantName;
        this.amount = amount;
        this.timestamp = timestamp;
        this.correlationId = correlationId;
        this.sourceSystem = sourceSystem;
    }

    // -------------------------------------------------------------------------
    // Getters and Setters
    // -------------------------------------------------------------------------

    public String getMessageId() { return messageId; }
    public void setMessageId(String messageId) { this.messageId = messageId; }

    public String getClaimNumber() { return claimNumber; }
    public void setClaimNumber(String claimNumber) { this.claimNumber = claimNumber; }

    public EventType getEventType() { return eventType; }
    public void setEventType(EventType eventType) { this.eventType = eventType; }

    public String getPolicyNumber() { return policyNumber; }
    public void setPolicyNumber(String policyNumber) { this.policyNumber = policyNumber; }

    public String getClaimantName() { return claimantName; }
    public void setClaimantName(String claimantName) { this.claimantName = claimantName; }

    public BigDecimal getAmount() { return amount; }
    public void setAmount(BigDecimal amount) { this.amount = amount; }

    public Date getTimestamp() { return timestamp; }
    public void setTimestamp(Date timestamp) { this.timestamp = timestamp; }

    public String getCorrelationId() { return correlationId; }
    public void setCorrelationId(String correlationId) { this.correlationId = correlationId; }

    public String getSourceSystem() { return sourceSystem; }
    public void setSourceSystem(String sourceSystem) { this.sourceSystem = sourceSystem; }

    // -------------------------------------------------------------------------
    // toString
    // -------------------------------------------------------------------------

    @Override
    public String toString() {
        return "ClaimEventMessage{" +
                "messageId='" + messageId + '\'' +
                ", claimNumber='" + claimNumber + '\'' +
                ", eventType=" + eventType +
                ", policyNumber='" + policyNumber + '\'' +
                ", claimantName='" + claimantName + '\'' +
                ", amount=" + amount +
                ", timestamp=" + timestamp +
                ", correlationId='" + correlationId + '\'' +
                ", sourceSystem='" + sourceSystem + '\'' +
                '}';
    }
}
