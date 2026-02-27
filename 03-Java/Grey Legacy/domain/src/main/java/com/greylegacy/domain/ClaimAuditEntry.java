package com.greylegacy.domain;

import org.hibernate.annotations.Immutable;

import javax.persistence.*;
import java.util.Date;

/**
 * Append-only audit trail entry for claim lifecycle events.
 * Records every significant action taken on a claim for compliance and traceability.
 * This entity is immutable — once persisted, rows are never updated or deleted.
 */
@Entity
@Table(name = "CLAIM_AUDIT")
@Immutable
@SequenceGenerator(name = "default_seq", sequenceName = "AUDIT_SEQ", allocationSize = 50)
public class ClaimAuditEntry extends BaseEntity {

    private static final long serialVersionUID = 1L;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "CLAIM_ID", updatable = false)
    private Claim claim;

    @Temporal(TemporalType.TIMESTAMP)
    @Column(name = "EVENT_TIMESTAMP", nullable = false, updatable = false)
    private Date eventTimestamp;

    @Column(name = "EVENT_TYPE", length = 50, nullable = false, updatable = false)
    private String eventType;

    @Column(name = "PREVIOUS_VALUE", length = 500, updatable = false)
    private String previousValue;

    @Column(name = "NEW_VALUE", length = 500, updatable = false)
    private String newValue;

    @Column(name = "PERFORMED_BY", length = 50, nullable = false, updatable = false)
    private String performedBy;

    @Column(name = "DESCRIPTION", length = 1000, updatable = false)
    private String description;

    @Column(name = "IP_ADDRESS", length = 45, updatable = false)
    private String ipAddress;

    // -------------------------------------------------------------------------
    // Constructors
    // -------------------------------------------------------------------------

    /**
     * Default constructor required by JPA.
     */
    protected ClaimAuditEntry() {
    }

    /**
     * Creates a fully-populated audit entry. All fields are set at construction
     * time and cannot be modified afterward.
     *
     * @param claim         the claim this audit entry belongs to
     * @param eventTimestamp when the event occurred
     * @param eventType     the type of event (e.g. STATUS_CHANGE, PAYMENT_ISSUED)
     * @param previousValue the previous value before the change
     * @param newValue      the new value after the change
     * @param performedBy   who performed the action
     * @param description   human-readable description of the event
     * @param ipAddress     IP address of the actor
     */
    public ClaimAuditEntry(Claim claim, Date eventTimestamp, String eventType,
                           String previousValue, String newValue,
                           String performedBy, String description, String ipAddress) {
        this.claim = claim;
        this.eventTimestamp = eventTimestamp;
        this.eventType = eventType;
        this.previousValue = previousValue;
        this.newValue = newValue;
        this.performedBy = performedBy;
        this.description = description;
        this.ipAddress = ipAddress;
    }

    // -------------------------------------------------------------------------
    // Getters (no setters — immutable entity)
    // -------------------------------------------------------------------------

    public Claim getClaim() {
        return claim;
    }

    public Date getEventTimestamp() {
        return eventTimestamp;
    }

    public String getEventType() {
        return eventType;
    }

    public String getPreviousValue() {
        return previousValue;
    }

    public String getNewValue() {
        return newValue;
    }

    public String getPerformedBy() {
        return performedBy;
    }

    public String getDescription() {
        return description;
    }

    public String getIpAddress() {
        return ipAddress;
    }

    // -------------------------------------------------------------------------
    // Package-private setter for bidirectional relationship support
    // -------------------------------------------------------------------------

    /**
     * Used by {@link Claim#addAuditEntry(ClaimAuditEntry)} to establish
     * the bidirectional relationship. Package-private to preserve immutability.
     */
    void setClaim(Claim claim) {
        this.claim = claim;
    }

    // -------------------------------------------------------------------------
    // toString
    // -------------------------------------------------------------------------

    @Override
    public String toString() {
        return "ClaimAuditEntry{" +
                "id=" + getId() +
                ", eventTimestamp=" + eventTimestamp +
                ", eventType='" + eventType + '\'' +
                ", previousValue='" + previousValue + '\'' +
                ", newValue='" + newValue + '\'' +
                ", performedBy='" + performedBy + '\'' +
                ", ipAddress='" + ipAddress + '\'' +
                '}';
    }
}
