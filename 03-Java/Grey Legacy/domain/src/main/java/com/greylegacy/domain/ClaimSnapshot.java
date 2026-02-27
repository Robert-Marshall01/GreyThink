package com.greylegacy.domain;

import org.hibernate.annotations.Immutable;

import javax.persistence.*;
import java.math.BigDecimal;
import java.util.Date;

/**
 * Point-in-time snapshot of a claim's state for audit and historical reference.
 * Snapshots are immutable once created — they capture the exact state of a claim
 * at a specific moment, preserving values as plain strings rather than enum
 * references so they remain accurate even if enum definitions change over time.
 */
@Entity
@Table(name = "CLAIM_SNAPSHOT", indexes = {
        @Index(name = "IDX_SNAPSHOT_CLAIM_ID", columnList = "CLAIM_ID"),
        @Index(name = "IDX_SNAPSHOT_DATE", columnList = "SNAPSHOT_DATE")
})
@Immutable
@SequenceGenerator(name = "default_seq", sequenceName = "SNAPSHOT_SEQ", allocationSize = 1)
public class ClaimSnapshot extends BaseEntity {

    private static final long serialVersionUID = 1L;

    @Column(name = "CLAIM_ID", nullable = false)
    private Long claimId;

    @Temporal(TemporalType.TIMESTAMP)
    @Column(name = "SNAPSHOT_DATE", nullable = false)
    private Date snapshotDate;

    @Column(name = "SNAPSHOT_REASON", length = 100)
    private String snapshotReason;

    @Column(name = "POLICY_NUMBER", length = 20)
    private String policyNumber;

    @Column(name = "CLAIM_NUMBER", length = 20)
    private String claimNumber;

    @Column(name = "STATUS", length = 20)
    private String status;

    @Column(name = "CLAIM_TYPE", length = 30)
    private String claimType;

    @Column(name = "ESTIMATED_LOSS", precision = 14, scale = 2)
    private BigDecimal estimatedLoss;

    @Column(name = "APPROVED_AMOUNT", precision = 14, scale = 2)
    private BigDecimal approvedAmount;

    @Column(name = "ADJUSTER_CODE", length = 10)
    private String adjusterCode;

    @Column(name = "FRAUD_SCORE")
    private Integer fraudScore;

    @Column(name = "FRAUD_RISK_LEVEL", length = 20)
    private String fraudRiskLevel;

    @Lob
    @Column(name = "SERIALIZED_DATA")
    private String serializedData;

    // -------------------------------------------------------------------------
    // Constructors
    // -------------------------------------------------------------------------

    /**
     * Default constructor required by JPA.
     */
    protected ClaimSnapshot() {
    }

    /**
     * Creates a snapshot from the current state of the given claim.
     *
     * @param claim          the claim to snapshot
     * @param snapshotReason why this snapshot is being taken
     * @param serializedData full JSON representation of the claim state
     */
    public ClaimSnapshot(Claim claim, String snapshotReason, String serializedData) {
        this.claimId = claim.getId();
        this.snapshotDate = new Date();
        this.snapshotReason = snapshotReason;
        this.claimNumber = claim.getClaimNumber();
        this.policyNumber = claim.getPolicy() != null
                ? claim.getPolicy().getPolicyNumber() : null;
        this.status = claim.getStatus() != null
                ? claim.getStatus().name() : null;
        this.claimType = claim.getClaimType() != null
                ? claim.getClaimType().name() : null;
        this.estimatedLoss = claim.getEstimatedLoss();
        this.approvedAmount = claim.getApprovedAmount();
        this.adjusterCode = claim.getAdjusterCode();
        this.fraudScore = claim.getFraudScore();
        this.fraudRiskLevel = claim.getFraudRiskLevel() != null
                ? claim.getFraudRiskLevel().name() : null;
        this.serializedData = serializedData;
    }

    // -------------------------------------------------------------------------
    // Getters only (immutable entity)
    // -------------------------------------------------------------------------

    public Long getClaimId() {
        return claimId;
    }

    public Date getSnapshotDate() {
        return snapshotDate;
    }

    public String getSnapshotReason() {
        return snapshotReason;
    }

    public String getPolicyNumber() {
        return policyNumber;
    }

    public String getClaimNumber() {
        return claimNumber;
    }

    public String getStatus() {
        return status;
    }

    public String getClaimType() {
        return claimType;
    }

    public BigDecimal getEstimatedLoss() {
        return estimatedLoss;
    }

    public BigDecimal getApprovedAmount() {
        return approvedAmount;
    }

    public String getAdjusterCode() {
        return adjusterCode;
    }

    public Integer getFraudScore() {
        return fraudScore;
    }

    public String getFraudRiskLevel() {
        return fraudRiskLevel;
    }

    public String getSerializedData() {
        return serializedData;
    }

    // -------------------------------------------------------------------------
    // toString
    // -------------------------------------------------------------------------

    @Override
    public String toString() {
        return "ClaimSnapshot{" +
                "id=" + getId() +
                ", claimId=" + claimId +
                ", snapshotDate=" + snapshotDate +
                ", snapshotReason='" + snapshotReason + '\'' +
                ", claimNumber='" + claimNumber + '\'' +
                ", status='" + status + '\'' +
                ", claimType='" + claimType + '\'' +
                ", estimatedLoss=" + estimatedLoss +
                ", approvedAmount=" + approvedAmount +
                ", adjusterCode='" + adjusterCode + '\'' +
                ", fraudScore=" + fraudScore +
                ", fraudRiskLevel='" + fraudRiskLevel + '\'' +
                '}';
    }
}
