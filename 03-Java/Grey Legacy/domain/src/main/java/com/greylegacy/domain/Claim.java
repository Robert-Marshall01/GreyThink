package com.greylegacy.domain;

import javax.persistence.*;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

/**
 * Core Claim entity for the insurance claims processing system.
 * Represents an insurance claim filed against a policy, tracking the full
 * lifecycle from initial report (FNOL) through investigation, approval,
 * settlement, and closure.
 */
@Entity
@Table(name = "CLAIM", indexes = {
        @Index(name = "IDX_CLAIM_CLAIM_NUMBER", columnList = "CLAIM_NUMBER", unique = true),
        @Index(name = "IDX_CLAIM_POLICY_ID", columnList = "POLICY_ID"),
        @Index(name = "IDX_CLAIM_STATUS", columnList = "STATUS"),
        @Index(name = "IDX_CLAIM_LOSS_DATE", columnList = "LOSS_DATE")
})
@SequenceGenerator(name = "default_seq", sequenceName = "CLAIM_SEQ", allocationSize = 1)
@NamedQueries({
        @NamedQuery(name = "Claim.findByClaimNumber",
                query = "SELECT c FROM Claim c WHERE c.claimNumber = :claimNumber"),
        @NamedQuery(name = "Claim.findByStatus",
                query = "SELECT c FROM Claim c WHERE c.status = :status"),
        @NamedQuery(name = "Claim.findByPolicyId",
                query = "SELECT c FROM Claim c WHERE c.policy.id = :policyId"),
        @NamedQuery(name = "Claim.findOpenClaims",
                query = "SELECT c FROM Claim c WHERE c.status NOT IN " +
                        "(com.greylegacy.domain.ClaimStatus.CLOSED, " +
                        "com.greylegacy.domain.ClaimStatus.DENIED, " +
                        "com.greylegacy.domain.ClaimStatus.SETTLED)"),
        @NamedQuery(name = "Claim.findClaimsOlderThan",
                query = "SELECT c FROM Claim c WHERE c.lossDate < :cutoffDate")
})
public class Claim extends BaseEntity {

    private static final long serialVersionUID = 1L;

    @Column(name = "CLAIM_NUMBER", length = 20, unique = true, nullable = false)
    private String claimNumber;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "POLICY_ID")
    private Policy policy;

    @Enumerated(EnumType.STRING)
    @Column(name = "STATUS", nullable = false)
    private ClaimStatus status;

    @Enumerated(EnumType.STRING)
    @Column(name = "CLAIM_TYPE", nullable = false)
    private ClaimType claimType;

    @Temporal(TemporalType.DATE)
    @Column(name = "LOSS_DATE", nullable = false)
    private Date lossDate;

    @Temporal(TemporalType.TIMESTAMP)
    @Column(name = "REPORTED_DATE", nullable = false)
    private Date reportedDate;

    @Column(name = "LOSS_DESCRIPTION", length = 2000)
    private String lossDescription;

    @Column(name = "LOSS_LOCATION", length = 200)
    private String lossLocation;

    @Column(name = "CLAIMANT_FIRST_NAME", length = 50, nullable = false)
    private String claimantFirstName;

    @Column(name = "CLAIMANT_LAST_NAME", length = 50, nullable = false)
    private String claimantLastName;

    @Column(name = "CLAIMANT_PHONE", length = 50)
    private String claimantPhone;

    @Column(name = "CLAIMANT_EMAIL", length = 100)
    private String claimantEmail;

    @Column(name = "ADJUSTER_CODE", length = 10)
    private String adjusterCode;

    @Column(name = "ESTIMATED_LOSS", precision = 14, scale = 2)
    private BigDecimal estimatedLoss;

    @Column(name = "APPROVED_AMOUNT", precision = 14, scale = 2)
    private BigDecimal approvedAmount;

    @Column(name = "DEDUCTIBLE_APPLIED", precision = 10, scale = 2)
    private BigDecimal deductibleApplied;

    @Enumerated(EnumType.STRING)
    @Column(name = "FRAUD_RISK_LEVEL")
    private FraudRiskLevel fraudRiskLevel;

    @Column(name = "FRAUD_SCORE")
    private Integer fraudScore;

    @Temporal(TemporalType.TIMESTAMP)
    @Column(name = "FRAUD_SCORED_DATE")
    private Date fraudScoredDate;

    @Temporal(TemporalType.TIMESTAMP)
    @Column(name = "CLOSED_DATE")
    private Date closedDate;

    @Column(name = "CLOSED_REASON", length = 500)
    private String closedReason;

    @OneToMany(mappedBy = "claim", cascade = CascadeType.ALL, orphanRemoval = true, fetch = FetchType.LAZY)
    private List<ClaimPayment> payments = new ArrayList<>();

    @OneToMany(mappedBy = "claim", cascade = CascadeType.ALL, orphanRemoval = true, fetch = FetchType.LAZY)
    private List<ClaimAuditEntry> auditEntries = new ArrayList<>();

    @OneToMany(mappedBy = "claim", cascade = CascadeType.ALL, orphanRemoval = true, fetch = FetchType.LAZY)
    private List<ClaimNote> notes = new ArrayList<>();

    // -------------------------------------------------------------------------
    // Helper Methods
    // -------------------------------------------------------------------------

    /**
     * Adds a payment to this claim, establishing the bidirectional relationship.
     *
     * @param payment the payment to add
     */
    public void addPayment(ClaimPayment payment) {
        payments.add(payment);
        payment.setClaim(this);
    }

    /**
     * Adds an audit entry to this claim, establishing the bidirectional relationship.
     *
     * @param auditEntry the audit entry to add
     */
    public void addAuditEntry(ClaimAuditEntry auditEntry) {
        auditEntries.add(auditEntry);
        auditEntry.setClaim(this);
    }

    /**
     * Adds a note to this claim, establishing the bidirectional relationship.
     *
     * @param note the note to add
     */
    public void addNote(ClaimNote note) {
        notes.add(note);
        note.setClaim(this);
    }

    /**
     * Returns {@code true} if the claim is in an open (non-terminal) status.
     *
     * @return whether the claim is still open
     */
    public boolean isOpen() {
        return status != null
                && status != ClaimStatus.CLOSED
                && status != ClaimStatus.DENIED
                && status != ClaimStatus.SETTLED;
    }

    /**
     * Returns {@code true} if the claim has been flagged for potential fraud,
     * either by status or by a HIGH/CRITICAL fraud risk level.
     *
     * @return whether fraud is suspected on this claim
     */
    public boolean isFraudSuspected() {
        return status == ClaimStatus.FRAUD_SUSPECTED
                || fraudRiskLevel == FraudRiskLevel.HIGH
                || fraudRiskLevel == FraudRiskLevel.CRITICAL;
    }

    // -------------------------------------------------------------------------
    // Getters and Setters
    // -------------------------------------------------------------------------

    public String getClaimNumber() {
        return claimNumber;
    }

    public void setClaimNumber(String claimNumber) {
        this.claimNumber = claimNumber;
    }

    public Policy getPolicy() {
        return policy;
    }

    public void setPolicy(Policy policy) {
        this.policy = policy;
    }

    public ClaimStatus getStatus() {
        return status;
    }

    public void setStatus(ClaimStatus status) {
        this.status = status;
    }

    public ClaimType getClaimType() {
        return claimType;
    }

    public void setClaimType(ClaimType claimType) {
        this.claimType = claimType;
    }

    public Date getLossDate() {
        return lossDate;
    }

    public void setLossDate(Date lossDate) {
        this.lossDate = lossDate;
    }

    public Date getReportedDate() {
        return reportedDate;
    }

    public void setReportedDate(Date reportedDate) {
        this.reportedDate = reportedDate;
    }

    public String getLossDescription() {
        return lossDescription;
    }

    public void setLossDescription(String lossDescription) {
        this.lossDescription = lossDescription;
    }

    public String getLossLocation() {
        return lossLocation;
    }

    public void setLossLocation(String lossLocation) {
        this.lossLocation = lossLocation;
    }

    public String getClaimantFirstName() {
        return claimantFirstName;
    }

    public void setClaimantFirstName(String claimantFirstName) {
        this.claimantFirstName = claimantFirstName;
    }

    public String getClaimantLastName() {
        return claimantLastName;
    }

    public void setClaimantLastName(String claimantLastName) {
        this.claimantLastName = claimantLastName;
    }

    public String getClaimantPhone() {
        return claimantPhone;
    }

    public void setClaimantPhone(String claimantPhone) {
        this.claimantPhone = claimantPhone;
    }

    public String getClaimantEmail() {
        return claimantEmail;
    }

    public void setClaimantEmail(String claimantEmail) {
        this.claimantEmail = claimantEmail;
    }

    public String getAdjusterCode() {
        return adjusterCode;
    }

    public void setAdjusterCode(String adjusterCode) {
        this.adjusterCode = adjusterCode;
    }

    public BigDecimal getEstimatedLoss() {
        return estimatedLoss;
    }

    public void setEstimatedLoss(BigDecimal estimatedLoss) {
        this.estimatedLoss = estimatedLoss;
    }

    public BigDecimal getApprovedAmount() {
        return approvedAmount;
    }

    public void setApprovedAmount(BigDecimal approvedAmount) {
        this.approvedAmount = approvedAmount;
    }

    public BigDecimal getDeductibleApplied() {
        return deductibleApplied;
    }

    public void setDeductibleApplied(BigDecimal deductibleApplied) {
        this.deductibleApplied = deductibleApplied;
    }

    public FraudRiskLevel getFraudRiskLevel() {
        return fraudRiskLevel;
    }

    public void setFraudRiskLevel(FraudRiskLevel fraudRiskLevel) {
        this.fraudRiskLevel = fraudRiskLevel;
    }

    public Integer getFraudScore() {
        return fraudScore;
    }

    public void setFraudScore(Integer fraudScore) {
        this.fraudScore = fraudScore;
    }

    public Date getFraudScoredDate() {
        return fraudScoredDate;
    }

    public void setFraudScoredDate(Date fraudScoredDate) {
        this.fraudScoredDate = fraudScoredDate;
    }

    public Date getClosedDate() {
        return closedDate;
    }

    public void setClosedDate(Date closedDate) {
        this.closedDate = closedDate;
    }

    public String getClosedReason() {
        return closedReason;
    }

    public void setClosedReason(String closedReason) {
        this.closedReason = closedReason;
    }

    public List<ClaimPayment> getPayments() {
        return payments;
    }

    public void setPayments(List<ClaimPayment> payments) {
        this.payments = payments;
    }

    public List<ClaimAuditEntry> getAuditEntries() {
        return auditEntries;
    }

    public void setAuditEntries(List<ClaimAuditEntry> auditEntries) {
        this.auditEntries = auditEntries;
    }

    public List<ClaimNote> getNotes() {
        return notes;
    }

    public void setNotes(List<ClaimNote> notes) {
        this.notes = notes;
    }

    // -------------------------------------------------------------------------
    // toString
    // -------------------------------------------------------------------------

    @Override
    public String toString() {
        return "Claim{" +
                "id=" + getId() +
                ", claimNumber='" + claimNumber + '\'' +
                ", status=" + status +
                ", claimType=" + claimType +
                ", lossDate=" + lossDate +
                ", reportedDate=" + reportedDate +
                ", claimantFirstName='" + claimantFirstName + '\'' +
                ", claimantLastName='" + claimantLastName + '\'' +
                ", adjusterCode='" + adjusterCode + '\'' +
                ", estimatedLoss=" + estimatedLoss +
                ", approvedAmount=" + approvedAmount +
                ", deductibleApplied=" + deductibleApplied +
                ", fraudRiskLevel=" + fraudRiskLevel +
                ", fraudScore=" + fraudScore +
                ", closedDate=" + closedDate +
                '}';
    }
}
