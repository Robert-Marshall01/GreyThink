package com.greylegacy.domain;

import javax.persistence.*;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

/**
 * Insurance policy entity. Root aggregate for policy-related data.
 * Demonstrates eager/lazy loading strategies and cascading rules.
 */
@Entity
@Table(name = "POLICY", indexes = {
    @Index(name = "IDX_POLICY_NUMBER", columnList = "POLICY_NUMBER", unique = true),
    @Index(name = "IDX_POLICY_STATUS", columnList = "STATUS")
})
@SequenceGenerator(name = "default_seq", sequenceName = "POLICY_SEQ", allocationSize = 1)
@NamedQueries({
    @NamedQuery(name = "Policy.findByPolicyNumber",
                query = "SELECT p FROM Policy p WHERE p.policyNumber = :policyNumber"),
    @NamedQuery(name = "Policy.findByStatus",
                query = "SELECT p FROM Policy p WHERE p.status = :status"),
    @NamedQuery(name = "Policy.findByHolderLastName",
                query = "SELECT p FROM Policy p WHERE p.holderLastName = :lastName"),
    @NamedQuery(name = "Policy.findActivePolicies",
                query = "SELECT p FROM Policy p WHERE p.status = 'ACTIVE' AND p.effectiveDate <= :asOfDate AND p.expirationDate >= :asOfDate")
})
public class Policy extends BaseEntity {

    private static final long serialVersionUID = 1L;

    @Column(name = "POLICY_NUMBER", length = 20, nullable = false, unique = true)
    private String policyNumber;

    @Enumerated(EnumType.STRING)
    @Column(name = "STATUS", length = 20, nullable = false)
    private PolicyStatus status;

    @Enumerated(EnumType.STRING)
    @Column(name = "POLICY_TYPE", length = 30, nullable = false)
    private PolicyType policyType;

    @Column(name = "HOLDER_FIRST_NAME", length = 50, nullable = false)
    private String holderFirstName;

    @Column(name = "HOLDER_LAST_NAME", length = 50, nullable = false)
    private String holderLastName;

    @Column(name = "HOLDER_SSN", length = 11)
    private String holderSsn;

    @Column(name = "HOLDER_EMAIL", length = 100)
    private String holderEmail;

    @Column(name = "HOLDER_PHONE", length = 20)
    private String holderPhone;

    @Temporal(TemporalType.DATE)
    @Column(name = "EFFECTIVE_DATE", nullable = false)
    private Date effectiveDate;

    @Temporal(TemporalType.DATE)
    @Column(name = "EXPIRATION_DATE", nullable = false)
    private Date expirationDate;

    @Column(name = "PREMIUM_AMOUNT", precision = 12, scale = 2)
    private BigDecimal premiumAmount;

    @Column(name = "COVERAGE_LIMIT", precision = 14, scale = 2)
    private BigDecimal coverageLimit;

    @Column(name = "DEDUCTIBLE", precision = 10, scale = 2)
    private BigDecimal deductible;

    @Column(name = "AGENT_CODE", length = 10)
    private String agentCode;

    @Column(name = "UNDERWRITER_CODE", length = 10)
    private String underwriterCode;

    // Lazy-loaded collection — avoid N+1 with explicit fetch joins
    @OneToMany(mappedBy = "policy", cascade = CascadeType.ALL, orphanRemoval = true, fetch = FetchType.LAZY)
    @OrderBy("lossDate DESC")
    private List<Claim> claims = new ArrayList<>();

    // Eager-loaded — small collection, always needed
    @OneToMany(mappedBy = "policy", cascade = CascadeType.ALL, orphanRemoval = true, fetch = FetchType.EAGER)
    private List<PolicyEndorsement> endorsements = new ArrayList<>();

    public Policy() {
    }

    // Helper methods

    public void addClaim(Claim claim) {
        claims.add(claim);
        claim.setPolicy(this);
    }

    public void removeClaim(Claim claim) {
        claims.remove(claim);
        claim.setPolicy(null);
    }

    public void addEndorsement(PolicyEndorsement endorsement) {
        endorsements.add(endorsement);
        endorsement.setPolicy(this);
    }

    public boolean isActive() {
        return PolicyStatus.ACTIVE.equals(this.status);
    }

    public boolean isCoverageValid(Date asOfDate) {
        return isActive()
            && !asOfDate.before(effectiveDate)
            && !asOfDate.after(expirationDate);
    }

    // Getters and Setters

    public String getPolicyNumber() {
        return policyNumber;
    }

    public void setPolicyNumber(String policyNumber) {
        this.policyNumber = policyNumber;
    }

    public PolicyStatus getStatus() {
        return status;
    }

    public void setStatus(PolicyStatus status) {
        this.status = status;
    }

    public PolicyType getPolicyType() {
        return policyType;
    }

    public void setPolicyType(PolicyType policyType) {
        this.policyType = policyType;
    }

    public String getHolderFirstName() {
        return holderFirstName;
    }

    public void setHolderFirstName(String holderFirstName) {
        this.holderFirstName = holderFirstName;
    }

    public String getHolderLastName() {
        return holderLastName;
    }

    public void setHolderLastName(String holderLastName) {
        this.holderLastName = holderLastName;
    }

    public String getHolderSsn() {
        return holderSsn;
    }

    public void setHolderSsn(String holderSsn) {
        this.holderSsn = holderSsn;
    }

    public String getHolderEmail() {
        return holderEmail;
    }

    public void setHolderEmail(String holderEmail) {
        this.holderEmail = holderEmail;
    }

    public String getHolderPhone() {
        return holderPhone;
    }

    public void setHolderPhone(String holderPhone) {
        this.holderPhone = holderPhone;
    }

    public Date getEffectiveDate() {
        return effectiveDate;
    }

    public void setEffectiveDate(Date effectiveDate) {
        this.effectiveDate = effectiveDate;
    }

    public Date getExpirationDate() {
        return expirationDate;
    }

    public void setExpirationDate(Date expirationDate) {
        this.expirationDate = expirationDate;
    }

    public BigDecimal getPremiumAmount() {
        return premiumAmount;
    }

    public void setPremiumAmount(BigDecimal premiumAmount) {
        this.premiumAmount = premiumAmount;
    }

    public BigDecimal getCoverageLimit() {
        return coverageLimit;
    }

    public void setCoverageLimit(BigDecimal coverageLimit) {
        this.coverageLimit = coverageLimit;
    }

    public BigDecimal getDeductible() {
        return deductible;
    }

    public void setDeductible(BigDecimal deductible) {
        this.deductible = deductible;
    }

    public String getAgentCode() {
        return agentCode;
    }

    public void setAgentCode(String agentCode) {
        this.agentCode = agentCode;
    }

    public String getUnderwriterCode() {
        return underwriterCode;
    }

    public void setUnderwriterCode(String underwriterCode) {
        this.underwriterCode = underwriterCode;
    }

    public List<Claim> getClaims() {
        return claims;
    }

    public void setClaims(List<Claim> claims) {
        this.claims = claims;
    }

    public List<PolicyEndorsement> getEndorsements() {
        return endorsements;
    }

    public void setEndorsements(List<PolicyEndorsement> endorsements) {
        this.endorsements = endorsements;
    }

    @Override
    public String toString() {
        return "Policy{" +
                "policyNumber='" + policyNumber + '\'' +
                ", status=" + status +
                ", policyType=" + policyType +
                ", holderLastName='" + holderLastName + '\'' +
                '}';
    }
}
