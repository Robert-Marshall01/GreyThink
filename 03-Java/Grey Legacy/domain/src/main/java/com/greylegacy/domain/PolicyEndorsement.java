package com.greylegacy.domain;

import javax.persistence.*;
import java.math.BigDecimal;
import java.util.Date;

/**
 * Represents an endorsement (amendment/rider) applied to an insurance policy.
 * Endorsements modify coverage terms, limits, or premiums on an existing policy.
 */
@Entity
@Table(name = "POLICY_ENDORSEMENT")
@SequenceGenerator(name = "default_seq", sequenceName = "ENDORSEMENT_SEQ", allocationSize = 1)
public class PolicyEndorsement extends BaseEntity {

    private static final long serialVersionUID = 1L;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "POLICY_ID")
    private Policy policy;

    @Column(name = "ENDORSEMENT_NUMBER", length = 20, unique = true)
    private String endorsementNumber;

    @Column(name = "ENDORSEMENT_TYPE", length = 50)
    private String endorsementType;

    @Temporal(TemporalType.DATE)
    @Column(name = "EFFECTIVE_DATE")
    private Date effectiveDate;

    @Column(name = "PREMIUM_ADJUSTMENT", precision = 12, scale = 2)
    private BigDecimal premiumAdjustment;

    @Column(name = "DESCRIPTION", length = 500)
    private String description;

    @Column(name = "ACTIVE")
    private boolean active = true;

    // -------------------------------------------------------------------------
    // Getters and Setters
    // -------------------------------------------------------------------------

    public Policy getPolicy() {
        return policy;
    }

    public void setPolicy(Policy policy) {
        this.policy = policy;
    }

    public String getEndorsementNumber() {
        return endorsementNumber;
    }

    public void setEndorsementNumber(String endorsementNumber) {
        this.endorsementNumber = endorsementNumber;
    }

    public String getEndorsementType() {
        return endorsementType;
    }

    public void setEndorsementType(String endorsementType) {
        this.endorsementType = endorsementType;
    }

    public Date getEffectiveDate() {
        return effectiveDate;
    }

    public void setEffectiveDate(Date effectiveDate) {
        this.effectiveDate = effectiveDate;
    }

    public BigDecimal getPremiumAdjustment() {
        return premiumAdjustment;
    }

    public void setPremiumAdjustment(BigDecimal premiumAdjustment) {
        this.premiumAdjustment = premiumAdjustment;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public boolean isActive() {
        return active;
    }

    public void setActive(boolean active) {
        this.active = active;
    }

    // -------------------------------------------------------------------------
    // toString
    // -------------------------------------------------------------------------

    @Override
    public String toString() {
        return "PolicyEndorsement{" +
                "id=" + getId() +
                ", endorsementNumber='" + endorsementNumber + '\'' +
                ", endorsementType='" + endorsementType + '\'' +
                ", effectiveDate=" + effectiveDate +
                ", premiumAdjustment=" + premiumAdjustment +
                ", active=" + active +
                '}';
    }
}
