package com.greylegacy.modernization.dto;

import java.math.BigDecimal;
import java.util.Date;

/**
 * Data Transfer Object for Policy — decouples API contract from domain model.
 * Excludes sensitive fields like SSN and internal endorsement data.
 */
public class PolicyDto {

    private Long id;
    private String policyNumber;
    private String status;
    private String policyType;
    private String holderFirstName;
    private String holderLastName;
    private String holderEmail;
    private String holderPhone;
    private Date effectiveDate;
    private Date expirationDate;
    private BigDecimal premiumAmount;
    private BigDecimal coverageLimit;
    private BigDecimal deductible;
    private String agentCode;
    private int claimCount;
    private Date createdDate;
    private Date updatedDate;

    public PolicyDto() {}

    public Long getId() { return id; }
    public void setId(Long id) { this.id = id; }

    public String getPolicyNumber() { return policyNumber; }
    public void setPolicyNumber(String policyNumber) { this.policyNumber = policyNumber; }

    public String getStatus() { return status; }
    public void setStatus(String status) { this.status = status; }

    public String getPolicyType() { return policyType; }
    public void setPolicyType(String policyType) { this.policyType = policyType; }

    public String getHolderFirstName() { return holderFirstName; }
    public void setHolderFirstName(String holderFirstName) { this.holderFirstName = holderFirstName; }

    public String getHolderLastName() { return holderLastName; }
    public void setHolderLastName(String holderLastName) { this.holderLastName = holderLastName; }

    public String getHolderEmail() { return holderEmail; }
    public void setHolderEmail(String holderEmail) { this.holderEmail = holderEmail; }

    public String getHolderPhone() { return holderPhone; }
    public void setHolderPhone(String holderPhone) { this.holderPhone = holderPhone; }

    public Date getEffectiveDate() { return effectiveDate; }
    public void setEffectiveDate(Date effectiveDate) { this.effectiveDate = effectiveDate; }

    public Date getExpirationDate() { return expirationDate; }
    public void setExpirationDate(Date expirationDate) { this.expirationDate = expirationDate; }

    public BigDecimal getPremiumAmount() { return premiumAmount; }
    public void setPremiumAmount(BigDecimal premiumAmount) { this.premiumAmount = premiumAmount; }

    public BigDecimal getCoverageLimit() { return coverageLimit; }
    public void setCoverageLimit(BigDecimal coverageLimit) { this.coverageLimit = coverageLimit; }

    public BigDecimal getDeductible() { return deductible; }
    public void setDeductible(BigDecimal deductible) { this.deductible = deductible; }

    public String getAgentCode() { return agentCode; }
    public void setAgentCode(String agentCode) { this.agentCode = agentCode; }

    public int getClaimCount() { return claimCount; }
    public void setClaimCount(int claimCount) { this.claimCount = claimCount; }

    public Date getCreatedDate() { return createdDate; }
    public void setCreatedDate(Date createdDate) { this.createdDate = createdDate; }

    public Date getUpdatedDate() { return updatedDate; }
    public void setUpdatedDate(Date updatedDate) { this.updatedDate = updatedDate; }
}
