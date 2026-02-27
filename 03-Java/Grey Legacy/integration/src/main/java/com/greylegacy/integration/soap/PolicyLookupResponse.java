package com.greylegacy.integration.soap;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import java.math.BigDecimal;
import java.util.Date;

/**
 * JAXB-annotated response containing policy information.
 */
@XmlRootElement(name = "policyLookupResponse", namespace = "http://www.greylegacy.com/schema/policy")
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "PolicyLookupResponse", namespace = "http://www.greylegacy.com/schema/policy",
    propOrder = {
        "policyNumber",
        "status",
        "holderName",
        "policyType",
        "effectiveDate",
        "expirationDate",
        "premiumAmount",
        "coverageLimit",
        "deductible"
    })
public class PolicyLookupResponse {

    @XmlElement(required = true)
    private String policyNumber;

    @XmlElement(required = true)
    private String status;

    @XmlElement(required = true)
    private String holderName;

    @XmlElement(required = true)
    private String policyType;

    @XmlElement(required = true)
    private Date effectiveDate;

    @XmlElement(required = true)
    private Date expirationDate;

    @XmlElement(required = true)
    private BigDecimal premiumAmount;

    @XmlElement(required = true)
    private BigDecimal coverageLimit;

    @XmlElement(required = true)
    private BigDecimal deductible;

    public PolicyLookupResponse() {
    }

    public String getPolicyNumber() {
        return policyNumber;
    }

    public void setPolicyNumber(String policyNumber) {
        this.policyNumber = policyNumber;
    }

    public String getStatus() {
        return status;
    }

    public void setStatus(String status) {
        this.status = status;
    }

    public String getHolderName() {
        return holderName;
    }

    public void setHolderName(String holderName) {
        this.holderName = holderName;
    }

    public String getPolicyType() {
        return policyType;
    }

    public void setPolicyType(String policyType) {
        this.policyType = policyType;
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

    @Override
    public String toString() {
        return "PolicyLookupResponse{" +
                "policyNumber='" + policyNumber + '\'' +
                ", status='" + status + '\'' +
                ", holderName='" + holderName + '\'' +
                ", policyType='" + policyType + '\'' +
                '}';
    }
}
