package com.greylegacy.integration.soap;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import java.math.BigDecimal;
import java.util.Date;

/**
 * JAXB-annotated response containing full claim details.
 */
@XmlRootElement(name = "getClaimDetailsResponse", namespace = "http://www.greylegacy.com/schema/claims")
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "GetClaimDetailsResponse", namespace = "http://www.greylegacy.com/schema/claims",
    propOrder = {
        "claimNumber",
        "status",
        "claimType",
        "lossDate",
        "reportedDate",
        "claimantFirstName",
        "claimantLastName",
        "adjusterCode",
        "estimatedLoss",
        "approvedAmount",
        "deductibleApplied",
        "fraudScore",
        "fraudRiskLevel",
        "policyNumber"
    })
public class GetClaimDetailsResponse {

    @XmlElement(required = true)
    private String claimNumber;

    @XmlElement(required = true)
    private String status;

    @XmlElement(required = true)
    private String claimType;

    @XmlElement(required = true)
    private Date lossDate;

    @XmlElement(required = true)
    private Date reportedDate;

    @XmlElement(required = true)
    private String claimantFirstName;

    @XmlElement(required = true)
    private String claimantLastName;

    private String adjusterCode;

    private BigDecimal estimatedLoss;

    private BigDecimal approvedAmount;

    private BigDecimal deductibleApplied;

    private Integer fraudScore;

    private String fraudRiskLevel;

    @XmlElement(required = true)
    private String policyNumber;

    public GetClaimDetailsResponse() {
    }

    public String getClaimNumber() {
        return claimNumber;
    }

    public void setClaimNumber(String claimNumber) {
        this.claimNumber = claimNumber;
    }

    public String getStatus() {
        return status;
    }

    public void setStatus(String status) {
        this.status = status;
    }

    public String getClaimType() {
        return claimType;
    }

    public void setClaimType(String claimType) {
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

    public Integer getFraudScore() {
        return fraudScore;
    }

    public void setFraudScore(Integer fraudScore) {
        this.fraudScore = fraudScore;
    }

    public String getFraudRiskLevel() {
        return fraudRiskLevel;
    }

    public void setFraudRiskLevel(String fraudRiskLevel) {
        this.fraudRiskLevel = fraudRiskLevel;
    }

    public String getPolicyNumber() {
        return policyNumber;
    }

    public void setPolicyNumber(String policyNumber) {
        this.policyNumber = policyNumber;
    }

    @Override
    public String toString() {
        return "GetClaimDetailsResponse{" +
                "claimNumber='" + claimNumber + '\'' +
                ", status='" + status + '\'' +
                ", claimType='" + claimType + '\'' +
                ", policyNumber='" + policyNumber + '\'' +
                '}';
    }
}
