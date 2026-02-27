package com.greylegacy.service;

import com.greylegacy.domain.ClaimType;
import java.math.BigDecimal;
import java.util.Date;

/**
 * Data Transfer Object for First Notice of Loss (FNOL) submissions.
 */
public class FnolRequest {

    private String policyNumber;
    private String claimantFirstName;
    private String claimantLastName;
    private String claimantPhone;
    private String claimantEmail;
    private ClaimType claimType;
    private Date lossDate;
    private String lossDescription;
    private String lossLocation;
    private BigDecimal estimatedLoss;
    private String reportedBy;

    public FnolRequest() {
    }

    public String getPolicyNumber() {
        return policyNumber;
    }

    public void setPolicyNumber(String policyNumber) {
        this.policyNumber = policyNumber;
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

    public BigDecimal getEstimatedLoss() {
        return estimatedLoss;
    }

    public void setEstimatedLoss(BigDecimal estimatedLoss) {
        this.estimatedLoss = estimatedLoss;
    }

    public String getReportedBy() {
        return reportedBy;
    }

    public void setReportedBy(String reportedBy) {
        this.reportedBy = reportedBy;
    }

    @Override
    public String toString() {
        return "FnolRequest{" +
                "policyNumber='" + policyNumber + '\'' +
                ", claimantFirstName='" + claimantFirstName + '\'' +
                ", claimantLastName='" + claimantLastName + '\'' +
                ", claimantPhone='" + claimantPhone + '\'' +
                ", claimantEmail='" + claimantEmail + '\'' +
                ", claimType=" + claimType +
                ", lossDate=" + lossDate +
                ", lossDescription='" + lossDescription + '\'' +
                ", lossLocation='" + lossLocation + '\'' +
                ", estimatedLoss=" + estimatedLoss +
                ", reportedBy='" + reportedBy + '\'' +
                '}';
    }
}
