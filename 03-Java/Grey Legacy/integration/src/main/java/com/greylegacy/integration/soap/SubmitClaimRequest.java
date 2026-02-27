package com.greylegacy.integration.soap;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import java.math.BigDecimal;
import java.util.Date;

/**
 * JAXB-annotated request for submitting a new insurance claim via SOAP.
 */
@XmlRootElement(name = "submitClaimRequest", namespace = "http://www.greylegacy.com/schema/claims")
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "SubmitClaimRequest", namespace = "http://www.greylegacy.com/schema/claims",
    propOrder = {
        "policyNumber",
        "claimantFirstName",
        "claimantLastName",
        "claimType",
        "lossDate",
        "lossDescription",
        "estimatedLoss"
    })
public class SubmitClaimRequest {

    @XmlElement(required = true)
    private String policyNumber;

    @XmlElement(required = true)
    private String claimantFirstName;

    @XmlElement(required = true)
    private String claimantLastName;

    @XmlElement(required = true)
    private String claimType;

    @XmlElement(required = true)
    private Date lossDate;

    @XmlElement(required = true)
    private String lossDescription;

    @XmlElement(required = true)
    private BigDecimal estimatedLoss;

    public SubmitClaimRequest() {
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

    public String getLossDescription() {
        return lossDescription;
    }

    public void setLossDescription(String lossDescription) {
        this.lossDescription = lossDescription;
    }

    public BigDecimal getEstimatedLoss() {
        return estimatedLoss;
    }

    public void setEstimatedLoss(BigDecimal estimatedLoss) {
        this.estimatedLoss = estimatedLoss;
    }

    @Override
    public String toString() {
        return "SubmitClaimRequest{" +
                "policyNumber='" + policyNumber + '\'' +
                ", claimantFirstName='" + claimantFirstName + '\'' +
                ", claimantLastName='" + claimantLastName + '\'' +
                ", claimType='" + claimType + '\'' +
                ", lossDate=" + lossDate +
                ", estimatedLoss=" + estimatedLoss +
                '}';
    }
}
