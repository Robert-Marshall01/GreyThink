package com.greylegacy.integration.soap;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import java.math.BigDecimal;

/**
 * JAXB-annotated response containing claim status information.
 */
@XmlRootElement(name = "getClaimStatusResponse", namespace = "http://www.greylegacy.com/schema/claims")
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "GetClaimStatusResponse", namespace = "http://www.greylegacy.com/schema/claims",
    propOrder = {
        "claimNumber",
        "status",
        "adjusterCode",
        "estimatedLoss",
        "approvedAmount"
    })
public class GetClaimStatusResponse {

    @XmlElement(required = true)
    private String claimNumber;

    @XmlElement(required = true)
    private String status;

    private String adjusterCode;

    private BigDecimal estimatedLoss;

    private BigDecimal approvedAmount;

    public GetClaimStatusResponse() {
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

    @Override
    public String toString() {
        return "GetClaimStatusResponse{" +
                "claimNumber='" + claimNumber + '\'' +
                ", status='" + status + '\'' +
                ", adjusterCode='" + adjusterCode + '\'' +
                ", estimatedLoss=" + estimatedLoss +
                ", approvedAmount=" + approvedAmount +
                '}';
    }
}
