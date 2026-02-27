package com.greylegacy.integration.soap;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import java.util.Date;

/**
 * JAXB-annotated request for validating policy coverage on a given date.
 */
@XmlRootElement(name = "validateCoverageRequest", namespace = "http://www.greylegacy.com/schema/policy")
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "ValidateCoverageRequest", namespace = "http://www.greylegacy.com/schema/policy",
    propOrder = {
        "policyNumber",
        "lossDate"
    })
public class ValidateCoverageRequest {

    @XmlElement(required = true)
    private String policyNumber;

    @XmlElement(required = true)
    private Date lossDate;

    public ValidateCoverageRequest() {
    }

    public String getPolicyNumber() {
        return policyNumber;
    }

    public void setPolicyNumber(String policyNumber) {
        this.policyNumber = policyNumber;
    }

    public Date getLossDate() {
        return lossDate;
    }

    public void setLossDate(Date lossDate) {
        this.lossDate = lossDate;
    }

    @Override
    public String toString() {
        return "ValidateCoverageRequest{" +
                "policyNumber='" + policyNumber + '\'' +
                ", lossDate=" + lossDate +
                '}';
    }
}
