package com.greylegacy.integration.soap;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

/**
 * JAXB-annotated response for coverage validation results.
 */
@XmlRootElement(name = "validateCoverageResponse", namespace = "http://www.greylegacy.com/schema/policy")
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "ValidateCoverageResponse", namespace = "http://www.greylegacy.com/schema/policy",
    propOrder = {
        "valid",
        "policyStatus",
        "reason"
    })
public class ValidateCoverageResponse {

    @XmlElement(required = true)
    private boolean valid;

    @XmlElement(required = true)
    private String policyStatus;

    private String reason;

    public ValidateCoverageResponse() {
    }

    public boolean isValid() {
        return valid;
    }

    public void setValid(boolean valid) {
        this.valid = valid;
    }

    public String getPolicyStatus() {
        return policyStatus;
    }

    public void setPolicyStatus(String policyStatus) {
        this.policyStatus = policyStatus;
    }

    public String getReason() {
        return reason;
    }

    public void setReason(String reason) {
        this.reason = reason;
    }

    @Override
    public String toString() {
        return "ValidateCoverageResponse{" +
                "valid=" + valid +
                ", policyStatus='" + policyStatus + '\'' +
                ", reason='" + reason + '\'' +
                '}';
    }
}
