package com.greylegacy.integration.soap;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

/**
 * JAXB-annotated response returned after submitting a claim via SOAP.
 */
@XmlRootElement(name = "submitClaimResponse", namespace = "http://www.greylegacy.com/schema/claims")
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "SubmitClaimResponse", namespace = "http://www.greylegacy.com/schema/claims",
    propOrder = {
        "claimNumber",
        "status",
        "message"
    })
public class SubmitClaimResponse {

    @XmlElement(required = true)
    private String claimNumber;

    @XmlElement(required = true)
    private String status;

    @XmlElement(required = true)
    private String message;

    public SubmitClaimResponse() {
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

    public String getMessage() {
        return message;
    }

    public void setMessage(String message) {
        this.message = message;
    }

    @Override
    public String toString() {
        return "SubmitClaimResponse{" +
                "claimNumber='" + claimNumber + '\'' +
                ", status='" + status + '\'' +
                ", message='" + message + '\'' +
                '}';
    }
}
