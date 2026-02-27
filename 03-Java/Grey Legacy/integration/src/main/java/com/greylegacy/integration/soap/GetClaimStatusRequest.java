package com.greylegacy.integration.soap;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

/**
 * JAXB-annotated request for retrieving claim status via SOAP.
 */
@XmlRootElement(name = "getClaimStatusRequest", namespace = "http://www.greylegacy.com/schema/claims")
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "GetClaimStatusRequest", namespace = "http://www.greylegacy.com/schema/claims",
    propOrder = { "claimNumber" })
public class GetClaimStatusRequest {

    @XmlElement(required = true)
    private String claimNumber;

    public GetClaimStatusRequest() {
    }

    public String getClaimNumber() {
        return claimNumber;
    }

    public void setClaimNumber(String claimNumber) {
        this.claimNumber = claimNumber;
    }

    @Override
    public String toString() {
        return "GetClaimStatusRequest{claimNumber='" + claimNumber + "'}";
    }
}
