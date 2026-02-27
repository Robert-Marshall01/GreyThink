package com.greylegacy.integration.soap;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

/**
 * JAXB-annotated request for retrieving full claim details via SOAP.
 */
@XmlRootElement(name = "getClaimDetailsRequest", namespace = "http://www.greylegacy.com/schema/claims")
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "GetClaimDetailsRequest", namespace = "http://www.greylegacy.com/schema/claims",
    propOrder = { "claimNumber" })
public class GetClaimDetailsRequest {

    @XmlElement(required = true)
    private String claimNumber;

    public GetClaimDetailsRequest() {
    }

    public String getClaimNumber() {
        return claimNumber;
    }

    public void setClaimNumber(String claimNumber) {
        this.claimNumber = claimNumber;
    }

    @Override
    public String toString() {
        return "GetClaimDetailsRequest{claimNumber='" + claimNumber + "'}";
    }
}
