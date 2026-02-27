package com.greylegacy.integration.soap;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

/**
 * JAXB-annotated request for looking up policy information via SOAP.
 */
@XmlRootElement(name = "policyLookupRequest", namespace = "http://www.greylegacy.com/schema/policy")
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "PolicyLookupRequest", namespace = "http://www.greylegacy.com/schema/policy",
    propOrder = { "policyNumber" })
public class PolicyLookupRequest {

    @XmlElement(required = true)
    private String policyNumber;

    public PolicyLookupRequest() {
    }

    public String getPolicyNumber() {
        return policyNumber;
    }

    public void setPolicyNumber(String policyNumber) {
        this.policyNumber = policyNumber;
    }

    @Override
    public String toString() {
        return "PolicyLookupRequest{policyNumber='" + policyNumber + "'}";
    }
}
