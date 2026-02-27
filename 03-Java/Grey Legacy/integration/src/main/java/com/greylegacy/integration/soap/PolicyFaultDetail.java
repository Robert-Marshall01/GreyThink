package com.greylegacy.integration.soap;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

/**
 * JAXB-annotated fault detail for policy service SOAP faults.
 */
@XmlRootElement(name = "policyFault", namespace = "http://www.greylegacy.com/schema/policy")
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "PolicyFault", namespace = "http://www.greylegacy.com/schema/policy",
    propOrder = {
        "errorCode",
        "errorMessage"
    })
public class PolicyFaultDetail {

    @XmlElement(required = true)
    private String errorCode;

    @XmlElement(required = true)
    private String errorMessage;

    public PolicyFaultDetail() {
    }

    public PolicyFaultDetail(String errorCode, String errorMessage) {
        this.errorCode = errorCode;
        this.errorMessage = errorMessage;
    }

    public String getErrorCode() {
        return errorCode;
    }

    public String getErrorMessage() {
        return errorMessage;
    }

    @Override
    public String toString() {
        return "PolicyFaultDetail{" +
                "errorCode='" + errorCode + '\'' +
                ", errorMessage='" + errorMessage + '\'' +
                '}';
    }
}
