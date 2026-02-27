package com.greylegacy.integration.soap;

import javax.xml.ws.WebFault;

/**
 * SOAP fault for policy service errors.
 */
@WebFault(
    name = "PolicyServiceFault",
    targetNamespace = "http://www.greylegacy.com/schema/policy"
)
public class PolicyServiceFault extends Exception {

    private static final long serialVersionUID = 1L;

    private PolicyFaultDetail faultDetail;

    public PolicyServiceFault(String message, PolicyFaultDetail faultDetail) {
        super(message);
        this.faultDetail = faultDetail;
    }

    public PolicyServiceFault(String message, PolicyFaultDetail faultDetail, Throwable cause) {
        super(message, cause);
        this.faultDetail = faultDetail;
    }

    public PolicyFaultDetail getFaultInfo() {
        return faultDetail;
    }
}
