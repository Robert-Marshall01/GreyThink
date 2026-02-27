package com.greylegacy.integration.soap;

import javax.xml.ws.WebFault;

/**
 * SOAP fault for claim service errors.
 */
@WebFault(
    name = "ClaimServiceFault",
    targetNamespace = "http://www.greylegacy.com/schema/claims"
)
public class ClaimServiceFault extends Exception {

    private static final long serialVersionUID = 1L;

    private ClaimFaultDetail faultDetail;

    public ClaimServiceFault(String message, ClaimFaultDetail faultDetail) {
        super(message);
        this.faultDetail = faultDetail;
    }

    public ClaimServiceFault(String message, ClaimFaultDetail faultDetail, Throwable cause) {
        super(message, cause);
        this.faultDetail = faultDetail;
    }

    public ClaimFaultDetail getFaultInfo() {
        return faultDetail;
    }
}
