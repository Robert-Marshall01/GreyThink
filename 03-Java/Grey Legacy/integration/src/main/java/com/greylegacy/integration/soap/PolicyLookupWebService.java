package com.greylegacy.integration.soap;

import javax.jws.WebMethod;
import javax.jws.WebParam;
import javax.jws.WebResult;
import javax.jws.WebService;
import javax.jws.soap.SOAPBinding;

/**
 * SOAP web service interface for policy lookup operations.
 * Used by external systems to verify policy data and coverage validity.
 */
@WebService(
    name = "PolicyLookupService",
    targetNamespace = "http://www.greylegacy.com/services/policy"
)
@SOAPBinding(style = SOAPBinding.Style.DOCUMENT, use = SOAPBinding.Use.LITERAL)
public interface PolicyLookupWebService {

    @WebMethod(operationName = "lookupPolicy")
    @WebResult(name = "policyLookupResponse")
    PolicyLookupResponse lookupPolicy(
        @WebParam(name = "policyLookupRequest") PolicyLookupRequest request
    ) throws PolicyServiceFault;

    @WebMethod(operationName = "validateCoverage")
    @WebResult(name = "validateCoverageResponse")
    ValidateCoverageResponse validateCoverage(
        @WebParam(name = "validateCoverageRequest") ValidateCoverageRequest request
    ) throws PolicyServiceFault;
}
