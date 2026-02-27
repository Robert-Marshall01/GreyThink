package com.greylegacy.integration.soap;

import javax.jws.WebMethod;
import javax.jws.WebParam;
import javax.jws.WebResult;
import javax.jws.WebService;
import javax.jws.soap.SOAPBinding;

/**
 * SOAP web service interface for external claim operations.
 * Used by partner systems, regulatory bodies, and third-party integrations.
 */
@WebService(
    name = "ClaimService",
    targetNamespace = "http://www.greylegacy.com/services/claims"
)
@SOAPBinding(style = SOAPBinding.Style.DOCUMENT, use = SOAPBinding.Use.LITERAL)
public interface ClaimWebService {

    @WebMethod(operationName = "submitClaim")
    @WebResult(name = "submitClaimResponse")
    SubmitClaimResponse submitClaim(
        @WebParam(name = "submitClaimRequest") SubmitClaimRequest request
    ) throws ClaimServiceFault;

    @WebMethod(operationName = "getClaimStatus")
    @WebResult(name = "getClaimStatusResponse")
    GetClaimStatusResponse getClaimStatus(
        @WebParam(name = "getClaimStatusRequest") GetClaimStatusRequest request
    ) throws ClaimServiceFault;

    @WebMethod(operationName = "getClaimDetails")
    @WebResult(name = "getClaimDetailsResponse")
    GetClaimDetailsResponse getClaimDetails(
        @WebParam(name = "getClaimDetailsRequest") GetClaimDetailsRequest request
    ) throws ClaimServiceFault;

    @WebMethod(operationName = "updateClaimStatus")
    @WebResult(name = "updateClaimStatusResponse")
    UpdateClaimStatusResponse updateClaimStatus(
        @WebParam(name = "updateClaimStatusRequest") UpdateClaimStatusRequest request
    ) throws ClaimServiceFault;
}
