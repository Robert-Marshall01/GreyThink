package com.greylegacy.integration.soap;

import com.greylegacy.domain.Claim;
import com.greylegacy.domain.ClaimStatus;
import com.greylegacy.domain.ClaimType;
import com.greylegacy.service.ClaimService;
import com.greylegacy.service.FnolRequest;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;

import javax.jws.WebService;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

/**
 * Implementation of the {@link ClaimWebService} SOAP endpoint.
 * Delegates to the internal {@link ClaimService} for business logic.
 */
@WebService(
    endpointInterface = "com.greylegacy.integration.soap.ClaimWebService",
    serviceName = "ClaimService",
    portName = "ClaimServicePort",
    targetNamespace = "http://www.greylegacy.com/services/claims"
)
public class ClaimWebServiceImpl implements ClaimWebService {

    private static final Logger LOG = LoggerFactory.getLogger(ClaimWebServiceImpl.class);

    @Autowired
    private ClaimService claimService;

    @Override
    public SubmitClaimResponse submitClaim(SubmitClaimRequest request) throws ClaimServiceFault {
        LOG.info("SOAP submitClaim invoked for policy {}", request.getPolicyNumber());
        try {
            validateSubmitRequest(request);

            FnolRequest fnol = new FnolRequest();
            fnol.setPolicyNumber(request.getPolicyNumber());
            fnol.setClaimantFirstName(request.getClaimantFirstName());
            fnol.setClaimantLastName(request.getClaimantLastName());
            fnol.setClaimType(ClaimType.valueOf(request.getClaimType()));
            fnol.setLossDate(request.getLossDate());
            fnol.setLossDescription(request.getLossDescription());
            fnol.setEstimatedLoss(request.getEstimatedLoss());
            fnol.setReportedBy("SOAP_INTEGRATION");

            Claim claim = claimService.submitFnol(fnol);

            SubmitClaimResponse response = new SubmitClaimResponse();
            response.setClaimNumber(claim.getClaimNumber());
            response.setStatus(claim.getStatus().name());
            response.setMessage("Claim submitted successfully");
            return response;

        } catch (ClaimServiceFault csf) {
            throw csf;
        } catch (IllegalArgumentException e) {
            LOG.error("Invalid claim type in SOAP request", e);
            throw buildValidationFault("VALIDATION.INVALID_ENUM",
                    "Invalid claim type: " + request.getClaimType(), "claimType");
        } catch (Exception e) {
            LOG.error("Error processing submitClaim SOAP request", e);
            throw buildSystemFault("SYSTEM.INTERNAL_ERROR",
                    "Failed to submit claim: " + e.getMessage(), true);
        }
    }

    @Override
    public GetClaimStatusResponse getClaimStatus(GetClaimStatusRequest request) throws ClaimServiceFault {
        LOG.info("SOAP getClaimStatus invoked for claim {}", request.getClaimNumber());
        try {
            if (request.getClaimNumber() == null || request.getClaimNumber().trim().isEmpty()) {
                throw buildValidationFault("VALIDATION.MISSING_FIELD",
                        "Claim number is required", "claimNumber");
            }

            Claim claim = claimService.findByClaimNumber(request.getClaimNumber());
            if (claim == null) {
                throw buildBusinessFault("BUSINESS.CLAIM_NOT_FOUND",
                    "No claim found with number: " + request.getClaimNumber());
            }

            GetClaimStatusResponse response = new GetClaimStatusResponse();
            response.setClaimNumber(claim.getClaimNumber());
            response.setStatus(claim.getStatus().name());
            response.setAdjusterCode(claim.getAdjusterCode());
            response.setEstimatedLoss(claim.getEstimatedLoss());
            response.setApprovedAmount(claim.getApprovedAmount());
            return response;

        } catch (ClaimServiceFault csf) {
            throw csf;
        } catch (Exception e) {
            LOG.error("Error processing getClaimStatus SOAP request", e);
            throw buildSystemFault("SYSTEM.INTERNAL_ERROR",
                "Failed to retrieve claim status: " + e.getMessage(), true);
        }
    }

    @Override
    public GetClaimDetailsResponse getClaimDetails(GetClaimDetailsRequest request) throws ClaimServiceFault {
        LOG.info("SOAP getClaimDetails invoked for claim {}", request.getClaimNumber());
        try {
            if (request.getClaimNumber() == null || request.getClaimNumber().trim().isEmpty()) {
                throw buildValidationFault("VALIDATION.MISSING_FIELD",
                        "Claim number is required", "claimNumber");
            }

            Claim claim = claimService.findByClaimNumber(request.getClaimNumber());
            if (claim == null) {
                throw buildBusinessFault("BUSINESS.CLAIM_NOT_FOUND",
                    "No claim found with number: " + request.getClaimNumber());
            }

            GetClaimDetailsResponse response = new GetClaimDetailsResponse();
            response.setClaimNumber(claim.getClaimNumber());
            response.setStatus(claim.getStatus().name());
            response.setClaimType(claim.getClaimType().name());
            response.setLossDate(claim.getLossDate());
            response.setReportedDate(claim.getReportedDate());
            response.setClaimantFirstName(claim.getClaimantFirstName());
            response.setClaimantLastName(claim.getClaimantLastName());
            response.setAdjusterCode(claim.getAdjusterCode());
            response.setEstimatedLoss(claim.getEstimatedLoss());
            response.setApprovedAmount(claim.getApprovedAmount());
            response.setDeductibleApplied(claim.getDeductibleApplied());
            response.setFraudScore(claim.getFraudScore());
            response.setFraudRiskLevel(
                claim.getFraudRiskLevel() != null ? claim.getFraudRiskLevel().name() : null);
            response.setPolicyNumber(
                claim.getPolicy() != null ? claim.getPolicy().getPolicyNumber() : null);
            return response;

        } catch (ClaimServiceFault csf) {
            throw csf;
        } catch (Exception e) {
            LOG.error("Error processing getClaimDetails SOAP request", e);
            throw buildSystemFault("SYSTEM.INTERNAL_ERROR",
                "Failed to retrieve claim details: " + e.getMessage(), true);
        }
    }

    // -------------------------------------------------------------------------
    // Helpers
    // -------------------------------------------------------------------------

    /**
     * Claim lifecycle state machine: defines valid status transitions.
     *
     * <pre>
     *   FNOL_RECEIVED  → UNDER_REVIEW
     *   UNDER_REVIEW   → ADJUSTER_ASSIGNED, DENIED
     *   ADJUSTER_ASSIGNED → INVESTIGATION, APPROVED, DENIED
     *   INVESTIGATION  → APPROVED, DENIED, FRAUD_SUSPECTED
     *   APPROVED       → SETTLED, REOPENED
     *   DENIED         → REOPENED, CLOSED
     *   SETTLED        → CLOSED, REOPENED
     *   FRAUD_SUSPECTED → INVESTIGATION, DENIED, CLOSED
     *   REOPENED       → UNDER_REVIEW
     *   CLOSED         → REOPENED (exceptional — requires reason)
     * </pre>
     */
    private static final Map<String, Set<String>> VALID_TRANSITIONS;
    static {
        Map<String, Set<String>> m = new HashMap<String, Set<String>>();
        m.put("FNOL_RECEIVED",     new HashSet<String>(Arrays.asList("UNDER_REVIEW")));
        m.put("UNDER_REVIEW",      new HashSet<String>(Arrays.asList("ADJUSTER_ASSIGNED", "DENIED")));
        m.put("ADJUSTER_ASSIGNED", new HashSet<String>(Arrays.asList("INVESTIGATION", "APPROVED", "DENIED")));
        m.put("INVESTIGATION",     new HashSet<String>(Arrays.asList("APPROVED", "DENIED", "FRAUD_SUSPECTED")));
        m.put("APPROVED",          new HashSet<String>(Arrays.asList("SETTLED", "REOPENED")));
        m.put("DENIED",            new HashSet<String>(Arrays.asList("REOPENED", "CLOSED")));
        m.put("SETTLED",           new HashSet<String>(Arrays.asList("CLOSED", "REOPENED")));
        m.put("FRAUD_SUSPECTED",   new HashSet<String>(Arrays.asList("INVESTIGATION", "DENIED", "CLOSED")));
        m.put("REOPENED",          new HashSet<String>(Arrays.asList("UNDER_REVIEW")));
        m.put("CLOSED",            new HashSet<String>(Arrays.asList("REOPENED")));
        VALID_TRANSITIONS = Collections.unmodifiableMap(m);
    }

    @Override
    public UpdateClaimStatusResponse updateClaimStatus(UpdateClaimStatusRequest request)
            throws ClaimServiceFault {
        LOG.info("SOAP updateClaimStatus invoked for claim {} → {}",
                request.getClaimNumber(), request.getNewStatus());
        try {
            // Validate required fields
            if (request.getClaimNumber() == null || request.getClaimNumber().trim().isEmpty()) {
                throw buildValidationFault("VALIDATION.MISSING_FIELD",
                        "Claim number is required", "claimNumber");
            }
            if (request.getNewStatus() == null || request.getNewStatus().trim().isEmpty()) {
                throw buildValidationFault("VALIDATION.MISSING_FIELD",
                        "New status is required", "newStatus");
            }
            if (request.getUpdatedBy() == null || request.getUpdatedBy().trim().isEmpty()) {
                throw buildValidationFault("VALIDATION.MISSING_FIELD",
                        "Updated-by user is required", "updatedBy");
            }

            // Validate the target status is a known enum value
            ClaimStatus newStatus;
            try {
                newStatus = ClaimStatus.valueOf(request.getNewStatus());
            } catch (IllegalArgumentException e) {
                throw buildValidationFault("VALIDATION.INVALID_ENUM",
                        "Invalid claim status: " + request.getNewStatus(), "newStatus");
            }

            // Look up the claim
            Claim claim = claimService.findByClaimNumber(request.getClaimNumber());
            if (claim == null) {
                throw buildBusinessFault("BUSINESS.CLAIM_NOT_FOUND",
                        "No claim found with number: " + request.getClaimNumber());
            }

            // Validate the status transition against the state machine
            String currentStatusName = claim.getStatus().name();
            Set<String> allowedTargets = VALID_TRANSITIONS.get(currentStatusName);
            if (allowedTargets == null || !allowedTargets.contains(request.getNewStatus())) {
                throw buildBusinessFault("BUSINESS.INVALID_STATUS_TRANSITION",
                        "Cannot transition from " + currentStatusName
                        + " to " + request.getNewStatus()
                        + ". Allowed transitions: "
                        + (allowedTargets != null ? allowedTargets : "none"));
            }

            // Reopening a CLOSED claim requires a reason
            if ("CLOSED".equals(currentStatusName) && "REOPENED".equals(request.getNewStatus())) {
                if (request.getReason() == null || request.getReason().trim().isEmpty()) {
                    throw buildValidationFault("VALIDATION.MISSING_FIELD",
                            "Reason is required when reopening a closed claim", "reason");
                }
            }

            // Perform the transition
            String previousStatus = currentStatusName;
            claim.setStatus(newStatus);
            claimService.updateClaim(claim);

            LOG.info("Claim {} status transitioned: {} → {}",
                    claim.getClaimNumber(), previousStatus, newStatus.name());

            UpdateClaimStatusResponse response = new UpdateClaimStatusResponse();
            response.setClaimNumber(claim.getClaimNumber());
            response.setPreviousStatus(previousStatus);
            response.setCurrentStatus(newStatus.name());
            response.setTransitionDate(new Date());
            response.setMessage("Status updated successfully from "
                    + previousStatus + " to " + newStatus.name());
            return response;

        } catch (ClaimServiceFault csf) {
            throw csf;
        } catch (Exception e) {
            LOG.error("Error processing updateClaimStatus SOAP request", e);
            throw buildSystemFault("SYSTEM.INTERNAL_ERROR",
                    "Failed to update claim status: " + e.getMessage(), true);
        }
    }

    private void validateSubmitRequest(SubmitClaimRequest request) throws ClaimServiceFault {
        if (request.getPolicyNumber() == null || request.getPolicyNumber().trim().isEmpty()) {
            throw buildValidationFault("VALIDATION.MISSING_FIELD",
                    "Policy number is required", "policyNumber");
        }
        if (request.getClaimantFirstName() == null || request.getClaimantFirstName().trim().isEmpty()) {
            throw buildValidationFault("VALIDATION.MISSING_FIELD",
                    "Claimant first name is required", "claimantFirstName");
        }
        if (request.getClaimantLastName() == null || request.getClaimantLastName().trim().isEmpty()) {
            throw buildValidationFault("VALIDATION.MISSING_FIELD",
                    "Claimant last name is required", "claimantLastName");
        }
        if (request.getClaimType() == null || request.getClaimType().trim().isEmpty()) {
            throw buildValidationFault("VALIDATION.MISSING_FIELD",
                    "Claim type is required", "claimType");
        }
        if (request.getLossDate() == null) {
            throw buildValidationFault("VALIDATION.MISSING_FIELD",
                    "Loss date is required", "lossDate");
        }
    }

    private ClaimServiceFault buildFault(String errorCode, String errorMessage) {
        ClaimFaultDetail detail = new ClaimFaultDetail(errorCode, errorMessage, new Date());
        return new ClaimServiceFault(errorMessage, detail);
    }

    /**
     * Builds a validation-category SOAP fault with the offending field name.
     */
    private ClaimServiceFault buildValidationFault(String errorCode, String errorMessage,
                                                    String fieldName) {
        ClaimFaultDetail detail = new ClaimFaultDetail(
                errorCode, errorMessage, "ERROR", new Date(), null, fieldName, false);
        return new ClaimServiceFault(errorMessage, detail);
    }

    /**
     * Builds a business-rule SOAP fault (non-retryable).
     */
    private ClaimServiceFault buildBusinessFault(String errorCode, String errorMessage) {
        ClaimFaultDetail detail = new ClaimFaultDetail(
                errorCode, errorMessage, "ERROR", new Date(), null, null, false);
        return new ClaimServiceFault(errorMessage, detail);
    }

    /**
     * Builds a system-level SOAP fault.
     *
     * @param retryable whether the client should retry
     */
    private ClaimServiceFault buildSystemFault(String errorCode, String errorMessage,
                                                boolean retryable) {
        ClaimFaultDetail detail = new ClaimFaultDetail(
                errorCode, errorMessage, "FATAL", new Date(), null, null, retryable);
        return new ClaimServiceFault(errorMessage, detail);
    }
}
