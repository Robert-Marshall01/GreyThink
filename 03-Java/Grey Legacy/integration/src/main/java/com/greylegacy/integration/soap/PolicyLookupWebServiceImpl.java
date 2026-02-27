package com.greylegacy.integration.soap;

import com.greylegacy.domain.Policy;
import com.greylegacy.service.UnderwritingService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;

import javax.jws.WebService;
import java.util.Date;

/**
 * Implementation of the {@link PolicyLookupWebService} SOAP endpoint.
 * Delegates to the internal {@link UnderwritingService} for policy data.
 */
@WebService(
    endpointInterface = "com.greylegacy.integration.soap.PolicyLookupWebService",
    serviceName = "PolicyLookupService",
    portName = "PolicyLookupServicePort",
    targetNamespace = "http://www.greylegacy.com/services/policy"
)
public class PolicyLookupWebServiceImpl implements PolicyLookupWebService {

    private static final Logger LOG = LoggerFactory.getLogger(PolicyLookupWebServiceImpl.class);

    @Autowired
    private UnderwritingService underwritingService;

    @Override
    public PolicyLookupResponse lookupPolicy(PolicyLookupRequest request) throws PolicyServiceFault {
        LOG.info("SOAP lookupPolicy invoked for policy {}", request.getPolicyNumber());
        try {
            if (request.getPolicyNumber() == null || request.getPolicyNumber().trim().isEmpty()) {
                throw buildFault("MISSING_POLICY_NUMBER", "Policy number is required");
            }

            Policy policy = underwritingService.findByPolicyNumber(request.getPolicyNumber());
            if (policy == null) {
                throw buildFault("POLICY_NOT_FOUND",
                    "No policy found with number: " + request.getPolicyNumber());
            }

            PolicyLookupResponse response = new PolicyLookupResponse();
            response.setPolicyNumber(policy.getPolicyNumber());
            response.setStatus(policy.getStatus().name());
            response.setHolderName(policy.getHolderFirstName() + " " + policy.getHolderLastName());
            response.setPolicyType(policy.getPolicyType().name());
            response.setEffectiveDate(policy.getEffectiveDate());
            response.setExpirationDate(policy.getExpirationDate());
            response.setPremiumAmount(policy.getPremiumAmount());
            response.setCoverageLimit(policy.getCoverageLimit());
            response.setDeductible(policy.getDeductible());
            return response;

        } catch (PolicyServiceFault psf) {
            throw psf;
        } catch (Exception e) {
            LOG.error("Error processing lookupPolicy SOAP request", e);
            throw buildFault("LOOKUP_FAILED", "Failed to look up policy: " + e.getMessage());
        }
    }

    @Override
    public ValidateCoverageResponse validateCoverage(ValidateCoverageRequest request) throws PolicyServiceFault {
        LOG.info("SOAP validateCoverage invoked for policy {} on date {}",
            request.getPolicyNumber(), request.getLossDate());
        try {
            if (request.getPolicyNumber() == null || request.getPolicyNumber().trim().isEmpty()) {
                throw buildFault("MISSING_POLICY_NUMBER", "Policy number is required");
            }
            if (request.getLossDate() == null) {
                throw buildFault("MISSING_LOSS_DATE", "Loss date is required");
            }

            Policy policy = underwritingService.findByPolicyNumber(request.getPolicyNumber());
            if (policy == null) {
                throw buildFault("POLICY_NOT_FOUND",
                    "No policy found with number: " + request.getPolicyNumber());
            }

            ValidateCoverageResponse response = new ValidateCoverageResponse();
            response.setPolicyStatus(policy.getStatus().name());

            boolean valid = policy.isCoverageValid(request.getLossDate());
            response.setValid(valid);

            if (!valid) {
                if (!policy.isActive()) {
                    response.setReason("Policy is not active. Current status: " + policy.getStatus().name());
                } else {
                    response.setReason("Loss date is outside the policy coverage period ("
                        + policy.getEffectiveDate() + " to " + policy.getExpirationDate() + ")");
                }
            }

            return response;

        } catch (PolicyServiceFault psf) {
            throw psf;
        } catch (Exception e) {
            LOG.error("Error processing validateCoverage SOAP request", e);
            throw buildFault("VALIDATION_FAILED",
                "Failed to validate coverage: " + e.getMessage());
        }
    }

    // -------------------------------------------------------------------------
    // Helpers
    // -------------------------------------------------------------------------

    private PolicyServiceFault buildFault(String errorCode, String errorMessage) {
        PolicyFaultDetail detail = new PolicyFaultDetail(errorCode, errorMessage);
        return new PolicyServiceFault(errorMessage, detail);
    }
}
