package com.greylegacy.service;

import com.greylegacy.domain.Policy;
import com.greylegacy.domain.PolicyStatus;
import java.math.BigDecimal;
import java.util.List;

/**
 * Underwriting service for policy management and risk assessment.
 */
public interface UnderwritingService {
    Policy createPolicy(PolicyCreationRequest request);
    void cancelPolicy(String policyNumber, String reason, String cancelledBy);
    void suspendPolicy(String policyNumber, String reason, String suspendedBy);
    Policy renewPolicy(String policyNumber);
    BigDecimal recalculatePremium(String policyNumber);
    List<Policy> findPoliciesForReview();
    Policy findByPolicyNumber(String policyNumber);
}
