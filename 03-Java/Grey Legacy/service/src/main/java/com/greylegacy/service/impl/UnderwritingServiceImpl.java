package com.greylegacy.service.impl;

import com.greylegacy.dao.ClaimDao;
import com.greylegacy.dao.PolicyDao;
import com.greylegacy.domain.*;
import com.greylegacy.service.PolicyCreationRequest;
import com.greylegacy.service.UnderwritingService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.*;

/**
 * Underwriting service implementation for policy lifecycle management
 * and risk-based premium calculation.
 */
@Service("underwritingService")
@Transactional(propagation = Propagation.REQUIRED)
public class UnderwritingServiceImpl implements UnderwritingService {

    private static final Logger log = LoggerFactory.getLogger(UnderwritingServiceImpl.class);
    private static final String POLICY_PREFIX = "POL";

    @Autowired
    private PolicyDao policyDao;

    @Autowired
    private ClaimDao claimDao;

    @Override
    public Policy createPolicy(PolicyCreationRequest request) {
        log.info("Creating new policy for holder: {} {}", request.getHolderFirstName(), request.getHolderLastName());

        // Validate dates
        if (request.getEffectiveDate() == null) {
            throw new IllegalArgumentException("Effective date is required");
        }
        if (request.getExpirationDate() == null) {
            throw new IllegalArgumentException("Expiration date is required");
        }
        if (!request.getExpirationDate().after(request.getEffectiveDate())) {
            throw new IllegalArgumentException("Expiration date must be after effective date");
        }

        Policy policy = new Policy();
        policy.setPolicyNumber(generatePolicyNumber());
        policy.setPolicyType(request.getPolicyType());
        policy.setStatus(PolicyStatus.ACTIVE);
        policy.setHolderFirstName(request.getHolderFirstName());
        policy.setHolderLastName(request.getHolderLastName());
        policy.setHolderSsn(request.getHolderSsn());
        policy.setHolderEmail(request.getHolderEmail());
        policy.setHolderPhone(request.getHolderPhone());
        policy.setEffectiveDate(request.getEffectiveDate());
        policy.setExpirationDate(request.getExpirationDate());
        policy.setPremiumAmount(request.getPremiumAmount());
        policy.setCoverageLimit(request.getCoverageLimit());
        policy.setDeductible(request.getDeductible());
        policy.setAgentCode(request.getAgentCode());
        policy.setUnderwriterCode(request.getUnderwriterCode());
        policy.setCreatedDate(new Date());

        policyDao.save(policy);

        log.info("Policy created successfully. Policy number: {}", policy.getPolicyNumber());
        return policy;
    }

    @Override
    public void cancelPolicy(String policyNumber, String reason, String cancelledBy) {
        log.info("Cancelling policy: {}", policyNumber);

        Policy policy = policyDao.findByPolicyNumber(policyNumber);
        if (policy == null) {
            throw new IllegalArgumentException("Policy not found: " + policyNumber);
        }

        if (PolicyStatus.CANCELLED.equals(policy.getStatus())) {
            throw new IllegalStateException("Policy is already cancelled");
        }

        policy.setStatus(PolicyStatus.CANCELLED);
        policy.setCancellationDate(new Date());
        policy.setCancellationReason(reason);
        policyDao.update(policy);

        log.info("Policy {} cancelled by {}. Reason: {}", policyNumber, cancelledBy, reason);
    }

    @Override
    public void suspendPolicy(String policyNumber, String reason, String suspendedBy) {
        log.info("Suspending policy: {}", policyNumber);

        Policy policy = policyDao.findByPolicyNumber(policyNumber);
        if (policy == null) {
            throw new IllegalArgumentException("Policy not found: " + policyNumber);
        }

        if (!PolicyStatus.ACTIVE.equals(policy.getStatus())) {
            throw new IllegalStateException("Can only suspend ACTIVE policies. Current status: " + policy.getStatus());
        }

        policy.setStatus(PolicyStatus.SUSPENDED);
        policyDao.update(policy);

        log.info("Policy {} suspended by {}. Reason: {}", policyNumber, suspendedBy, reason);
    }

    @Override
    public Policy renewPolicy(String policyNumber) {
        log.info("Renewing policy: {}", policyNumber);

        Policy existing = policyDao.findByPolicyNumber(policyNumber);
        if (existing == null) {
            throw new IllegalArgumentException("Policy not found: " + policyNumber);
        }

        // Mark existing policy as expired
        existing.setStatus(PolicyStatus.EXPIRED);
        policyDao.update(existing);

        // Create renewed policy with dates shifted +1 year
        Policy renewed = new Policy();
        renewed.setPolicyNumber(generatePolicyNumber());
        renewed.setPolicyType(existing.getPolicyType());
        renewed.setStatus(PolicyStatus.ACTIVE);
        renewed.setHolderFirstName(existing.getHolderFirstName());
        renewed.setHolderLastName(existing.getHolderLastName());
        renewed.setHolderSsn(existing.getHolderSsn());
        renewed.setHolderEmail(existing.getHolderEmail());
        renewed.setHolderPhone(existing.getHolderPhone());
        renewed.setCoverageLimit(existing.getCoverageLimit());
        renewed.setDeductible(existing.getDeductible());
        renewed.setAgentCode(existing.getAgentCode());
        renewed.setUnderwriterCode(existing.getUnderwriterCode());
        renewed.setCreatedDate(new Date());

        // Shift dates forward by one year
        Calendar cal = Calendar.getInstance();
        cal.setTime(existing.getExpirationDate());
        renewed.setEffectiveDate(cal.getTime());
        cal.add(Calendar.YEAR, 1);
        renewed.setExpirationDate(cal.getTime());

        // Recalculate premium for the renewed policy
        BigDecimal newPremium = calculateRiskAdjustedPremium(existing);
        renewed.setPremiumAmount(newPremium);

        policyDao.save(renewed);

        log.info("Policy {} renewed as {}. New premium: ${}", policyNumber, renewed.getPolicyNumber(), newPremium);
        return renewed;
    }

    @Override
    public BigDecimal recalculatePremium(String policyNumber) {
        log.info("Recalculating premium for policy: {}", policyNumber);

        Policy policy = policyDao.findByPolicyNumber(policyNumber);
        if (policy == null) {
            throw new IllegalArgumentException("Policy not found: " + policyNumber);
        }

        BigDecimal newPremium = calculateRiskAdjustedPremium(policy);
        policy.setPremiumAmount(newPremium);
        policyDao.update(policy);

        log.info("Premium recalculated for policy {}: ${}", policyNumber, newPremium);
        return newPremium;
    }

    @Override
    @Transactional(readOnly = true)
    public List<Policy> findPoliciesForReview() {
        return policyDao.findByStatus(PolicyStatus.PENDING_REVIEW);
    }

    @Override
    @Transactional(readOnly = true)
    public Policy findByPolicyNumber(String policyNumber) {
        return policyDao.findByPolicyNumber(policyNumber);
    }

    // ---- PRIVATE HELPERS ----

    /**
     * Risk-based premium calculation.
     * Base premium * claim history multiplier.
     */
    private BigDecimal calculateRiskAdjustedPremium(Policy policy) {
        BigDecimal basePremium = policy.getPremiumAmount();
        if (basePremium == null) {
            basePremium = BigDecimal.valueOf(1200.00); // default base premium
        }

        // Claim history multiplier: more claims = higher premium
        List<Claim> claims = claimDao.findByPolicyId(policy.getId());
        int claimCount = claims != null ? claims.size() : 0;

        BigDecimal multiplier;
        if (claimCount == 0) {
            multiplier = BigDecimal.valueOf(0.95); // 5% discount for claim-free
        } else if (claimCount == 1) {
            multiplier = BigDecimal.ONE; // no change
        } else if (claimCount == 2) {
            multiplier = BigDecimal.valueOf(1.15); // 15% surcharge
        } else if (claimCount <= 4) {
            multiplier = BigDecimal.valueOf(1.35); // 35% surcharge
        } else {
            multiplier = BigDecimal.valueOf(1.60); // 60% surcharge
        }

        return basePremium.multiply(multiplier).setScale(2, RoundingMode.HALF_UP);
    }

    private String generatePolicyNumber() {
        return POLICY_PREFIX + "-" + System.currentTimeMillis() + "-" + (int)(Math.random() * 1000);
    }
}
