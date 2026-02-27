package com.greylegacy.batch;

import com.greylegacy.dao.ClaimDao;
import com.greylegacy.dao.PolicyDao;
import com.greylegacy.domain.Claim;
import com.greylegacy.domain.Policy;
import com.greylegacy.domain.PolicyEndorsement;
import com.greylegacy.domain.PolicyStatus;
import org.quartz.Job;
import org.quartz.JobExecutionContext;
import org.quartz.JobExecutionException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.Calendar;
import java.util.Date;
import java.util.List;

/**
 * Nightly batch job that recalculates premiums for active policies
 * based on claims history.
 *
 * Premium adjustment rules:
 *   - 0 claims in last 12 months: 5% discount
 *   - 1 claim: no change
 *   - 2 claims: 10% surcharge
 *   - 3+ claims: 25% surcharge
 *   - Loss ratio > 1.0: additional 15% surcharge
 *
 * Each adjustment is recorded as a {@link PolicyEndorsement}.
 *
 * Runs monthly on the 1st at 1:00 AM via Quartz scheduler.
 */
@Component("premiumRecalculationJob")
public class PremiumRecalculationJob implements Job {

    private static final Logger log = LoggerFactory.getLogger(PremiumRecalculationJob.class);

    private static final BigDecimal DISCOUNT_RATE = new BigDecimal("-0.05");
    private static final BigDecimal SURCHARGE_2_CLAIMS = new BigDecimal("0.10");
    private static final BigDecimal SURCHARGE_3_PLUS = new BigDecimal("0.25");
    private static final BigDecimal SURCHARGE_HIGH_LOSS_RATIO = new BigDecimal("0.15");
    private static final BigDecimal LOSS_RATIO_THRESHOLD = BigDecimal.ONE;

    @Autowired
    private PolicyDao policyDao;

    @Autowired
    private ClaimDao claimDao;

    @Override
    @Transactional
    public void execute(JobExecutionContext context) throws JobExecutionException {
        log.info("=== PREMIUM RECALCULATION JOB STARTED ===");
        long startTime = System.currentTimeMillis();

        int totalPolicies = 0;
        int discountCount = 0;
        int noChangeCount = 0;
        int surchargeCount = 0;

        try {
            Date today = new Date();
            Date twelveMonthsAgo = getDateMinusMonths(12);

            List<Policy> activePolicies = policyDao.findByStatus(PolicyStatus.ACTIVE);
            log.info("Found {} active policies to evaluate", activePolicies.size());

            for (Policy policy : activePolicies) {
                totalPolicies++;

                // Gather claims in the last 12 months for this policy
                List<Claim> policyClaims = claimDao.findByPolicyId(policy.getId());
                int recentClaimCount = 0;
                BigDecimal totalApproved = BigDecimal.ZERO;

                for (Claim claim : policyClaims) {
                    if (claim.getReportedDate() != null
                            && claim.getReportedDate().after(twelveMonthsAgo)) {
                        recentClaimCount++;
                        if (claim.getApprovedAmount() != null) {
                            totalApproved = totalApproved.add(claim.getApprovedAmount());
                        }
                    }
                }

                // Calculate premium adjustment
                BigDecimal currentPremium = policy.getPremiumAmount();
                if (currentPremium == null || currentPremium.compareTo(BigDecimal.ZERO) <= 0) {
                    log.warn("Policy {} has no valid premium amount, skipping",
                            policy.getPolicyNumber());
                    continue;
                }

                BigDecimal adjustmentRate = calculateAdjustmentRate(
                        recentClaimCount, totalApproved, currentPremium);

                if (adjustmentRate.compareTo(BigDecimal.ZERO) == 0) {
                    noChangeCount++;
                    log.debug("Policy {}: no premium adjustment (1 claim, no loss ratio issue)",
                            policy.getPolicyNumber());
                    continue;
                }

                BigDecimal adjustmentAmount = currentPremium.multiply(adjustmentRate)
                        .setScale(2, RoundingMode.HALF_UP);

                // Create endorsement recording the adjustment
                PolicyEndorsement endorsement = new PolicyEndorsement();
                endorsement.setEndorsementNumber(generateEndorsementNumber(policy));
                endorsement.setEndorsementType("PREMIUM_RECALCULATION");
                endorsement.setEffectiveDate(today);
                endorsement.setPremiumAdjustment(adjustmentAmount);
                endorsement.setDescription(buildDescription(
                        recentClaimCount, totalApproved, currentPremium, adjustmentRate));
                endorsement.setActive(true);

                policy.addEndorsement(endorsement);

                // Update the premium amount
                BigDecimal newPremium = currentPremium.add(adjustmentAmount);
                policy.setPremiumAmount(newPremium);

                policyDao.update(policy);

                if (adjustmentRate.compareTo(BigDecimal.ZERO) < 0) {
                    discountCount++;
                    log.debug("Policy {}: {} discount applied (new premium: {})",
                            policy.getPolicyNumber(), adjustmentRate, newPremium);
                } else {
                    surchargeCount++;
                    log.debug("Policy {}: {} surcharge applied (new premium: {})",
                            policy.getPolicyNumber(), adjustmentRate, newPremium);
                }
            }

            policyDao.flush();

        } catch (Exception e) {
            log.error("Premium recalculation job failed", e);
            throw new JobExecutionException("Premium recalculation job failed", e);
        }

        long duration = System.currentTimeMillis() - startTime;
        log.info("=== PREMIUM RECALCULATION JOB COMPLETED in {}ms ===", duration);
        log.info("Evaluated {} policies: {} discounts, {} surcharges, {} no change",
                totalPolicies, discountCount, surchargeCount, noChangeCount);
    }

    /**
     * Calculates the premium adjustment rate based on claims history.
     *
     * @param claimCount    number of claims in last 12 months
     * @param totalApproved total approved payout amount
     * @param premium       current policy premium
     * @return adjustment rate (negative for discount, positive for surcharge, zero for no change)
     */
    private BigDecimal calculateAdjustmentRate(int claimCount, BigDecimal totalApproved,
                                                BigDecimal premium) {
        BigDecimal rate = BigDecimal.ZERO;

        // Base adjustment by claim count
        if (claimCount == 0) {
            rate = DISCOUNT_RATE; // 5% discount
        } else if (claimCount == 1) {
            rate = BigDecimal.ZERO; // no change
        } else if (claimCount == 2) {
            rate = SURCHARGE_2_CLAIMS; // 10% surcharge
        } else {
            rate = SURCHARGE_3_PLUS; // 25% surcharge
        }

        // Additional surcharge for high loss ratio
        if (premium.compareTo(BigDecimal.ZERO) > 0) {
            BigDecimal lossRatio = totalApproved.divide(premium, 4, RoundingMode.HALF_UP);
            if (lossRatio.compareTo(LOSS_RATIO_THRESHOLD) > 0) {
                rate = rate.add(SURCHARGE_HIGH_LOSS_RATIO);
                log.debug("High loss ratio {} detected, adding 15% surcharge", lossRatio);
            }
        }

        return rate;
    }

    /**
     * Generates a unique endorsement number for premium recalculation.
     */
    private String generateEndorsementNumber(Policy policy) {
        return "PREM-" + policy.getPolicyNumber() + "-"
                + System.currentTimeMillis() % 100000;
    }

    /**
     * Builds a human-readable description for the endorsement.
     */
    private String buildDescription(int claimCount, BigDecimal totalApproved,
                                     BigDecimal premium, BigDecimal adjustmentRate) {
        BigDecimal lossRatio = BigDecimal.ZERO;
        if (premium.compareTo(BigDecimal.ZERO) > 0) {
            lossRatio = totalApproved.divide(premium, 4, RoundingMode.HALF_UP);
        }

        StringBuilder sb = new StringBuilder();
        sb.append("Premium recalculation: ");
        sb.append(claimCount).append(" claim(s) in last 12 months, ");
        sb.append("total approved=").append(totalApproved).append(", ");
        sb.append("loss ratio=").append(lossRatio).append(", ");
        sb.append("adjustment rate=").append(adjustmentRate.multiply(new BigDecimal("100"))
                .setScale(1, RoundingMode.HALF_UP)).append("%");
        return sb.toString();
    }

    private Date getDateMinusMonths(int months) {
        Calendar cal = Calendar.getInstance();
        cal.add(Calendar.MONTH, -months);
        return cal.getTime();
    }
}
