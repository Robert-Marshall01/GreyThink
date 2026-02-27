package com.greylegacy.batch;

import com.greylegacy.dao.ClaimDao;
import com.greylegacy.dao.jdbc.ClaimJdbcDao;
import com.greylegacy.dao.jdbc.ClaimJdbcDao.AuditEntryInput;
import com.greylegacy.dao.jdbc.ClaimJdbcDao.FraudScoreUpdate;
import com.greylegacy.domain.Claim;
import com.greylegacy.domain.ClaimStatus;
import com.greylegacy.domain.ClaimType;
import com.greylegacy.domain.FraudRiskLevel;
import org.quartz.Job;
import org.quartz.JobExecutionContext;
import org.quartz.JobExecutionException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;

/**
 * Nightly batch job that scores unscored claims for fraud risk.
 *
 * Uses heuristic rules to calculate a fraud score (0-100) for each claim
 * and maps the score to a {@link FraudRiskLevel}. CRITICAL-scored claims
 * are automatically flagged with status {@link ClaimStatus#FRAUD_SUSPECTED}.
 *
 * Scoring is performed via JDBC batch operations for performance.
 *
 * Runs nightly at 2:30 AM via Quartz scheduler.
 */
@Component("fraudScoringJob")
public class FraudScoringJob implements Job {

    private static final Logger log = LoggerFactory.getLogger(FraudScoringJob.class);

    /** Score thresholds for fraud risk levels. */
    private static final int LOW_THRESHOLD = 25;
    private static final int MEDIUM_THRESHOLD = 50;
    private static final int HIGH_THRESHOLD = 75;

    /** Days from policy inception that trigger an early-filing flag. */
    private static final int EARLY_FILING_DAYS = 90;

    /** Coverage ratio threshold (estimatedLoss / coverageLimit). */
    private static final BigDecimal HIGH_LOSS_RATIO = new BigDecimal("0.80");

    @Autowired
    private ClaimDao claimDao;

    @Autowired
    private ClaimJdbcDao claimJdbcDao;

    @Override
    @Transactional
    public void execute(JobExecutionContext context) throws JobExecutionException {
        log.info("=== FRAUD SCORING JOB STARTED ===");
        long startTime = System.currentTimeMillis();

        int totalScored = 0;
        int lowCount = 0, mediumCount = 0, highCount = 0, criticalCount = 0;

        try {
            List<Claim> unscoredClaims = claimDao.findUnscored();
            log.info("Found {} unscored claims to process", unscoredClaims.size());

            if (unscoredClaims.isEmpty()) {
                log.info("No unscored claims found. Job complete.");
                return;
            }

            List<FraudScoreUpdate> scoreUpdates = new ArrayList<>();
            List<AuditEntryInput> auditEntries = new ArrayList<>();
            List<Long> criticalClaimIds = new ArrayList<>();

            for (Claim claim : unscoredClaims) {
                int score = calculateFraudScore(claim);
                FraudRiskLevel riskLevel = mapScoreToRiskLevel(score);

                scoreUpdates.add(new FraudScoreUpdate(
                        claim.getId(), score, riskLevel.name()));

                // Create audit entry input
                AuditEntryInput audit = new AuditEntryInput();
                audit.setClaimId(claim.getId());
                audit.setEventType("FRAUD_SCORED");
                audit.setPreviousValue(null);
                audit.setNewValue("Score=" + score + ", Level=" + riskLevel.name());
                audit.setPerformedBy("BATCH_FRAUD_SCORER");
                audit.setDescription("Automated fraud scoring: score=" + score
                        + ", riskLevel=" + riskLevel.name());
                audit.setIpAddress(null);
                auditEntries.add(audit);

                // Track CRITICAL claims for status update
                if (riskLevel == FraudRiskLevel.CRITICAL) {
                    criticalClaimIds.add(claim.getId());
                    criticalCount++;
                } else if (riskLevel == FraudRiskLevel.HIGH) {
                    highCount++;
                } else if (riskLevel == FraudRiskLevel.MEDIUM) {
                    mediumCount++;
                } else {
                    lowCount++;
                }

                totalScored++;
            }

            // Batch update fraud scores via JDBC for performance
            int updated = claimJdbcDao.batchUpdateFraudScores(scoreUpdates);
            log.info("Batch updated {} fraud scores via JDBC", updated);

            // Batch insert audit entries via JDBC
            int audited = claimJdbcDao.batchInsertAuditEntries(auditEntries);
            log.info("Batch inserted {} audit entries via JDBC", audited);

            // Flag CRITICAL claims as FRAUD_SUSPECTED
            for (Long claimId : criticalClaimIds) {
                Claim claim = claimDao.findById(claimId);
                if (claim != null && claim.getStatus() != ClaimStatus.FRAUD_SUSPECTED) {
                    log.warn("CRITICAL fraud score on claim {} — flagging as FRAUD_SUSPECTED",
                            claim.getClaimNumber());
                    claim.setStatus(ClaimStatus.FRAUD_SUSPECTED);
                    claimDao.update(claim);
                }
            }

        } catch (Exception e) {
            log.error("Fraud scoring job failed", e);
            throw new JobExecutionException("Fraud scoring job failed", e);
        }

        long duration = System.currentTimeMillis() - startTime;
        log.info("=== FRAUD SCORING JOB COMPLETED in {}ms ===", duration);
        log.info("Scored {} claims: LOW={}, MEDIUM={}, HIGH={}, CRITICAL={}",
                totalScored, lowCount, mediumCount, highCount, criticalCount);
    }

    /**
     * Calculates a fraud score (0-100) based on heuristic rules.
     */
    private int calculateFraudScore(Claim claim) {
        int score = 0;

        // Rule 1: High estimated loss relative to policy coverage (+20)
        if (claim.getPolicy() != null && claim.getEstimatedLoss() != null
                && claim.getPolicy().getCoverageLimit() != null) {
            BigDecimal coverageLimit = claim.getPolicy().getCoverageLimit();
            if (coverageLimit.compareTo(BigDecimal.ZERO) > 0) {
                BigDecimal lossRatio = claim.getEstimatedLoss()
                        .divide(coverageLimit, 4, BigDecimal.ROUND_HALF_UP);
                if (lossRatio.compareTo(HIGH_LOSS_RATIO) >= 0) {
                    score += 20;
                    log.trace("Claim {}: +20 (high loss ratio {})", claim.getClaimNumber(), lossRatio);
                }
            }
        }

        // Rule 2: Claim filed soon after policy inception (+15)
        if (claim.getPolicy() != null && claim.getLossDate() != null
                && claim.getPolicy().getEffectiveDate() != null) {
            long daysSinceInception = daysBetween(
                    claim.getPolicy().getEffectiveDate(), claim.getLossDate());
            if (daysSinceInception <= EARLY_FILING_DAYS) {
                score += 15;
                log.trace("Claim {}: +15 (early filing, {} days after inception)",
                        claim.getClaimNumber(), daysSinceInception);
            }
        }

        // Rule 3: Multiple claims on same policy (+10 per extra claim)
        if (claim.getPolicy() != null && claim.getPolicy().getClaims() != null) {
            int claimCount = claim.getPolicy().getClaims().size();
            if (claimCount > 1) {
                int extraClaims = claimCount - 1;
                int bonus = extraClaims * 10;
                score += bonus;
                log.trace("Claim {}: +{} ({} extra claims on policy)",
                        claim.getClaimNumber(), bonus, extraClaims);
            }
        }

        // Rule 4: Certain claim types are higher risk (+10)
        if (claim.getClaimType() == ClaimType.THEFT || claim.getClaimType() == ClaimType.FIRE) {
            score += 10;
            log.trace("Claim {}: +10 (high-risk claim type {})",
                    claim.getClaimNumber(), claim.getClaimType());
        }

        // Rule 5: Weekend/holiday loss date (+5)
        if (claim.getLossDate() != null && isWeekend(claim.getLossDate())) {
            score += 5;
            log.trace("Claim {}: +5 (weekend loss date)", claim.getClaimNumber());
        }

        // Rule 6: Missing claimant contact info (+10)
        if (isMissingContactInfo(claim)) {
            score += 10;
            log.trace("Claim {}: +10 (missing contact info)", claim.getClaimNumber());
        }

        // Cap at 100
        return Math.min(score, 100);
    }

    /**
     * Maps a fraud score to a {@link FraudRiskLevel}.
     *
     * @param score the fraud score (0-100)
     * @return the corresponding risk level
     */
    private FraudRiskLevel mapScoreToRiskLevel(int score) {
        if (score > HIGH_THRESHOLD) {
            return FraudRiskLevel.CRITICAL;
        } else if (score > MEDIUM_THRESHOLD) {
            return FraudRiskLevel.HIGH;
        } else if (score > LOW_THRESHOLD) {
            return FraudRiskLevel.MEDIUM;
        } else {
            return FraudRiskLevel.LOW;
        }
    }

    /**
     * Checks if a date falls on a weekend (Saturday or Sunday).
     */
    private boolean isWeekend(Date date) {
        Calendar cal = Calendar.getInstance();
        cal.setTime(date);
        int dayOfWeek = cal.get(Calendar.DAY_OF_WEEK);
        return dayOfWeek == Calendar.SATURDAY || dayOfWeek == Calendar.SUNDAY;
    }

    /**
     * Checks if the claimant's contact information is incomplete.
     */
    private boolean isMissingContactInfo(Claim claim) {
        boolean missingPhone = claim.getClaimantPhone() == null
                || claim.getClaimantPhone().trim().isEmpty();
        boolean missingEmail = claim.getClaimantEmail() == null
                || claim.getClaimantEmail().trim().isEmpty();
        return missingPhone && missingEmail;
    }

    /**
     * Calculates the number of days between two dates.
     */
    private long daysBetween(Date start, Date end) {
        long diffMillis = end.getTime() - start.getTime();
        return diffMillis / (1000L * 60 * 60 * 24);
    }
}
