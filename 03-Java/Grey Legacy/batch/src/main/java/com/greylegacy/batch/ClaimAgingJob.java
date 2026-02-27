package com.greylegacy.batch;

import com.greylegacy.dao.ClaimDao;
import com.greylegacy.dao.ClaimAuditDao;
import com.greylegacy.domain.Claim;
import com.greylegacy.domain.ClaimAuditEntry;
import com.greylegacy.domain.ClaimStatus;
import org.quartz.Job;
import org.quartz.JobExecutionContext;
import org.quartz.JobExecutionException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import java.util.Calendar;
import java.util.Date;
import java.util.List;

/**
 * Nightly batch job for claim aging analysis.
 * 
 * Identifies claims that have been open beyond configurable thresholds
 * and escalates or flags them for supervisor review.
 * 
 * Aging tiers:
 *   - 30 days: Flag for review
 *   - 60 days: Escalate to supervisor
 *   - 90 days: Auto-escalate to management
 *   - 120+ days: SLA breach flag
 * 
 * Runs nightly at 2:00 AM via Quartz scheduler.
 */
@Component("claimAgingJob")
public class ClaimAgingJob implements Job {

    private static final Logger log = LoggerFactory.getLogger(ClaimAgingJob.class);

    private static final int TIER_1_DAYS = 30;
    private static final int TIER_2_DAYS = 60;
    private static final int TIER_3_DAYS = 90;
    private static final int TIER_4_DAYS = 120;

    @Autowired
    private ClaimDao claimDao;

    @Override
    @Transactional
    public void execute(JobExecutionContext context) throws JobExecutionException {
        log.info("=== CLAIM AGING JOB STARTED ===");
        long startTime = System.currentTimeMillis();

        int tier1Count = 0, tier2Count = 0, tier3Count = 0, tier4Count = 0;

        try {
            // Process Tier 4 first (oldest), then work down
            Date tier4Cutoff = getDateMinusDays(TIER_4_DAYS);
            Date tier3Cutoff = getDateMinusDays(TIER_3_DAYS);
            Date tier2Cutoff = getDateMinusDays(TIER_2_DAYS);
            Date tier1Cutoff = getDateMinusDays(TIER_1_DAYS);

            // Tier 4: SLA breach (120+ days)
            List<Claim> tier4Claims = claimDao.findClaimsOlderThan(tier4Cutoff);
            for (Claim claim : tier4Claims) {
                if (isOpenClaim(claim)) {
                    processTier4(claim);
                    tier4Count++;
                }
            }

            // Tier 3: Management escalation (90-119 days)
            List<Claim> tier3Claims = claimDao.findClaimsOlderThan(tier3Cutoff);
            for (Claim claim : tier3Claims) {
                if (isOpenClaim(claim) && !isOlderThan(claim, TIER_4_DAYS)) {
                    processTier3(claim);
                    tier3Count++;
                }
            }

            // Tier 2: Supervisor escalation (60-89 days)
            List<Claim> tier2Claims = claimDao.findClaimsOlderThan(tier2Cutoff);
            for (Claim claim : tier2Claims) {
                if (isOpenClaim(claim) && !isOlderThan(claim, TIER_3_DAYS)) {
                    processTier2(claim);
                    tier2Count++;
                }
            }

            // Tier 1: Review flag (30-59 days)
            List<Claim> tier1Claims = claimDao.findClaimsOlderThan(tier1Cutoff);
            for (Claim claim : tier1Claims) {
                if (isOpenClaim(claim) && !isOlderThan(claim, TIER_2_DAYS)) {
                    processTier1(claim);
                    tier1Count++;
                }
            }

            // Flush batch
            claimDao.flush();

        } catch (Exception e) {
            log.error("Claim aging job failed", e);
            throw new JobExecutionException("Claim aging job failed", e);
        }

        long duration = System.currentTimeMillis() - startTime;
        log.info("=== CLAIM AGING JOB COMPLETED in {}ms ===", duration);
        log.info("Results: Tier1(30d)={}, Tier2(60d)={}, Tier3(90d)={}, Tier4(120d+)={}",
                tier1Count, tier2Count, tier3Count, tier4Count);
    }

    private void processTier1(Claim claim) {
        log.debug("Tier 1 aging: claim {} open since {}", claim.getClaimNumber(), claim.getReportedDate());
        createAgingAuditEntry(claim, "AGING_TIER_1", "Claim open 30+ days. Flagged for review.");
    }

    private void processTier2(Claim claim) {
        log.debug("Tier 2 aging: claim {} - escalating to supervisor", claim.getClaimNumber());
        createAgingAuditEntry(claim, "AGING_TIER_2", "Claim open 60+ days. Escalated to supervisor.");
    }

    private void processTier3(Claim claim) {
        log.debug("Tier 3 aging: claim {} - escalating to management", claim.getClaimNumber());
        createAgingAuditEntry(claim, "AGING_TIER_3", "Claim open 90+ days. Escalated to management.");
    }

    private void processTier4(Claim claim) {
        log.warn("Tier 4 SLA BREACH: claim {} open 120+ days", claim.getClaimNumber());
        createAgingAuditEntry(claim, "AGING_SLA_BREACH", "SLA BREACH: Claim open 120+ days.");
    }

    private void createAgingAuditEntry(Claim claim, String eventType, String description) {
        ClaimAuditEntry audit = new ClaimAuditEntry(
                claim, new Date(), eventType, null, null,
                "BATCH_AGING_JOB", description, null);
        claim.addAuditEntry(audit);
        claimDao.update(claim);
    }

    private boolean isOpenClaim(Claim claim) {
        return claim.getStatus() != ClaimStatus.CLOSED
            && claim.getStatus() != ClaimStatus.DENIED
            && claim.getStatus() != ClaimStatus.SETTLED;
    }

    private boolean isOlderThan(Claim claim, int days) {
        Date cutoff = getDateMinusDays(days);
        return claim.getReportedDate().before(cutoff);
    }

    private Date getDateMinusDays(int days) {
        Calendar cal = Calendar.getInstance();
        cal.add(Calendar.DAY_OF_MONTH, -days);
        return cal.getTime();
    }
}
