package com.greylegacy.dao.impl;

import com.greylegacy.dao.AbstractHibernateDao;
import com.greylegacy.dao.ClaimDao;
import com.greylegacy.domain.Claim;
import com.greylegacy.domain.ClaimStatus;
import com.greylegacy.domain.FraudRiskLevel;
import org.hibernate.criterion.Restrictions;
import org.springframework.stereotype.Repository;

import java.util.Date;
import java.util.List;

/**
 * Hibernate implementation of {@link ClaimDao}.
 * Uses named queries for standard lookups, Criteria API for dynamic filters,
 * HQL fetch joins to avoid N+1, and HQL bulk updates for batch operations.
 */
@Repository("claimDao")
@SuppressWarnings("unchecked")
public class ClaimDaoImpl extends AbstractHibernateDao<Claim, Long> implements ClaimDao {

    public ClaimDaoImpl() {
        super(Claim.class);
    }

    @Override
    public Claim findByClaimNumber(String claimNumber) {
        log.debug("Finding Claim by claimNumber: {}", claimNumber);
        List<Claim> results = findByNamedQuery("Claim.findByClaimNumber",
                "claimNumber", claimNumber);
        return results.isEmpty() ? null : results.get(0);
    }

    @Override
    public List<Claim> findByStatus(ClaimStatus status) {
        log.debug("Finding Claims by status: {}", status);
        return findByNamedQuery("Claim.findByStatus", "status", status);
    }

    @Override
    public List<Claim> findByPolicyId(Long policyId) {
        log.debug("Finding Claims by policyId: {}", policyId);
        return findByNamedQuery("Claim.findByPolicyId", "policyId", policyId);
    }

    @Override
    public List<Claim> findOpenClaims() {
        log.debug("Finding all open Claims");
        return findByNamedQuery("Claim.findOpenClaims");
    }

    @Override
    public List<Claim> findClaimsOlderThan(Date date) {
        log.debug("Finding Claims older than: {}", date);
        return findByNamedQuery("Claim.findClaimsOlderThan", "cutoffDate", date);
    }

    /**
     * Uses Criteria API to find claims assigned to a specific adjuster.
     */
    @Override
    public List<Claim> findClaimsByAdjuster(String adjusterCode) {
        log.debug("Finding Claims by adjusterCode: {}", adjusterCode);
        return getCurrentSession()
                .createCriteria(Claim.class)
                .add(Restrictions.eq("adjusterCode", adjusterCode))
                .list();
    }

    /**
     * Uses HQL with LEFT JOIN FETCH to eagerly load payments in a single query,
     * avoiding the N+1 select problem when accessing claim payments.
     */
    @Override
    public List<Claim> findClaimsWithPaymentsFetchJoin(Long claimId) {
        log.debug("Finding Claim with payments fetch join, claimId: {}", claimId);
        return getCurrentSession()
                .createQuery("SELECT DISTINCT c FROM Claim c " +
                        "LEFT JOIN FETCH c.payments " +
                        "WHERE c.id = :claimId")
                .setParameter("claimId", claimId)
                .list();
    }

    /**
     * Finds claims that have not yet been scored for fraud.
     * Excludes claims in terminal statuses (CLOSED, DENIED).
     */
    @Override
    public List<Claim> findUnscored() {
        log.debug("Finding unscored Claims");
        return getCurrentSession()
                .createQuery("SELECT c FROM Claim c " +
                        "WHERE c.fraudScore IS NULL " +
                        "AND c.status NOT IN (:excludedStatuses)")
                .setParameterList("excludedStatuses",
                        new ClaimStatus[]{ClaimStatus.CLOSED, ClaimStatus.DENIED})
                .list();
    }

    /**
     * Bulk-updates fraud scores and risk levels for a batch of claim IDs.
     * Returns the number of rows affected.
     */
    @Override
    public int bulkUpdateFraudScores(int score, String riskLevel, List<Long> claimIds) {
        log.debug("Bulk updating fraud scores for {} claims, score={}, riskLevel={}",
                claimIds.size(), score, riskLevel);
        return getCurrentSession()
                .createQuery("UPDATE Claim c SET " +
                        "c.fraudScore = :score, " +
                        "c.fraudRiskLevel = :riskLevel, " +
                        "c.fraudScoredDate = :scoredDate " +
                        "WHERE c.id IN (:claimIds)")
                .setParameter("score", score)
                .setParameter("riskLevel", FraudRiskLevel.valueOf(riskLevel))
                .setParameter("scoredDate", new Date())
                .setParameterList("claimIds", claimIds)
                .executeUpdate();
    }
}
