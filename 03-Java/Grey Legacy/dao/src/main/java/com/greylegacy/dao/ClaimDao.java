package com.greylegacy.dao;

import com.greylegacy.domain.Claim;
import com.greylegacy.domain.ClaimStatus;
import java.util.Date;
import java.util.List;

public interface ClaimDao extends GenericDao<Claim, Long> {
    Claim findByClaimNumber(String claimNumber);
    List<Claim> findByStatus(ClaimStatus status);
    List<Claim> findByPolicyId(Long policyId);
    List<Claim> findOpenClaims();
    List<Claim> findClaimsOlderThan(Date date);
    List<Claim> findClaimsByAdjuster(String adjusterCode);
    List<Claim> findClaimsWithPaymentsFetchJoin(Long claimId);
    List<Claim> findUnscored();
    int bulkUpdateFraudScores(int score, String riskLevel, List<Long> claimIds);
}
