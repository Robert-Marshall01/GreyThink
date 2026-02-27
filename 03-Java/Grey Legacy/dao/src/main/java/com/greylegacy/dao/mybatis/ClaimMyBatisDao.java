package com.greylegacy.dao.mybatis;

import com.greylegacy.domain.Claim;
import org.apache.ibatis.session.SqlSession;
import org.apache.ibatis.session.SqlSessionFactory;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import java.math.BigDecimal;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * MyBatis-based DAO for complex Claim queries.
 * <p>
 * This DAO complements the Hibernate-based {@code ClaimDaoImpl} by handling
 * queries that are better expressed in raw SQL — dynamic search with many
 * optional filters, aggregation reports, and stored procedure calls.
 * <p>
 * <b>Hybrid Strategy:</b>
 * <ul>
 *   <li>Hibernate ({@code ClaimDaoImpl}) → entity CRUD, lifecycle callbacks,
 *       lazy loading, unit-of-work pattern, named queries</li>
 *   <li>MyBatis (this class) → reporting, dynamic search, aggregations,
 *       stored procedure invocations, export queries</li>
 *   <li>Raw JDBC ({@code ClaimJdbcDao}) → batch inserts/updates where
 *       neither ORM is efficient enough</li>
 * </ul>
 * <p>
 * This is a common evolution in legacy enterprise systems: Hibernate was
 * adopted for its productivity gains on CRUD operations, but reporting
 * teams later introduced MyBatis for its SQL transparency and the ability
 * to use database-specific functions, window functions, and CTEs without
 * resorting to native queries embedded in Java strings.
 *
 * @see com.greylegacy.dao.impl.ClaimDaoImpl
 * @see com.greylegacy.dao.jdbc.ClaimJdbcDao
 */
@Repository("claimMyBatisDao")
public class ClaimMyBatisDao {

    private static final Logger log = LoggerFactory.getLogger(ClaimMyBatisDao.class);

    private static final String NAMESPACE = "com.greylegacy.dao.mybatis.ClaimMyBatisDao";

    @Autowired
    private SqlSessionFactory sqlSessionFactory;

    /**
     * Find a claim with its associated policy eagerly loaded via a SQL JOIN
     * (avoids N+1 without requiring Hibernate fetch join or entity graphs).
     */
    public Claim findClaimWithPolicy(Long claimId) {
        log.debug("MyBatis: findClaimWithPolicy({})", claimId);
        try (SqlSession session = sqlSessionFactory.openSession()) {
            return session.selectOne(NAMESPACE + ".findClaimWithPolicy", claimId);
        }
    }

    /**
     * Dynamic multi-criteria search.  All parameters are optional.
     * MyBatis {@code <if>} tags build the WHERE clause at runtime.
     *
     * @param criteria map of optional search criteria:
     *   - claimNumber (String, prefix match)
     *   - status (String, exact match)
     *   - claimType (String, exact match)
     *   - policyNumber (String, prefix match)
     *   - claimantLastName (String, prefix match, case-insensitive)
     *   - adjusterCode (String, exact match)
     *   - lossDateFrom (Date)
     *   - lossDateTo (Date)
     *   - minEstimatedLoss (BigDecimal)
     *   - fraudRiskLevel (String)
     *   - sortColumn (String: "lossDate" | "estimatedLoss" | "daysOpen")
     *   - limit (Integer)
     *   - offset (Integer)
     * @return list of claim summary maps
     */
    @SuppressWarnings("unchecked")
    public List<Map<String, Object>> searchClaims(Map<String, Object> criteria) {
        log.debug("MyBatis: searchClaims with {} criteria", criteria.size());
        try (SqlSession session = sqlSessionFactory.openSession()) {
            return session.selectList(NAMESPACE + ".searchClaims", criteria);
        }
    }

    /**
     * Paginated claim search with default page size.
     */
    @SuppressWarnings("unchecked")
    public List<Map<String, Object>> searchClaims(Map<String, Object> criteria,
                                                   int page, int pageSize) {
        criteria.put("limit", pageSize);
        criteria.put("offset", (page - 1) * pageSize);
        return searchClaims(criteria);
    }

    /**
     * Batch update fraud scores using MyBatis {@code <foreach>}.
     * Alternative to the raw JDBC batch in {@code ClaimJdbcDao}.
     */
    public int batchUpdateFraudScores(List<Long> claimIds, int fraudScore,
                                       String fraudRiskLevel) {
        log.debug("MyBatis: batchUpdateFraudScores for {} claims", claimIds.size());
        Map<String, Object> params = new HashMap<>();
        params.put("claimIds", claimIds);
        params.put("fraudScore", fraudScore);
        params.put("fraudRiskLevel", fraudRiskLevel);

        try (SqlSession session = sqlSessionFactory.openSession()) {
            int updated = session.update(NAMESPACE + ".batchUpdateFraudScores", params);
            session.commit();
            log.info("MyBatis: batch updated {} fraud scores", updated);
            return updated;
        }
    }

    /**
     * Dashboard statistics grouped by claim status.
     * Returns aggregate counts, sums, and averages per status bucket.
     */
    @SuppressWarnings("unchecked")
    public List<Map<String, Object>> getClaimStatisticsByStatus() {
        log.debug("MyBatis: getClaimStatisticsByStatus");
        try (SqlSession session = sqlSessionFactory.openSession()) {
            return session.selectList(NAMESPACE + ".getClaimStatisticsByStatus");
        }
    }

    /**
     * Adjuster workload report showing active cases, total exposure, and
     * average days open per adjuster.
     */
    @SuppressWarnings("unchecked")
    public List<Map<String, Object>> getAdjusterWorkload() {
        log.debug("MyBatis: getAdjusterWorkload");
        try (SqlSession session = sqlSessionFactory.openSession()) {
            return session.selectList(NAMESPACE + ".getAdjusterWorkload");
        }
    }

    /**
     * Invoke the SP_CALCULATE_CLAIM_RESERVE stored procedure via MyBatis.
     * Demonstrates MyBatis callable statement integration.
     */
    public BigDecimal calculateClaimReserve(Long claimId) {
        log.debug("MyBatis: calling SP_CALCULATE_CLAIM_RESERVE({})", claimId);
        try (SqlSession session = sqlSessionFactory.openSession()) {
            Map<String, Object> params = new HashMap<>();
            params.put("claimId", claimId);
            session.selectOne(NAMESPACE + ".calculateClaimReserve", params);
            session.commit();
            return (BigDecimal) params.get("result");
        }
    }
}
