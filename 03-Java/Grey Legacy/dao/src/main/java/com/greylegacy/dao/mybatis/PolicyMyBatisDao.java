package com.greylegacy.dao.mybatis;

import com.greylegacy.domain.Policy;
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
 * MyBatis-based DAO for Policy reporting and lookup queries.
 * <p>
 * Complements the Hibernate-based {@code PolicyDaoImpl} with queries
 * that benefit from explicit SQL control:
 * <ul>
 *   <li>Multi-criteria dynamic search with optional parameters</li>
 *   <li>Aggregation queries (exposure reports, loss ratios)</li>
 *   <li>Partial updates using dynamic {@code <set>} clauses</li>
 *   <li>SCD Type 2 history lookups</li>
 *   <li>Pagination with LIMIT/OFFSET</li>
 * </ul>
 */
@Repository("policyMyBatisDao")
public class PolicyMyBatisDao {

    private static final Logger log = LoggerFactory.getLogger(PolicyMyBatisDao.class);

    private static final String NAMESPACE = "com.greylegacy.dao.mybatis.PolicyMyBatisDao";

    @Autowired
    private SqlSessionFactory sqlSessionFactory;

    /**
     * Lookup policy by policy number — the single most common query
     * in the system. Uses MyBatis for read-only lookups that don't
     * need Hibernate session management overhead.
     */
    public Policy findByPolicyNumber(String policyNumber) {
        log.debug("MyBatis: findByPolicyNumber({})", policyNumber);
        try (SqlSession session = sqlSessionFactory.openSession()) {
            return session.selectOne(NAMESPACE + ".findByPolicyNumber", policyNumber);
        }
    }

    /**
     * Dynamic search with optional filters.
     * All parameters in the criteria map are optional; MyBatis builds
     * the WHERE clause dynamically.
     *
     * @param criteria optional filters: policyNumber, status, policyType,
     *                 holderLastName, agentCode, effectiveDateFrom, effectiveDateTo
     * @return matching policies
     */
    public List<Policy> searchPolicies(Map<String, Object> criteria) {
        log.debug("MyBatis: searchPolicies with {} criteria", criteria.size());
        try (SqlSession session = sqlSessionFactory.openSession()) {
            return session.selectList(NAMESPACE + ".searchPolicies", criteria);
        }
    }

    /**
     * Paginated policy search.
     */
    public List<Policy> searchPolicies(Map<String, Object> criteria,
                                        int page, int pageSize) {
        criteria.put("limit", pageSize);
        criteria.put("offset", (page - 1) * pageSize);
        return searchPolicies(criteria);
    }

    /**
     * Policy exposure report with aggregated claim data.
     * Shows claim count, total exposure, and loss ratio per policy.
     */
    @SuppressWarnings("unchecked")
    public List<Map<String, Object>> getPolicyExposureReport(String sortBy) {
        log.debug("MyBatis: getPolicyExposureReport, sortBy={}", sortBy);
        Map<String, Object> params = new HashMap<>();
        params.put("sortBy", sortBy);
        try (SqlSession session = sqlSessionFactory.openSession()) {
            return session.selectList(NAMESPACE + ".getPolicyExposureReport", params);
        }
    }

    /**
     * Find policies expiring within the specified number of days.
     * Used by the renewal processing batch job.
     */
    public List<Policy> findExpiringPolicies(int daysAhead) {
        log.debug("MyBatis: findExpiringPolicies(daysAhead={})", daysAhead);
        try (SqlSession session = sqlSessionFactory.openSession()) {
            return session.selectList(NAMESPACE + ".findExpiringPolicies", daysAhead);
        }
    }

    /**
     * Partial update — only modifies columns present in the update map.
     * Uses MyBatis dynamic {@code <set>} to avoid overwriting fields
     * the caller didn't provide.
     * <p>
     * Includes optimistic locking check: the update only succeeds if
     * the current VERSION matches expectedVersion.
     *
     * @return number of rows updated (0 = optimistic lock failure)
     */
    public int partialUpdate(Long policyId, int expectedVersion,
                              Map<String, Object> updates, String updatedBy) {
        log.debug("MyBatis: partialUpdate(policyId={}, version={})", policyId, expectedVersion);
        updates.put("id", policyId);
        updates.put("expectedVersion", expectedVersion);
        updates.put("updatedBy", updatedBy);

        try (SqlSession session = sqlSessionFactory.openSession()) {
            int rows = session.update(NAMESPACE + ".partialUpdate", updates);
            session.commit();

            if (rows == 0) {
                log.warn("MyBatis: Optimistic lock failure — policy {} version {}",
                        policyId, expectedVersion);
            }
            return rows;
        }
    }

    /**
     * Retrieve the full SCD Type 2 history for a policy.
     * Returns a chronological list of all versions of the policy,
     * including change reasons and who made each change.
     */
    @SuppressWarnings("unchecked")
    public List<Map<String, Object>> getPolicyHistory(Long policyId) {
        log.debug("MyBatis: getPolicyHistory({})", policyId);
        try (SqlSession session = sqlSessionFactory.openSession()) {
            return session.selectList(NAMESPACE + ".getPolicyHistory", policyId);
        }
    }
}
