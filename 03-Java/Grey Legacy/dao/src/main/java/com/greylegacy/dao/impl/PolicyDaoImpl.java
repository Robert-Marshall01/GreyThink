package com.greylegacy.dao.impl;

import com.greylegacy.dao.AbstractHibernateDao;
import com.greylegacy.dao.PolicyDao;
import com.greylegacy.domain.Policy;
import com.greylegacy.domain.PolicyStatus;
import org.hibernate.criterion.Restrictions;
import org.springframework.stereotype.Repository;

import java.util.Date;
import java.util.List;

/**
 * Hibernate implementation of {@link PolicyDao}.
 * Uses named queries for simple lookups, HQL fetch joins to avoid N+1,
 * and Criteria API for dynamic queries.
 */
@Repository("policyDao")
@SuppressWarnings("unchecked")
public class PolicyDaoImpl extends AbstractHibernateDao<Policy, Long> implements PolicyDao {

    public PolicyDaoImpl() {
        super(Policy.class);
    }

    @Override
    public Policy findByPolicyNumber(String policyNumber) {
        log.debug("Finding Policy by policyNumber: {}", policyNumber);
        List<Policy> results = findByNamedQuery("Policy.findByPolicyNumber",
                "policyNumber", policyNumber);
        return results.isEmpty() ? null : results.get(0);
    }

    @Override
    public List<Policy> findByStatus(PolicyStatus status) {
        log.debug("Finding Policies by status: {}", status);
        return findByNamedQuery("Policy.findByStatus", "status", status);
    }

    @Override
    public List<Policy> findByHolderLastName(String lastName) {
        log.debug("Finding Policies by holder last name: {}", lastName);
        return findByNamedQuery("Policy.findByHolderLastName", "lastName", lastName);
    }

    @Override
    public List<Policy> findActivePolicies(Date asOfDate) {
        log.debug("Finding active Policies as of: {}", asOfDate);
        return findByNamedQuery("Policy.findActivePolicies", "asOfDate", asOfDate);
    }

    /**
     * Uses HQL with LEFT JOIN FETCH to eagerly load claims in a single query,
     * avoiding the N+1 select problem when iterating over policy claims.
     */
    @Override
    public List<Policy> findPoliciesWithClaimsFetchJoin(PolicyStatus status) {
        log.debug("Finding Policies with claims fetch join, status: {}", status);
        return getCurrentSession()
                .createQuery("SELECT DISTINCT p FROM Policy p " +
                        "LEFT JOIN FETCH p.claims " +
                        "WHERE p.status = :status")
                .setParameter("status", status)
                .list();
    }

    /**
     * Uses Criteria API with Restrictions.between to find policies
     * whose expiration date falls within the given date range.
     */
    @Override
    public List<Policy> findExpiringPolicies(Date startDate, Date endDate) {
        log.debug("Finding expiring Policies between {} and {}", startDate, endDate);
        return getCurrentSession()
                .createCriteria(Policy.class)
                .add(Restrictions.between("expirationDate", startDate, endDate))
                .list();
    }
}
