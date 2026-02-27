package com.greylegacy.dao.impl;

import com.greylegacy.dao.AbstractHibernateDao;
import com.greylegacy.dao.ClaimAuditDao;
import com.greylegacy.domain.ClaimAuditEntry;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.Restrictions;
import org.springframework.stereotype.Repository;

import java.util.Date;
import java.util.List;

/**
 * Hibernate implementation of {@link ClaimAuditDao}.
 * Uses Criteria API for all queries on the immutable audit trail.
 */
@Repository("claimAuditDao")
@SuppressWarnings("unchecked")
public class ClaimAuditDaoImpl extends AbstractHibernateDao<ClaimAuditEntry, Long> implements ClaimAuditDao {

    public ClaimAuditDaoImpl() {
        super(ClaimAuditEntry.class);
    }

    @Override
    public List<ClaimAuditEntry> findByClaimId(Long claimId) {
        log.debug("Finding ClaimAuditEntries by claimId: {}", claimId);
        return getCurrentSession()
                .createCriteria(ClaimAuditEntry.class)
                .add(Restrictions.eq("claim.id", claimId))
                .addOrder(Order.asc("eventTimestamp"))
                .list();
    }

    @Override
    public List<ClaimAuditEntry> findByEventType(String eventType) {
        log.debug("Finding ClaimAuditEntries by eventType: {}", eventType);
        return getCurrentSession()
                .createCriteria(ClaimAuditEntry.class)
                .add(Restrictions.eq("eventType", eventType))
                .addOrder(Order.desc("eventTimestamp"))
                .list();
    }

    @Override
    public List<ClaimAuditEntry> findByDateRange(Date startDate, Date endDate) {
        log.debug("Finding ClaimAuditEntries between {} and {}", startDate, endDate);
        return getCurrentSession()
                .createCriteria(ClaimAuditEntry.class)
                .add(Restrictions.between("eventTimestamp", startDate, endDate))
                .addOrder(Order.asc("eventTimestamp"))
                .list();
    }
}
