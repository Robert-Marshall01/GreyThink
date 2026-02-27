package com.greylegacy.dao.impl;

import com.greylegacy.dao.AbstractHibernateDao;
import com.greylegacy.dao.ClaimSnapshotDao;
import com.greylegacy.domain.ClaimSnapshot;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.Restrictions;
import org.springframework.stereotype.Repository;

import java.util.List;

/**
 * Hibernate implementation of {@link ClaimSnapshotDao}.
 * Uses Criteria API for queries on the immutable snapshot table.
 */
@Repository("claimSnapshotDao")
@SuppressWarnings("unchecked")
public class ClaimSnapshotDaoImpl extends AbstractHibernateDao<ClaimSnapshot, Long> implements ClaimSnapshotDao {

    public ClaimSnapshotDaoImpl() {
        super(ClaimSnapshot.class);
    }

    @Override
    public List<ClaimSnapshot> findByClaimId(Long claimId) {
        log.debug("Finding ClaimSnapshots by claimId: {}", claimId);
        return getCurrentSession()
                .createCriteria(ClaimSnapshot.class)
                .add(Restrictions.eq("claimId", claimId))
                .addOrder(Order.desc("snapshotDate"))
                .list();
    }

    /**
     * Returns the most recent snapshot for the given claim by ordering
     * by snapshotDate DESC and limiting to 1 result.
     */
    @Override
    public ClaimSnapshot findLatestSnapshot(Long claimId) {
        log.debug("Finding latest ClaimSnapshot for claimId: {}", claimId);
        List<ClaimSnapshot> results = getCurrentSession()
                .createCriteria(ClaimSnapshot.class)
                .add(Restrictions.eq("claimId", claimId))
                .addOrder(Order.desc("snapshotDate"))
                .setMaxResults(1)
                .list();
        return results.isEmpty() ? null : results.get(0);
    }
}
