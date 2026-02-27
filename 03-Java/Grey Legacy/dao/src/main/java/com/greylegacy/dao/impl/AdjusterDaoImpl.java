package com.greylegacy.dao.impl;

import com.greylegacy.dao.AbstractHibernateDao;
import com.greylegacy.dao.AdjusterDao;
import com.greylegacy.domain.Adjuster;
import org.hibernate.criterion.Restrictions;
import org.springframework.stereotype.Repository;

import java.util.List;

/**
 * Hibernate implementation of {@link AdjusterDao}.
 * Uses Criteria API and HQL for adjuster lookups.
 */
@Repository("adjusterDao")
@SuppressWarnings("unchecked")
public class AdjusterDaoImpl extends AbstractHibernateDao<Adjuster, Long> implements AdjusterDao {

    public AdjusterDaoImpl() {
        super(Adjuster.class);
    }

    @Override
    public Adjuster findByAdjusterCode(String adjusterCode) {
        log.debug("Finding Adjuster by adjusterCode: {}", adjusterCode);
        List<Adjuster> results = getCurrentSession()
                .createCriteria(Adjuster.class)
                .add(Restrictions.eq("adjusterCode", adjusterCode))
                .list();
        return results.isEmpty() ? null : results.get(0);
    }

    /**
     * Finds adjusters who are active and whose current caseload is below
     * their maximum caseload, making them available for new assignments.
     */
    @Override
    public List<Adjuster> findAvailableAdjusters() {
        log.debug("Finding available Adjusters");
        return getCurrentSession()
                .createQuery("SELECT a FROM Adjuster a " +
                        "WHERE a.active = true " +
                        "AND a.currentCaseload < a.maxCaseload")
                .list();
    }

    @Override
    public List<Adjuster> findBySpecialization(String specialization) {
        log.debug("Finding Adjusters by specialization: {}", specialization);
        return getCurrentSession()
                .createCriteria(Adjuster.class)
                .add(Restrictions.eq("specialization", specialization))
                .list();
    }
}
