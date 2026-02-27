package com.greylegacy.dao.impl;

import com.greylegacy.dao.AbstractHibernateDao;
import com.greylegacy.dao.ClaimPaymentDao;
import com.greylegacy.domain.ClaimPayment;
import com.greylegacy.domain.PaymentStatus;
import org.hibernate.criterion.Restrictions;
import org.springframework.stereotype.Repository;

import java.util.Date;
import java.util.List;

/**
 * Hibernate implementation of {@link ClaimPaymentDao}.
 * Uses Criteria API for dynamic queries and HQL for bulk updates.
 */
@Repository("claimPaymentDao")
@SuppressWarnings("unchecked")
public class ClaimPaymentDaoImpl extends AbstractHibernateDao<ClaimPayment, Long> implements ClaimPaymentDao {

    public ClaimPaymentDaoImpl() {
        super(ClaimPayment.class);
    }

    @Override
    public ClaimPayment findByPaymentNumber(String paymentNumber) {
        log.debug("Finding ClaimPayment by paymentNumber: {}", paymentNumber);
        List<ClaimPayment> results = getCurrentSession()
                .createCriteria(ClaimPayment.class)
                .add(Restrictions.eq("paymentNumber", paymentNumber))
                .list();
        return results.isEmpty() ? null : results.get(0);
    }

    @Override
    public List<ClaimPayment> findByClaimId(Long claimId) {
        log.debug("Finding ClaimPayments by claimId: {}", claimId);
        return getCurrentSession()
                .createQuery("SELECT cp FROM ClaimPayment cp WHERE cp.claim.id = :claimId")
                .setParameter("claimId", claimId)
                .list();
    }

    @Override
    public List<ClaimPayment> findByStatus(PaymentStatus status) {
        log.debug("Finding ClaimPayments by status: {}", status);
        return getCurrentSession()
                .createCriteria(ClaimPayment.class)
                .add(Restrictions.eq("paymentStatus", status))
                .list();
    }

    /**
     * Finds payments with SCHEDULED status whose scheduled date is on or before
     * the given date, indicating they are due for processing.
     */
    @Override
    public List<ClaimPayment> findScheduledPaymentsDue(Date asOfDate) {
        log.debug("Finding scheduled payments due as of: {}", asOfDate);
        return getCurrentSession()
                .createCriteria(ClaimPayment.class)
                .add(Restrictions.eq("paymentStatus", PaymentStatus.SCHEDULED))
                .add(Restrictions.le("scheduledDate", asOfDate))
                .list();
    }

    /**
     * Bulk-updates payment status for all payments matching the old status
     * whose scheduled date is before the given date.
     * Returns the number of rows affected.
     */
    @Override
    public int bulkUpdatePaymentStatus(PaymentStatus newStatus, PaymentStatus oldStatus, Date beforeDate) {
        log.debug("Bulk updating payment status from {} to {} before {}",
                oldStatus, newStatus, beforeDate);
        return getCurrentSession()
                .createQuery("UPDATE ClaimPayment cp SET " +
                        "cp.paymentStatus = :newStatus, " +
                        "cp.processedDate = :processedDate " +
                        "WHERE cp.paymentStatus = :oldStatus " +
                        "AND cp.scheduledDate < :beforeDate")
                .setParameter("newStatus", newStatus)
                .setParameter("processedDate", new Date())
                .setParameter("oldStatus", oldStatus)
                .setParameter("beforeDate", beforeDate)
                .executeUpdate();
    }
}
