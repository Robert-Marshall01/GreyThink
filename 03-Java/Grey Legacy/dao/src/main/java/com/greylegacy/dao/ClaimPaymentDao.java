package com.greylegacy.dao;

import com.greylegacy.domain.ClaimPayment;
import com.greylegacy.domain.PaymentStatus;
import java.util.Date;
import java.util.List;

public interface ClaimPaymentDao extends GenericDao<ClaimPayment, Long> {
    ClaimPayment findByPaymentNumber(String paymentNumber);
    List<ClaimPayment> findByClaimId(Long claimId);
    List<ClaimPayment> findByStatus(PaymentStatus status);
    List<ClaimPayment> findScheduledPaymentsDue(Date asOfDate);
    int bulkUpdatePaymentStatus(PaymentStatus newStatus, PaymentStatus oldStatus, Date beforeDate);
}
