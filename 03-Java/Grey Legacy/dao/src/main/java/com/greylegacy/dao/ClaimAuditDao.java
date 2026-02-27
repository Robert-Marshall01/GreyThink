package com.greylegacy.dao;

import com.greylegacy.domain.ClaimAuditEntry;
import java.util.Date;
import java.util.List;

public interface ClaimAuditDao extends GenericDao<ClaimAuditEntry, Long> {
    List<ClaimAuditEntry> findByClaimId(Long claimId);
    List<ClaimAuditEntry> findByEventType(String eventType);
    List<ClaimAuditEntry> findByDateRange(Date startDate, Date endDate);
}
