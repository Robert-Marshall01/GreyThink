package com.greylegacy.dao;

import com.greylegacy.domain.ClaimSnapshot;
import java.util.List;

public interface ClaimSnapshotDao extends GenericDao<ClaimSnapshot, Long> {
    List<ClaimSnapshot> findByClaimId(Long claimId);
    ClaimSnapshot findLatestSnapshot(Long claimId);
}
