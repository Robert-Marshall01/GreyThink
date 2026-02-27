package com.greylegacy.dao;

import com.greylegacy.domain.Policy;
import com.greylegacy.domain.PolicyStatus;
import java.util.Date;
import java.util.List;

public interface PolicyDao extends GenericDao<Policy, Long> {
    Policy findByPolicyNumber(String policyNumber);
    List<Policy> findByStatus(PolicyStatus status);
    List<Policy> findByHolderLastName(String lastName);
    List<Policy> findActivePolicies(Date asOfDate);
    List<Policy> findPoliciesWithClaimsFetchJoin(PolicyStatus status);
    List<Policy> findExpiringPolicies(Date startDate, Date endDate);
}
