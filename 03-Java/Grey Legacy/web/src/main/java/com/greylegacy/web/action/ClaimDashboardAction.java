package com.greylegacy.web.action;

import com.greylegacy.domain.Claim;
import com.greylegacy.domain.ClaimStatus;
import com.greylegacy.service.ClaimService;
import org.apache.struts.action.Action;
import org.apache.struts.action.ActionForm;
import org.apache.struts.action.ActionForward;
import org.apache.struts.action.ActionMapping;
import org.apache.struts.action.ActionMessage;
import org.apache.struts.action.ActionMessages;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.web.context.WebApplicationContext;
import org.springframework.web.context.support.WebApplicationContextUtils;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Struts Action for the main claims dashboard.
 * Loads summary counts by claim status, retrieves recent open claims,
 * and sets all dashboard data on request attributes for the view.
 */
public class ClaimDashboardAction extends Action {

    private static final Logger log = LoggerFactory.getLogger(ClaimDashboardAction.class);

    @Override
    public ActionForward execute(ActionMapping mapping, ActionForm form,
                                  HttpServletRequest request, HttpServletResponse response) throws Exception {

        ClaimService claimService = getClaimService(request);

        try {
            // Build status counts map
            Map<String, Integer> statusCounts = new HashMap<>();
            for (ClaimStatus status : ClaimStatus.values()) {
                List<Claim> claimsByStatus = claimService.findClaimsByStatus(status);
                int count = (claimsByStatus != null) ? claimsByStatus.size() : 0;
                statusCounts.put(status.name(), count);
            }
            request.setAttribute("statusCounts", statusCounts);

            // Calculate total open claims count
            int totalOpen = 0;
            totalOpen += getCount(statusCounts, ClaimStatus.FNOL_RECEIVED.name());
            totalOpen += getCount(statusCounts, ClaimStatus.UNDER_REVIEW.name());
            totalOpen += getCount(statusCounts, ClaimStatus.ADJUSTER_ASSIGNED.name());
            totalOpen += getCount(statusCounts, ClaimStatus.INVESTIGATION.name());
            totalOpen += getCount(statusCounts, ClaimStatus.APPROVED.name());
            totalOpen += getCount(statusCounts, ClaimStatus.REOPENED.name());
            request.setAttribute("totalOpenClaims", totalOpen);

            // Get recent open claims for the dashboard listing
            List<Claim> recentOpenClaims = claimService.findOpenClaims();
            request.setAttribute("recentOpenClaims", recentOpenClaims);

            // Fraud-suspected claims count
            int fraudCount = getCount(statusCounts, ClaimStatus.FRAUD_SUSPECTED.name());
            request.setAttribute("fraudSuspectedCount", fraudCount);

            log.info("Dashboard loaded - totalOpen: {}, fraudSuspected: {}", totalOpen, fraudCount);
            return mapping.findForward("dashboard");

        } catch (Exception e) {
            log.error("Unexpected error loading claims dashboard", e);
            ActionMessages errors = new ActionMessages();
            errors.add(ActionMessages.GLOBAL_MESSAGE,
                    new ActionMessage("dashboard.error.system"));
            saveErrors(request, errors);
            return mapping.findForward("dashboard");
        }
    }

    private int getCount(Map<String, Integer> counts, String key) {
        Integer value = counts.get(key);
        return (value != null) ? value : 0;
    }

    private ClaimService getClaimService(HttpServletRequest request) {
        WebApplicationContext ctx = WebApplicationContextUtils
                .getRequiredWebApplicationContext(request.getServletContext());
        return (ClaimService) ctx.getBean("claimService");
    }
}
