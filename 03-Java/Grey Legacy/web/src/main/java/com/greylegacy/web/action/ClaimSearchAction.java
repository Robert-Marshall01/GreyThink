package com.greylegacy.web.action;

import com.greylegacy.domain.Claim;
import com.greylegacy.domain.ClaimStatus;
import com.greylegacy.service.ClaimService;
import com.greylegacy.web.form.ClaimSearchForm;
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
import java.util.ArrayList;
import java.util.List;

/**
 * Struts Action for claim search functionality.
 * Supports searching by claim number, policy number, claim status,
 * or claimant last name. Retrieves ClaimService from the Spring
 * WebApplicationContext.
 */
public class ClaimSearchAction extends Action {

    private static final Logger log = LoggerFactory.getLogger(ClaimSearchAction.class);

    @Override
    public ActionForward execute(ActionMapping mapping, ActionForm form,
                                  HttpServletRequest request, HttpServletResponse response) throws Exception {

        ClaimSearchForm searchForm = (ClaimSearchForm) form;
        ClaimService claimService = getClaimService(request);

        log.info("Claim search initiated - claimNumber: {}, policyNumber: {}, status: {}, claimantLastName: {}",
                searchForm.getClaimNumber(), searchForm.getPolicyNumber(),
                searchForm.getStatus(), searchForm.getClaimantLastName());

        try {
            List<Claim> claims = new ArrayList<>();

            // Search by claim number (exact match, most specific)
            if (isNotEmpty(searchForm.getClaimNumber())) {
                Claim claim = claimService.findByClaimNumber(searchForm.getClaimNumber().trim());
                if (claim != null) {
                    claims.add(claim);
                }

            // Search by policy number
            } else if (isNotEmpty(searchForm.getPolicyNumber())) {
                List<Claim> policyClaims = claimService.findClaimsByPolicy(searchForm.getPolicyNumber().trim());
                if (policyClaims != null) {
                    claims.addAll(policyClaims);
                }

            // Search by status
            } else if (isNotEmpty(searchForm.getStatus())) {
                ClaimStatus status = ClaimStatus.valueOf(searchForm.getStatus().trim());
                List<Claim> statusClaims = claimService.findClaimsByStatus(status);
                if (statusClaims != null) {
                    claims.addAll(statusClaims);
                }

            // Search by claimant last name — use open claims filtered by name
            } else if (isNotEmpty(searchForm.getClaimantLastName())) {
                String lastName = searchForm.getClaimantLastName().trim().toLowerCase();
                List<Claim> allOpen = claimService.findOpenClaims();
                if (allOpen != null) {
                    for (Claim c : allOpen) {
                        if (c.getClaimantLastName() != null
                                && c.getClaimantLastName().toLowerCase().contains(lastName)) {
                            claims.add(c);
                        }
                    }
                }
            }

            request.setAttribute("claims", claims);
            request.setAttribute("resultCount", claims.size());

            log.info("Claim search returned {} result(s)", claims.size());
            return mapping.findForward("results");

        } catch (IllegalArgumentException e) {
            log.warn("Invalid search criteria: {}", e.getMessage());
            ActionMessages errors = new ActionMessages();
            errors.add(ActionMessages.GLOBAL_MESSAGE,
                    new ActionMessage("search.error.invalid", e.getMessage()));
            saveErrors(request, errors);
            return mapping.findForward("failure");

        } catch (Exception e) {
            log.error("Unexpected error during claim search", e);
            ActionMessages errors = new ActionMessages();
            errors.add(ActionMessages.GLOBAL_MESSAGE,
                    new ActionMessage("search.error.system"));
            saveErrors(request, errors);
            return mapping.findForward("failure");
        }
    }

    private boolean isNotEmpty(String value) {
        return value != null && !value.trim().isEmpty();
    }

    private ClaimService getClaimService(HttpServletRequest request) {
        WebApplicationContext ctx = WebApplicationContextUtils
                .getRequiredWebApplicationContext(request.getServletContext());
        return (ClaimService) ctx.getBean("claimService");
    }
}
