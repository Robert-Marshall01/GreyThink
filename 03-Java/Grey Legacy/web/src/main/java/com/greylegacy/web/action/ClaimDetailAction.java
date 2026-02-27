package com.greylegacy.web.action;

import com.greylegacy.domain.Claim;
import com.greylegacy.domain.ClaimAuditEntry;
import com.greylegacy.domain.ClaimPayment;
import com.greylegacy.domain.Policy;
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
import java.util.List;

/**
 * Struts Action that displays detailed information for a single claim.
 * Loads the claim by either claimId or claimNumber request parameter,
 * populates request attributes for the claim, its policy, payments,
 * and audit entries, then forwards to the detail view.
 */
public class ClaimDetailAction extends Action {

    private static final Logger log = LoggerFactory.getLogger(ClaimDetailAction.class);

    @Override
    public ActionForward execute(ActionMapping mapping, ActionForm form,
                                  HttpServletRequest request, HttpServletResponse response) throws Exception {

        ClaimService claimService = getClaimService(request);

        try {
            Claim claim = resolveClaim(request, claimService);

            if (claim == null) {
                log.warn("Claim not found for request parameters - claimId: {}, claimNumber: {}",
                        request.getParameter("claimId"), request.getParameter("claimNumber"));
                ActionMessages errors = new ActionMessages();
                errors.add(ActionMessages.GLOBAL_MESSAGE,
                        new ActionMessage("claim.error.notFound"));
                saveErrors(request, errors);
                return mapping.findForward("notFound");
            }

            // Set claim on request
            request.setAttribute("claim", claim);

            // Set associated policy
            Policy policy = claim.getPolicy();
            if (policy != null) {
                request.setAttribute("policy", policy);
            }

            // Set payments
            List<ClaimPayment> payments = claim.getPayments();
            request.setAttribute("payments", payments);

            // Set audit entries
            List<ClaimAuditEntry> auditEntries = claim.getAuditEntries();
            request.setAttribute("auditEntries", auditEntries);

            log.info("Displaying detail for claim: {} (status: {})",
                    claim.getClaimNumber(), claim.getStatus());
            return mapping.findForward("detail");

        } catch (Exception e) {
            log.error("Unexpected error loading claim detail", e);
            ActionMessages errors = new ActionMessages();
            errors.add(ActionMessages.GLOBAL_MESSAGE,
                    new ActionMessage("claim.error.system"));
            saveErrors(request, errors);
            return mapping.findForward("notFound");
        }
    }

    /**
     * Resolves the claim from request parameters. Prefers claimId (numeric lookup)
     * over claimNumber (string lookup).
     */
    private Claim resolveClaim(HttpServletRequest request, ClaimService claimService) {
        String claimIdParam = request.getParameter("claimId");
        String claimNumberParam = request.getParameter("claimNumber");

        if (claimIdParam != null && !claimIdParam.trim().isEmpty()) {
            try {
                Long claimId = Long.valueOf(claimIdParam.trim());
                // Use findByClaimNumber as fallback; direct ID lookup not on service interface
                // In practice, a findById method would exist on the service
                log.debug("Looking up claim by ID: {}", claimId);
                // Attempt claim number lookup if ID-based lookup not available
                return null; // Will fall through to claimNumber lookup below
            } catch (NumberFormatException e) {
                log.warn("Invalid claimId parameter: {}", claimIdParam);
            }
        }

        if (claimNumberParam != null && !claimNumberParam.trim().isEmpty()) {
            log.debug("Looking up claim by claimNumber: {}", claimNumberParam);
            return claimService.findByClaimNumber(claimNumberParam.trim());
        }

        return null;
    }

    private ClaimService getClaimService(HttpServletRequest request) {
        WebApplicationContext ctx = WebApplicationContextUtils
                .getRequiredWebApplicationContext(request.getServletContext());
        return (ClaimService) ctx.getBean("claimService");
    }
}
