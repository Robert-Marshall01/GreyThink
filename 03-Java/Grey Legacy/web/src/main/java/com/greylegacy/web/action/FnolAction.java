package com.greylegacy.web.action;

import com.greylegacy.domain.Claim;
import com.greylegacy.domain.ClaimType;
import com.greylegacy.service.ClaimService;
import com.greylegacy.service.FnolRequest;
import com.greylegacy.web.form.FnolForm;
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
import java.math.BigDecimal;
import java.text.SimpleDateFormat;
import java.util.Date;

/**
 * Struts Action for First Notice of Loss (FNOL) submission.
 * Handles the form submission, converts form data to service DTOs,
 * and delegates to ClaimService for business logic.
 * 
 * Spring beans are retrieved from the WebApplicationContext since
 * Struts 1 does not natively support Spring DI in action classes.
 */
public class FnolAction extends Action {

    private static final Logger log = LoggerFactory.getLogger(FnolAction.class);
    private static final String DATE_FORMAT = "yyyy-MM-dd";

    @Override
    public ActionForward execute(ActionMapping mapping, ActionForm form,
                                  HttpServletRequest request, HttpServletResponse response) throws Exception {

        FnolForm fnolForm = (FnolForm) form;
        log.info("FNOL submission received for policy: {}", fnolForm.getPolicyNumber());

        // Retrieve Spring-managed service
        ClaimService claimService = getClaimService(request);

        try {
            // Convert form to service request
            FnolRequest fnolRequest = new FnolRequest();
            fnolRequest.setPolicyNumber(fnolForm.getPolicyNumber());
            fnolRequest.setClaimantFirstName(fnolForm.getClaimantFirstName());
            fnolRequest.setClaimantLastName(fnolForm.getClaimantLastName());
            fnolRequest.setClaimantPhone(fnolForm.getClaimantPhone());
            fnolRequest.setClaimantEmail(fnolForm.getClaimantEmail());
            fnolRequest.setClaimType(ClaimType.valueOf(fnolForm.getClaimType()));
            fnolRequest.setLossDescription(fnolForm.getLossDescription());
            fnolRequest.setLossLocation(fnolForm.getLossLocation());
            fnolRequest.setReportedBy(request.getRemoteUser() != null ? request.getRemoteUser() : "WEB_USER");

            // Parse date
            if (fnolForm.getLossDate() != null && !fnolForm.getLossDate().trim().isEmpty()) {
                SimpleDateFormat sdf = new SimpleDateFormat(DATE_FORMAT);
                sdf.setLenient(false);
                fnolRequest.setLossDate(sdf.parse(fnolForm.getLossDate()));
            } else {
                fnolRequest.setLossDate(new Date());
            }

            // Parse estimated loss
            if (fnolForm.getEstimatedLoss() != null && !fnolForm.getEstimatedLoss().trim().isEmpty()) {
                fnolRequest.setEstimatedLoss(new BigDecimal(fnolForm.getEstimatedLoss().replace(",", "")));
            }

            // Submit FNOL
            Claim claim = claimService.submitFnol(fnolRequest);

            // Set success attributes
            request.setAttribute("claim", claim);
            request.setAttribute("successMessage", "FNOL submitted successfully. Claim Number: " + claim.getClaimNumber());

            ActionMessages messages = new ActionMessages();
            messages.add(ActionMessages.GLOBAL_MESSAGE,
                    new ActionMessage("fnol.success", claim.getClaimNumber()));
            saveMessages(request, messages);

            log.info("FNOL submitted successfully. Claim: {}", claim.getClaimNumber());
            return mapping.findForward("success");

        } catch (IllegalArgumentException e) {
            log.warn("FNOL validation error: {}", e.getMessage());
            ActionMessages errors = new ActionMessages();
            errors.add(ActionMessages.GLOBAL_MESSAGE, new ActionMessage("fnol.error.validation", e.getMessage()));
            saveErrors(request, errors);
            return mapping.findForward("failure");

        } catch (IllegalStateException e) {
            log.warn("FNOL business rule violation: {}", e.getMessage());
            ActionMessages errors = new ActionMessages();
            errors.add(ActionMessages.GLOBAL_MESSAGE, new ActionMessage("fnol.error.business", e.getMessage()));
            saveErrors(request, errors);
            return mapping.findForward("failure");

        } catch (Exception e) {
            log.error("Unexpected error during FNOL submission", e);
            ActionMessages errors = new ActionMessages();
            errors.add(ActionMessages.GLOBAL_MESSAGE, new ActionMessage("fnol.error.system"));
            saveErrors(request, errors);
            return mapping.findForward("error");
        }
    }

    private ClaimService getClaimService(HttpServletRequest request) {
        WebApplicationContext ctx = WebApplicationContextUtils
                .getRequiredWebApplicationContext(request.getServletContext());
        return (ClaimService) ctx.getBean("claimService");
    }
}
