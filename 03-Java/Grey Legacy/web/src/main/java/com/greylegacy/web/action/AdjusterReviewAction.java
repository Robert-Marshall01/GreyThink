package com.greylegacy.web.action;

import com.greylegacy.service.AdjusterReviewRequest;
import com.greylegacy.service.ClaimService;
import com.greylegacy.web.form.AdjusterReviewForm;
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

/**
 * Struts Action for adjuster review submission.
 * Converts the AdjusterReviewForm to an AdjusterReviewRequest DTO
 * and delegates to ClaimService.submitAdjusterReview().
 * On success, redirects to the claim detail page.
 */
public class AdjusterReviewAction extends Action {

    private static final Logger log = LoggerFactory.getLogger(AdjusterReviewAction.class);

    @Override
    public ActionForward execute(ActionMapping mapping, ActionForm form,
                                  HttpServletRequest request, HttpServletResponse response) throws Exception {

        AdjusterReviewForm reviewForm = (AdjusterReviewForm) form;
        ClaimService claimService = getClaimService(request);

        log.info("Adjuster review submitted for claimId: {}, recommendation: {}",
                reviewForm.getClaimId(), reviewForm.getRecommendation());

        try {
            // Parse claim ID
            Long claimId = Long.valueOf(reviewForm.getClaimId().trim());

            // Convert form to service DTO
            AdjusterReviewRequest reviewRequest = new AdjusterReviewRequest();
            reviewRequest.setAdjusterCode(reviewForm.getAdjusterCode());
            reviewRequest.setFindings(reviewForm.getFindings());
            reviewRequest.setRecommendation(reviewForm.getRecommendation());

            // Parse recommended amount
            if (reviewForm.getRecommendedAmount() != null
                    && !reviewForm.getRecommendedAmount().trim().isEmpty()) {
                reviewRequest.setRecommendedAmount(
                        new BigDecimal(reviewForm.getRecommendedAmount().replace(",", "")));
            }

            // Submit the review
            claimService.submitAdjusterReview(claimId, reviewRequest);

            // Set claim ID for redirect to detail page
            request.setAttribute("claimId", claimId);

            ActionMessages messages = new ActionMessages();
            messages.add(ActionMessages.GLOBAL_MESSAGE,
                    new ActionMessage("adjusterReview.success"));
            saveMessages(request, messages);

            log.info("Adjuster review submitted successfully for claimId: {}", claimId);
            return mapping.findForward("success");

        } catch (NumberFormatException e) {
            log.warn("Invalid claimId in adjuster review form: {}", reviewForm.getClaimId());
            ActionMessages errors = new ActionMessages();
            errors.add(ActionMessages.GLOBAL_MESSAGE,
                    new ActionMessage("adjusterReview.error.invalidClaimId"));
            saveErrors(request, errors);
            return mapping.findForward("failure");

        } catch (IllegalArgumentException e) {
            log.warn("Adjuster review validation error: {}", e.getMessage());
            ActionMessages errors = new ActionMessages();
            errors.add(ActionMessages.GLOBAL_MESSAGE,
                    new ActionMessage("adjusterReview.error.validation", e.getMessage()));
            saveErrors(request, errors);
            return mapping.findForward("failure");

        } catch (IllegalStateException e) {
            log.warn("Adjuster review business rule violation: {}", e.getMessage());
            ActionMessages errors = new ActionMessages();
            errors.add(ActionMessages.GLOBAL_MESSAGE,
                    new ActionMessage("adjusterReview.error.business", e.getMessage()));
            saveErrors(request, errors);
            return mapping.findForward("failure");

        } catch (Exception e) {
            log.error("Unexpected error during adjuster review submission", e);
            ActionMessages errors = new ActionMessages();
            errors.add(ActionMessages.GLOBAL_MESSAGE,
                    new ActionMessage("adjusterReview.error.system"));
            saveErrors(request, errors);
            return mapping.findForward("failure");
        }
    }

    private ClaimService getClaimService(HttpServletRequest request) {
        WebApplicationContext ctx = WebApplicationContextUtils
                .getRequiredWebApplicationContext(request.getServletContext());
        return (ClaimService) ctx.getBean("claimService");
    }
}
