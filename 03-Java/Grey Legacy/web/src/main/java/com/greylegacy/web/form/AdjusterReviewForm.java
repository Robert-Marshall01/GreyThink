package com.greylegacy.web.form;

import org.apache.struts.action.ActionErrors;
import org.apache.struts.action.ActionForm;
import org.apache.struts.action.ActionMapping;
import org.apache.struts.action.ActionMessage;

import javax.servlet.http.HttpServletRequest;

/**
 * Struts ActionForm for adjuster review submission.
 * Captures the adjuster's findings, recommended amount, and final recommendation
 * (APPROVE, DENY, or ESCALATE) for a given claim.
 */
public class AdjusterReviewForm extends ActionForm {

    private static final long serialVersionUID = 1L;

    private String claimId;
    private String adjusterCode;
    private String findings;
    private String recommendedAmount;
    private String recommendation;

    @Override
    public ActionErrors validate(ActionMapping mapping, HttpServletRequest request) {
        ActionErrors errors = new ActionErrors();

        if (claimId == null || claimId.trim().isEmpty()) {
            errors.add("claimId", new ActionMessage("error.claimId.required"));
        }
        if (recommendation == null || recommendation.trim().isEmpty()) {
            errors.add("recommendation", new ActionMessage("error.recommendation.required"));
        } else if (!"APPROVE".equals(recommendation)
                && !"DENY".equals(recommendation)
                && !"ESCALATE".equals(recommendation)) {
            errors.add("recommendation", new ActionMessage("error.recommendation.invalid"));
        }

        return errors;
    }

    @Override
    public void reset(ActionMapping mapping, HttpServletRequest request) {
        this.claimId = null;
        this.adjusterCode = null;
        this.findings = null;
        this.recommendedAmount = null;
        this.recommendation = null;
    }

    // All getters and setters
    public String getClaimId() { return claimId; }
    public void setClaimId(String claimId) { this.claimId = claimId; }
    public String getAdjusterCode() { return adjusterCode; }
    public void setAdjusterCode(String adjusterCode) { this.adjusterCode = adjusterCode; }
    public String getFindings() { return findings; }
    public void setFindings(String findings) { this.findings = findings; }
    public String getRecommendedAmount() { return recommendedAmount; }
    public void setRecommendedAmount(String recommendedAmount) { this.recommendedAmount = recommendedAmount; }
    public String getRecommendation() { return recommendation; }
    public void setRecommendation(String recommendation) { this.recommendation = recommendation; }
}
