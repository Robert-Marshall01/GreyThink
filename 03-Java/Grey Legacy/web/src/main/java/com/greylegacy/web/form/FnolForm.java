package com.greylegacy.web.form;

import org.apache.struts.action.ActionErrors;
import org.apache.struts.action.ActionForm;
import org.apache.struts.action.ActionMapping;
import org.apache.struts.action.ActionMessage;

import javax.servlet.http.HttpServletRequest;

/**
 * Struts ActionForm for First Notice of Loss submission.
 * Captures claimant info, policy number, and incident details.
 */
public class FnolForm extends ActionForm {

    private static final long serialVersionUID = 1L;

    private String policyNumber;
    private String claimantFirstName;
    private String claimantLastName;
    private String claimantPhone;
    private String claimantEmail;
    private String claimType;
    private String lossDate;
    private String lossDescription;
    private String lossLocation;
    private String estimatedLoss;

    @Override
    public ActionErrors validate(ActionMapping mapping, HttpServletRequest request) {
        ActionErrors errors = new ActionErrors();

        if (policyNumber == null || policyNumber.trim().isEmpty()) {
            errors.add("policyNumber", new ActionMessage("error.policyNumber.required"));
        }
        if (claimantFirstName == null || claimantFirstName.trim().isEmpty()) {
            errors.add("claimantFirstName", new ActionMessage("error.claimantFirstName.required"));
        }
        if (claimantLastName == null || claimantLastName.trim().isEmpty()) {
            errors.add("claimantLastName", new ActionMessage("error.claimantLastName.required"));
        }
        if (claimType == null || claimType.trim().isEmpty()) {
            errors.add("claimType", new ActionMessage("error.claimType.required"));
        }
        if (lossDate == null || lossDate.trim().isEmpty()) {
            errors.add("lossDate", new ActionMessage("error.lossDate.required"));
        }
        if (lossDescription == null || lossDescription.trim().isEmpty()) {
            errors.add("lossDescription", new ActionMessage("error.lossDescription.required"));
        }

        return errors;
    }

    @Override
    public void reset(ActionMapping mapping, HttpServletRequest request) {
        this.policyNumber = null;
        this.claimantFirstName = null;
        this.claimantLastName = null;
        this.claimantPhone = null;
        this.claimantEmail = null;
        this.claimType = null;
        this.lossDate = null;
        this.lossDescription = null;
        this.lossLocation = null;
        this.estimatedLoss = null;
    }

    // All getters and setters
    public String getPolicyNumber() { return policyNumber; }
    public void setPolicyNumber(String policyNumber) { this.policyNumber = policyNumber; }
    public String getClaimantFirstName() { return claimantFirstName; }
    public void setClaimantFirstName(String claimantFirstName) { this.claimantFirstName = claimantFirstName; }
    public String getClaimantLastName() { return claimantLastName; }
    public void setClaimantLastName(String claimantLastName) { this.claimantLastName = claimantLastName; }
    public String getClaimantPhone() { return claimantPhone; }
    public void setClaimantPhone(String claimantPhone) { this.claimantPhone = claimantPhone; }
    public String getClaimantEmail() { return claimantEmail; }
    public void setClaimantEmail(String claimantEmail) { this.claimantEmail = claimantEmail; }
    public String getClaimType() { return claimType; }
    public void setClaimType(String claimType) { this.claimType = claimType; }
    public String getLossDate() { return lossDate; }
    public void setLossDate(String lossDate) { this.lossDate = lossDate; }
    public String getLossDescription() { return lossDescription; }
    public void setLossDescription(String lossDescription) { this.lossDescription = lossDescription; }
    public String getLossLocation() { return lossLocation; }
    public void setLossLocation(String lossLocation) { this.lossLocation = lossLocation; }
    public String getEstimatedLoss() { return estimatedLoss; }
    public void setEstimatedLoss(String estimatedLoss) { this.estimatedLoss = estimatedLoss; }
}
