package com.greylegacy.web.form;

import org.apache.struts.action.ActionErrors;
import org.apache.struts.action.ActionForm;
import org.apache.struts.action.ActionMapping;
import org.apache.struts.action.ActionMessage;

import javax.servlet.http.HttpServletRequest;

/**
 * Struts ActionForm for claim search.
 * Allows searching by claim number, policy number, claimant name, status, or date range.
 * At least one search criterion must be provided.
 */
public class ClaimSearchForm extends ActionForm {

    private static final long serialVersionUID = 1L;

    private String claimNumber;
    private String policyNumber;
    private String claimantLastName;
    private String status;
    private String dateFrom;
    private String dateTo;

    @Override
    public ActionErrors validate(ActionMapping mapping, HttpServletRequest request) {
        ActionErrors errors = new ActionErrors();

        boolean hasClaimNumber = claimNumber != null && !claimNumber.trim().isEmpty();
        boolean hasPolicyNumber = policyNumber != null && !policyNumber.trim().isEmpty();
        boolean hasClaimantLastName = claimantLastName != null && !claimantLastName.trim().isEmpty();
        boolean hasStatus = status != null && !status.trim().isEmpty();
        boolean hasDateFrom = dateFrom != null && !dateFrom.trim().isEmpty();
        boolean hasDateTo = dateTo != null && !dateTo.trim().isEmpty();

        if (!hasClaimNumber && !hasPolicyNumber && !hasClaimantLastName
                && !hasStatus && !hasDateFrom && !hasDateTo) {
            errors.add("claimNumber", new ActionMessage("error.search.atLeastOneRequired"));
        }

        return errors;
    }

    @Override
    public void reset(ActionMapping mapping, HttpServletRequest request) {
        this.claimNumber = null;
        this.policyNumber = null;
        this.claimantLastName = null;
        this.status = null;
        this.dateFrom = null;
        this.dateTo = null;
    }

    // All getters and setters
    public String getClaimNumber() { return claimNumber; }
    public void setClaimNumber(String claimNumber) { this.claimNumber = claimNumber; }
    public String getPolicyNumber() { return policyNumber; }
    public void setPolicyNumber(String policyNumber) { this.policyNumber = policyNumber; }
    public String getClaimantLastName() { return claimantLastName; }
    public void setClaimantLastName(String claimantLastName) { this.claimantLastName = claimantLastName; }
    public String getStatus() { return status; }
    public void setStatus(String status) { this.status = status; }
    public String getDateFrom() { return dateFrom; }
    public void setDateFrom(String dateFrom) { this.dateFrom = dateFrom; }
    public String getDateTo() { return dateTo; }
    public void setDateTo(String dateTo) { this.dateTo = dateTo; }
}
