package com.greylegacy.service;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

/**
 * Data Transfer Object representing the result of a claim adjudication.
 */
public class AdjudicationResult {

    private Long claimId;
    private boolean coverageValid;
    private boolean policyActive;
    private boolean adjusterAssigned;
    private String adjusterCode;
    private BigDecimal preliminaryLiability;
    private List<String> adjudicationNotes;
    private String adjudicationStatus; // "APPROVED", "DENIED", "PENDING_REVIEW"

    public AdjudicationResult() {
        this.adjudicationNotes = new ArrayList<>();
    }

    /**
     * Builder-style method to add a note to the adjudication result.
     */
    public AdjudicationResult addNote(String note) {
        if (this.adjudicationNotes == null) {
            this.adjudicationNotes = new ArrayList<>();
        }
        this.adjudicationNotes.add(note);
        return this;
    }

    public Long getClaimId() {
        return claimId;
    }

    public void setClaimId(Long claimId) {
        this.claimId = claimId;
    }

    public boolean isCoverageValid() {
        return coverageValid;
    }

    public void setCoverageValid(boolean coverageValid) {
        this.coverageValid = coverageValid;
    }

    public boolean isPolicyActive() {
        return policyActive;
    }

    public void setPolicyActive(boolean policyActive) {
        this.policyActive = policyActive;
    }

    public boolean isAdjusterAssigned() {
        return adjusterAssigned;
    }

    public void setAdjusterAssigned(boolean adjusterAssigned) {
        this.adjusterAssigned = adjusterAssigned;
    }

    public String getAdjusterCode() {
        return adjusterCode;
    }

    public void setAdjusterCode(String adjusterCode) {
        this.adjusterCode = adjusterCode;
    }

    public BigDecimal getPreliminaryLiability() {
        return preliminaryLiability;
    }

    public void setPreliminaryLiability(BigDecimal preliminaryLiability) {
        this.preliminaryLiability = preliminaryLiability;
    }

    public List<String> getAdjudicationNotes() {
        return adjudicationNotes;
    }

    public void setAdjudicationNotes(List<String> adjudicationNotes) {
        this.adjudicationNotes = adjudicationNotes;
    }

    public String getAdjudicationStatus() {
        return adjudicationStatus;
    }

    public void setAdjudicationStatus(String adjudicationStatus) {
        this.adjudicationStatus = adjudicationStatus;
    }
}
