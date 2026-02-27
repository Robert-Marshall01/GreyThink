package com.greylegacy.service;

import java.math.BigDecimal;
import java.util.List;

/**
 * Data Transfer Object for adjuster review submissions.
 */
public class AdjusterReviewRequest {

    private String adjusterCode;
    private String findings;
    private BigDecimal recommendedAmount;
    private String recommendation; // "APPROVE", "DENY", "ESCALATE"
    private List<String> supportingDocuments;

    public AdjusterReviewRequest() {
    }

    public String getAdjusterCode() {
        return adjusterCode;
    }

    public void setAdjusterCode(String adjusterCode) {
        this.adjusterCode = adjusterCode;
    }

    public String getFindings() {
        return findings;
    }

    public void setFindings(String findings) {
        this.findings = findings;
    }

    public BigDecimal getRecommendedAmount() {
        return recommendedAmount;
    }

    public void setRecommendedAmount(BigDecimal recommendedAmount) {
        this.recommendedAmount = recommendedAmount;
    }

    public String getRecommendation() {
        return recommendation;
    }

    public void setRecommendation(String recommendation) {
        this.recommendation = recommendation;
    }

    public List<String> getSupportingDocuments() {
        return supportingDocuments;
    }

    public void setSupportingDocuments(List<String> supportingDocuments) {
        this.supportingDocuments = supportingDocuments;
    }
}
