package com.greylegacy.modernization.dto;

import java.math.BigDecimal;
import java.util.Date;
import java.util.List;

/**
 * Data Transfer Object for Claim — replaces direct entity exposure in REST APIs.
 *
 * <p>In the legacy system, Struts Actions and Spring MVC controllers often
 * passed Hibernate entities directly to JSP views, creating tight coupling
 * between the persistence layer and the presentation layer.</p>
 *
 * <p>This DTO decouples the API contract from the internal domain model,
 * preventing issues like:</p>
 * <ul>
 *   <li>LazyInitializationException from serializing lazy-loaded collections</li>
 *   <li>Circular reference loops (Claim → Policy → Claims → ...)</li>
 *   <li>Exposing internal fields (fraud scores, SSNs) to API consumers</li>
 *   <li>Breaking API contracts when the domain model changes</li>
 * </ul>
 */
public class ClaimDto {

    private Long id;
    private String claimNumber;
    private String policyNumber;
    private String status;
    private String claimType;
    private Date lossDate;
    private Date reportedDate;
    private String lossDescription;
    private String lossLocation;
    private String claimantFirstName;
    private String claimantLastName;
    private String claimantPhone;
    private String claimantEmail;
    private String adjusterCode;
    private BigDecimal estimatedLoss;
    private BigDecimal approvedAmount;
    private BigDecimal deductibleApplied;
    private String fraudRiskLevel;
    private Date closedDate;
    private String closedReason;
    private List<PaymentDto> payments;
    private Date createdDate;
    private Date updatedDate;

    public ClaimDto() {}

    // --- Getters and Setters ---

    public Long getId() { return id; }
    public void setId(Long id) { this.id = id; }

    public String getClaimNumber() { return claimNumber; }
    public void setClaimNumber(String claimNumber) { this.claimNumber = claimNumber; }

    public String getPolicyNumber() { return policyNumber; }
    public void setPolicyNumber(String policyNumber) { this.policyNumber = policyNumber; }

    public String getStatus() { return status; }
    public void setStatus(String status) { this.status = status; }

    public String getClaimType() { return claimType; }
    public void setClaimType(String claimType) { this.claimType = claimType; }

    public Date getLossDate() { return lossDate; }
    public void setLossDate(Date lossDate) { this.lossDate = lossDate; }

    public Date getReportedDate() { return reportedDate; }
    public void setReportedDate(Date reportedDate) { this.reportedDate = reportedDate; }

    public String getLossDescription() { return lossDescription; }
    public void setLossDescription(String lossDescription) { this.lossDescription = lossDescription; }

    public String getLossLocation() { return lossLocation; }
    public void setLossLocation(String lossLocation) { this.lossLocation = lossLocation; }

    public String getClaimantFirstName() { return claimantFirstName; }
    public void setClaimantFirstName(String claimantFirstName) { this.claimantFirstName = claimantFirstName; }

    public String getClaimantLastName() { return claimantLastName; }
    public void setClaimantLastName(String claimantLastName) { this.claimantLastName = claimantLastName; }

    public String getClaimantPhone() { return claimantPhone; }
    public void setClaimantPhone(String claimantPhone) { this.claimantPhone = claimantPhone; }

    public String getClaimantEmail() { return claimantEmail; }
    public void setClaimantEmail(String claimantEmail) { this.claimantEmail = claimantEmail; }

    public String getAdjusterCode() { return adjusterCode; }
    public void setAdjusterCode(String adjusterCode) { this.adjusterCode = adjusterCode; }

    public BigDecimal getEstimatedLoss() { return estimatedLoss; }
    public void setEstimatedLoss(BigDecimal estimatedLoss) { this.estimatedLoss = estimatedLoss; }

    public BigDecimal getApprovedAmount() { return approvedAmount; }
    public void setApprovedAmount(BigDecimal approvedAmount) { this.approvedAmount = approvedAmount; }

    public BigDecimal getDeductibleApplied() { return deductibleApplied; }
    public void setDeductibleApplied(BigDecimal deductibleApplied) { this.deductibleApplied = deductibleApplied; }

    public String getFraudRiskLevel() { return fraudRiskLevel; }
    public void setFraudRiskLevel(String fraudRiskLevel) { this.fraudRiskLevel = fraudRiskLevel; }

    public Date getClosedDate() { return closedDate; }
    public void setClosedDate(Date closedDate) { this.closedDate = closedDate; }

    public String getClosedReason() { return closedReason; }
    public void setClosedReason(String closedReason) { this.closedReason = closedReason; }

    public List<PaymentDto> getPayments() { return payments; }
    public void setPayments(List<PaymentDto> payments) { this.payments = payments; }

    public Date getCreatedDate() { return createdDate; }
    public void setCreatedDate(Date createdDate) { this.createdDate = createdDate; }

    public Date getUpdatedDate() { return updatedDate; }
    public void setUpdatedDate(Date updatedDate) { this.updatedDate = updatedDate; }

    /**
     * Nested DTO for payment data — avoids exposing ClaimPayment entity.
     */
    public static class PaymentDto {
        private Long id;
        private String paymentNumber;
        private String status;
        private String method;
        private BigDecimal amount;
        private Date scheduledDate;
        private Date processedDate;
        private String payeeName;

        public Long getId() { return id; }
        public void setId(Long id) { this.id = id; }
        public String getPaymentNumber() { return paymentNumber; }
        public void setPaymentNumber(String paymentNumber) { this.paymentNumber = paymentNumber; }
        public String getStatus() { return status; }
        public void setStatus(String status) { this.status = status; }
        public String getMethod() { return method; }
        public void setMethod(String method) { this.method = method; }
        public BigDecimal getAmount() { return amount; }
        public void setAmount(BigDecimal amount) { this.amount = amount; }
        public Date getScheduledDate() { return scheduledDate; }
        public void setScheduledDate(Date scheduledDate) { this.scheduledDate = scheduledDate; }
        public Date getProcessedDate() { return processedDate; }
        public void setProcessedDate(Date processedDate) { this.processedDate = processedDate; }
        public String getPayeeName() { return payeeName; }
        public void setPayeeName(String payeeName) { this.payeeName = payeeName; }
    }
}
