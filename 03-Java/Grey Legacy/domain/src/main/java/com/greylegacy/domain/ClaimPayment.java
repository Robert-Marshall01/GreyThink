package com.greylegacy.domain;

import javax.persistence.*;
import java.math.BigDecimal;
import java.util.Date;

/**
 * Represents a payment issued against an insurance claim.
 * Tracks the full payment lifecycle from scheduling through processing.
 */
@Entity
@Table(name = "CLAIM_PAYMENT", indexes = {
        @Index(name = "IDX_PAYMENT_CLAIM_ID", columnList = "CLAIM_ID"),
        @Index(name = "IDX_PAYMENT_STATUS", columnList = "PAYMENT_STATUS")
})
@SequenceGenerator(name = "default_seq", sequenceName = "PAYMENT_SEQ", allocationSize = 1)
public class ClaimPayment extends BaseEntity {

    private static final long serialVersionUID = 1L;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "CLAIM_ID")
    private Claim claim;

    @Column(name = "PAYMENT_NUMBER", length = 20, unique = true, nullable = false)
    private String paymentNumber;

    @Enumerated(EnumType.STRING)
    @Column(name = "PAYMENT_STATUS", nullable = false)
    private PaymentStatus paymentStatus;

    @Enumerated(EnumType.STRING)
    @Column(name = "PAYMENT_METHOD")
    private PaymentMethod paymentMethod;

    @Column(name = "AMOUNT", precision = 14, scale = 2, nullable = false)
    private BigDecimal amount;

    @Temporal(TemporalType.DATE)
    @Column(name = "SCHEDULED_DATE")
    private Date scheduledDate;

    @Temporal(TemporalType.TIMESTAMP)
    @Column(name = "PROCESSED_DATE")
    private Date processedDate;

    @Column(name = "PAYEE_NAME", length = 100, nullable = false)
    private String payeeName;

    @Column(name = "PAYEE_ADDRESS", length = 200)
    private String payeeAddress;

    @Column(name = "CHECK_NUMBER", length = 20)
    private String checkNumber;

    @Column(name = "MEMO", length = 500)
    private String memo;

    @Column(name = "APPROVED_BY", length = 50)
    private String approvedBy;

    // -------------------------------------------------------------------------
    // Getters and Setters
    // -------------------------------------------------------------------------

    public Claim getClaim() {
        return claim;
    }

    public void setClaim(Claim claim) {
        this.claim = claim;
    }

    public String getPaymentNumber() {
        return paymentNumber;
    }

    public void setPaymentNumber(String paymentNumber) {
        this.paymentNumber = paymentNumber;
    }

    public PaymentStatus getPaymentStatus() {
        return paymentStatus;
    }

    public void setPaymentStatus(PaymentStatus paymentStatus) {
        this.paymentStatus = paymentStatus;
    }

    public PaymentMethod getPaymentMethod() {
        return paymentMethod;
    }

    public void setPaymentMethod(PaymentMethod paymentMethod) {
        this.paymentMethod = paymentMethod;
    }

    public BigDecimal getAmount() {
        return amount;
    }

    public void setAmount(BigDecimal amount) {
        this.amount = amount;
    }

    public Date getScheduledDate() {
        return scheduledDate;
    }

    public void setScheduledDate(Date scheduledDate) {
        this.scheduledDate = scheduledDate;
    }

    public Date getProcessedDate() {
        return processedDate;
    }

    public void setProcessedDate(Date processedDate) {
        this.processedDate = processedDate;
    }

    public String getPayeeName() {
        return payeeName;
    }

    public void setPayeeName(String payeeName) {
        this.payeeName = payeeName;
    }

    public String getPayeeAddress() {
        return payeeAddress;
    }

    public void setPayeeAddress(String payeeAddress) {
        this.payeeAddress = payeeAddress;
    }

    public String getCheckNumber() {
        return checkNumber;
    }

    public void setCheckNumber(String checkNumber) {
        this.checkNumber = checkNumber;
    }

    public String getMemo() {
        return memo;
    }

    public void setMemo(String memo) {
        this.memo = memo;
    }

    public String getApprovedBy() {
        return approvedBy;
    }

    public void setApprovedBy(String approvedBy) {
        this.approvedBy = approvedBy;
    }

    // -------------------------------------------------------------------------
    // toString
    // -------------------------------------------------------------------------

    @Override
    public String toString() {
        return "ClaimPayment{" +
                "id=" + getId() +
                ", paymentNumber='" + paymentNumber + '\'' +
                ", paymentStatus=" + paymentStatus +
                ", paymentMethod=" + paymentMethod +
                ", amount=" + amount +
                ", scheduledDate=" + scheduledDate +
                ", processedDate=" + processedDate +
                ", payeeName='" + payeeName + '\'' +
                ", checkNumber='" + checkNumber + '\'' +
                ", approvedBy='" + approvedBy + '\'' +
                '}';
    }
}
