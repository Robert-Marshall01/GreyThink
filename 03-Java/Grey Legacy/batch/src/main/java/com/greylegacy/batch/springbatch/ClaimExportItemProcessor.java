package com.greylegacy.batch.springbatch;

import com.greylegacy.domain.Claim;
import com.greylegacy.domain.FraudRiskLevel;
import com.greylegacy.domain.Policy;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.item.ItemProcessor;

import java.io.Serializable;
import java.math.BigDecimal;
import java.text.DecimalFormat;
import java.text.SimpleDateFormat;
import java.util.regex.Pattern;

/**
 * Spring Batch {@link ItemProcessor} that transforms a {@link Claim} domain
 * entity into a {@link ClaimExportRecord} suitable for CSV export.
 *
 * <p>Processing includes date formatting, currency formatting, SSN masking,
 * and string sanitisation for CSV safety. Claims with a {@code null} policy
 * are filtered out (the processor returns {@code null}).</p>
 *
 * <h3>Thread Safety</h3>
 * <p>{@link SimpleDateFormat} and {@link DecimalFormat} are <strong>not</strong>
 * thread-safe. This processor uses {@link ThreadLocal} instances so that the
 * same processor bean can be used safely inside a partitioned step where
 * multiple threads process chunks concurrently.</p>
 *
 * @author Grey Legacy Batch Team
 * @since 1.0.0
 */
public class ClaimExportItemProcessor implements ItemProcessor<Claim, ClaimExportItemProcessor.ClaimExportRecord> {

    private static final Logger log = LoggerFactory.getLogger(ClaimExportItemProcessor.class);

    /**
     * ThreadLocal date formatter — each thread gets its own instance,
     * avoiding the well-known SimpleDateFormat concurrency bug.
     */
    private static final ThreadLocal<SimpleDateFormat> DATE_FORMAT =
            new ThreadLocal<SimpleDateFormat>() {
                @Override
                protected SimpleDateFormat initialValue() {
                    return new SimpleDateFormat("yyyy-MM-dd");
                }
            };

    /**
     * ThreadLocal currency formatter — each thread gets its own instance.
     */
    private static final ThreadLocal<DecimalFormat> CURRENCY_FORMAT =
            new ThreadLocal<DecimalFormat>() {
                @Override
                protected DecimalFormat initialValue() {
                    return new DecimalFormat("#,##0.00");
                }
            };

    private static final Pattern SSN_PATTERN = Pattern.compile("\\d{3}-\\d{2}-\\d{4}");

    // -------------------------------------------------------------------------
    // ItemProcessor implementation
    // -------------------------------------------------------------------------

    /**
     * {@inheritDoc}
     *
     * Transforms a {@link Claim} into a {@link ClaimExportRecord}. Returns
     * {@code null} to filter out claims that have no associated policy.
     */
    @Override
    public ClaimExportRecord process(Claim claim) throws Exception {
        if (claim.getPolicy() == null) {
            log.warn("Skipping claim [{}]: no policy associated", claim.getClaimNumber());
            return null;
        }

        log.debug("Processing claim [{}] for export", claim.getClaimNumber());

        Policy policy = claim.getPolicy();
        ClaimExportRecord record = new ClaimExportRecord();

        record.setClaimNumber(sanitize(claim.getClaimNumber()));
        record.setPolicyNumber(sanitize(policy.getPolicyNumber()));
        record.setClaimantName(sanitize(
                claim.getClaimantFirstName() + " " + claim.getClaimantLastName()));
        record.setStatus(claim.getStatus() != null ? claim.getStatus().name() : "");
        record.setLossDate(claim.getLossDate() != null
                ? DATE_FORMAT.get().format(claim.getLossDate()) : "");
        record.setEstimatedLoss(formatCurrency(claim.getEstimatedLoss()));
        record.setApprovedAmount(formatCurrency(claim.getApprovedAmount()));
        record.setAdjusterCode(sanitize(claim.getAdjusterCode()));
        record.setFraudRiskLevel(claim.getFraudRiskLevel() != null
                ? claim.getFraudRiskLevel().name() : FraudRiskLevel.LOW.name());

        return record;
    }

    // -------------------------------------------------------------------------
    // Helper methods
    // -------------------------------------------------------------------------

    /**
     * Sanitises a string for safe CSV output: removes commas, newlines,
     * and masks SSN-like patterns (XXX-XX-1234).
     */
    private String sanitize(String value) {
        if (value == null) {
            return "";
        }
        String cleaned = value.replace(",", " ").replace("\n", " ").replace("\r", "");
        cleaned = SSN_PATTERN.matcher(cleaned).replaceAll("XXX-XX-$0".substring(
                "XXX-XX-$0".length() - 4));
        // Mask all but last 4 digits of any SSN pattern
        cleaned = SSN_PATTERN.matcher(cleaned).replaceAll("XXX-XX-****");
        return cleaned.trim();
    }

    /**
     * Formats a {@link BigDecimal} as a currency string, returning "0.00"
     * for {@code null} values.
     */
    private String formatCurrency(BigDecimal amount) {
        if (amount == null) {
            return "0.00";
        }
        return CURRENCY_FORMAT.get().format(amount);
    }

    // -------------------------------------------------------------------------
    // Inner class: ClaimExportRecord
    // -------------------------------------------------------------------------

    /**
     * Flat data-transfer object representing a single claim row in the
     * CSV export file.
     */
    public static class ClaimExportRecord implements Serializable {

        private static final long serialVersionUID = 1L;

        private String claimNumber;
        private String policyNumber;
        private String claimantName;
        private String status;
        private String lossDate;
        private String estimatedLoss;
        private String approvedAmount;
        private String adjusterCode;
        private String fraudRiskLevel;

        public String getClaimNumber()                    { return claimNumber; }
        public void setClaimNumber(String claimNumber)    { this.claimNumber = claimNumber; }

        public String getPolicyNumber()                       { return policyNumber; }
        public void setPolicyNumber(String policyNumber)      { this.policyNumber = policyNumber; }

        public String getClaimantName()                       { return claimantName; }
        public void setClaimantName(String claimantName)      { this.claimantName = claimantName; }

        public String getStatus()                             { return status; }
        public void setStatus(String status)                  { this.status = status; }

        public String getLossDate()                           { return lossDate; }
        public void setLossDate(String lossDate)              { this.lossDate = lossDate; }

        public String getEstimatedLoss()                      { return estimatedLoss; }
        public void setEstimatedLoss(String estimatedLoss)    { this.estimatedLoss = estimatedLoss; }

        public String getApprovedAmount()                     { return approvedAmount; }
        public void setApprovedAmount(String approvedAmount)  { this.approvedAmount = approvedAmount; }

        public String getAdjusterCode()                       { return adjusterCode; }
        public void setAdjusterCode(String adjusterCode)      { this.adjusterCode = adjusterCode; }

        public String getFraudRiskLevel()                         { return fraudRiskLevel; }
        public void setFraudRiskLevel(String fraudRiskLevel)      { this.fraudRiskLevel = fraudRiskLevel; }
    }
}
