package com.greylegacy.batch.springbatch;

import com.greylegacy.batch.springbatch.PolicyFileImportReader.PolicyImportRecord;
import com.greylegacy.domain.Policy;
import com.greylegacy.domain.PolicyStatus;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.item.ItemProcessor;

import java.math.BigDecimal;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashSet;
import java.util.Set;

/**
 * Spring Batch {@link ItemProcessor} that validates and transforms a
 * {@link PolicyImportRecord} (flat CSV row) into a {@link Policy} domain
 * entity suitable for persistence.
 *
 * <h3>Processing Pipeline</h3>
 * <ol>
 *   <li>Validates required fields (policyNumber, holderFirst/Last, dates)</li>
 *   <li>Parses date strings into {@link Date} objects</li>
 *   <li>Validates business rules (effectiveDate &lt; expirationDate,
 *       positive premium, deductible &le; coverageLimit)</li>
 *   <li>Detects duplicate policy numbers within the same batch run</li>
 *   <li>Constructs a fully populated {@link Policy} entity</li>
 * </ol>
 *
 * <h3>Filtering</h3>
 * <p>Returns {@code null} for records that fail validation, which causes
 * Spring Batch to filter (skip) them and increment the filter count.</p>
 *
 * <h3>Thread Safety</h3>
 * <p>Uses {@link ThreadLocal} for {@link SimpleDateFormat} to support
 * partitioned steps with concurrent threads.</p>
 *
 * @author Grey Legacy Batch Team
 * @since 1.0.0
 */
public class PolicyImportProcessor
        implements ItemProcessor<PolicyImportRecord, Policy> {

    private static final Logger log = LoggerFactory.getLogger(PolicyImportProcessor.class);

    /** Thread-safe date parser for policy effective/expiration dates. */
    private static final ThreadLocal<SimpleDateFormat> DATE_FORMAT =
            new ThreadLocal<SimpleDateFormat>() {
                @Override
                protected SimpleDateFormat initialValue() {
                    SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd");
                    sdf.setLenient(false);
                    return sdf;
                }
            };

    /** Tracks policy numbers already processed in this run to detect duplicates. */
    private final Set<String> seenPolicyNumbers = new HashSet<>();

    private int validCount;
    private int filteredCount;

    // -------------------------------------------------------------------------
    // ItemProcessor implementation
    // -------------------------------------------------------------------------

    /**
     * {@inheritDoc}
     *
     * Transforms a {@link PolicyImportRecord} into a {@link Policy}.
     * Returns {@code null} to filter records that fail validation.
     */
    @Override
    public Policy process(PolicyImportRecord record) throws Exception {
        log.debug("Processing policy import record: {}", record);

        // ---- Required-field validation ------------------------------------

        if (isBlank(record.getPolicyNumber())) {
            log.warn("Skipping record: policyNumber is blank");
            filteredCount++;
            return null;
        }

        if (isBlank(record.getHolderFirstName()) || isBlank(record.getHolderLastName())) {
            log.warn("Skipping policy [{}]: missing holder name",
                    record.getPolicyNumber());
            filteredCount++;
            return null;
        }

        if (isBlank(record.getEffectiveDate()) || isBlank(record.getExpirationDate())) {
            log.warn("Skipping policy [{}]: missing effective/expiration date",
                    record.getPolicyNumber());
            filteredCount++;
            return null;
        }

        // ---- Duplicate detection ------------------------------------------

        if (seenPolicyNumbers.contains(record.getPolicyNumber())) {
            log.warn("Skipping duplicate policy number [{}] within this batch",
                    record.getPolicyNumber());
            filteredCount++;
            return null;
        }

        // ---- Date parsing -------------------------------------------------

        Date effectiveDate;
        Date expirationDate;
        try {
            effectiveDate = DATE_FORMAT.get().parse(record.getEffectiveDate());
        } catch (ParseException e) {
            log.warn("Skipping policy [{}]: invalid effectiveDate '{}'",
                    record.getPolicyNumber(), record.getEffectiveDate());
            filteredCount++;
            return null;
        }
        try {
            expirationDate = DATE_FORMAT.get().parse(record.getExpirationDate());
        } catch (ParseException e) {
            log.warn("Skipping policy [{}]: invalid expirationDate '{}'",
                    record.getPolicyNumber(), record.getExpirationDate());
            filteredCount++;
            return null;
        }

        // ---- Business rule validation -------------------------------------

        if (!effectiveDate.before(expirationDate)) {
            log.warn("Skipping policy [{}]: effectiveDate {} is not before expirationDate {}",
                    record.getPolicyNumber(), record.getEffectiveDate(),
                    record.getExpirationDate());
            filteredCount++;
            return null;
        }

        BigDecimal premium = record.getPremiumAmount();
        if (premium == null || premium.compareTo(BigDecimal.ZERO) <= 0) {
            log.warn("Skipping policy [{}]: invalid premium amount {}",
                    record.getPolicyNumber(), premium);
            filteredCount++;
            return null;
        }

        BigDecimal coverageLimit = record.getCoverageLimit();
        BigDecimal deductible = record.getDeductible();
        if (coverageLimit != null && deductible != null
                && deductible.compareTo(coverageLimit) > 0) {
            log.warn("Skipping policy [{}]: deductible {} exceeds coverageLimit {}",
                    record.getPolicyNumber(), deductible, coverageLimit);
            filteredCount++;
            return null;
        }

        // ---- Entity construction ------------------------------------------

        Policy policy = new Policy();
        policy.setPolicyNumber(record.getPolicyNumber().trim());
        policy.setHolderFirstName(record.getHolderFirstName().trim());
        policy.setHolderLastName(record.getHolderLastName().trim());
        policy.setPolicyType(record.getPolicyType());
        policy.setEffectiveDate(effectiveDate);
        policy.setExpirationDate(expirationDate);
        policy.setPremiumAmount(premium);
        policy.setCoverageLimit(coverageLimit);
        policy.setDeductible(deductible);
        policy.setStatus(PolicyStatus.ACTIVE);

        seenPolicyNumbers.add(record.getPolicyNumber());
        validCount++;

        log.debug("Policy [{}] validated and transformed. Valid so far: {}, filtered: {}",
                record.getPolicyNumber(), validCount, filteredCount);

        return policy;
    }

    // -------------------------------------------------------------------------
    // Metrics — accessible for step listeners or testing
    // -------------------------------------------------------------------------

    public int getValidCount() {
        return validCount;
    }

    public int getFilteredCount() {
        return filteredCount;
    }

    // -------------------------------------------------------------------------
    // Helpers
    // -------------------------------------------------------------------------

    private boolean isBlank(String value) {
        return value == null || value.trim().isEmpty();
    }
}
