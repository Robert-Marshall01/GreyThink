package com.greylegacy.batch.springbatch;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.item.ExecutionContext;
import org.springframework.batch.item.ItemReader;
import org.springframework.batch.item.ItemStream;
import org.springframework.batch.item.ItemStreamException;
import org.springframework.batch.item.file.FlatFileItemReader;
import org.springframework.batch.item.file.mapping.BeanWrapperFieldSetMapper;
import org.springframework.batch.item.file.mapping.DefaultLineMapper;
import org.springframework.batch.item.file.transform.DelimitedLineTokenizer;
import org.springframework.core.io.Resource;

import java.io.Serializable;
import java.math.BigDecimal;

/**
 * Spring Batch {@link ItemReader} that reads policy records from a
 * delimited (CSV) flat file using Spring Batch's built-in
 * {@link FlatFileItemReader} infrastructure.
 *
 * <p>The reader maps each CSV row to a {@link PolicyImportRecord} using a
 * {@link DelimitedLineTokenizer} and {@link BeanWrapperFieldSetMapper}.
 * The first line of the file is treated as a header and skipped.</p>
 *
 * <h3>ItemStream contract</h3>
 * <p>Implements {@link ItemStream} by delegating to the underlying
 * {@link FlatFileItemReader}. This ensures Spring Batch properly manages
 * the file resource lifecycle and supports restart from the last
 * successfully committed line.</p>
 *
 * <p>Expected CSV column order:
 * {@code policyNumber, holderFirstName, holderLastName, policyType,
 * effectiveDate, expirationDate, premiumAmount, coverageLimit, deductible}</p>
 *
 * @author Grey Legacy Batch Team
 * @since 1.0.0
 */
public class PolicyFileImportReader
        implements ItemReader<PolicyFileImportReader.PolicyImportRecord>, ItemStream {

    private static final Logger log = LoggerFactory.getLogger(PolicyFileImportReader.class);

    private final FlatFileItemReader<PolicyImportRecord> delegate;

    /**
     * Constructs the reader, configuring the tokenizer, field-set mapper,
     * and line mapper for parsing the policy CSV file.
     */
    public PolicyFileImportReader() {
        delegate = new FlatFileItemReader<>();
        delegate.setLinesToSkip(1); // skip header row

        DelimitedLineTokenizer tokenizer = new DelimitedLineTokenizer();
        tokenizer.setNames(new String[]{
                "policyNumber",
                "holderFirstName",
                "holderLastName",
                "policyType",
                "effectiveDate",
                "expirationDate",
                "premiumAmount",
                "coverageLimit",
                "deductible"
        });

        BeanWrapperFieldSetMapper<PolicyImportRecord> fieldSetMapper =
                new BeanWrapperFieldSetMapper<>();
        fieldSetMapper.setTargetType(PolicyImportRecord.class);

        DefaultLineMapper<PolicyImportRecord> lineMapper = new DefaultLineMapper<>();
        lineMapper.setLineTokenizer(tokenizer);
        lineMapper.setFieldSetMapper(fieldSetMapper);

        delegate.setLineMapper(lineMapper);

        log.info("PolicyFileImportReader initialised with header-skip and {} fields",
                tokenizer.getNames().length);
    }

    // -------------------------------------------------------------------------
    // Configuration
    // -------------------------------------------------------------------------

    /**
     * Sets the input file resource. Must be called before reading begins.
     *
     * @param resource the CSV file resource
     */
    public void setResource(Resource resource) {
        delegate.setResource(resource);
        log.info("Input resource set to: {}", resource);
    }

    /**
     * Opens the delegate reader. Should be called by the step lifecycle.
     *
     * @param executionContext the execution context for restart state
     * @throws ItemStreamException if the resource cannot be opened
     */
    @Override
    public void open(ExecutionContext executionContext) throws ItemStreamException {
        delegate.open(executionContext);
        log.info("PolicyFileImportReader opened");
    }

    /**
     * Persists the current read position to the execution context for
     * restartability.
     *
     * @param executionContext the execution context to update
     * @throws ItemStreamException if the update fails
     */
    @Override
    public void update(ExecutionContext executionContext) throws ItemStreamException {
        delegate.update(executionContext);
    }

    /**
     * Closes the delegate reader, releasing file resources.
     *
     * @throws ItemStreamException if closing fails
     */
    @Override
    public void close() throws ItemStreamException {
        delegate.close();
        log.info("PolicyFileImportReader closed");
    }

    // -------------------------------------------------------------------------
    // ItemReader implementation
    // -------------------------------------------------------------------------

    /** {@inheritDoc} */
    @Override
    public PolicyImportRecord read() throws Exception {
        return delegate.read();
    }

    // -------------------------------------------------------------------------
    // Inner class: PolicyImportRecord
    // -------------------------------------------------------------------------

    /**
     * Flat data-transfer object representing a single row from the policy
     * import CSV file.
     */
    public static class PolicyImportRecord implements Serializable {

        private static final long serialVersionUID = 1L;

        private String policyNumber;
        private String holderFirstName;
        private String holderLastName;
        private String policyType;
        private String effectiveDate;
        private String expirationDate;
        private BigDecimal premiumAmount;
        private BigDecimal coverageLimit;
        private BigDecimal deductible;

        public String getPolicyNumber()                           { return policyNumber; }
        public void setPolicyNumber(String policyNumber)          { this.policyNumber = policyNumber; }

        public String getHolderFirstName()                        { return holderFirstName; }
        public void setHolderFirstName(String holderFirstName)    { this.holderFirstName = holderFirstName; }

        public String getHolderLastName()                         { return holderLastName; }
        public void setHolderLastName(String holderLastName)      { this.holderLastName = holderLastName; }

        public String getPolicyType()                             { return policyType; }
        public void setPolicyType(String policyType)              { this.policyType = policyType; }

        public String getEffectiveDate()                          { return effectiveDate; }
        public void setEffectiveDate(String effectiveDate)        { this.effectiveDate = effectiveDate; }

        public String getExpirationDate()                         { return expirationDate; }
        public void setExpirationDate(String expirationDate)      { this.expirationDate = expirationDate; }

        public BigDecimal getPremiumAmount()                       { return premiumAmount; }
        public void setPremiumAmount(BigDecimal premiumAmount)     { this.premiumAmount = premiumAmount; }

        public BigDecimal getCoverageLimit()                       { return coverageLimit; }
        public void setCoverageLimit(BigDecimal coverageLimit)     { this.coverageLimit = coverageLimit; }

        public BigDecimal getDeductible()                          { return deductible; }
        public void setDeductible(BigDecimal deductible)           { this.deductible = deductible; }

        @Override
        public String toString() {
            return "PolicyImportRecord{policyNumber='" + policyNumber + "', holderLastName='" +
                    holderLastName + "', policyType='" + policyType + "'}";
        }
    }
}
