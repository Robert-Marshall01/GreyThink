package com.greylegacy.batch.springbatch;

import com.greylegacy.batch.springbatch.ClaimExportItemProcessor.ClaimExportRecord;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.item.ExecutionContext;
import org.springframework.batch.item.ItemStream;
import org.springframework.batch.item.ItemStreamException;
import org.springframework.batch.item.ItemWriter;

import java.io.BufferedWriter;
import java.io.File;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.FileOutputStream;
import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;

/**
 * Spring Batch {@link ItemWriter} that writes {@link ClaimExportRecord}
 * instances to a CSV file with UTF-8 encoding.
 *
 * <p>The writer creates the output file when {@link #open} is called by the
 * step, writes a header row, and appends data rows for each chunk. On
 * {@link #close} the writer appends a summary footer and releases the
 * underlying stream.</p>
 *
 * <h3>ItemStream contract</h3>
 * <p>Implements {@link ItemStream} so that Spring Batch manages the file
 * lifecycle through the step's registered-streams mechanism. This replaces
 * the previous {@code InitializingBean}/{@code DisposableBean} approach
 * which was fragile: Spring could call {@code afterPropertiesSet} before
 * the job was actually launched, and {@code destroy} might never fire if
 * the JVM was killed.</p>
 *
 * <h3>Restartability</h3>
 * <p>The current record count and total approved amount are persisted to
 * the {@link ExecutionContext} via {@link #update}, enabling the writer
 * to resume appending on restart.</p>
 *
 * @author Grey Legacy Batch Team
 * @since 1.0.0
 */
public class ClaimExportItemWriter
        implements ItemWriter<ClaimExportRecord>, ItemStream {

    private static final Logger log = LoggerFactory.getLogger(ClaimExportItemWriter.class);

    private static final String RECORD_COUNT_KEY = "claim.export.write.count";
    private static final String TOTAL_AMOUNT_KEY = "claim.export.total.amount";

    private static final String HEADER =
            "ClaimNumber,PolicyNumber,ClaimantName,Status,LossDate," +
            "EstimatedLoss,ApprovedAmount,AdjusterCode,FraudRiskLevel";

    private static final String SEPARATOR = ",";

    private String outputDirectory;
    private String filenamePattern = "claim_export_%s.csv";

    private BufferedWriter writer;
    private int totalRecordCount;
    private BigDecimal totalApprovedAmount;
    private String outputFilePath;

    // -------------------------------------------------------------------------
    // Configuration setters
    // -------------------------------------------------------------------------

    /**
     * Sets the directory where export files will be written.
     *
     * @param outputDirectory absolute or relative path to the output directory
     */
    public void setOutputDirectory(String outputDirectory) {
        this.outputDirectory = outputDirectory;
    }

    /**
     * Sets the filename pattern. Must contain a single {@code %s} placeholder
     * that will be replaced with a date-stamp.
     *
     * @param filenamePattern the filename pattern (e.g. {@code "export_%s.csv"})
     */
    public void setFilenamePattern(String filenamePattern) {
        this.filenamePattern = filenamePattern;
    }

    // -------------------------------------------------------------------------
    // ItemStream lifecycle
    // -------------------------------------------------------------------------

    /**
     * Opens the CSV output file and writes the header row. On restart,
     * restores the record count and total amount from the execution context
     * and opens the file in append mode.
     *
     * @param executionContext the execution context for restart state
     * @throws ItemStreamException if the file cannot be created or opened
     */
    @Override
    public void open(ExecutionContext executionContext) throws ItemStreamException {
        boolean isRestart = executionContext.containsKey(RECORD_COUNT_KEY);

        try {
            File dir = new File(outputDirectory);
            if (!dir.exists() && !dir.mkdirs()) {
                throw new IOException("Unable to create output directory: " + outputDirectory);
            }

            if (isRestart) {
                // On restart, restore counters and append to the existing file
                totalRecordCount = executionContext.getInt(RECORD_COUNT_KEY);
                String storedAmount = executionContext.getString(TOTAL_AMOUNT_KEY);
                totalApprovedAmount = storedAmount != null
                        ? new BigDecimal(storedAmount) : BigDecimal.ZERO;

                // Reuse the same output file path if stored; otherwise generate new
                outputFilePath = executionContext.getString("claim.export.file.path");
                if (outputFilePath == null) {
                    outputFilePath = generateFilePath(dir);
                }

                log.info("Restarting CSV writer — appending to: {} (records so far: {})",
                        outputFilePath, totalRecordCount);
                writer = new BufferedWriter(new OutputStreamWriter(
                        new FileOutputStream(outputFilePath, true), StandardCharsets.UTF_8));
            } else {
                // Fresh start
                totalRecordCount = 0;
                totalApprovedAmount = BigDecimal.ZERO;
                outputFilePath = generateFilePath(dir);

                log.info("Opening CSV writer: {}", outputFilePath);
                writer = new BufferedWriter(new OutputStreamWriter(
                        new FileOutputStream(outputFilePath), StandardCharsets.UTF_8));
                writer.write(HEADER);
                writer.newLine();
            }
        } catch (IOException e) {
            throw new ItemStreamException("Failed to open CSV writer", e);
        }
    }

    /**
     * Persists the current record count, total amount, and file path to
     * the execution context for restartability.
     *
     * @param executionContext the execution context to update
     * @throws ItemStreamException if the update fails
     */
    @Override
    public void update(ExecutionContext executionContext) throws ItemStreamException {
        executionContext.putInt(RECORD_COUNT_KEY, totalRecordCount);
        executionContext.putString(TOTAL_AMOUNT_KEY, totalApprovedAmount.toPlainString());
        executionContext.putString("claim.export.file.path", outputFilePath);
        log.trace("Checkpointed write state: records={}, amount={}",
                totalRecordCount, totalApprovedAmount);
    }

    /**
     * Writes the summary footer, flushes, and closes the output stream.
     *
     * @throws ItemStreamException if the stream cannot be closed
     */
    @Override
    public void close() throws ItemStreamException {
        if (writer != null) {
            try {
                writeFooter();
                writer.flush();
            } catch (IOException e) {
                log.warn("Error writing footer", e);
            } finally {
                try {
                    writer.close();
                    log.info("CSV writer closed. File: {}", outputFilePath);
                } catch (IOException e) {
                    log.warn("Error closing CSV writer", e);
                }
            }
        }
    }

    // -------------------------------------------------------------------------
    // ItemWriter implementation
    // -------------------------------------------------------------------------

    /**
     * {@inheritDoc}
     *
     * Writes a chunk of {@link ClaimExportRecord} rows to the CSV file.
     */
    @Override
    public void write(List<? extends ClaimExportRecord> items) throws Exception {
        for (ClaimExportRecord record : items) {
            StringBuilder sb = new StringBuilder();
            sb.append(record.getClaimNumber()).append(SEPARATOR)
              .append(record.getPolicyNumber()).append(SEPARATOR)
              .append(record.getClaimantName()).append(SEPARATOR)
              .append(record.getStatus()).append(SEPARATOR)
              .append(record.getLossDate()).append(SEPARATOR)
              .append(record.getEstimatedLoss()).append(SEPARATOR)
              .append(record.getApprovedAmount()).append(SEPARATOR)
              .append(record.getAdjusterCode()).append(SEPARATOR)
              .append(record.getFraudRiskLevel());

            writer.write(sb.toString());
            writer.newLine();

            totalRecordCount++;
            try {
                BigDecimal amt = new BigDecimal(
                        record.getApprovedAmount().replace(",", ""));
                totalApprovedAmount = totalApprovedAmount.add(amt);
            } catch (NumberFormatException e) {
                log.debug("Non-numeric approved amount for claim {}: {}",
                        record.getClaimNumber(), record.getApprovedAmount());
            }
        }

        writer.flush();
        log.debug("Wrote chunk of {} records (total so far: {})", items.size(), totalRecordCount);
    }

    // -------------------------------------------------------------------------
    // Internal helpers
    // -------------------------------------------------------------------------

    /**
     * Generates a timestamped file path in the given directory.
     */
    private String generateFilePath(File dir) {
        String timestamp = new SimpleDateFormat("yyyyMMdd_HHmmss").format(new Date());
        String filename = String.format(filenamePattern, timestamp);
        return new File(dir, filename).getAbsolutePath();
    }

    /**
     * Writes a summary footer line containing the total record count and
     * the sum of all approved amounts.
     */
    private void writeFooter() throws IOException {
        writer.newLine();
        writer.write("# SUMMARY");
        writer.newLine();
        writer.write("# Total Records: " + totalRecordCount);
        writer.newLine();
        writer.write("# Total Approved Amount: " + totalApprovedAmount.toPlainString());
        writer.newLine();
        log.info("Export complete. Records: {}, Total Approved: {}",
                totalRecordCount, totalApprovedAmount.toPlainString());
    }
}
