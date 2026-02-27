package com.greylegacy.integration.file;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.math.BigDecimal;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.greylegacy.dao.ClaimDao;
import com.greylegacy.domain.Claim;
import com.greylegacy.domain.ClaimStatus;
import com.greylegacy.domain.ClaimType;

/**
 * Imports insurance claims from CSV files into the claims database.
 *
 * <p>Reads CSV data using {@link BufferedReader} (no external libraries —
 * legacy style). The first row is expected to be a header that defines
 * column positions. Each subsequent row is parsed into a {@link Claim}
 * entity and persisted via {@link ClaimDao}.</p>
 *
 * <p>Processing is fault-tolerant: errors on individual rows are collected
 * and reported without aborting the entire file. A batch-commit strategy
 * flushes every {@value #BATCH_SIZE} rows to control memory.</p>
 *
 * @author Grey Legacy Integration Team
 * @since 1.0
 */
public class CsvClaimImporter {

    private static final Logger LOG = LoggerFactory.getLogger(CsvClaimImporter.class);

    private static final String DATE_FORMAT = "yyyy-MM-dd";
    private static final int BATCH_SIZE = 50;

    private ClaimDao claimDao;

    // -------------------------------------------------------------------------
    // Spring setter injection
    // -------------------------------------------------------------------------

    public void setClaimDao(ClaimDao claimDao) {
        this.claimDao = claimDao;
    }

    // -------------------------------------------------------------------------
    // Public API
    // -------------------------------------------------------------------------

    /**
     * Imports claims from the given CSV file.
     *
     * @param file the CSV file to import
     * @return a summary of the import result
     */
    public ImportSummary importFile(File file) {
        LOG.info("Starting CSV claim import from file: {}", file.getAbsolutePath());

        ImportSummary summary = new ImportSummary();
        BufferedReader reader = null;

        try {
            reader = new BufferedReader(new FileReader(file));

            // Parse header row
            String headerLine = reader.readLine();
            if (headerLine == null || headerLine.trim().isEmpty()) {
                LOG.error("CSV file is empty or missing header row: {}", file.getName());
                summary.addError(0, "File is empty or missing header row");
                return summary;
            }

            Map<String, Integer> columnMap = buildColumnMap(headerLine);
            LOG.debug("Column mapping: {}", columnMap);

            // Process data rows
            String line;
            int lineNumber = 1; // 1-based, header is line 0
            List<Claim> batch = new ArrayList<Claim>();

            while ((line = reader.readLine()) != null) {
                lineNumber++;
                if (line.trim().isEmpty()) {
                    continue;
                }

                summary.incrementTotal();

                try {
                    String[] fields = parseCsvLine(line);
                    Claim claim = mapToClaim(fields, columnMap, lineNumber);

                    if (claim != null) {
                        batch.add(claim);
                        summary.incrementImported();
                    } else {
                        summary.incrementSkipped();
                        summary.addError(lineNumber, "Failed to map CSV row to Claim");
                    }

                    // Batch commit
                    if (batch.size() >= BATCH_SIZE) {
                        persistBatch(batch);
                        batch.clear();
                    }

                } catch (Exception ex) {
                    LOG.warn("Error parsing line {}: {}", lineNumber, ex.getMessage());
                    summary.incrementSkipped();
                    summary.addError(lineNumber, ex.getMessage());
                }
            }

            // Flush remaining batch
            if (!batch.isEmpty()) {
                persistBatch(batch);
                batch.clear();
            }

        } catch (IOException ex) {
            LOG.error("I/O error reading CSV file {}: {}", file.getName(), ex.getMessage(), ex);
            summary.addError(0, "I/O error: " + ex.getMessage());
        } finally {
            if (reader != null) {
                try {
                    reader.close();
                } catch (IOException ex) {
                    LOG.debug("Error closing reader: {}", ex.getMessage());
                }
            }
        }

        LOG.info("CSV import complete for {}. {}", file.getName(), summary);
        return summary;
    }

    // -------------------------------------------------------------------------
    // CSV parsing helpers
    // -------------------------------------------------------------------------

    private Map<String, Integer> buildColumnMap(String headerLine) {
        String[] headers = parseCsvLine(headerLine);
        Map<String, Integer> map = new HashMap<String, Integer>();
        for (int i = 0; i < headers.length; i++) {
            map.put(headers[i].trim().toLowerCase(), i);
        }
        return map;
    }

    /**
     * Parses a single CSV line, handling quoted fields and escaped commas.
     * Supports double-quoted fields that may contain commas and escaped
     * quotes (doubled double-quotes).
     */
    private String[] parseCsvLine(String line) {
        List<String> fields = new ArrayList<String>();
        StringBuilder current = new StringBuilder();
        boolean inQuotes = false;

        for (int i = 0; i < line.length(); i++) {
            char ch = line.charAt(i);

            if (inQuotes) {
                if (ch == '"') {
                    // Check for escaped quote (doubled)
                    if (i + 1 < line.length() && line.charAt(i + 1) == '"') {
                        current.append('"');
                        i++; // skip next quote
                    } else {
                        inQuotes = false;
                    }
                } else {
                    current.append(ch);
                }
            } else {
                if (ch == '"') {
                    inQuotes = true;
                } else if (ch == ',') {
                    fields.add(current.toString().trim());
                    current = new StringBuilder();
                } else {
                    current.append(ch);
                }
            }
        }
        fields.add(current.toString().trim());

        return fields.toArray(new String[0]);
    }

    // -------------------------------------------------------------------------
    // Mapping
    // -------------------------------------------------------------------------

    private Claim mapToClaim(String[] fields, Map<String, Integer> columnMap, int lineNumber) {
        Claim claim = new Claim();

        // Required fields
        String claimNumber = getField(fields, columnMap, "claimnumber");
        if (claimNumber == null || claimNumber.isEmpty()) {
            LOG.warn("Line {}: Missing required field 'claimNumber'", lineNumber);
            return null;
        }
        claim.setClaimNumber(claimNumber);

        String claimantFirst = getField(fields, columnMap, "claimantfirstname");
        String claimantLast = getField(fields, columnMap, "claimantlastname");
        if (claimantFirst == null || claimantLast == null) {
            LOG.warn("Line {}: Missing claimant name fields", lineNumber);
            return null;
        }
        claim.setClaimantFirstName(claimantFirst);
        claim.setClaimantLastName(claimantLast);

        // Claim type
        String claimTypeStr = getField(fields, columnMap, "claimtype");
        if (claimTypeStr != null && !claimTypeStr.isEmpty()) {
            try {
                claim.setClaimType(ClaimType.valueOf(claimTypeStr.toUpperCase()));
            } catch (IllegalArgumentException ex) {
                LOG.warn("Line {}: Invalid claim type '{}', defaulting to OTHER", lineNumber, claimTypeStr);
                claim.setClaimType(ClaimType.OTHER);
            }
        } else {
            claim.setClaimType(ClaimType.OTHER);
        }

        // Loss date
        String lossDateStr = getField(fields, columnMap, "lossdate");
        if (lossDateStr != null && !lossDateStr.isEmpty()) {
            try {
                SimpleDateFormat sdf = new SimpleDateFormat(DATE_FORMAT);
                sdf.setLenient(false);
                claim.setLossDate(sdf.parse(lossDateStr));
            } catch (ParseException ex) {
                LOG.warn("Line {}: Invalid date format '{}': {}", lineNumber, lossDateStr, ex.getMessage());
                return null;
            }
        } else {
            claim.setLossDate(new Date());
        }

        // Optional fields
        claim.setLossDescription(getField(fields, columnMap, "lossdescription"));
        claim.setLossLocation(getField(fields, columnMap, "losslocation"));

        String estimatedStr = getField(fields, columnMap, "estimatedloss");
        if (estimatedStr != null && !estimatedStr.isEmpty()) {
            try {
                claim.setEstimatedLoss(new BigDecimal(estimatedStr.replace("$", "").replace(",", "")));
            } catch (NumberFormatException ex) {
                LOG.warn("Line {}: Invalid estimated loss '{}': {}", lineNumber, estimatedStr, ex.getMessage());
            }
        }

        // Default status for imported claims
        claim.setStatus(ClaimStatus.OPEN);
        claim.setReportedDate(new Date());

        return claim;
    }

    private String getField(String[] fields, Map<String, Integer> columnMap, String columnName) {
        Integer index = columnMap.get(columnName);
        if (index == null || index >= fields.length) {
            return null;
        }
        String value = fields[index];
        return (value != null && !value.isEmpty()) ? value : null;
    }

    // -------------------------------------------------------------------------
    // Persistence
    // -------------------------------------------------------------------------

    private void persistBatch(List<Claim> batch) {
        LOG.debug("Persisting batch of {} claims", batch.size());
        for (Claim claim : batch) {
            try {
                claimDao.save(claim);
            } catch (Exception ex) {
                LOG.error("Failed to persist claim {}: {}", claim.getClaimNumber(), ex.getMessage());
            }
        }
    }

    // -------------------------------------------------------------------------
    // Import summary
    // -------------------------------------------------------------------------

    /**
     * Summary report for a CSV import operation.
     */
    public static class ImportSummary {
        private int totalRows;
        private int importedRows;
        private int skippedRows;
        private List<String> errors = new ArrayList<String>();

        public void incrementTotal() { totalRows++; }
        public void incrementImported() { importedRows++; }
        public void incrementSkipped() { skippedRows++; }

        public void addError(int line, String message) {
            errors.add("Line " + line + ": " + message);
        }

        public int getTotalRows() { return totalRows; }
        public int getImportedRows() { return importedRows; }
        public int getSkippedRows() { return skippedRows; }
        public List<String> getErrors() { return errors; }

        @Override
        public String toString() {
            return "ImportSummary{total=" + totalRows
                    + ", imported=" + importedRows
                    + ", skipped=" + skippedRows
                    + ", errors=" + errors.size() + "}";
        }
    }
}
