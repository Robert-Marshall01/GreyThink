package com.greylegacy.integration.file;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.text.SimpleDateFormat;
import java.util.Date;

import org.quartz.Job;
import org.quartz.JobExecutionContext;
import org.quartz.JobExecutionException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Quartz {@link Job} that polls a configurable inbound directory for new
 * CSV and XML files and routes them to the appropriate importer.
 *
 * <ul>
 *   <li>{@code .csv} files → {@link CsvClaimImporter}</li>
 *   <li>{@code .xml} files → {@link XmlPolicyImporter}</li>
 * </ul>
 *
 * <p>Processed files are moved to a {@code processed/} directory with a
 * timestamp suffix. Files that fail processing are moved to an
 * {@code error/} directory. A basic file-lock check ensures that
 * partially-written files are not picked up prematurely.</p>
 *
 * @author Grey Legacy Integration Team
 * @since 1.0
 */
public class FileDropMonitorJob implements Job {

    private static final Logger LOG = LoggerFactory.getLogger(FileDropMonitorJob.class);

    private static final String TIMESTAMP_FORMAT = "yyyyMMdd_HHmmssSSS";

    private String inboundDirectory = "/data/inbound/";
    private String processedDirectory = "/data/processed/";
    private String errorDirectory = "/data/error/";

    private CsvClaimImporter csvClaimImporter;
    private XmlPolicyImporter xmlPolicyImporter;

    // -------------------------------------------------------------------------
    // Spring setter injection
    // -------------------------------------------------------------------------

    public void setInboundDirectory(String inboundDirectory) {
        this.inboundDirectory = inboundDirectory;
    }

    public void setProcessedDirectory(String processedDirectory) {
        this.processedDirectory = processedDirectory;
    }

    public void setErrorDirectory(String errorDirectory) {
        this.errorDirectory = errorDirectory;
    }

    public void setCsvClaimImporter(CsvClaimImporter csvClaimImporter) {
        this.csvClaimImporter = csvClaimImporter;
    }

    public void setXmlPolicyImporter(XmlPolicyImporter xmlPolicyImporter) {
        this.xmlPolicyImporter = xmlPolicyImporter;
    }

    // -------------------------------------------------------------------------
    // Job execution
    // -------------------------------------------------------------------------

    @Override
    public void execute(JobExecutionContext context) throws JobExecutionException {
        LOG.info("FileDropMonitorJob started — scanning {}", inboundDirectory);

        File inboundDir = new File(inboundDirectory);
        if (!inboundDir.exists() || !inboundDir.isDirectory()) {
            LOG.warn("Inbound directory does not exist or is not a directory: {}", inboundDirectory);
            return;
        }

        ensureDirectoryExists(processedDirectory);
        ensureDirectoryExists(errorDirectory);

        File[] files = inboundDir.listFiles();
        if (files == null || files.length == 0) {
            LOG.info("No files found in inbound directory");
            return;
        }

        LOG.info("Found {} file(s) in inbound directory", files.length);

        for (File file : files) {
            if (file.isDirectory()) {
                continue;
            }

            // Skip files that may still be written to
            if (!isFileComplete(file)) {
                LOG.debug("File appears incomplete (still being written): {}", file.getName());
                continue;
            }

            processFile(file);
        }

        LOG.info("FileDropMonitorJob completed");
    }

    // -------------------------------------------------------------------------
    // File processing
    // -------------------------------------------------------------------------

    private void processFile(File file) {
        String fileName = file.getName().toLowerCase();
        boolean success = false;

        try {
            if (fileName.endsWith(".csv")) {
                LOG.info("Routing CSV file to CsvClaimImporter: {}", file.getName());
                csvClaimImporter.importFile(file);
                success = true;
            } else if (fileName.endsWith(".xml")) {
                LOG.info("Routing XML file to XmlPolicyImporter: {}", file.getName());
                xmlPolicyImporter.importFile(file);
                success = true;
            } else {
                LOG.warn("Unsupported file type — skipping: {}", file.getName());
                return;
            }
        } catch (Exception ex) {
            LOG.error("Error processing file {}: {}", file.getName(), ex.getMessage(), ex);
            success = false;
        }

        if (success) {
            moveFile(file, processedDirectory);
        } else {
            moveFile(file, errorDirectory);
        }
    }

    /**
     * Rudimentary file-lock check: attempts to rename the file to itself.
     * On many OS/file-system combinations a file that is still open for
     * writing cannot be renamed. This is not bulletproof but works in
     * most Windows and NFS scenarios encountered in legacy deployments.
     */
    private boolean isFileComplete(File file) {
        return file.renameTo(file);
    }

    private void moveFile(File sourceFile, String targetDirPath) {
        String timestamp = new SimpleDateFormat(TIMESTAMP_FORMAT).format(new Date());
        String targetName = sourceFile.getName() + "." + timestamp;
        File targetDir = new File(targetDirPath);
        File targetFile = new File(targetDir, targetName);

        // Try atomic rename first
        if (sourceFile.renameTo(targetFile)) {
            LOG.info("Moved {} → {}", sourceFile.getName(), targetFile.getAbsolutePath());
            return;
        }

        // Fallback: copy-and-delete (cross-filesystem moves)
        LOG.debug("Atomic rename failed — falling back to copy-and-delete for {}", sourceFile.getName());
        InputStream in = null;
        OutputStream out = null;
        try {
            in = new FileInputStream(sourceFile);
            out = new FileOutputStream(targetFile);
            byte[] buffer = new byte[4096];
            int bytesRead;
            while ((bytesRead = in.read(buffer)) != -1) {
                out.write(buffer, 0, bytesRead);
            }
            out.flush();

            if (!sourceFile.delete()) {
                LOG.warn("Could not delete source file after copy: {}", sourceFile.getAbsolutePath());
            }
            LOG.info("Copied and deleted {} → {}", sourceFile.getName(), targetFile.getAbsolutePath());
        } catch (IOException ex) {
            LOG.error("Failed to move file {} to {}: {}", sourceFile.getName(), targetDirPath, ex.getMessage());
        } finally {
            closeQuietly(in);
            closeQuietly(out);
        }
    }

    private void ensureDirectoryExists(String path) {
        File dir = new File(path);
        if (!dir.exists()) {
            if (dir.mkdirs()) {
                LOG.info("Created directory: {}", path);
            } else {
                LOG.warn("Failed to create directory: {}", path);
            }
        }
    }

    private void closeQuietly(java.io.Closeable closeable) {
        if (closeable != null) {
            try {
                closeable.close();
            } catch (IOException ex) {
                LOG.debug("Error closing stream: {}", ex.getMessage());
            }
        }
    }
}
