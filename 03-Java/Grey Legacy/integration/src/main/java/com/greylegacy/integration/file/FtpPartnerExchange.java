package com.greylegacy.integration.file;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.Vector;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jcraft.jsch.Channel;
import com.jcraft.jsch.ChannelSftp;
import com.jcraft.jsch.JSch;
import com.jcraft.jsch.JSchException;
import com.jcraft.jsch.Session;
import com.jcraft.jsch.SftpException;

/**
 * SFTP file-exchange utility for transferring files to and from
 * partner organisations and upstream/downstream systems.
 *
 * <p>Uses the JSch library ({@code com.jcraft.jsch}) for SFTP connectivity.
 * Connections are established on demand and closed after each operation.
 * Retry logic with exponential back-off handles transient network failures.</p>
 *
 * <p><strong>Production note:</strong> for high-throughput scenarios,
 * consider using a connection pool (e.g. Apache Commons Pool wrapping
 * JSch sessions) rather than creating a new session per call.</p>
 *
 * @author Grey Legacy Integration Team
 * @since 1.0
 */
public class FtpPartnerExchange {

    private static final Logger LOG = LoggerFactory.getLogger(FtpPartnerExchange.class);

    /** Maximum number of retry attempts for transient failures. */
    private static final int MAX_RETRIES = 3;

    /** Initial backoff in milliseconds (doubles on each retry). */
    private static final long INITIAL_BACKOFF_MS = 2000L;

    /** Connection timeout in milliseconds. */
    private static final int CONNECT_TIMEOUT_MS = 15000;

    private String host;
    private int port = 22;
    private String username;
    private String password;
    private String privateKeyPath;
    private String remoteBaseDir = "/";

    // -------------------------------------------------------------------------
    // Spring setter injection / configuration
    // -------------------------------------------------------------------------

    public void setHost(String host) { this.host = host; }
    public void setPort(int port) { this.port = port; }
    public void setUsername(String username) { this.username = username; }
    public void setPassword(String password) { this.password = password; }
    public void setPrivateKeyPath(String privateKeyPath) { this.privateKeyPath = privateKeyPath; }
    public void setRemoteBaseDir(String remoteBaseDir) { this.remoteBaseDir = remoteBaseDir; }

    // -------------------------------------------------------------------------
    // Public API
    // -------------------------------------------------------------------------

    /**
     * Uploads a local file to the remote SFTP server.
     *
     * @param localPath  absolute path to the local file
     * @param remotePath remote path (relative to remote base dir)
     */
    public void uploadFile(String localPath, String remotePath) {
        LOG.info("Uploading file: {} → sftp://{}:{}{}", localPath, host, port, remotePath);

        executeWithRetry(new SftpOperation() {
            @Override
            public void execute(ChannelSftp sftp) throws SftpException, IOException {
                InputStream inputStream = null;
                try {
                    inputStream = new FileInputStream(new File(localPath));
                    sftp.put(inputStream, remotePath);
                    LOG.info("Upload complete: {}", remotePath);
                } finally {
                    if (inputStream != null) {
                        try { inputStream.close(); } catch (IOException ignored) { }
                    }
                }
            }
        });
    }

    /**
     * Downloads a file from the remote SFTP server to a local path.
     *
     * @param remotePath remote path to the file
     * @param localPath  absolute path for the local destination
     */
    public void downloadFile(String remotePath, String localPath) {
        LOG.info("Downloading file: sftp://{}:{}{} → {}", host, port, remotePath, localPath);

        executeWithRetry(new SftpOperation() {
            @Override
            public void execute(ChannelSftp sftp) throws SftpException, IOException {
                OutputStream outputStream = null;
                try {
                    outputStream = new FileOutputStream(new File(localPath));
                    sftp.get(remotePath, outputStream);
                    LOG.info("Download complete: {}", localPath);
                } finally {
                    if (outputStream != null) {
                        try { outputStream.close(); } catch (IOException ignored) { }
                    }
                }
            }
        });
    }

    /**
     * Lists files in a remote directory.
     *
     * @param remoteDir the remote directory path
     * @return list of filenames in the directory
     */
    @SuppressWarnings("unchecked")
    public List<String> listRemoteFiles(final String remoteDir) {
        LOG.info("Listing remote directory: sftp://{}:{}{}", host, port, remoteDir);

        final List<String> fileNames = new ArrayList<String>();

        executeWithRetry(new SftpOperation() {
            @Override
            public void execute(ChannelSftp sftp) throws SftpException {
                Vector<ChannelSftp.LsEntry> entries = sftp.ls(remoteDir);
                for (ChannelSftp.LsEntry entry : entries) {
                    String name = entry.getFilename();
                    if (!".".equals(name) && !"..".equals(name)) {
                        fileNames.add(name);
                    }
                }
                LOG.info("Found {} files in {}", fileNames.size(), remoteDir);
            }
        });

        return fileNames;
    }

    // -------------------------------------------------------------------------
    // Retry logic
    // -------------------------------------------------------------------------

    private void executeWithRetry(SftpOperation operation) {
        int attempt = 0;
        long backoff = INITIAL_BACKOFF_MS;

        while (attempt < MAX_RETRIES) {
            attempt++;
            Session session = null;
            ChannelSftp sftpChannel = null;

            try {
                session = createSession();
                sftpChannel = openSftpChannel(session);
                operation.execute(sftpChannel);
                return; // success

            } catch (Exception ex) {
                LOG.warn("SFTP operation failed (attempt {}/{}): {}", attempt, MAX_RETRIES, ex.getMessage());

                if (attempt >= MAX_RETRIES) {
                    throw new RuntimeException("SFTP operation failed after " + MAX_RETRIES + " attempts", ex);
                }

                LOG.info("Retrying in {} ms...", backoff);
                try {
                    Thread.sleep(backoff);
                } catch (InterruptedException ie) {
                    Thread.currentThread().interrupt();
                    throw new RuntimeException("SFTP retry interrupted", ie);
                }
                backoff *= 2; // exponential backoff

            } finally {
                if (sftpChannel != null && sftpChannel.isConnected()) {
                    sftpChannel.disconnect();
                }
                if (session != null && session.isConnected()) {
                    session.disconnect();
                }
            }
        }
    }

    // -------------------------------------------------------------------------
    // Connection helpers
    // -------------------------------------------------------------------------

    private Session createSession() throws JSchException {
        JSch jsch = new JSch();

        // Use private key authentication if configured
        if (privateKeyPath != null && !privateKeyPath.isEmpty()) {
            jsch.addIdentity(privateKeyPath);
            LOG.debug("Using private key authentication: {}", privateKeyPath);
        }

        Session session = jsch.getSession(username, host, port);

        if (password != null && !password.isEmpty()) {
            session.setPassword(password);
        }

        // Disable strict host key checking for legacy environments
        // In production, use known_hosts file or a custom HostKeyRepository
        java.util.Properties config = new java.util.Properties();
        config.put("StrictHostKeyChecking", "no");
        session.setConfig(config);

        session.setTimeout(CONNECT_TIMEOUT_MS);
        session.connect(CONNECT_TIMEOUT_MS);
        LOG.debug("SFTP session established: {}@{}:{}", username, host, port);

        return session;
    }

    private ChannelSftp openSftpChannel(Session session) throws JSchException {
        Channel channel = session.openChannel("sftp");
        channel.connect(CONNECT_TIMEOUT_MS);
        return (ChannelSftp) channel;
    }

    // -------------------------------------------------------------------------
    // Functional interface for SFTP operations
    // -------------------------------------------------------------------------

    private interface SftpOperation {
        void execute(ChannelSftp sftp) throws SftpException, IOException;
    }
}
