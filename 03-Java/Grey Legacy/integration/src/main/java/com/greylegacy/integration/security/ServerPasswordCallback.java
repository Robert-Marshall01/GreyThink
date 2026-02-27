package com.greylegacy.integration.security;

import org.apache.wss4j.common.ext.WSPasswordCallback;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.security.auth.callback.Callback;
import javax.security.auth.callback.CallbackHandler;
import javax.security.auth.callback.UnsupportedCallbackException;
import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;

/**
 * WS-Security callback handler for server-side UsernameToken validation.
 * <p>
 * This class implements the JAAS {@link CallbackHandler} interface as
 * required by WSS4J (Web Services Security for Java). When a SOAP request
 * arrives with a WS-Security UsernameToken header, CXF's WSS4JInInterceptor
 * invokes this handler to retrieve the expected password for the given username.
 * <p>
 * WSS4J then compares the expected password against the password
 * (or password digest) in the incoming SOAP header. If they don't match,
 * the request is rejected with a SOAP Fault.
 * <p>
 * <b>Authentication flow:</b>
 * <ol>
 *   <li>Client sends SOAP request with WS-Security header containing
 *       UsernameToken (username + password digest + nonce + timestamp)</li>
 *   <li>CXF WSS4JInInterceptor extracts the username from the header</li>
 *   <li>WSS4J calls this handler with a {@link WSPasswordCallback}</li>
 *   <li>This handler looks up the expected password from the credentials store</li>
 *   <li>WSS4J computes digest = Base64(SHA1(nonce + timestamp + password))
 *       and compares with the client's digest</li>
 *   <li>If matched → request proceeds; if not → SOAP Fault</li>
 * </ol>
 * <p>
 * <b>Production note:</b> In production, replace the properties file
 * with LDAP lookup, database query, or JAAS LoginModule integration.
 * The properties file approach is for development and testing only.
 *
 * @see org.apache.wss4j.common.ext.WSPasswordCallback
 * @see org.apache.cxf.ws.security.wss4j.WSS4JInInterceptor
 */
public class ServerPasswordCallback implements CallbackHandler {

    private static final Logger log = LoggerFactory.getLogger(ServerPasswordCallback.class);

    /** Service account credentials loaded from ws-security.properties */
    private final Properties credentials = new Properties();

    /** Path to the credentials file on the classpath */
    private String credentialsFile;

    /**
     * Set the classpath location of the credentials properties file.
     * Injected by Spring from applicationContext-ws-security.xml.
     */
    public void setCredentialsFile(String credentialsFile) {
        this.credentialsFile = credentialsFile;
        loadCredentials();
    }

    private void loadCredentials() {
        if (credentialsFile == null) {
            return;
        }
        String path = credentialsFile.replace("classpath:", "");
        try (InputStream is = getClass().getClassLoader().getResourceAsStream(path)) {
            if (is != null) {
                credentials.load(is);
                log.info("Loaded {} service account credentials from {}",
                        credentials.size(), credentialsFile);
            } else {
                log.warn("Credentials file not found: {}", credentialsFile);
            }
        } catch (IOException e) {
            log.error("Failed to load credentials file: {}", credentialsFile, e);
        }
    }

    /**
     * Handle the password callback from WSS4J.
     * <p>
     * For {@code DECRYPT} and {@code SIGNATURE} usage types, this handler
     * would return a private key password. For {@code USERNAME_TOKEN},
     * it returns the expected password for server-side validation.
     *
     * @param callbacks array of JAAS callbacks (WSS4J sends WSPasswordCallback)
     * @throws IOException if credential lookup fails
     * @throws UnsupportedCallbackException if callback type is not supported
     */
    @Override
    public void handle(Callback[] callbacks) throws IOException, UnsupportedCallbackException {
        for (Callback callback : callbacks) {
            if (callback instanceof WSPasswordCallback) {
                WSPasswordCallback pc = (WSPasswordCallback) callback;
                String username = pc.getIdentifier();

                log.debug("WS-Security authentication request for user: {}", username);

                // Look up the expected password for this service account
                String expectedPassword = credentials.getProperty("ws.user." + username);

                if (expectedPassword != null) {
                    // Set the expected password — WSS4J will compare this
                    // against the password/digest in the incoming SOAP header
                    pc.setPassword(expectedPassword);
                    log.info("WS-Security: credentials found for user '{}'", username);
                } else {
                    log.warn("WS-Security: no credentials found for user '{}' — "
                            + "authentication will fail", username);
                    // Throwing here would also reject, but letting WSS4J
                    // handle the null password produces a cleaner SOAP Fault
                }
            } else {
                throw new UnsupportedCallbackException(callback,
                        "Unsupported callback type: " + callback.getClass().getName());
            }
        }
    }
}
