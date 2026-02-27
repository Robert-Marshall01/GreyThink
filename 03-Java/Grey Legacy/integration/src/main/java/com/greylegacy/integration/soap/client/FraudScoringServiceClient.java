package com.greylegacy.integration.soap.client;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import javax.xml.namespace.QName;
import javax.xml.soap.MessageFactory;
import javax.xml.soap.SOAPBody;
import javax.xml.soap.SOAPConnection;
import javax.xml.soap.SOAPConnectionFactory;
import javax.xml.soap.SOAPElement;
import javax.xml.soap.SOAPEnvelope;
import javax.xml.soap.SOAPException;
import javax.xml.soap.SOAPFault;
import javax.xml.soap.SOAPHeader;
import javax.xml.soap.SOAPMessage;
import javax.xml.soap.SOAPPart;
import java.io.ByteArrayOutputStream;
import java.math.BigDecimal;
import java.net.URL;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.UUID;

/**
 * SOAP client that calls an external partner fraud-scoring service.
 *
 * <p>In real insurance systems, fraud scoring is often provided by an external
 * vendor (e.g., ISO ClaimSearch, Verisk, NICB) via SOAP web services.  This
 * client demonstrates the patterns required to consume such a service:</p>
 *
 * <ul>
 *   <li>Building a SOAP envelope from scratch (no generated stubs)</li>
 *   <li>Adding WS-Security {@code UsernameToken} headers</li>
 *   <li>Handling SOAP faults from a remote service</li>
 *   <li>Circuit-breaker / retry / timeout patterns</li>
 *   <li>Response parsing via the SAAJ API</li>
 * </ul>
 *
 * <p>This approach (SAAJ-based, no codegen) is common in legacy systems
 * where the partner's WSDL was unavailable, poorly formed, or where the
 * team chose to avoid wsimport/wsdl2java tooling.</p>
 *
 * @author Grey Legacy Integration Team
 * @since 1.0
 */
@Component("fraudScoringServiceClient")
public class FraudScoringServiceClient {

    private static final Logger LOG = LoggerFactory.getLogger(FraudScoringServiceClient.class);

    private static final String FRAUD_NS = "http://partner.fraudscore.com/services/scoring/v2";
    private static final String WSS_NS = "http://docs.oasis-open.org/wss/2004/01/"
            + "oasis-200401-wss-wssecurity-secext-1.0.xsd";
    private static final String WSU_NS = "http://docs.oasis-open.org/wss/2004/01/"
            + "oasis-200401-wss-wssecurity-utility-1.0.xsd";
    private static final String PASSWORD_TEXT_TYPE =
            "http://docs.oasis-open.org/wss/2004/01/"
            + "oasis-200401-wss-username-token-profile-1.0#PasswordText";

    /** Maximum number of retry attempts on transient failures. */
    private static final int MAX_RETRIES = 3;

    /** Base delay between retries (exponential backoff). */
    private static final long BASE_RETRY_DELAY_MS = 1000L;

    /** Connection timeout in milliseconds. */
    private static final int CONNECT_TIMEOUT_MS = 5000;

    /** Read timeout in milliseconds. */
    private static final int READ_TIMEOUT_MS = 10000;

    @Value("${fraud.scoring.endpoint:http://localhost:9090/partner/FraudScoringService}")
    private String endpointUrl;

    @Value("${fraud.scoring.username:greylegacy_client}")
    private String username;

    @Value("${fraud.scoring.password:client_secret_dev}")
    private String password;

    private MessageFactory messageFactory;

    @PostConstruct
    public void init() throws SOAPException {
        messageFactory = MessageFactory.newInstance();
        LOG.info("FraudScoringServiceClient initialised. Endpoint: {}", endpointUrl);
    }

    // -------------------------------------------------------------------------
    // Public API
    // -------------------------------------------------------------------------

    /**
     * Calls the external fraud scoring service for a given claim.
     *
     * @param claimNumber    the claim identifier
     * @param policyNumber   the associated policy number
     * @param claimType      type of claim (e.g., COLLISION, THEFT)
     * @param estimatedLoss  the estimated loss amount
     * @param lossDate       when the loss occurred
     * @return the fraud score result from the partner service
     * @throws FraudScoringException if the call fails after all retries
     */
    public FraudScoreResult scoreClaim(String claimNumber, String policyNumber,
                                        String claimType, BigDecimal estimatedLoss,
                                        Date lossDate) throws FraudScoringException {
        LOG.info("Calling fraud scoring service for claim {} (policy={})", claimNumber, policyNumber);

        String requestId = UUID.randomUUID().toString();
        SOAPMessage request = null;
        int attempt = 0;

        while (attempt < MAX_RETRIES) {
            attempt++;
            SOAPConnection connection = null;

            try {
                // Build the SOAP request
                request = buildScoreClaimRequest(requestId, claimNumber, policyNumber,
                        claimType, estimatedLoss, lossDate);

                if (LOG.isDebugEnabled()) {
                    LOG.debug("SOAP Request (attempt {}/{}):\n{}", attempt, MAX_RETRIES,
                            soapMessageToString(request));
                }

                // Send the request
                SOAPConnectionFactory connectionFactory = SOAPConnectionFactory.newInstance();
                connection = connectionFactory.createConnection();
                SOAPMessage response = connection.call(request, new URL(endpointUrl));

                // Check for SOAP fault
                SOAPBody responseBody = response.getSOAPBody();
                if (responseBody.hasFault()) {
                    SOAPFault fault = responseBody.getFault();
                    String faultCode = fault.getFaultCode();
                    String faultString = fault.getFaultString();
                    LOG.error("SOAP fault from fraud scoring service: code={}, message={}",
                            faultCode, faultString);

                    // Certain faults are retryable (server errors, timeouts)
                    if (isRetryableFault(faultCode) && attempt < MAX_RETRIES) {
                        long delay = BASE_RETRY_DELAY_MS * (long) Math.pow(2, attempt - 1);
                        LOG.warn("Retryable fault encountered. Retrying in {}ms (attempt {}/{})",
                                delay, attempt, MAX_RETRIES);
                        Thread.sleep(delay);
                        continue;
                    }

                    throw new FraudScoringException(
                            "Fraud scoring service returned fault: " + faultString,
                            faultCode, false);
                }

                // Parse the response
                FraudScoreResult result = parseScoreResponse(responseBody, requestId);
                LOG.info("Fraud scoring complete for claim {}: score={}, riskLevel={}",
                        claimNumber, result.getScore(), result.getRiskLevel());
                return result;

            } catch (FraudScoringException fse) {
                throw fse; // Don't retry application-level errors
            } catch (Exception e) {
                LOG.error("Error calling fraud scoring service (attempt {}/{}): {}",
                        attempt, MAX_RETRIES, e.getMessage(), e);

                if (attempt >= MAX_RETRIES) {
                    throw new FraudScoringException(
                            "Fraud scoring service unavailable after " + MAX_RETRIES + " attempts: "
                            + e.getMessage(), "SYSTEM.SERVICE_UNAVAILABLE", true);
                }

                // Exponential backoff
                try {
                    long delay = BASE_RETRY_DELAY_MS * (long) Math.pow(2, attempt - 1);
                    LOG.warn("Retrying in {}ms...", delay);
                    Thread.sleep(delay);
                } catch (InterruptedException ie) {
                    Thread.currentThread().interrupt();
                    throw new FraudScoringException(
                            "Interrupted during retry backoff", "SYSTEM.INTERNAL_ERROR", false);
                }
            } finally {
                if (connection != null) {
                    try {
                        connection.close();
                    } catch (SOAPException se) {
                        LOG.debug("Error closing SOAP connection", se);
                    }
                }
            }
        }

        // Should not reach here, but safety net
        throw new FraudScoringException(
                "Exhausted all retry attempts", "SYSTEM.SERVICE_UNAVAILABLE", true);
    }

    // -------------------------------------------------------------------------
    // SOAP Message Construction
    // -------------------------------------------------------------------------

    /**
     * Builds the SOAP request envelope with WS-Security headers.
     */
    private SOAPMessage buildScoreClaimRequest(String requestId, String claimNumber,
                                                String policyNumber, String claimType,
                                                BigDecimal estimatedLoss, Date lossDate)
            throws SOAPException {

        SOAPMessage message = messageFactory.createMessage();
        SOAPPart soapPart = message.getSOAPPart();
        SOAPEnvelope envelope = soapPart.getEnvelope();

        // Add namespace declarations
        envelope.addNamespaceDeclaration("fraud", FRAUD_NS);
        envelope.addNamespaceDeclaration("wsse", WSS_NS);
        envelope.addNamespaceDeclaration("wsu", WSU_NS);

        // ------- WS-Security Header -------
        SOAPHeader header = envelope.getHeader();
        addWsSecurityHeader(header);

        // ------- SOAP Body -------
        SOAPBody body = envelope.getBody();
        SOAPElement scoreRequest = body.addChildElement("ScoreClaimRequest", "fraud");

        scoreRequest.addChildElement("RequestId", "fraud").addTextNode(requestId);
        scoreRequest.addChildElement("ClaimNumber", "fraud").addTextNode(claimNumber);
        scoreRequest.addChildElement("PolicyNumber", "fraud").addTextNode(policyNumber);
        scoreRequest.addChildElement("ClaimType", "fraud").addTextNode(claimType);
        scoreRequest.addChildElement("EstimatedLoss", "fraud")
                .addTextNode(estimatedLoss != null ? estimatedLoss.toPlainString() : "0");

        SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd");
        scoreRequest.addChildElement("LossDate", "fraud")
                .addTextNode(lossDate != null ? sdf.format(lossDate) : "");

        // Source system identification
        scoreRequest.addChildElement("SourceSystem", "fraud").addTextNode("GREY_LEGACY");
        scoreRequest.addChildElement("ServiceVersion", "fraud").addTextNode("2.0");

        message.saveChanges();
        return message;
    }

    /**
     * Adds a WS-Security {@code UsernameToken} header to the SOAP envelope.
     *
     * <p>This is the standard approach for authenticating to partner SOAP
     * services in the insurance industry. Production systems would use
     * PasswordDigest instead of PasswordText.</p>
     */
    private void addWsSecurityHeader(SOAPHeader header) throws SOAPException {
        SOAPElement security = header.addChildElement("Security", "wsse", WSS_NS);
        security.addAttribute(
                new QName("http://schemas.xmlsoap.org/soap/envelope/", "mustUnderstand"), "1");

        // Timestamp
        SOAPElement timestamp = security.addChildElement("Timestamp", "wsu", WSU_NS);
        SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'");
        String now = sdf.format(new Date());
        timestamp.addChildElement("Created", "wsu").addTextNode(now);
        // Expires in 5 minutes
        Date expires = new Date(System.currentTimeMillis() + 300_000L);
        timestamp.addChildElement("Expires", "wsu").addTextNode(sdf.format(expires));

        // UsernameToken
        SOAPElement usernameToken = security.addChildElement("UsernameToken", "wsse");
        usernameToken.addChildElement("Username", "wsse").addTextNode(username);

        SOAPElement passwordElement = usernameToken.addChildElement("Password", "wsse");
        passwordElement.setAttribute("Type", PASSWORD_TEXT_TYPE);
        passwordElement.addTextNode(password);
    }

    // -------------------------------------------------------------------------
    // Response Parsing
    // -------------------------------------------------------------------------

    /**
     * Parses the fraud scoring response from the partner service.
     */
    private FraudScoreResult parseScoreResponse(SOAPBody body, String requestId) {
        FraudScoreResult result = new FraudScoreResult();
        result.setRequestId(requestId);
        result.setResponseTimestamp(new Date());

        try {
            // Navigate to the response element
            // Partner response structure:
            //   <ScoreClaimResponse>
            //     <Score>75</Score>
            //     <RiskLevel>HIGH</RiskLevel>
            //     <Indicators>...</Indicators>
            //     <Confidence>0.85</Confidence>
            //   </ScoreClaimResponse>

            javax.xml.soap.Node responseNode = null;
            java.util.Iterator<?> children = body.getChildElements();
            while (children.hasNext()) {
                Object child = children.next();
                if (child instanceof SOAPElement) {
                    responseNode = (javax.xml.soap.Node) child;
                    break;
                }
            }

            if (responseNode instanceof SOAPElement) {
                SOAPElement responseElement = (SOAPElement) responseNode;

                String scoreStr = getChildText(responseElement, "Score");
                if (scoreStr != null) {
                    result.setScore(Integer.parseInt(scoreStr));
                }

                result.setRiskLevel(getChildText(responseElement, "RiskLevel"));
                result.setConfidence(getChildText(responseElement, "Confidence"));

                String indicators = getChildText(responseElement, "Indicators");
                result.setIndicators(indicators);
            }

        } catch (Exception e) {
            LOG.error("Error parsing fraud scoring response: {}", e.getMessage(), e);
            // Return partial result rather than failing
            result.setScore(-1);
            result.setRiskLevel("PARSE_ERROR");
        }

        return result;
    }

    /**
     * Extracts the text content of a child element by local name.
     */
    private String getChildText(SOAPElement parent, String localName) {
        java.util.Iterator<?> children = parent.getChildElements();
        while (children.hasNext()) {
            Object child = children.next();
            if (child instanceof SOAPElement) {
                SOAPElement element = (SOAPElement) child;
                if (localName.equals(element.getLocalName())) {
                    return element.getTextContent();
                }
            }
        }
        return null;
    }

    // -------------------------------------------------------------------------
    // Utility
    // -------------------------------------------------------------------------

    private boolean isRetryableFault(String faultCode) {
        return faultCode != null && (
                faultCode.contains("Server") ||
                faultCode.contains("TIMEOUT") ||
                faultCode.contains("UNAVAILABLE"));
    }

    private String soapMessageToString(SOAPMessage message) {
        try {
            ByteArrayOutputStream baos = new ByteArrayOutputStream();
            message.writeTo(baos);
            return baos.toString("UTF-8");
        } catch (Exception e) {
            return "[unable to serialize SOAP message]";
        }
    }

    // -------------------------------------------------------------------------
    // Result DTO
    // -------------------------------------------------------------------------

    /**
     * Holds the fraud score result from the external partner service.
     */
    public static class FraudScoreResult {
        private String requestId;
        private int score;
        private String riskLevel;
        private String confidence;
        private String indicators;
        private Date responseTimestamp;

        public String getRequestId()                        { return requestId; }
        public void setRequestId(String requestId)          { this.requestId = requestId; }

        public int getScore()                               { return score; }
        public void setScore(int score)                     { this.score = score; }

        public String getRiskLevel()                        { return riskLevel; }
        public void setRiskLevel(String riskLevel)          { this.riskLevel = riskLevel; }

        public String getConfidence()                       { return confidence; }
        public void setConfidence(String confidence)        { this.confidence = confidence; }

        public String getIndicators()                       { return indicators; }
        public void setIndicators(String indicators)        { this.indicators = indicators; }

        public Date getResponseTimestamp()                       { return responseTimestamp; }
        public void setResponseTimestamp(Date responseTimestamp) { this.responseTimestamp = responseTimestamp; }

        @Override
        public String toString() {
            return "FraudScoreResult{score=" + score + ", riskLevel='" + riskLevel
                    + "', confidence='" + confidence + "', indicators='" + indicators + "'}";
        }
    }

    // -------------------------------------------------------------------------
    // Exception
    // -------------------------------------------------------------------------

    /**
     * Exception thrown when the external fraud scoring service call fails.
     */
    public static class FraudScoringException extends Exception {
        private static final long serialVersionUID = 1L;
        private final String faultCode;
        private final boolean retryable;

        public FraudScoringException(String message, String faultCode, boolean retryable) {
            super(message);
            this.faultCode = faultCode;
            this.retryable = retryable;
        }

        public String getFaultCode() { return faultCode; }
        public boolean isRetryable() { return retryable; }
    }
}
