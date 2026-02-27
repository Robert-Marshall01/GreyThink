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
import java.net.URL;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.UUID;

/**
 * SOAP client that calls an external VIN (Vehicle Identification Number)
 * lookup service for automobile insurance claims.
 *
 * <p>In auto insurance, VIN decoding is required to verify the vehicle's
 * make, model, year, and value before processing a claim.  This is typically
 * provided by an external partner service (e.g., NHTSA vPIC, ISO, CCC ONE)
 * exposed over SOAP.</p>
 *
 * <p>This client demonstrates:</p>
 * <ul>
 *   <li>SAAJ-based SOAP message construction (no generated stubs)</li>
 *   <li>WS-Security header injection</li>
 *   <li>Retry with exponential backoff for transient failures</li>
 *   <li>SOAP fault handling and classification</li>
 *   <li>Response parsing via DOM traversal</li>
 *   <li>Circuit-breaker pattern (tracks consecutive failures)</li>
 * </ul>
 *
 * @author Grey Legacy Integration Team
 * @since 1.0
 */
@Component("vinLookupServiceClient")
public class VinLookupServiceClient {

    private static final Logger LOG = LoggerFactory.getLogger(VinLookupServiceClient.class);

    private static final String VIN_NS = "http://partner.vinlookup.com/services/decode/v1";
    private static final String WSS_NS = "http://docs.oasis-open.org/wss/2004/01/"
            + "oasis-200401-wss-wssecurity-secext-1.0.xsd";

    private static final int MAX_RETRIES = 3;
    private static final long BASE_RETRY_DELAY_MS = 500L;

    /** Circuit breaker: consecutive failure threshold. */
    private static final int CIRCUIT_BREAKER_THRESHOLD = 5;

    /** Circuit breaker: cooldown period in milliseconds. */
    private static final long CIRCUIT_BREAKER_COOLDOWN_MS = 60_000L;

    @Value("${vin.lookup.endpoint:http://localhost:9091/partner/VinLookupService}")
    private String endpointUrl;

    @Value("${vin.lookup.api.key:dev_vin_api_key}")
    private String apiKey;

    private MessageFactory messageFactory;

    /** Circuit breaker state. */
    private volatile int consecutiveFailures = 0;
    private volatile long circuitOpenedAt = 0;

    @PostConstruct
    public void init() throws SOAPException {
        messageFactory = MessageFactory.newInstance();
        LOG.info("VinLookupServiceClient initialised. Endpoint: {}", endpointUrl);
    }

    // -------------------------------------------------------------------------
    // Public API
    // -------------------------------------------------------------------------

    /**
     * Decodes a VIN to retrieve vehicle details.
     *
     * @param vin       the 17-character Vehicle Identification Number
     * @param modelYear optional model year hint for more accurate decoding
     * @return the decoded vehicle information
     * @throws VinLookupException if the lookup fails
     */
    public VehicleInfo decodeVin(String vin, Integer modelYear) throws VinLookupException {
        LOG.info("VIN lookup requested: vin={}, modelYear={}", maskVin(vin), modelYear);

        // Circuit breaker check
        if (isCircuitOpen()) {
            long elapsed = System.currentTimeMillis() - circuitOpenedAt;
            if (elapsed < CIRCUIT_BREAKER_COOLDOWN_MS) {
                throw new VinLookupException(
                        "VIN lookup service circuit breaker is OPEN. "
                        + "Too many consecutive failures. Cooldown remaining: "
                        + (CIRCUIT_BREAKER_COOLDOWN_MS - elapsed) + "ms",
                        "SYSTEM.SERVICE_UNAVAILABLE", false);
            }
            // Half-open: allow a single attempt
            LOG.info("Circuit breaker half-open — allowing probe request");
        }

        // Validate VIN format
        if (vin == null || vin.length() != 17) {
            throw new VinLookupException(
                    "Invalid VIN format. Must be exactly 17 characters.",
                    "VALIDATION.INVALID_FORMAT", false);
        }

        String requestId = UUID.randomUUID().toString();
        int attempt = 0;

        while (attempt < MAX_RETRIES) {
            attempt++;
            SOAPConnection connection = null;

            try {
                SOAPMessage request = buildDecodeVinRequest(requestId, vin, modelYear);

                if (LOG.isDebugEnabled()) {
                    LOG.debug("VIN SOAP Request (attempt {}/{}):\n{}", attempt, MAX_RETRIES,
                            soapMessageToString(request));
                }

                SOAPConnectionFactory connectionFactory = SOAPConnectionFactory.newInstance();
                connection = connectionFactory.createConnection();
                SOAPMessage response = connection.call(request, new URL(endpointUrl));

                SOAPBody responseBody = response.getSOAPBody();
                if (responseBody.hasFault()) {
                    SOAPFault fault = responseBody.getFault();
                    LOG.error("SOAP fault from VIN service: code={}, message={}",
                            fault.getFaultCode(), fault.getFaultString());

                    if (isRetryable(fault.getFaultCode()) && attempt < MAX_RETRIES) {
                        long delay = BASE_RETRY_DELAY_MS * (long) Math.pow(2, attempt - 1);
                        LOG.warn("Retrying VIN lookup in {}ms...", delay);
                        Thread.sleep(delay);
                        continue;
                    }

                    recordFailure();
                    throw new VinLookupException(
                            "VIN service fault: " + fault.getFaultString(),
                            fault.getFaultCode(), false);
                }

                // Success — reset circuit breaker
                VehicleInfo info = parseDecodeResponse(responseBody);
                recordSuccess();
                LOG.info("VIN decoded: vin={}, make={}, model={}, year={}",
                        maskVin(vin), info.getMake(), info.getModel(), info.getYear());
                return info;

            } catch (VinLookupException vle) {
                throw vle;
            } catch (Exception e) {
                LOG.error("VIN lookup error (attempt {}/{}): {}", attempt, MAX_RETRIES, e.getMessage());

                if (attempt >= MAX_RETRIES) {
                    recordFailure();
                    throw new VinLookupException(
                            "VIN lookup failed after " + MAX_RETRIES + " attempts: " + e.getMessage(),
                            "SYSTEM.SERVICE_UNAVAILABLE", true);
                }

                try {
                    long delay = BASE_RETRY_DELAY_MS * (long) Math.pow(2, attempt - 1);
                    Thread.sleep(delay);
                } catch (InterruptedException ie) {
                    Thread.currentThread().interrupt();
                    throw new VinLookupException("Interrupted", "SYSTEM.INTERNAL_ERROR", false);
                }
            } finally {
                if (connection != null) {
                    try { connection.close(); } catch (SOAPException se) { /* ignore */ }
                }
            }
        }

        throw new VinLookupException("Exhausted retries", "SYSTEM.SERVICE_UNAVAILABLE", true);
    }

    // -------------------------------------------------------------------------
    // SOAP Message Construction
    // -------------------------------------------------------------------------

    private SOAPMessage buildDecodeVinRequest(String requestId, String vin, Integer modelYear)
            throws SOAPException {

        SOAPMessage message = messageFactory.createMessage();
        SOAPPart soapPart = message.getSOAPPart();
        SOAPEnvelope envelope = soapPart.getEnvelope();

        envelope.addNamespaceDeclaration("vin", VIN_NS);
        envelope.addNamespaceDeclaration("wsse", WSS_NS);

        // WS-Security: API key authentication
        SOAPHeader header = envelope.getHeader();
        SOAPElement security = header.addChildElement("Security", "wsse", WSS_NS);
        SOAPElement apiKeyElement = security.addChildElement("ApiKey", "wsse");
        apiKeyElement.addTextNode(apiKey);

        // Body
        SOAPBody body = envelope.getBody();
        SOAPElement decodeRequest = body.addChildElement("DecodeVinRequest", "vin");
        decodeRequest.addChildElement("RequestId", "vin").addTextNode(requestId);
        decodeRequest.addChildElement("Vin", "vin").addTextNode(vin);
        if (modelYear != null) {
            decodeRequest.addChildElement("ModelYear", "vin").addTextNode(String.valueOf(modelYear));
        }
        decodeRequest.addChildElement("SourceSystem", "vin").addTextNode("GREY_LEGACY");

        message.saveChanges();
        return message;
    }

    // -------------------------------------------------------------------------
    // Response Parsing
    // -------------------------------------------------------------------------

    private VehicleInfo parseDecodeResponse(SOAPBody body) {
        VehicleInfo info = new VehicleInfo();

        try {
            java.util.Iterator<?> children = body.getChildElements();
            while (children.hasNext()) {
                Object child = children.next();
                if (child instanceof SOAPElement) {
                    SOAPElement responseElement = (SOAPElement) child;
                    info.setMake(getChildText(responseElement, "Make"));
                    info.setModel(getChildText(responseElement, "Model"));

                    String yearStr = getChildText(responseElement, "Year");
                    if (yearStr != null) {
                        info.setYear(Integer.parseInt(yearStr));
                    }

                    info.setBodyType(getChildText(responseElement, "BodyType"));
                    info.setDriveType(getChildText(responseElement, "DriveType"));
                    info.setEngineType(getChildText(responseElement, "EngineType"));
                    info.setFuelType(getChildText(responseElement, "FuelType"));

                    String msrpStr = getChildText(responseElement, "Msrp");
                    if (msrpStr != null) {
                        info.setMsrp(new java.math.BigDecimal(msrpStr));
                    }

                    break;
                }
            }
        } catch (Exception e) {
            LOG.error("Error parsing VIN decode response", e);
        }

        return info;
    }

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
    // Circuit Breaker
    // -------------------------------------------------------------------------

    private synchronized void recordFailure() {
        consecutiveFailures++;
        if (consecutiveFailures >= CIRCUIT_BREAKER_THRESHOLD) {
            circuitOpenedAt = System.currentTimeMillis();
            LOG.error("Circuit breaker OPENED after {} consecutive failures", consecutiveFailures);
        }
    }

    private synchronized void recordSuccess() {
        if (consecutiveFailures > 0) {
            LOG.info("Circuit breaker reset after successful call (was at {} failures)",
                    consecutiveFailures);
        }
        consecutiveFailures = 0;
        circuitOpenedAt = 0;
    }

    private boolean isCircuitOpen() {
        return consecutiveFailures >= CIRCUIT_BREAKER_THRESHOLD;
    }

    // -------------------------------------------------------------------------
    // Utility
    // -------------------------------------------------------------------------

    private boolean isRetryable(String faultCode) {
        return faultCode != null && (
                faultCode.contains("Server") ||
                faultCode.contains("TIMEOUT") ||
                faultCode.contains("THROTTLE"));
    }

    /**
     * Masks VIN for logging (show first 3 + last 4 characters).
     */
    private String maskVin(String vin) {
        if (vin == null || vin.length() < 7) return "***";
        return vin.substring(0, 3) + "***********" + vin.substring(vin.length() - 4);
    }

    private String soapMessageToString(SOAPMessage message) {
        try {
            ByteArrayOutputStream baos = new ByteArrayOutputStream();
            message.writeTo(baos);
            return baos.toString("UTF-8");
        } catch (Exception e) {
            return "[unable to serialize]";
        }
    }

    // -------------------------------------------------------------------------
    // DTOs
    // -------------------------------------------------------------------------

    /**
     * Vehicle information returned by the VIN decoder.
     */
    public static class VehicleInfo {
        private String make;
        private String model;
        private int year;
        private String bodyType;
        private String driveType;
        private String engineType;
        private String fuelType;
        private java.math.BigDecimal msrp;

        public String getMake()                     { return make; }
        public void setMake(String make)            { this.make = make; }
        public String getModel()                    { return model; }
        public void setModel(String model)          { this.model = model; }
        public int getYear()                        { return year; }
        public void setYear(int year)               { this.year = year; }
        public String getBodyType()                 { return bodyType; }
        public void setBodyType(String bodyType)    { this.bodyType = bodyType; }
        public String getDriveType()                { return driveType; }
        public void setDriveType(String driveType)  { this.driveType = driveType; }
        public String getEngineType()               { return engineType; }
        public void setEngineType(String engineType) { this.engineType = engineType; }
        public String getFuelType()                 { return fuelType; }
        public void setFuelType(String fuelType)    { this.fuelType = fuelType; }
        public java.math.BigDecimal getMsrp()       { return msrp; }
        public void setMsrp(java.math.BigDecimal msrp) { this.msrp = msrp; }

        @Override
        public String toString() {
            return year + " " + make + " " + model + " (" + bodyType + ")";
        }
    }

    /**
     * Exception thrown when VIN lookup fails.
     */
    public static class VinLookupException extends Exception {
        private static final long serialVersionUID = 1L;
        private final String faultCode;
        private final boolean retryable;

        public VinLookupException(String message, String faultCode, boolean retryable) {
            super(message);
            this.faultCode = faultCode;
            this.retryable = retryable;
        }

        public String getFaultCode() { return faultCode; }
        public boolean isRetryable() { return retryable; }
    }
}
