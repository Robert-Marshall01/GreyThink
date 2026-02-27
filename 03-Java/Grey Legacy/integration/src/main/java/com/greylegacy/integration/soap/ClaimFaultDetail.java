package com.greylegacy.integration.soap;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import java.util.Date;

/**
 * JAXB-annotated fault detail containing error information for SOAP faults.
 *
 * <p>Follows the fault taxonomy defined in the claims-types.xsd schema,
 * using hierarchical error codes (e.g., VALIDATION.MISSING_FIELD,
 * BUSINESS.POLICY_NOT_FOUND) to enable structured error handling by
 * SOAP clients.</p>
 *
 * <p>The {@code severity} field (ERROR, WARNING, FATAL) allows clients
 * to distinguish between recoverable and non-recoverable faults.
 * The {@code retryable} flag indicates whether the same request might
 * succeed on a subsequent attempt (e.g., SYSTEM.TIMEOUT is retryable,
 * VALIDATION.MISSING_FIELD is not).</p>
 */
@XmlRootElement(name = "claimFault", namespace = "http://www.greylegacy.com/schema/claims")
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "ClaimFault", namespace = "http://www.greylegacy.com/schema/claims",
    propOrder = {
        "errorCode",
        "errorMessage",
        "severity",
        "timestamp",
        "correlationId",
        "fieldName"
    })
public class ClaimFaultDetail {

    @XmlElement(required = true)
    private String errorCode;

    @XmlElement(required = true)
    private String errorMessage;

    private String severity;

    @XmlElement(required = true)
    private Date timestamp;

    private String correlationId;

    private String fieldName;

    @XmlAttribute
    private boolean retryable;

    public ClaimFaultDetail() {
    }

    public ClaimFaultDetail(String errorCode, String errorMessage, Date timestamp) {
        this.errorCode = errorCode;
        this.errorMessage = errorMessage;
        this.timestamp = timestamp;
        this.severity = "ERROR";
        this.retryable = false;
    }

    /**
     * Full constructor for the enhanced fault taxonomy.
     *
     * @param errorCode     hierarchical fault code (e.g., VALIDATION.MISSING_FIELD)
     * @param errorMessage  human-readable description
     * @param severity      ERROR, WARNING, or FATAL
     * @param timestamp     when the fault occurred
     * @param correlationId trace correlation ID from the SOAP header
     * @param fieldName     the request field that caused a validation fault (nullable)
     * @param retryable     whether the client should retry
     */
    public ClaimFaultDetail(String errorCode, String errorMessage, String severity,
                            Date timestamp, String correlationId, String fieldName,
                            boolean retryable) {
        this.errorCode = errorCode;
        this.errorMessage = errorMessage;
        this.severity = severity;
        this.timestamp = timestamp;
        this.correlationId = correlationId;
        this.fieldName = fieldName;
        this.retryable = retryable;
    }

    public String getErrorCode() {
        return errorCode;
    }

    public String getErrorMessage() {
        return errorMessage;
    }

    public String getSeverity() {
        return severity;
    }

    public Date getTimestamp() {
        return timestamp;
    }

    public String getCorrelationId() {
        return correlationId;
    }

    public String getFieldName() {
        return fieldName;
    }

    public boolean isRetryable() {
        return retryable;
    }

    @Override
    public String toString() {
        return "ClaimFaultDetail{" +
                "errorCode='" + errorCode + '\'' +
                ", errorMessage='" + errorMessage + '\'' +
                ", severity='" + severity + '\'' +
                ", timestamp=" + timestamp +
                ", correlationId='" + correlationId + '\'' +
                ", fieldName='" + fieldName + '\'' +
                ", retryable=" + retryable +
                '}';
    }
}
