package com.greylegacy.integration.soap;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import java.io.Serializable;
import java.util.Date;

/**
 * JAXB-bound response DTO for the {@code updateClaimStatus} SOAP operation.
 *
 * <p>Returns the previous and current status, along with the transition
 * timestamp and a human-readable confirmation message.</p>
 *
 * @see UpdateClaimStatusRequest
 */
@XmlRootElement(name = "updateClaimStatusResponse",
        namespace = "http://www.greylegacy.com/schema/claims")
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "UpdateClaimStatusResponse",
        namespace = "http://www.greylegacy.com/schema/claims",
        propOrder = {"claimNumber", "previousStatus", "currentStatus", "transitionDate", "message"})
public class UpdateClaimStatusResponse implements Serializable {

    private static final long serialVersionUID = 1L;

    @XmlElement(required = true)
    private String claimNumber;

    @XmlElement(required = true)
    private String previousStatus;

    @XmlElement(required = true)
    private String currentStatus;

    @XmlElement(required = true)
    private Date transitionDate;

    @XmlElement(required = true)
    private String message;

    public UpdateClaimStatusResponse() {
    }

    public String getClaimNumber()                          { return claimNumber; }
    public void setClaimNumber(String claimNumber)          { this.claimNumber = claimNumber; }

    public String getPreviousStatus()                       { return previousStatus; }
    public void setPreviousStatus(String previousStatus)    { this.previousStatus = previousStatus; }

    public String getCurrentStatus()                        { return currentStatus; }
    public void setCurrentStatus(String currentStatus)      { this.currentStatus = currentStatus; }

    public Date getTransitionDate()                         { return transitionDate; }
    public void setTransitionDate(Date transitionDate)      { this.transitionDate = transitionDate; }

    public String getMessage()                              { return message; }
    public void setMessage(String message)                  { this.message = message; }
}
