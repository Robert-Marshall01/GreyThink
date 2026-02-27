package com.greylegacy.integration.soap;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import java.io.Serializable;

/**
 * JAXB-bound request DTO for the {@code updateClaimStatus} SOAP operation.
 *
 * <p>Represents a request to transition a claim from its current status
 * to a new status.  The service validates the transition against the
 * claim lifecycle state machine before applying it.</p>
 *
 * @see UpdateClaimStatusResponse
 */
@XmlRootElement(name = "updateClaimStatusRequest",
        namespace = "http://www.greylegacy.com/schema/claims")
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "UpdateClaimStatusRequest",
        namespace = "http://www.greylegacy.com/schema/claims",
        propOrder = {"claimNumber", "newStatus", "reason", "updatedBy"})
public class UpdateClaimStatusRequest implements Serializable {

    private static final long serialVersionUID = 1L;

    @XmlElement(required = true)
    private String claimNumber;

    @XmlElement(required = true)
    private String newStatus;

    private String reason;

    @XmlElement(required = true)
    private String updatedBy;

    public UpdateClaimStatusRequest() {
    }

    public String getClaimNumber()                    { return claimNumber; }
    public void setClaimNumber(String claimNumber)    { this.claimNumber = claimNumber; }

    public String getNewStatus()                      { return newStatus; }
    public void setNewStatus(String newStatus)        { this.newStatus = newStatus; }

    public String getReason()                         { return reason; }
    public void setReason(String reason)              { this.reason = reason; }

    public String getUpdatedBy()                      { return updatedBy; }
    public void setUpdatedBy(String updatedBy)        { this.updatedBy = updatedBy; }
}
