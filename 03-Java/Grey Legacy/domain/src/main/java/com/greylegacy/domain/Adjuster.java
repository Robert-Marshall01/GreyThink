package com.greylegacy.domain;

import javax.persistence.*;

/**
 * Represents a claims adjuster who investigates and settles insurance claims.
 * Tracks adjuster availability based on active status and current caseload
 * relative to their maximum allowed caseload.
 */
@Entity
@Table(name = "ADJUSTER", indexes = {
        @Index(name = "IDX_ADJUSTER_CODE", columnList = "ADJUSTER_CODE", unique = true)
})
@SequenceGenerator(name = "default_seq", sequenceName = "ADJUSTER_SEQ", allocationSize = 1)
public class Adjuster extends BaseEntity {

    private static final long serialVersionUID = 1L;

    @Column(name = "ADJUSTER_CODE", length = 10, unique = true, nullable = false)
    private String adjusterCode;

    @Column(name = "FIRST_NAME", length = 50, nullable = false)
    private String firstName;

    @Column(name = "LAST_NAME", length = 50, nullable = false)
    private String lastName;

    @Column(name = "EMAIL", length = 100)
    private String email;

    @Column(name = "PHONE", length = 20)
    private String phone;

    @Column(name = "SPECIALIZATION", length = 50)
    private String specialization;

    @Column(name = "ACTIVE")
    private boolean active = true;

    @Column(name = "MAX_CASELOAD")
    private int maxCaseload = 50;

    @Column(name = "CURRENT_CASELOAD")
    private int currentCaseload = 0;

    // -------------------------------------------------------------------------
    // Helper Methods
    // -------------------------------------------------------------------------

    /**
     * Returns {@code true} if this adjuster is active and has capacity
     * to take on additional claims.
     *
     * @return whether the adjuster is available for new assignments
     */
    public boolean isAvailable() {
        return active && currentCaseload < maxCaseload;
    }

    // -------------------------------------------------------------------------
    // Getters and Setters
    // -------------------------------------------------------------------------

    public String getAdjusterCode() {
        return adjusterCode;
    }

    public void setAdjusterCode(String adjusterCode) {
        this.adjusterCode = adjusterCode;
    }

    public String getFirstName() {
        return firstName;
    }

    public void setFirstName(String firstName) {
        this.firstName = firstName;
    }

    public String getLastName() {
        return lastName;
    }

    public void setLastName(String lastName) {
        this.lastName = lastName;
    }

    public String getEmail() {
        return email;
    }

    public void setEmail(String email) {
        this.email = email;
    }

    public String getPhone() {
        return phone;
    }

    public void setPhone(String phone) {
        this.phone = phone;
    }

    public String getSpecialization() {
        return specialization;
    }

    public void setSpecialization(String specialization) {
        this.specialization = specialization;
    }

    public boolean isActive() {
        return active;
    }

    public void setActive(boolean active) {
        this.active = active;
    }

    public int getMaxCaseload() {
        return maxCaseload;
    }

    public void setMaxCaseload(int maxCaseload) {
        this.maxCaseload = maxCaseload;
    }

    public int getCurrentCaseload() {
        return currentCaseload;
    }

    public void setCurrentCaseload(int currentCaseload) {
        this.currentCaseload = currentCaseload;
    }

    // -------------------------------------------------------------------------
    // toString
    // -------------------------------------------------------------------------

    @Override
    public String toString() {
        return "Adjuster{" +
                "id=" + getId() +
                ", adjusterCode='" + adjusterCode + '\'' +
                ", firstName='" + firstName + '\'' +
                ", lastName='" + lastName + '\'' +
                ", email='" + email + '\'' +
                ", specialization='" + specialization + '\'' +
                ", active=" + active +
                ", maxCaseload=" + maxCaseload +
                ", currentCaseload=" + currentCaseload +
                '}';
    }
}
