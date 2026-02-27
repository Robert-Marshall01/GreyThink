package com.greylegacy.domain;

import javax.persistence.*;

/**
 * Represents a note attached to a claim. Notes may be internal adjuster notes,
 * claimant communications, or adjuster observations.
 */
@Entity
@Table(name = "CLAIM_NOTE")
@SequenceGenerator(name = "default_seq", sequenceName = "NOTE_SEQ", allocationSize = 1)
public class ClaimNote extends BaseEntity {

    private static final long serialVersionUID = 1L;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "CLAIM_ID")
    private Claim claim;

    @Column(name = "NOTE_TYPE", length = 30)
    private String noteType;

    @Column(name = "SUBJECT", length = 200, nullable = false)
    private String subject;

    @Lob
    @Column(name = "CONTENT")
    private String content;

    @Column(name = "AUTHOR_NAME", length = 50, nullable = false)
    private String authorName;

    @Column(name = "CONFIDENTIAL")
    private boolean confidential = false;

    // -------------------------------------------------------------------------
    // Getters and Setters
    // -------------------------------------------------------------------------

    public Claim getClaim() {
        return claim;
    }

    public void setClaim(Claim claim) {
        this.claim = claim;
    }

    public String getNoteType() {
        return noteType;
    }

    public void setNoteType(String noteType) {
        this.noteType = noteType;
    }

    public String getSubject() {
        return subject;
    }

    public void setSubject(String subject) {
        this.subject = subject;
    }

    public String getContent() {
        return content;
    }

    public void setContent(String content) {
        this.content = content;
    }

    public String getAuthorName() {
        return authorName;
    }

    public void setAuthorName(String authorName) {
        this.authorName = authorName;
    }

    public boolean isConfidential() {
        return confidential;
    }

    public void setConfidential(boolean confidential) {
        this.confidential = confidential;
    }

    // -------------------------------------------------------------------------
    // toString
    // -------------------------------------------------------------------------

    @Override
    public String toString() {
        return "ClaimNote{" +
                "id=" + getId() +
                ", noteType='" + noteType + '\'' +
                ", subject='" + subject + '\'' +
                ", authorName='" + authorName + '\'' +
                ", confidential=" + confidential +
                '}';
    }
}
