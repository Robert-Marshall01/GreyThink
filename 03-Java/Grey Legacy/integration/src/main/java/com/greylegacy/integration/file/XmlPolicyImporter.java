package com.greylegacy.integration.file;

import java.io.File;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import javax.xml.XMLConstants;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.transform.stream.StreamSource;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.xml.sax.SAXException;

import com.greylegacy.dao.PolicyDao;
import com.greylegacy.domain.Policy;
import com.greylegacy.domain.PolicyStatus;
import com.greylegacy.domain.PolicyType;

/**
 * Imports insurance policies from XML files using JAXB unmarshalling.
 *
 * <p>The XML document is validated against a configurable XSD schema
 * before import. Each {@code <policy>} entry is mapped to a
 * {@link Policy} domain entity and persisted via {@link PolicyDao}.
 * Duplicate detection is performed by policy number; existing policies
 * are updated (upsert semantics).</p>
 *
 * @author Grey Legacy Integration Team
 * @since 1.0
 */
public class XmlPolicyImporter {

    private static final Logger LOG = LoggerFactory.getLogger(XmlPolicyImporter.class);

    private PolicyDao policyDao;
    private String xsdSchemaPath;

    // -------------------------------------------------------------------------
    // Spring setter injection
    // -------------------------------------------------------------------------

    public void setPolicyDao(PolicyDao policyDao) {
        this.policyDao = policyDao;
    }

    /**
     * Optional path to the XSD schema for validation.
     * If {@code null}, schema validation is skipped.
     */
    public void setXsdSchemaPath(String xsdSchemaPath) {
        this.xsdSchemaPath = xsdSchemaPath;
    }

    // -------------------------------------------------------------------------
    // Public API
    // -------------------------------------------------------------------------

    /**
     * Imports policies from the given XML file.
     *
     * @param file the XML file to import
     * @return a summary of the import result
     */
    public ImportSummary importFile(File file) {
        LOG.info("Starting XML policy import from file: {}", file.getAbsolutePath());

        ImportSummary summary = new ImportSummary();

        try {
            PolicyImportDocument document = unmarshal(file);

            if (document == null || document.getPolicies() == null || document.getPolicies().isEmpty()) {
                LOG.warn("No policy entries found in XML file: {}", file.getName());
                summary.addError("No policy entries found in document");
                return summary;
            }

            List<PolicyImportEntry> entries = document.getPolicies();
            LOG.info("Found {} policy entries in XML file", entries.size());

            for (int i = 0; i < entries.size(); i++) {
                PolicyImportEntry entry = entries.get(i);
                summary.incrementTotal();

                try {
                    processEntry(entry, i + 1, summary);
                } catch (Exception ex) {
                    LOG.error("Error processing policy entry {} (index {}): {}",
                            entry.getPolicyNumber(), i + 1, ex.getMessage(), ex);
                    summary.addError("Entry " + (i + 1) + ": " + ex.getMessage());
                    summary.incrementSkipped();
                }
            }

        } catch (JAXBException ex) {
            LOG.error("JAXB unmarshalling error for file {}: {}", file.getName(), ex.getMessage(), ex);
            summary.addError("JAXB error: " + ex.getMessage());
        } catch (SAXException ex) {
            LOG.error("XSD validation error for file {}: {}", file.getName(), ex.getMessage(), ex);
            summary.addError("Schema validation error: " + ex.getMessage());
        }

        LOG.info("XML policy import complete for {}. {}", file.getName(), summary);
        return summary;
    }

    // -------------------------------------------------------------------------
    // JAXB unmarshalling
    // -------------------------------------------------------------------------

    private PolicyImportDocument unmarshal(File file) throws JAXBException, SAXException {
        JAXBContext context = JAXBContext.newInstance(PolicyImportDocument.class);
        Unmarshaller unmarshaller = context.createUnmarshaller();

        // Apply XSD validation if schema path is configured
        if (xsdSchemaPath != null && !xsdSchemaPath.isEmpty()) {
            File xsdFile = new File(xsdSchemaPath);
            if (xsdFile.exists()) {
                SchemaFactory schemaFactory = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI);
                Schema schema = schemaFactory.newSchema(new StreamSource(xsdFile));
                unmarshaller.setSchema(schema);
                LOG.debug("XSD validation enabled: {}", xsdSchemaPath);
            } else {
                LOG.warn("XSD schema file not found: {} — skipping validation", xsdSchemaPath);
            }
        }

        return (PolicyImportDocument) unmarshaller.unmarshal(file);
    }

    // -------------------------------------------------------------------------
    // Entry processing — update or insert
    // -------------------------------------------------------------------------

    private void processEntry(PolicyImportEntry entry, int index, ImportSummary summary) {
        if (entry.getPolicyNumber() == null || entry.getPolicyNumber().trim().isEmpty()) {
            LOG.warn("Entry {}: Missing policy number — skipping", index);
            summary.addError("Entry " + index + ": Missing policy number");
            summary.incrementSkipped();
            return;
        }

        // Duplicate detection
        Policy existing = policyDao.findByPolicyNumber(entry.getPolicyNumber());

        if (existing != null) {
            LOG.info("Policy {} already exists (id={}) — updating", entry.getPolicyNumber(), existing.getId());
            updatePolicy(existing, entry);
            policyDao.update(existing);
            summary.incrementUpdated();
        } else {
            LOG.info("Creating new policy: {}", entry.getPolicyNumber());
            Policy newPolicy = createPolicy(entry);
            policyDao.save(newPolicy);
            summary.incrementImported();
        }
    }

    private Policy createPolicy(PolicyImportEntry entry) {
        Policy policy = new Policy();
        mapEntryToPolicy(policy, entry);
        return policy;
    }

    private void updatePolicy(Policy policy, PolicyImportEntry entry) {
        mapEntryToPolicy(policy, entry);
    }

    private void mapEntryToPolicy(Policy policy, PolicyImportEntry entry) {
        policy.setPolicyNumber(entry.getPolicyNumber());
        policy.setHolderFirstName(entry.getHolderFirstName());
        policy.setHolderLastName(entry.getHolderLastName());
        policy.setHolderEmail(entry.getHolderEmail());
        policy.setHolderPhone(entry.getHolderPhone());

        if (entry.getPolicyType() != null) {
            try {
                policy.setPolicyType(PolicyType.valueOf(entry.getPolicyType().toUpperCase()));
            } catch (IllegalArgumentException ex) {
                LOG.warn("Unknown policy type '{}' for policy {} — defaulting to AUTO",
                        entry.getPolicyType(), entry.getPolicyNumber());
                policy.setPolicyType(PolicyType.AUTO);
            }
        }

        if (entry.getStatus() != null) {
            try {
                policy.setStatus(PolicyStatus.valueOf(entry.getStatus().toUpperCase()));
            } catch (IllegalArgumentException ex) {
                policy.setStatus(PolicyStatus.ACTIVE);
            }
        } else {
            policy.setStatus(PolicyStatus.ACTIVE);
        }

        policy.setEffectiveDate(entry.getEffectiveDate());
        policy.setExpirationDate(entry.getExpirationDate());
        policy.setPremiumAmount(entry.getPremiumAmount());
        policy.setCoverageLimit(entry.getCoverageLimit());
        policy.setDeductible(entry.getDeductible());
        policy.setAgentCode(entry.getAgentCode());
    }

    // =========================================================================
    // JAXB-annotated inner classes
    // =========================================================================

    /**
     * Root element for the policy import XML document.
     */
    @XmlRootElement(name = "policyImport", namespace = "http://greylegacy.com/schema/import")
    @XmlAccessorType(XmlAccessType.FIELD)
    public static class PolicyImportDocument {

        @XmlElement(name = "policy", namespace = "http://greylegacy.com/schema/import")
        private List<PolicyImportEntry> policies = new ArrayList<PolicyImportEntry>();

        public List<PolicyImportEntry> getPolicies() { return policies; }
        public void setPolicies(List<PolicyImportEntry> policies) { this.policies = policies; }
    }

    /**
     * Represents a single policy entry within the import document.
     */
    @XmlAccessorType(XmlAccessType.FIELD)
    public static class PolicyImportEntry {

        @XmlElement(namespace = "http://greylegacy.com/schema/import")
        private String policyNumber;
        @XmlElement(namespace = "http://greylegacy.com/schema/import")
        private String policyType;
        @XmlElement(namespace = "http://greylegacy.com/schema/import")
        private String status;
        @XmlElement(namespace = "http://greylegacy.com/schema/import")
        private String holderFirstName;
        @XmlElement(namespace = "http://greylegacy.com/schema/import")
        private String holderLastName;
        @XmlElement(namespace = "http://greylegacy.com/schema/import")
        private String holderEmail;
        @XmlElement(namespace = "http://greylegacy.com/schema/import")
        private String holderPhone;
        @XmlElement(namespace = "http://greylegacy.com/schema/import")
        private Date effectiveDate;
        @XmlElement(namespace = "http://greylegacy.com/schema/import")
        private Date expirationDate;
        @XmlElement(namespace = "http://greylegacy.com/schema/import")
        private BigDecimal premiumAmount;
        @XmlElement(namespace = "http://greylegacy.com/schema/import")
        private BigDecimal coverageLimit;
        @XmlElement(namespace = "http://greylegacy.com/schema/import")
        private BigDecimal deductible;
        @XmlElement(namespace = "http://greylegacy.com/schema/import")
        private String agentCode;

        public String getPolicyNumber() { return policyNumber; }
        public void setPolicyNumber(String policyNumber) { this.policyNumber = policyNumber; }
        public String getPolicyType() { return policyType; }
        public void setPolicyType(String policyType) { this.policyType = policyType; }
        public String getStatus() { return status; }
        public void setStatus(String status) { this.status = status; }
        public String getHolderFirstName() { return holderFirstName; }
        public void setHolderFirstName(String holderFirstName) { this.holderFirstName = holderFirstName; }
        public String getHolderLastName() { return holderLastName; }
        public void setHolderLastName(String holderLastName) { this.holderLastName = holderLastName; }
        public String getHolderEmail() { return holderEmail; }
        public void setHolderEmail(String holderEmail) { this.holderEmail = holderEmail; }
        public String getHolderPhone() { return holderPhone; }
        public void setHolderPhone(String holderPhone) { this.holderPhone = holderPhone; }
        public Date getEffectiveDate() { return effectiveDate; }
        public void setEffectiveDate(Date effectiveDate) { this.effectiveDate = effectiveDate; }
        public Date getExpirationDate() { return expirationDate; }
        public void setExpirationDate(Date expirationDate) { this.expirationDate = expirationDate; }
        public BigDecimal getPremiumAmount() { return premiumAmount; }
        public void setPremiumAmount(BigDecimal premiumAmount) { this.premiumAmount = premiumAmount; }
        public BigDecimal getCoverageLimit() { return coverageLimit; }
        public void setCoverageLimit(BigDecimal coverageLimit) { this.coverageLimit = coverageLimit; }
        public BigDecimal getDeductible() { return deductible; }
        public void setDeductible(BigDecimal deductible) { this.deductible = deductible; }
        public String getAgentCode() { return agentCode; }
        public void setAgentCode(String agentCode) { this.agentCode = agentCode; }
    }

    // =========================================================================
    // Import summary
    // =========================================================================

    /**
     * Summary report for an XML policy import operation.
     */
    public static class ImportSummary {
        private int totalEntries;
        private int importedEntries;
        private int updatedEntries;
        private int skippedEntries;
        private List<String> errors = new ArrayList<String>();

        public void incrementTotal() { totalEntries++; }
        public void incrementImported() { importedEntries++; }
        public void incrementUpdated() { updatedEntries++; }
        public void incrementSkipped() { skippedEntries++; }
        public void addError(String message) { errors.add(message); }

        public int getTotalEntries() { return totalEntries; }
        public int getImportedEntries() { return importedEntries; }
        public int getUpdatedEntries() { return updatedEntries; }
        public int getSkippedEntries() { return skippedEntries; }
        public List<String> getErrors() { return errors; }

        @Override
        public String toString() {
            return "ImportSummary{total=" + totalEntries
                    + ", imported=" + importedEntries
                    + ", updated=" + updatedEntries
                    + ", skipped=" + skippedEntries
                    + ", errors=" + errors.size() + "}";
        }
    }
}
