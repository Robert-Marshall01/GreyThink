package com.greylegacy.service.ejb;

import com.greylegacy.domain.Claim;
import com.greylegacy.domain.ClaimStatus;
import com.greylegacy.domain.Policy;
import com.greylegacy.service.ClaimService;
import com.greylegacy.service.UnderwritingService;
import com.greylegacy.service.dto.AdjudicationResult;
import com.greylegacy.service.dto.FnolRequest;
import com.greylegacy.service.dto.PaymentRequest;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import javax.annotation.PostConstruct;
import javax.annotation.PreDestroy;
import java.math.BigDecimal;
import java.util.Date;
import java.util.List;

/**
 * EJB-Style Session Bean Facade for Claim Processing.
 * <p>
 * This class emulates the Stateless Session Bean (SLSB) pattern from
 * EJB 2.x/3.x using Spring's annotation-based container management.
 * In legacy Java EE applications, the session bean facade was the primary
 * entry point for all business logic, providing:
 * <ul>
 *   <li><b>Container-Managed Transactions (CMT)</b> — the container
 *       (Spring in this case) demarcates transaction boundaries around
 *       each business method. Equivalent to EJB's
 *       {@code @TransactionAttribute(REQUIRED)}.</li>
 *   <li><b>Facade pattern</b> — clients interact with a single coarse-grained
 *       interface rather than multiple fine-grained services. This reduces
 *       network round-trips when the facade is accessed via RMI/IIOP.</li>
 *   <li><b>Lifecycle callbacks</b> — {@code @PostConstruct} and
 *       {@code @PreDestroy} mirror EJB's lifecycle callbacks
 *       ({@code ejbCreate()} / {@code ejbRemove()} in EJB 2.x).</li>
 *   <li><b>Thread safety contract</b> — like SLSB, this bean is stateless
 *       and safe for concurrent access by multiple clients.</li>
 * </ul>
 * <p>
 * <b>Historical context:</b> In EJB 2.x, this facade would be defined
 * as a Session Bean in {@code ejb-jar.xml}, with a Home interface, a
 * Remote/Local interface, and the implementation class. EJB 3.0+ simplified
 * this with annotations ({@code @Stateless}), and Spring replaces the
 * EJB container entirely with {@code @Service} + {@code @Transactional}.
 * <p>
 * <b>EJB 2.x equivalent:</b>
 * <pre>
 *   &lt;session&gt;
 *     &lt;ejb-name&gt;ClaimServiceBean&lt;/ejb-name&gt;
 *     &lt;home&gt;com.greylegacy.ejb.ClaimServiceHome&lt;/home&gt;
 *     &lt;remote&gt;com.greylegacy.ejb.ClaimServiceRemote&lt;/remote&gt;
 *     &lt;ejb-class&gt;com.greylegacy.ejb.ClaimServiceBean&lt;/ejb-class&gt;
 *     &lt;session-type&gt;Stateless&lt;/session-type&gt;
 *     &lt;transaction-type&gt;Container&lt;/transaction-type&gt;
 *   &lt;/session&gt;
 * </pre>
 * <p>
 * <b>EJB 3.x equivalent:</b>
 * <pre>
 *   &#64;Stateless
 *   &#64;TransactionAttribute(TransactionAttributeType.REQUIRED)
 *   public class ClaimServiceBean implements ClaimServiceRemote { ... }
 * </pre>
 *
 * @see com.greylegacy.service.ClaimService
 * @see com.greylegacy.service.UnderwritingService
 */
@Service("claimServiceSessionBean")
@Transactional(propagation = Propagation.REQUIRED, rollbackFor = Exception.class)
public class ClaimServiceSessionBean {

    private static final Logger log = LoggerFactory.getLogger(ClaimServiceSessionBean.class);

    // --- Injected Dependencies (equivalent to EJB @EJB injection) ---
    @Autowired
    private ClaimService claimService;

    @Autowired
    private UnderwritingService underwritingService;

    // --- Lifecycle Callbacks (mirror EJB lifecycle) ---

    /**
     * Equivalent to EJB 2.x {@code ejbCreate()} or EJB 3.x {@code @PostConstruct}.
     * Invoked by the container after dependency injection is complete.
     */
    @PostConstruct
    public void init() {
        log.info("ClaimServiceSessionBean initialized — facade ready for business method invocations");
    }

    /**
     * Equivalent to EJB 2.x {@code ejbRemove()} or EJB 3.x {@code @PreDestroy}.
     * Invoked by the container before the bean is destroyed.
     */
    @PreDestroy
    public void destroy() {
        log.info("ClaimServiceSessionBean destroyed — releasing resources");
    }

    // ========================================================================
    // Coarse-Grained Business Methods (Session Facade Pattern)
    // ========================================================================

    /**
     * Submit a First Notice of Loss — the primary entry point for new claims.
     * <p>
     * In EJB terms, this is a business method on the Remote interface.
     * Transaction propagation is REQUIRED (EJB default): if a transaction
     * exists, join it; otherwise start a new one.
     * <p>
     * This facade method orchestrates multiple service calls within a
     * single transaction boundary:
     * <ol>
     *   <li>Validate coverage via underwriting service</li>
     *   <li>Submit the FNOL via claim service</li>
     *   <li>Log the operation for auditing</li>
     * </ol>
     *
     * @param request the FNOL submission request
     * @return the created Claim entity
     * @throws IllegalArgumentException if the request is invalid
     * @throws RuntimeException triggers transaction rollback
     */
    public Claim submitFirstNoticeOfLoss(FnolRequest request) {
        log.info("Session Facade: submitFirstNoticeOfLoss for policy {}",
                request.getPolicyNumber());

        // Validate coverage first — if this throws, entire tx rolls back
        boolean hasCoverage = underwritingService.validateCoverage(
                request.getPolicyNumber(), request.getClaimType());

        if (!hasCoverage) {
            throw new IllegalStateException(
                    "Coverage validation failed for policy " + request.getPolicyNumber());
        }

        // Submit the FNOL — delegates to the fine-grained service
        Claim claim = claimService.submitFnol(request);

        log.info("Session Facade: FNOL submitted successfully, claim {} created",
                claim.getClaimNumber());
        return claim;
    }

    /**
     * Complete claim adjudication — approve or deny a claim.
     * <p>
     * Transaction propagation: REQUIRED.
     * If the adjudication approves the claim, a payment is also scheduled
     * within the same transaction.
     */
    public AdjudicationResult adjudicateClaim(Long claimId, String adjusterCode,
                                               BigDecimal approvedAmount, String decision) {
        log.info("Session Facade: adjudicateClaim {} with decision {}",
                claimId, decision);

        AdjudicationResult result = claimService.adjudicateClaim(claimId);

        // If approved, schedule initial payment within the same tx
        if ("APPROVE".equalsIgnoreCase(decision) && approvedAmount != null) {
            PaymentRequest paymentRequest = new PaymentRequest();
            paymentRequest.setClaimId(claimId);
            paymentRequest.setAmount(approvedAmount);
            paymentRequest.setPayeeName(result.getAdjusterCode());

            claimService.processPayment(paymentRequest);
            log.info("Session Facade: Payment scheduled for claim {} amount {}",
                    claimId, approvedAmount);
        }

        return result;
    }

    /**
     * Bulk operation: process all claims in a given status.
     * <p>
     * Transaction propagation: REQUIRES_NEW — each claim is processed
     * in its own transaction, so one failure doesn't roll back all.
     * This mirrors EJB's {@code TransactionAttributeType.REQUIRES_NEW}.
     */
    @Transactional(propagation = Propagation.REQUIRES_NEW)
    public int processClaimsBatch(ClaimStatus targetStatus) {
        log.info("Session Facade: processClaimsBatch for status {}", targetStatus);

        List<Claim> claims = claimService.findClaimsByStatus(targetStatus);
        int processed = 0;

        for (Claim claim : claims) {
            try {
                claimService.adjudicateClaim(claim.getId());
                processed++;
            } catch (Exception e) {
                log.error("Session Facade: Failed to process claim {}",
                        claim.getClaimNumber(), e);
                // Continue processing other claims
            }
        }

        log.info("Session Facade: Batch processed {}/{} claims", processed, claims.size());
        return processed;
    }

    /**
     * Read-only business method — transaction propagation SUPPORTS.
     * Equivalent to EJB {@code TransactionAttributeType.SUPPORTS}.
     * If a transaction exists, joins it; otherwise runs without one.
     */
    @Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
    public Claim findClaimByNumber(String claimNumber) {
        log.debug("Session Facade: findClaimByNumber({})", claimNumber);
        return claimService.findByClaimNumber(claimNumber);
    }

    /**
     * Non-transactional operation — propagation NOT_SUPPORTED.
     * Equivalent to EJB {@code TransactionAttributeType.NOT_SUPPORTED}.
     * Suspends any existing transaction.
     */
    @Transactional(propagation = Propagation.NOT_SUPPORTED)
    public List<Claim> listOpenClaims() {
        log.debug("Session Facade: listOpenClaims (non-transactional)");
        return claimService.findOpenClaims();
    }
}
