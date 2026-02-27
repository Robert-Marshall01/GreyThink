package com.greylegacy.modernization.controller;

import com.greylegacy.domain.Claim;
import com.greylegacy.domain.ClaimStatus;
import com.greylegacy.modernization.dto.ClaimDto;
import com.greylegacy.modernization.mapper.ClaimMapper;
import com.greylegacy.modernization.metrics.ClaimMetricsService;
import com.greylegacy.service.ClaimService;
import com.greylegacy.service.FnolRequest;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Modern REST controller replacing both the legacy Struts Actions and
 * the pre-Boot Spring MVC controllers.
 *
 * <h3>Migration Comparison:</h3>
 * <pre>
 * LEGACY (Struts Action):
 *   public ActionForward execute(ActionMapping mapping, ActionForm form,
 *       HttpServletRequest request, HttpServletResponse response) {
 *     FnolForm fnolForm = (FnolForm) form;
 *     ClaimService claimService = getClaimService(request); // manual Spring lookup
 *     ...
 *     return mapping.findForward("success");
 *   }
 *
 * LEGACY (pre-Boot Spring MVC):
 *   @Controller
 *   @RequestMapping("/api/claims")
 *   public class ClaimApiController {
 *     @RequestMapping(method = RequestMethod.GET)
 *     @ResponseBody
 *     public void search(HttpServletResponse response) {
 *       ObjectMapper mapper = new ObjectMapper(); // manual JSON
 *       ...
 *     }
 *   }
 *
 * MODERN (Spring Boot):
 *   @RestController // combines @Controller + @ResponseBody
 *   @RequestMapping("/api/v2/claims")
 *   public class ClaimRestController {
 *     @Autowired private ClaimService claimService; // annotation-based DI
 *
 *     @GetMapping("/{claimNumber}")
 *     public ResponseEntity&lt;ClaimDto&gt; getClaim(@PathVariable String claimNumber) {
 *       ...
 *     }
 *   }
 * </pre>
 */
@RestController
@RequestMapping("/api/v2/claims")
public class ClaimRestController {

    private static final Logger log = LoggerFactory.getLogger(ClaimRestController.class);

    @Autowired
    private ClaimService claimService;

    @Autowired
    private ClaimMapper claimMapper;

    @Autowired
    private ClaimMetricsService metricsService;

    /**
     * GET /api/v2/claims/{claimNumber}
     * Replaces: /claimDetail.do?claimNumber=...
     */
    @GetMapping("/{claimNumber}")
    public ResponseEntity<ClaimDto> getClaimByNumber(@PathVariable String claimNumber) {
        log.debug("REST: Looking up claim {}", claimNumber);
        metricsService.recordClaimLookup();

        Claim claim = claimService.findByClaimNumber(claimNumber);
        if (claim == null) {
            return ResponseEntity.notFound().build();
        }

        ClaimDto dto = claimMapper.toDto(claim);
        return ResponseEntity.ok(dto);
    }

    /**
     * GET /api/v2/claims?status=FNOL_RECEIVED
     * Replaces: /executeSearch.do with ClaimSearchForm
     */
    @GetMapping
    public ResponseEntity<List<ClaimDto>> searchClaims(
            @RequestParam(required = false) String status) {

        List<Claim> claims;
        if (status != null && !status.isEmpty()) {
            claims = claimService.findClaimsByStatus(ClaimStatus.valueOf(status));
        } else {
            claims = claimService.findOpenClaims();
        }

        List<ClaimDto> dtos = claimMapper.toDtoList(claims);
        return ResponseEntity.ok(dtos);
    }

    /**
     * POST /api/v2/claims/fnol
     * Replaces: /submitFnol.do with FnolForm → FnolAction
     */
    @PostMapping("/fnol")
    public ResponseEntity<?> submitFnol(@RequestBody FnolRequest request) {
        log.info("REST: FNOL submission for policy {}", request.getPolicyNumber());
        metricsService.recordFnolSubmission();

        try {
            Claim claim = claimService.submitFnol(request);
            ClaimDto dto = claimMapper.toDto(claim);
            return ResponseEntity.status(HttpStatus.CREATED).body(dto);

        } catch (IllegalArgumentException e) {
            return ResponseEntity.badRequest().body(errorResponse("VALIDATION_ERROR", e.getMessage()));
        } catch (IllegalStateException e) {
            return ResponseEntity.unprocessableEntity().body(errorResponse("BUSINESS_RULE_VIOLATION", e.getMessage()));
        } catch (Exception e) {
            log.error("FNOL submission failed", e);
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                    .body(errorResponse("SYSTEM_ERROR", "An unexpected error occurred"));
        }
    }

    /**
     * PUT /api/v2/claims/{id}/approve
     * Replaces: /submitReview.do with AdjusterReviewForm → AdjusterReviewAction
     */
    @PutMapping("/{id}/approve")
    public ResponseEntity<ClaimDto> approveClaim(
            @PathVariable Long id,
            @RequestBody Map<String, Object> body) {
        String approvedBy = (String) body.get("approvedBy");
        java.math.BigDecimal amount = new java.math.BigDecimal(body.get("amount").toString());

        claimService.approveClaim(id, approvedBy, amount);
        metricsService.recordClaimApproval();

        Claim updated = claimService.findByClaimNumber(
                claimService.findByClaimNumber(null) != null ? "" : "");
        return ResponseEntity.ok().build();
    }

    /**
     * GET /api/v2/claims/dashboard
     * Replaces: /dashboard.do → ClaimDashboardAction
     */
    @GetMapping("/dashboard")
    public ResponseEntity<Map<String, Object>> getDashboard() {
        Map<String, Object> dashboard = new HashMap<>();
        dashboard.put("openClaims", claimService.findOpenClaims().size());
        dashboard.put("fnolReceived", claimService.findClaimsByStatus(ClaimStatus.FNOL_RECEIVED).size());
        dashboard.put("underReview", claimService.findClaimsByStatus(ClaimStatus.UNDER_REVIEW).size());
        dashboard.put("approved", claimService.findClaimsByStatus(ClaimStatus.APPROVED).size());
        dashboard.put("fraudSuspected", claimService.findClaimsByStatus(ClaimStatus.FRAUD_SUSPECTED).size());
        return ResponseEntity.ok(dashboard);
    }

    /**
     * POST /api/v2/claims/{id}/notes
     */
    @PostMapping("/{id}/notes")
    public ResponseEntity<Void> addNote(
            @PathVariable Long id,
            @RequestBody Map<String, String> body) {
        claimService.addClaimNote(id,
                body.get("noteType"),
                body.get("subject"),
                body.get("content"),
                body.get("author"));
        return ResponseEntity.status(HttpStatus.CREATED).build();
    }

    private Map<String, String> errorResponse(String code, String message) {
        Map<String, String> error = new HashMap<>();
        error.put("errorCode", code);
        error.put("errorMessage", message);
        return error;
    }
}
