package com.greylegacy.web.controller;

import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.greylegacy.dao.ClaimDao;
import com.greylegacy.domain.Claim;
import com.greylegacy.service.ClaimService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

/**
 * Early Spring MVC REST controller exposing claim data as JSON.
 * <p>
 * This controller predates Spring Boot and uses manual Jackson serialization
 * and direct {@link HttpServletResponse} manipulation – typical of the
 * Spring 3.x / 4.x era before {@code @RestController} and
 * {@code ResponseEntity} became idiomatic.
 * </p>
 *
 * @since 2.0.0
 */
@Controller
@RequestMapping("/api/claims")
public class ClaimApiController {

    private static final Logger LOG = LoggerFactory.getLogger(ClaimApiController.class);
    private static final String APPLICATION_JSON = "application/json";
    private static final String UTF_8 = "UTF-8";

    @Autowired
    private ClaimService claimService;

    @Autowired
    private ClaimDao claimDao;

    private final ObjectMapper objectMapper = new ObjectMapper();

    // ── GET /api/claims/search ──────────────────────────────────────────

    /**
     * Searches claims by status and optionally by adjuster code.
     */
    @RequestMapping(value = "/search", method = RequestMethod.GET)
    public void searchClaims(@RequestParam("status") String status,
                             @RequestParam(value = "adjusterCode", required = false) String adjusterCode,
                             HttpServletResponse response) {
        LOG.info("searchClaims invoked — status={}, adjusterCode={}", status, adjusterCode);
        try {
            List<Claim> results;
            if (adjusterCode != null && !adjusterCode.isEmpty()) {
                results = claimDao.findByStatusAndAdjuster(status, adjusterCode);
            } else {
                results = claimDao.findByStatus(status);
            }

            LOG.debug("Search returned {} claim(s)", results.size());
            writeJsonResponse(response, HttpServletResponse.SC_OK, results);
        } catch (Exception e) {
            LOG.error("Error searching claims", e);
            writeErrorResponse(response, HttpServletResponse.SC_INTERNAL_SERVER_ERROR,
                    "Error searching claims: " + e.getMessage());
        }
    }

    // ── GET /api/claims/{claimNumber} ───────────────────────────────────

    /**
     * Returns the full detail of a single claim.
     */
    @RequestMapping(value = "/{claimNumber}", method = RequestMethod.GET)
    public void getClaimDetail(@PathVariable("claimNumber") String claimNumber,
                               HttpServletResponse response) {
        LOG.info("getClaimDetail invoked — claimNumber={}", claimNumber);
        try {
            Claim claim = claimDao.findByClaimNumber(claimNumber);
            if (claim == null) {
                LOG.warn("Claim not found: {}", claimNumber);
                writeErrorResponse(response, HttpServletResponse.SC_NOT_FOUND,
                        "Claim not found: " + claimNumber);
                return;
            }
            writeJsonResponse(response, HttpServletResponse.SC_OK, claim);
        } catch (Exception e) {
            LOG.error("Error retrieving claim " + claimNumber, e);
            writeErrorResponse(response, HttpServletResponse.SC_INTERNAL_SERVER_ERROR,
                    "Error retrieving claim: " + e.getMessage());
        }
    }

    // ── GET /api/claims/dashboard/summary ───────────────────────────────

    /**
     * Returns a summary of claim counts grouped by status for the dashboard.
     */
    @RequestMapping(value = "/dashboard/summary", method = RequestMethod.GET)
    public void getDashboardSummary(HttpServletResponse response) {
        LOG.info("getDashboardSummary invoked");
        try {
            Map<String, Long> summary = new HashMap<String, Long>();
            summary.put("OPEN", claimDao.countByStatus("OPEN"));
            summary.put("PENDING", claimDao.countByStatus("PENDING"));
            summary.put("APPROVED", claimDao.countByStatus("APPROVED"));
            summary.put("DENIED", claimDao.countByStatus("DENIED"));
            summary.put("CLOSED", claimDao.countByStatus("CLOSED"));
            summary.put("UNDER_REVIEW", claimDao.countByStatus("UNDER_REVIEW"));

            writeJsonResponse(response, HttpServletResponse.SC_OK, summary);
        } catch (Exception e) {
            LOG.error("Error building dashboard summary", e);
            writeErrorResponse(response, HttpServletResponse.SC_INTERNAL_SERVER_ERROR,
                    "Error building dashboard summary: " + e.getMessage());
        }
    }

    // ── POST /api/claims/{claimNumber}/notes ────────────────────────────

    /**
     * Adds a note to an existing claim.
     */
    @RequestMapping(value = "/{claimNumber}/notes", method = RequestMethod.POST)
    public void addNote(@PathVariable("claimNumber") String claimNumber,
                        HttpServletRequest request,
                        HttpServletResponse response) {
        LOG.info("addNote invoked — claimNumber={}", claimNumber);
        try {
            Claim claim = claimDao.findByClaimNumber(claimNumber);
            if (claim == null) {
                LOG.warn("Cannot add note — claim not found: {}", claimNumber);
                writeErrorResponse(response, HttpServletResponse.SC_NOT_FOUND,
                        "Claim not found: " + claimNumber);
                return;
            }

            @SuppressWarnings("unchecked")
            Map<String, String> body = objectMapper.readValue(request.getInputStream(), Map.class);
            String noteText = body.get("note");
            if (noteText == null || noteText.trim().isEmpty()) {
                writeErrorResponse(response, HttpServletResponse.SC_BAD_REQUEST, "Note text is required");
                return;
            }

            claimService.addClaimNote(claimNumber, noteText);
            LOG.info("Note added to claim {}", claimNumber);

            Map<String, String> result = new HashMap<String, String>();
            result.put("status", "created");
            result.put("claimNumber", claimNumber);
            writeJsonResponse(response, HttpServletResponse.SC_CREATED, result);
        } catch (Exception e) {
            LOG.error("Error adding note to claim " + claimNumber, e);
            writeErrorResponse(response, HttpServletResponse.SC_INTERNAL_SERVER_ERROR,
                    "Error adding note: " + e.getMessage());
        }
    }

    // ── Helpers ─────────────────────────────────────────────────────────

    private void writeJsonResponse(HttpServletResponse response, int status, Object body) {
        response.setContentType(APPLICATION_JSON);
        response.setCharacterEncoding(UTF_8);
        response.setStatus(status);
        try {
            objectMapper.writeValue(response.getWriter(), body);
        } catch (IOException e) {
            LOG.error("Failed to write JSON response", e);
        }
    }

    private void writeErrorResponse(HttpServletResponse response, int status, String message) {
        Map<String, Object> error = new HashMap<String, Object>();
        error.put("error", true);
        error.put("status", status);
        error.put("message", message);
        writeJsonResponse(response, status, error);
    }
}
