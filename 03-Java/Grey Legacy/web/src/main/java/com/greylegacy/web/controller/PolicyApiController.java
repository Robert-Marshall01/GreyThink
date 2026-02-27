package com.greylegacy.web.controller;

import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.greylegacy.dao.PolicyDao;
import com.greylegacy.domain.Claim;
import com.greylegacy.domain.Policy;
import com.greylegacy.service.UnderwritingService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

/**
 * Early Spring MVC REST controller for policy data.
 * <p>
 * Mirrors the manual-JSON-serialization style used throughout the Grey Legacy
 * web tier – pre-{@code @RestController} era.
 * </p>
 *
 * @since 2.0.0
 */
@Controller
@RequestMapping("/api/policies")
public class PolicyApiController {

    private static final Logger LOG = LoggerFactory.getLogger(PolicyApiController.class);
    private static final String APPLICATION_JSON = "application/json";
    private static final String UTF_8 = "UTF-8";

    @Autowired
    private UnderwritingService underwritingService;

    @Autowired
    private PolicyDao policyDao;

    private final ObjectMapper objectMapper = new ObjectMapper();

    // ── GET /api/policies/{policyNumber} ────────────────────────────────

    @RequestMapping(value = "/{policyNumber}", method = RequestMethod.GET)
    public void lookupPolicy(@PathVariable("policyNumber") String policyNumber,
                             HttpServletResponse response) {
        LOG.info("lookupPolicy invoked — policyNumber={}", policyNumber);
        try {
            Policy policy = policyDao.findByPolicyNumber(policyNumber);
            if (policy == null) {
                LOG.warn("Policy not found: {}", policyNumber);
                writeErrorResponse(response, HttpServletResponse.SC_NOT_FOUND,
                        "Policy not found: " + policyNumber);
                return;
            }
            writeJsonResponse(response, HttpServletResponse.SC_OK, policy);
        } catch (Exception e) {
            LOG.error("Error looking up policy " + policyNumber, e);
            writeErrorResponse(response, HttpServletResponse.SC_INTERNAL_SERVER_ERROR,
                    "Error looking up policy: " + e.getMessage());
        }
    }

    // ── GET /api/policies/{policyNumber}/claims ─────────────────────────

    @RequestMapping(value = "/{policyNumber}/claims", method = RequestMethod.GET)
    public void listClaimsForPolicy(@PathVariable("policyNumber") String policyNumber,
                                    HttpServletResponse response) {
        LOG.info("listClaimsForPolicy invoked — policyNumber={}", policyNumber);
        try {
            List<Claim> claims = policyDao.findClaimsByPolicyNumber(policyNumber);
            LOG.debug("Found {} claim(s) for policy {}", claims.size(), policyNumber);
            writeJsonResponse(response, HttpServletResponse.SC_OK, claims);
        } catch (Exception e) {
            LOG.error("Error listing claims for policy " + policyNumber, e);
            writeErrorResponse(response, HttpServletResponse.SC_INTERNAL_SERVER_ERROR,
                    "Error listing claims: " + e.getMessage());
        }
    }

    // ── GET /api/policies/search?holderLastName=X ───────────────────────

    @RequestMapping(value = "/search", method = RequestMethod.GET)
    public void searchByHolderLastName(@RequestParam("holderLastName") String lastName,
                                       HttpServletResponse response) {
        LOG.info("searchByHolderLastName invoked — lastName={}", lastName);
        try {
            List<Policy> policies = policyDao.findByHolderLastName(lastName);
            LOG.debug("Search returned {} policy(ies)", policies.size());
            writeJsonResponse(response, HttpServletResponse.SC_OK, policies);
        } catch (Exception e) {
            LOG.error("Error searching policies by last name", e);
            writeErrorResponse(response, HttpServletResponse.SC_INTERNAL_SERVER_ERROR,
                    "Error searching policies: " + e.getMessage());
        }
    }

    // ── PUT /api/policies/{policyNumber}/status ─────────────────────────

    @RequestMapping(value = "/{policyNumber}/status", method = RequestMethod.PUT)
    public void updatePolicyStatus(@PathVariable("policyNumber") String policyNumber,
                                   HttpServletRequest request,
                                   HttpServletResponse response) {
        LOG.info("updatePolicyStatus invoked — policyNumber={}", policyNumber);
        try {
            @SuppressWarnings("unchecked")
            Map<String, String> body = objectMapper.readValue(request.getInputStream(), Map.class);
            String newStatus = body.get("status");
            if (newStatus == null || newStatus.trim().isEmpty()) {
                writeErrorResponse(response, HttpServletResponse.SC_BAD_REQUEST, "Status is required");
                return;
            }

            underwritingService.updatePolicyStatus(policyNumber, newStatus);
            LOG.info("Policy {} status updated to {}", policyNumber, newStatus);

            Map<String, String> result = new HashMap<String, String>();
            result.put("policyNumber", policyNumber);
            result.put("status", newStatus);
            writeJsonResponse(response, HttpServletResponse.SC_OK, result);
        } catch (Exception e) {
            LOG.error("Error updating policy status for " + policyNumber, e);
            writeErrorResponse(response, HttpServletResponse.SC_INTERNAL_SERVER_ERROR,
                    "Error updating policy status: " + e.getMessage());
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
