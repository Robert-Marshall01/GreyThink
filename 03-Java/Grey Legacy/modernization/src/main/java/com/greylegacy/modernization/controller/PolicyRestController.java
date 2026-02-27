package com.greylegacy.modernization.controller;

import com.greylegacy.domain.Policy;
import com.greylegacy.modernization.dto.PolicyDto;
import com.greylegacy.modernization.mapper.PolicyMapper;
import com.greylegacy.service.UnderwritingService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.math.BigDecimal;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Modern REST controller for policy operations.
 * Replaces legacy PolicyApiController (pre-Boot Spring MVC with manual Jackson serialization).
 */
@RestController
@RequestMapping("/api/v2/policies")
public class PolicyRestController {

    private static final Logger log = LoggerFactory.getLogger(PolicyRestController.class);

    @Autowired
    private UnderwritingService underwritingService;

    @Autowired
    private PolicyMapper policyMapper;

    @GetMapping("/{policyNumber}")
    public ResponseEntity<PolicyDto> getPolicy(@PathVariable String policyNumber) {
        Policy policy = underwritingService.findByPolicyNumber(policyNumber);
        if (policy == null) {
            return ResponseEntity.notFound().build();
        }
        return ResponseEntity.ok(policyMapper.toDto(policy));
    }

    @GetMapping("/review")
    public ResponseEntity<List<PolicyDto>> getPoliciesForReview() {
        List<Policy> policies = underwritingService.findPoliciesForReview();
        return ResponseEntity.ok(policyMapper.toDtoList(policies));
    }

    @PutMapping("/{policyNumber}/cancel")
    public ResponseEntity<Void> cancelPolicy(
            @PathVariable String policyNumber,
            @RequestBody Map<String, String> body) {
        underwritingService.cancelPolicy(policyNumber,
                body.get("reason"), body.get("cancelledBy"));
        return ResponseEntity.noContent().build();
    }

    @PutMapping("/{policyNumber}/suspend")
    public ResponseEntity<Void> suspendPolicy(
            @PathVariable String policyNumber,
            @RequestBody Map<String, String> body) {
        underwritingService.suspendPolicy(policyNumber,
                body.get("reason"), body.get("suspendedBy"));
        return ResponseEntity.noContent().build();
    }

    @PostMapping("/{policyNumber}/renew")
    public ResponseEntity<PolicyDto> renewPolicy(@PathVariable String policyNumber) {
        Policy renewed = underwritingService.renewPolicy(policyNumber);
        return ResponseEntity.ok(policyMapper.toDto(renewed));
    }

    @GetMapping("/{policyNumber}/premium/recalculate")
    public ResponseEntity<Map<String, Object>> recalculatePremium(
            @PathVariable String policyNumber) {
        BigDecimal newPremium = underwritingService.recalculatePremium(policyNumber);
        Map<String, Object> result = new HashMap<>();
        result.put("policyNumber", policyNumber);
        result.put("newPremium", newPremium);
        return ResponseEntity.ok(result);
    }
}
