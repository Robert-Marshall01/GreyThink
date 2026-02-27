package com.greylegacy.modernization.metrics;

import io.micrometer.core.instrument.Counter;
import io.micrometer.core.instrument.MeterRegistry;
import io.micrometer.core.instrument.Timer;
import org.springframework.stereotype.Service;

import java.util.concurrent.TimeUnit;

/**
 * Micrometer-based metrics service replacing legacy JMX MBeans.
 *
 * <h3>Migration from JMX MBeans to Micrometer:</h3>
 * <pre>
 * LEGACY (JMX MBean):
 *   public interface ClaimProcessingMBean {
 *     long getTotalClaimsProcessed();
 *     double getAverageProcessingTimeMs();
 *     void resetCounters();
 *   }
 *   // Accessed via JConsole or VisualVM
 *   // Requires MBeanServer registration in applicationContext-jmx.xml
 *
 * MODERN (Micrometer):
 *   Counter.builder("claims.processed.total")
 *     .tag("type", "fnol")
 *     .register(registry);
 *   // Accessed via /actuator/prometheus or /actuator/metrics
 *   // Auto-configured by Spring Boot Actuator
 * </pre>
 *
 * <h3>Observability Stack:</h3>
 * <ul>
 *   <li>Micrometer → Prometheus scrape endpoint</li>
 *   <li>Prometheus → time series storage</li>
 *   <li>Grafana → dashboards and alerting</li>
 * </ul>
 */
@Service
public class ClaimMetricsService {

    private final Counter fnolSubmissions;
    private final Counter claimApprovals;
    private final Counter claimDenials;
    private final Counter claimLookups;
    private final Counter fraudDetections;
    private final Timer claimProcessingTimer;
    private final Timer paymentProcessingTimer;

    public ClaimMetricsService(MeterRegistry registry) {
        this.fnolSubmissions = Counter.builder("claims.fnol.submissions")
                .description("Total FNOL submissions received")
                .tag("module", "claims")
                .register(registry);

        this.claimApprovals = Counter.builder("claims.decisions")
                .description("Claim approval decisions")
                .tag("decision", "approved")
                .tag("module", "claims")
                .register(registry);

        this.claimDenials = Counter.builder("claims.decisions")
                .description("Claim denial decisions")
                .tag("decision", "denied")
                .tag("module", "claims")
                .register(registry);

        this.claimLookups = Counter.builder("claims.lookups")
                .description("Claim detail lookups")
                .tag("module", "claims")
                .register(registry);

        this.fraudDetections = Counter.builder("claims.fraud.detected")
                .description("Claims flagged as fraud suspected")
                .tag("module", "fraud")
                .register(registry);

        this.claimProcessingTimer = Timer.builder("claims.processing.duration")
                .description("Time to process a claim from FNOL to decision")
                .tag("module", "claims")
                .register(registry);

        this.paymentProcessingTimer = Timer.builder("payments.processing.duration")
                .description("Time to process a payment")
                .tag("module", "payments")
                .register(registry);
    }

    public void recordFnolSubmission() {
        fnolSubmissions.increment();
    }

    public void recordClaimApproval() {
        claimApprovals.increment();
    }

    public void recordClaimDenial() {
        claimDenials.increment();
    }

    public void recordClaimLookup() {
        claimLookups.increment();
    }

    public void recordFraudDetection() {
        fraudDetections.increment();
    }

    public void recordClaimProcessingTime(long durationMs) {
        claimProcessingTimer.record(durationMs, TimeUnit.MILLISECONDS);
    }

    public void recordPaymentProcessingTime(long durationMs) {
        paymentProcessingTimer.record(durationMs, TimeUnit.MILLISECONDS);
    }
}
