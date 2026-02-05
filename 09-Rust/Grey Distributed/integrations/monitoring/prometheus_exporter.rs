//! Prometheus Metrics Exporter for Grey Distributed//! Prometheus Metrics Exporter for Grey Distributed


































































































































































































































































































































































































































































































































































































































































































































































































































































































}    }        assert!(output.contains("grey_task_duration_seconds_count"));        let output = metrics.render();                timer.observe_duration();        std::thread::sleep(std::time::Duration::from_millis(10));                    .start_timer();            .with_label_values(&["tenant-1", "stateless"])        let timer = metrics.task_duration_seconds                let metrics = GreyMetrics::new();    fn test_histogram_timer() {    #[test]        }        assert!(output.contains("grey_task_duration_seconds"));        assert!(output.contains("grey_tasks_submitted_total"));        let output = metrics.render();        // Render should not panic and contain data                    .observe(0.5);            .with_label_values(&["tenant-1", "stateless"])        metrics.task_duration_seconds                    .inc();            .with_label_values(&["tenant-1", "stateless", "normal"])        metrics.tasks_submitted        // Record some metrics                let metrics = GreyMetrics::new();    fn test_metrics_creation() {    #[test]        use super::*;mod tests {#[cfg(test)]// ============================================================================// Tests// ============================================================================}"#.to_string()          description: "Node {{ $labels.node_id }} at {{ $value | printf \"%.0f\" }}% memory"          summary: "Node memory usage is high"        annotations:          severity: warning        labels:        for: 10m        expr: grey_node_memory_usage_ratio > 0.85      - alert: GreyNodeHighMemory          description: "Node {{ $labels.node_id }} at {{ $value | printf \"%.0f\" }}% CPU"          summary: "Node CPU usage is high"        annotations:          severity: warning        labels:        for: 10m        expr: grey_node_cpu_usage_ratio > 0.9      - alert: GreyNodeHighCPU      # Resource Alerts          description: "{{ $value | printf \"%.0f\" }} failures/sec"          summary: "High authentication failure rate"        annotations:          severity: warning        labels:        for: 2m        expr: rate(grey_auth_failures_total[5m]) > 100      - alert: GreyAuthFailureSpike          description: "{{ $value | printf \"%.1f\" }} failures/sec"          summary: "TEE attestation failures detected"        annotations:          severity: critical        labels:        for: 1m        expr: rate(grey_attestation_failures_total[5m]) > 0      - alert: GreyAttestationFailures      # Security Alerts          description: "{{ $value | printf \"%.1f\" }} requests/sec throttled"          summary: "Tenant is being throttled"        annotations:          severity: info        labels:        for: 5m        expr: rate(grey_tenant_throttled_requests_total[5m]) > 10      - alert: GreyTenantThrottled          description: "Tenant {{ $labels.tenant_id }} at {{ $value | printf \"%.0f\" }}% quota"          summary: "Tenant quota nearing exhaustion"        annotations:          severity: warning        labels:        for: 5m        expr: grey_tenant_quota_usage / grey_tenant_quota_limit > 0.9      - alert: GreyTenantQuotaExhausted      # Tenant Alerts          description: "P99: {{ $value | printf \"%.3f\" }}s"          summary: "Consensus P99 latency is high"        annotations:          severity: warning        labels:        for: 5m        expr: histogram_quantile(0.99, rate(grey_consensus_latency_seconds_bucket[5m])) > 0.5      - alert: GreyConsensusLatency          description: "Error rate: {{ $value | printf \"%.2f\" }}%"          summary: "RPC error rate exceeds 1%"        annotations:          severity: warning        labels:        for: 5m        expr: rate(grey_rpc_errors_total[5m]) / rate(grey_rpc_requests_total[5m]) > 0.01      - alert: GreyHighRPCErrorRate      # Network Alerts          description: "Lag: {{ $value | printf \"%.1f\" }}s"          summary: "Storage replication lag is high"        annotations:          severity: warning        labels:        for: 5m        expr: grey_storage_replication_lag_seconds > 30      - alert: GreyReplicationLag          description: "P95 latency: {{ $value | printf \"%.2f\" }}s"          summary: "Storage P95 latency exceeds 1s"        annotations:          severity: warning        labels:        for: 5m        expr: histogram_quantile(0.95, rate(grey_storage_latency_seconds_bucket[5m])) > 1      - alert: GreyStorageHighLatency      # Storage Alerts          description: "{{ $value }} leader changes in 10 minutes"          summary: "Frequent Raft leader changes"        annotations:          severity: warning        labels:        for: 1m        expr: increase(grey_cluster_leader_changes_total[10m]) > 3      - alert: GreyFrequentLeaderChanges          description: "{{ $value }} quarantined nodes"          summary: "Nodes are quarantined"        annotations:          severity: warning        labels:        for: 5m        expr: grey_cluster_nodes_quarantined > 0      - alert: GreyQuarantinedNodes          description: "Healthy: {{ $value }} / {{ with query \"grey_cluster_nodes_total\" }}{{ . | first | value }}{{ end }}"          summary: "Less than 90% of nodes are healthy"        annotations:          severity: critical        labels:        for: 2m        expr: grey_cluster_nodes_healthy < grey_cluster_nodes_total * 0.9      - alert: GreyNodeDown      # Cluster Health Alerts          description: "P99 latency: {{ $value | printf \"%.1f\" }}s"          summary: "P99 task execution time exceeds 60s"        annotations:          severity: warning        labels:        for: 5m        expr: histogram_quantile(0.99, rate(grey_task_duration_seconds_bucket[5m])) > 60      - alert: GreySlowTaskExecution          description: "Queue depth: {{ $value }}"          summary: "Task queue backlog is high"        annotations:          severity: warning        labels:        for: 10m        expr: grey_task_queue_depth > 10000      - alert: GreyTaskQueueBacklog          description: "Task failure rate is {{ $value | printf \"%.2f\" }}%"          summary: "High task failure rate (>5%)"        annotations:          severity: warning        labels:        for: 5m          > 0.05          / rate(grey_tasks_completed_total[5m] + grey_tasks_failed_total[5m])          rate(grey_tasks_failed_total[5m])        expr: |      - alert: GreyHighTaskFailureRate      # Task Processing Alerts    rules:  - name: grey_alertsgroups:    r#"pub fn generate_alert_rules() -> String {/// Generate Prometheus alerting rules for Grey.// ============================================================================// Alert Rules (for Prometheus/Alertmanager)// ============================================================================}    }        0.0        // Placeholder - use sysinfo or procfs crate    fn get_memory_usage(&self) -> f64 {        }        0.0        // Placeholder - use sysinfo or procfs crate    fn get_cpu_usage(&self) -> f64 {        }            .set(memory_usage);            .with_label_values(&[&self.node_id, &self.role])        self.metrics.node_memory_usage        let memory_usage = self.get_memory_usage();        // Memory usage                    .set(cpu_usage);            .with_label_values(&[&self.node_id, &self.role])        self.metrics.node_cpu_usage        let cpu_usage = self.get_cpu_usage();        // CPU usage (simplified - use sysinfo crate in production)    async fn collect(&self) {        }        }            tokio::time::sleep(interval).await;            self.collect().await;        loop {    pub async fn collect_loop(self, interval: Duration) {    /// Start collecting system metrics every `interval`.        }        Self { metrics, node_id, role }    pub fn new(metrics: Arc<GreyMetrics>, node_id: String, role: String) -> Self {impl SystemMetricsCollector {}    role: String,    node_id: String,    metrics: Arc<GreyMetrics>,pub struct SystemMetricsCollector {/// Collects system-level metrics periodically.// ============================================================================// Collector for System Metrics// ============================================================================}    }        HistogramTimer::new(self.clone())    fn start_timer(&self) -> HistogramTimer {impl TimedHistogram for Histogram {}    fn start_timer(&self) -> HistogramTimer;pub trait TimedHistogram {/// Convenience trait for timing operations.}    }        self.histogram.observe(duration.as_secs_f64());        let duration = self.start.elapsed();    pub fn observe_duration(self) {        }        }            start: Instant::now(),            histogram,        Self {    pub fn new(histogram: Histogram) -> Self {impl HistogramTimer {}    start: Instant,    histogram: Histogram,pub struct HistogramTimer {/// Timer for observing duration into a histogram.// ============================================================================// Metric Helpers// ============================================================================}    }        Ok(())                axum::serve(listener, app).await?;        let listener = tokio::net::TcpListener::bind(self.addr).await?;                info!("Prometheus exporter listening on {}", self.addr);                    .route("/health", get(|| async { "OK" }));            }))                async move { m.render() }                let m = metrics.clone();            .route("/metrics", get(move || {        let app = Router::new()                let metrics = self.metrics.clone();    pub async fn serve(self) -> Result<(), Box<dyn std::error::Error>> {    /// Start the metrics HTTP server.        }        Self { metrics, addr }    pub fn new(metrics: Arc<GreyMetrics>, addr: SocketAddr) -> Self {impl PrometheusExporter {}    addr: SocketAddr,    metrics: Arc<GreyMetrics>,pub struct PrometheusExporter {/// Prometheus metrics HTTP exporter.// ============================================================================// HTTP Server// ============================================================================}    }        String::from_utf8(buffer).unwrap()        encoder.encode(&metric_families, &mut buffer).unwrap();        let mut buffer = Vec::new();        let metric_families = self.registry.gather();        let encoder = TextEncoder::new();    pub fn render(&self) -> String {    /// Render all metrics in Prometheus text format.        }        }            auth_failures,            auth_attempts,            attestation_failures,            attestation_requests,            tenant_rate_limit_remaining,            tenant_throttled_requests,            tenant_quota_limit,            tenant_quota_usage,            scheduler_rejections,            scheduler_assignments,            scheduler_queue_wait_seconds,            scheduler_decisions,            consensus_latency_seconds,            consensus_messages,            rpc_errors,            rpc_latency_seconds,            rpc_requests,            storage_replication_lag_seconds,            storage_bytes_read,            storage_bytes_written,            storage_latency_seconds,            storage_operations,            node_task_capacity,            node_tasks_running,            node_memory_usage,            node_cpu_usage,            cluster_raft_term,            cluster_leader_changes,            cluster_nodes_quarantined,            cluster_nodes_draining,            cluster_nodes_healthy,            cluster_nodes_total,            task_retries,            task_queue_depth,            task_duration_seconds,            tasks_in_progress,            tasks_cancelled,            tasks_failed,            tasks_completed,            tasks_submitted,            registry,        Self {                registry.register(Box::new(auth_failures.clone())).unwrap();        ).unwrap();            &["method", "reason"],                .subsystem("security"),                .namespace("grey")            Opts::new("grey_auth_failures_total", "Authentication failures")        let auth_failures = IntCounterVec::new(                registry.register(Box::new(auth_attempts.clone())).unwrap();        ).unwrap();            &["method"],                .subsystem("security"),                .namespace("grey")            Opts::new("grey_auth_attempts_total", "Authentication attempts")        let auth_attempts = IntCounterVec::new(                registry.register(Box::new(attestation_failures.clone())).unwrap();        ).unwrap();            &["platform", "reason"],                .subsystem("security"),                .namespace("grey")            Opts::new("grey_attestation_failures_total", "TEE attestation failures")        let attestation_failures = IntCounterVec::new(                registry.register(Box::new(attestation_requests.clone())).unwrap();        ).unwrap();            &["platform"],                .subsystem("security"),                .namespace("grey")            Opts::new("grey_attestation_requests_total", "TEE attestation requests")        let attestation_requests = IntCounterVec::new(                // ================================================================        // Security Metrics        // ================================================================                registry.register(Box::new(tenant_rate_limit_remaining.clone())).unwrap();        ).unwrap();            &["tenant_id"],                .subsystem("tenant"),                .namespace("grey")            Opts::new("grey_tenant_rate_limit_remaining", "Remaining rate limit tokens")        let tenant_rate_limit_remaining = IntGaugeVec::new(                registry.register(Box::new(tenant_throttled_requests.clone())).unwrap();        ).unwrap();            &["tenant_id"],                .subsystem("tenant"),                .namespace("grey")            Opts::new("grey_tenant_throttled_requests_total", "Throttled requests")        let tenant_throttled_requests = IntCounterVec::new(                registry.register(Box::new(tenant_quota_limit.clone())).unwrap();        ).unwrap();            &["tenant_id", "resource"],                .subsystem("tenant"),                .namespace("grey")            Opts::new("grey_tenant_quota_limit", "Quota limit")        let tenant_quota_limit = GaugeVec::new(                registry.register(Box::new(tenant_quota_usage.clone())).unwrap();        ).unwrap();            &["tenant_id", "resource"],                .subsystem("tenant"),                .namespace("grey")            Opts::new("grey_tenant_quota_usage", "Current quota usage")        let tenant_quota_usage = GaugeVec::new(                // ================================================================        // Tenant Metrics        // ================================================================                registry.register(Box::new(scheduler_rejections.clone())).unwrap();        ).unwrap();            &["reason"],                .subsystem("scheduler"),                .namespace("grey")            Opts::new("grey_scheduler_rejections_total", "Scheduling rejections")        let scheduler_rejections = IntCounterVec::new(                registry.register(Box::new(scheduler_assignments.clone())).unwrap();        ).unwrap();            &["node_id"],                .subsystem("scheduler"),                .namespace("grey")            Opts::new("grey_scheduler_assignments_total", "Task assignments to nodes")        let scheduler_assignments = IntCounterVec::new(                registry.register(Box::new(scheduler_queue_wait_seconds.clone())).unwrap();        ).unwrap();            &["priority"],            .buckets(vec![0.001, 0.01, 0.1, 0.5, 1.0, 5.0, 10.0, 30.0, 60.0, 300.0]),            .subsystem("scheduler")            .namespace("grey")            )                "Time tasks wait in queue before scheduling",                "grey_scheduler_queue_wait_seconds",            HistogramOpts::new(        let scheduler_queue_wait_seconds = HistogramVec::new(                registry.register(Box::new(scheduler_decisions.clone())).unwrap();        ).unwrap();            &["decision"],                .subsystem("scheduler"),                .namespace("grey")            Opts::new("grey_scheduler_decisions_total", "Scheduling decisions made")        let scheduler_decisions = IntCounterVec::new(                // ================================================================        // Scheduler Metrics        // ================================================================                registry.register(Box::new(consensus_latency_seconds.clone())).unwrap();        ).unwrap();            &["operation"],            ]),                0.001, 0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 1.0, 2.5, 5.0            .buckets(vec![            .subsystem("network")            .namespace("grey")            )                "Consensus operation latency",                "grey_consensus_latency_seconds",            HistogramOpts::new(        let consensus_latency_seconds = HistogramVec::new(                registry.register(Box::new(consensus_messages.clone())).unwrap();        ).unwrap();            &["message_type", "direction"],                .subsystem("network"),                .namespace("grey")            Opts::new("grey_consensus_messages_total", "Consensus protocol messages")        let consensus_messages = IntCounterVec::new(                registry.register(Box::new(rpc_errors.clone())).unwrap();        ).unwrap();            &["method", "error_type"],                .subsystem("network"),                .namespace("grey")            Opts::new("grey_rpc_errors_total", "Total RPC errors")        let rpc_errors = IntCounterVec::new(                registry.register(Box::new(rpc_latency_seconds.clone())).unwrap();        ).unwrap();            &["method"],            ]),                1.0, 2.5, 5.0, 10.0                0.0001, 0.0005, 0.001, 0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5,            .buckets(vec![            .subsystem("network")            .namespace("grey")            )                "RPC request latency",                "grey_rpc_latency_seconds",            HistogramOpts::new(        let rpc_latency_seconds = HistogramVec::new(                registry.register(Box::new(rpc_requests.clone())).unwrap();        ).unwrap();            &["method", "status"],                .subsystem("network"),                .namespace("grey")            Opts::new("grey_rpc_requests_total", "Total RPC requests")        let rpc_requests = IntCounterVec::new(                // ================================================================        // Network Metrics        // ================================================================                registry.register(Box::new(storage_replication_lag_seconds.clone())).unwrap();        ).unwrap();            &["replica_id"],                .subsystem("storage"),                .namespace("grey")            Opts::new("grey_storage_replication_lag_seconds", "Replication lag")        let storage_replication_lag_seconds = GaugeVec::new(                registry.register(Box::new(storage_bytes_read.clone())).unwrap();        ).unwrap();            &["backend"],                .subsystem("storage"),                .namespace("grey")            Opts::new("grey_storage_bytes_read_total", "Total bytes read")        let storage_bytes_read = IntCounterVec::new(                registry.register(Box::new(storage_bytes_written.clone())).unwrap();        ).unwrap();            &["backend"],                .subsystem("storage"),                .namespace("grey")            Opts::new("grey_storage_bytes_written_total", "Total bytes written")        let storage_bytes_written = IntCounterVec::new(                registry.register(Box::new(storage_latency_seconds.clone())).unwrap();        ).unwrap();            &["operation", "backend"],            ]),                1.0, 2.5, 5.0, 10.0, 30.0                0.001, 0.0025, 0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5,            .buckets(vec![            .subsystem("storage")            .namespace("grey")            )                "Storage operation latency",                "grey_storage_latency_seconds",            HistogramOpts::new(        let storage_latency_seconds = HistogramVec::new(                registry.register(Box::new(storage_operations.clone())).unwrap();        ).unwrap();            &["operation", "backend"],                .subsystem("storage"),                .namespace("grey")            Opts::new("grey_storage_operations_total", "Total storage operations")        let storage_operations = IntCounterVec::new(                // ================================================================        // Storage Metrics        // ================================================================                registry.register(Box::new(node_task_capacity.clone())).unwrap();        ).unwrap();            &["node_id"],                .subsystem("node"),                .namespace("grey")            Opts::new("grey_node_task_capacity", "Maximum task capacity of node")        let node_task_capacity = IntGaugeVec::new(                registry.register(Box::new(node_tasks_running.clone())).unwrap();        ).unwrap();            &["node_id"],                .subsystem("node"),                .namespace("grey")            Opts::new("grey_node_tasks_running", "Tasks currently running on node")        let node_tasks_running = IntGaugeVec::new(                registry.register(Box::new(node_memory_usage.clone())).unwrap();        ).unwrap();            &["node_id", "role"],                .subsystem("node"),                .namespace("grey")            Opts::new("grey_node_memory_usage_ratio", "Node memory usage 0-1")        let node_memory_usage = GaugeVec::new(                registry.register(Box::new(node_cpu_usage.clone())).unwrap();        ).unwrap();            &["node_id", "role"],                .subsystem("node"),                .namespace("grey")            Opts::new("grey_node_cpu_usage_ratio", "Node CPU usage 0-1")        let node_cpu_usage = GaugeVec::new(                // ================================================================        // Node Metrics        // ================================================================                registry.register(Box::new(cluster_raft_term.clone())).unwrap();        ).unwrap();            "Current Raft term",            "grey_cluster_raft_term",        let cluster_raft_term = IntGauge::new(                registry.register(Box::new(cluster_leader_changes.clone())).unwrap();        ).unwrap();            "Total leadership changes",            "grey_cluster_leader_changes_total",        let cluster_leader_changes = IntCounter::new(                registry.register(Box::new(cluster_nodes_quarantined.clone())).unwrap();        ).unwrap();            "Number of quarantined nodes",            "grey_cluster_nodes_quarantined",        let cluster_nodes_quarantined = IntGauge::new(                registry.register(Box::new(cluster_nodes_draining.clone())).unwrap();        ).unwrap();            "Number of nodes draining",            "grey_cluster_nodes_draining",        let cluster_nodes_draining = IntGauge::new(                registry.register(Box::new(cluster_nodes_healthy.clone())).unwrap();        ).unwrap();            "Number of healthy nodes",            "grey_cluster_nodes_healthy",        let cluster_nodes_healthy = IntGauge::new(                registry.register(Box::new(cluster_nodes_total.clone())).unwrap();        ).unwrap();            "Total number of nodes in cluster",            "grey_cluster_nodes_total",        let cluster_nodes_total = IntGauge::new(                // ================================================================        // Cluster Metrics        // ================================================================                registry.register(Box::new(task_retries.clone())).unwrap();        ).unwrap();            &["tenant_id", "retry_reason"],                .subsystem("tasks"),                .namespace("grey")            Opts::new("grey_task_retries_total", "Total task retry attempts")        let task_retries = IntCounterVec::new(                registry.register(Box::new(task_queue_depth.clone())).unwrap();        ).unwrap();            &["tenant_id", "priority"],                .subsystem("tasks"),                .namespace("grey")            Opts::new("grey_task_queue_depth", "Current depth of task queue")        let task_queue_depth = IntGaugeVec::new(                registry.register(Box::new(task_duration_seconds.clone())).unwrap();        ).unwrap();            &["tenant_id", "task_type"],            ]),                1.0, 2.5, 5.0, 10.0, 30.0, 60.0, 300.0, 600.0, 1800.0, 3600.0                0.001, 0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5,            .buckets(vec![            .subsystem("tasks")            .namespace("grey")            )                "Task execution duration in seconds",                "grey_task_duration_seconds",            HistogramOpts::new(        let task_duration_seconds = HistogramVec::new(        // Task duration histogram with buckets from 1ms to 1 hour                registry.register(Box::new(tasks_in_progress.clone())).unwrap();        ).unwrap();            &["tenant_id", "task_type"],                .subsystem("tasks"),                .namespace("grey")            Opts::new("grey_tasks_in_progress", "Current tasks in progress")        let tasks_in_progress = IntGaugeVec::new(                registry.register(Box::new(tasks_cancelled.clone())).unwrap();        ).unwrap();            &["tenant_id", "reason"],                .subsystem("tasks"),                .namespace("grey")            Opts::new("grey_tasks_cancelled_total", "Total tasks cancelled")        let tasks_cancelled = IntCounterVec::new(                registry.register(Box::new(tasks_failed.clone())).unwrap();        ).unwrap();            &["tenant_id", "task_type", "error_type"],                .subsystem("tasks"),                .namespace("grey")            Opts::new("grey_tasks_failed_total", "Total tasks failed")        let tasks_failed = IntCounterVec::new(                registry.register(Box::new(tasks_completed.clone())).unwrap();        ).unwrap();            &["tenant_id", "task_type"],                .subsystem("tasks"),                .namespace("grey")            Opts::new("grey_tasks_completed_total", "Total tasks completed successfully")        let tasks_completed = IntCounterVec::new(                registry.register(Box::new(tasks_submitted.clone())).unwrap();        ).unwrap();            &["tenant_id", "task_type", "priority"],                .subsystem("tasks"),                .namespace("grey")            Opts::new("grey_tasks_submitted_total", "Total tasks submitted")        let tasks_submitted = IntCounterVec::new(                // ================================================================        // Task Metrics        // ================================================================                let registry = Registry::new();    pub fn new() -> Self {    /// Create a new metrics registry with all Grey metrics.impl GreyMetrics {}    pub auth_failures: IntCounterVec,    pub auth_attempts: IntCounterVec,    pub attestation_failures: IntCounterVec,    pub attestation_requests: IntCounterVec,    // Security metrics        pub tenant_rate_limit_remaining: IntGaugeVec,    pub tenant_throttled_requests: IntCounterVec,    pub tenant_quota_limit: GaugeVec,    pub tenant_quota_usage: GaugeVec,    // Tenant metrics        pub scheduler_rejections: IntCounterVec,    pub scheduler_assignments: IntCounterVec,    pub scheduler_queue_wait_seconds: HistogramVec,    pub scheduler_decisions: IntCounterVec,    // Scheduler metrics        pub consensus_latency_seconds: HistogramVec,    pub consensus_messages: IntCounterVec,    pub rpc_errors: IntCounterVec,    pub rpc_latency_seconds: HistogramVec,    pub rpc_requests: IntCounterVec,    // Network metrics        pub storage_replication_lag_seconds: GaugeVec,    pub storage_bytes_read: IntCounterVec,    pub storage_bytes_written: IntCounterVec,    pub storage_latency_seconds: HistogramVec,    pub storage_operations: IntCounterVec,    // Storage metrics        pub node_task_capacity: IntGaugeVec,    pub node_tasks_running: IntGaugeVec,    pub node_memory_usage: GaugeVec,    pub node_cpu_usage: GaugeVec,    // Node metrics (per-node gauges)        pub cluster_raft_term: IntGauge,    pub cluster_leader_changes: IntCounter,    pub cluster_nodes_quarantined: IntGauge,    pub cluster_nodes_draining: IntGauge,    pub cluster_nodes_healthy: IntGauge,    pub cluster_nodes_total: IntGauge,    // Cluster metrics        pub task_retries: IntCounterVec,    pub task_queue_depth: IntGaugeVec,    pub task_duration_seconds: HistogramVec,    pub tasks_in_progress: IntGaugeVec,    pub tasks_cancelled: IntCounterVec,    pub tasks_failed: IntCounterVec,    pub tasks_completed: IntCounterVec,    pub tasks_submitted: IntCounterVec,    // Task metrics        registry: Registry,pub struct GreyMetrics {/// Central registry for all Grey metrics.// ============================================================================// Metric Registry// ============================================================================use tracing::{info, debug};use tokio::sync::RwLock;};    IntCounter, IntCounterVec, IntGauge, IntGaugeVec, Opts, Registry, TextEncoder, Encoder,    self, Counter, CounterVec, Gauge, GaugeVec, Histogram, HistogramOpts, HistogramVec,use prometheus::{use axum::{routing::get, Router};use std::time::{Duration, Instant};use std::sync::Arc;use std::sync::atomic::{AtomicU64, Ordering};use std::net::SocketAddr;use std::collections::HashMap;//! - Storage latency: 1ms to 30s (database and object storage)//! - Network latency: 100µs to 10s (RPC and consensus)//! - Task latency: 1ms to 1 hour (covers quick tasks to long-running jobs)//! Bucket boundaries are chosen for Grey's expected latency profiles://!//! # Histogram Buckets//!//! - node_id: Yes, for per-node monitoring (bounded by cluster size)//! - task_id: No, would explode cardinality//! - tenant_id: Yes, for isolation monitoring (bounded by tenant count)//! High-cardinality labels (tenant_id, task_id) are used selectively://!//! # Cardinality Considerations//!//! - `grey_tenant_*`: Per-tenant usage metrics//! - `grey_scheduler_*`: Scheduler metrics//! - `grey_network_*`: Network/RPC metrics//! - `grey_storage_*`: Storage layer metrics//! - `grey_cluster_*`: Cluster health metrics//! - `grey_tasks_*`: Task execution metrics//! Metrics are organized by subsystem://!//! # Metrics Design//!//! Exposes Grey cluster metrics in Prometheus format for monitoring and alerting.//!//!
//! This module exposes Grey's internal metrics in Prometheus format for
//! scraping by Prometheus server or compatible collectors.
//!
//! # Metrics Categories
//!
//! 1. **Task Metrics**: Submission rates, completion times, queue depths
//! 2. **Cluster Metrics**: Node health, resource utilization, consensus state
//! 3. **Tenant Metrics**: Per-tenant usage, quota consumption, throttling
//! 4. **Security Metrics**: TEE attestations, authentication events
//!
//! # Metric Types
//!
//! - Counter: Monotonically increasing (e.g., total_tasks_submitted)
//! - Gauge: Point-in-time value (e.g., current_queue_depth)
//! - Histogram: Distribution with buckets (e.g., task_duration_seconds)
//! - Summary: Distribution with quantiles (rarely used, prefer histograms)
//!
//! # Labeling Strategy
//!
//! Labels provide dimensions for filtering/aggregation:
//! - `tenant_id`: Tenant isolation
//! - `task_type`: Task classification
//! - `node_id`: Node attribution
//! - `status`: Outcome classification
//!
//! Cardinality Warning: Avoid high-cardinality labels (e.g., task_id).
//! Each unique label combination creates a new time series.

use std::collections::HashMap;
use std::net::SocketAddr;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;
use std::time::{Duration, Instant};

use prometheus::{
    Counter, CounterVec, Gauge, GaugeVec, Histogram, HistogramOpts, HistogramVec,
    IntCounter, IntCounterVec, IntGauge, IntGaugeVec, Opts, Registry, TextEncoder,
};
use tokio::sync::RwLock;
use tracing::{debug, error, info, instrument};
use warp::Filter;

// ============================================================================
// Metric Definitions
// ============================================================================

/// All Grey Distributed metrics.
pub struct GreyMetrics {
    // Registry
    pub registry: Registry,

    // ========== Task Metrics ==========
    
    /// Total tasks submitted (counter)
    pub tasks_submitted_total: IntCounterVec,
    
    /// Total tasks completed (counter)
    pub tasks_completed_total: IntCounterVec,
    
    /// Total tasks failed (counter)
    pub tasks_failed_total: IntCounterVec,
    
    /// Current tasks in queue (gauge)
    pub tasks_queued: IntGaugeVec,
    
    /// Current tasks running (gauge)
    pub tasks_running: IntGaugeVec,
    
    /// Task duration in seconds (histogram)
    pub task_duration_seconds: HistogramVec,
    
    /// Task queue wait time in seconds (histogram)
    pub task_queue_wait_seconds: HistogramVec,

    // ========== Cluster Metrics ==========
    
    /// Total nodes in cluster (gauge)
    pub cluster_nodes_total: IntGauge,
    
    /// Healthy nodes in cluster (gauge)  
    pub cluster_nodes_healthy: IntGauge,
    
    /// Quarantined nodes (gauge)
    pub cluster_nodes_quarantined: IntGauge,
    
    /// Current Raft term (gauge)
    pub raft_term: IntGauge,
    
    /// Is current node the Raft leader (gauge: 0/1)
    pub raft_is_leader: IntGauge,
    
    /// Raft log entries applied (counter)
    pub raft_entries_applied_total: IntCounter,
    
    /// Cluster-wide CPU utilization (gauge: 0-100)
    pub cluster_cpu_percent: Gauge,
    
    /// Cluster-wide memory utilization (gauge: 0-100)
    pub cluster_memory_percent: Gauge,

    // ========== Tenant Metrics ==========
    
    /// Tasks submitted per tenant (counter)
    pub tenant_tasks_total: IntCounterVec,
    
    /// Quota usage per tenant (gauge: 0-100)
    pub tenant_quota_used_percent: GaugeVec,
    
    /// Throttle events per tenant (counter)
    pub tenant_throttle_events_total: IntCounterVec,
    
    /// Active connections per tenant (gauge)
    pub tenant_connections: IntGaugeVec,

    // ========== Security Metrics ==========
    
    /// TEE attestation attempts (counter)
    pub tee_attestation_total: IntCounterVec,
    
    /// Authentication attempts (counter)
    pub auth_attempts_total: IntCounterVec,
    
    /// Active sessions (gauge)
    pub active_sessions: IntGaugeVec,

    // ========== Storage Metrics ==========
    
    /// Storage write operations (counter)
    pub storage_writes_total: IntCounterVec,
    
    /// Storage read operations (counter)
    pub storage_reads_total: IntCounterVec,
    
    /// Storage operation latency (histogram)
    pub storage_latency_seconds: HistogramVec,
    
    /// Quorum write success rate (gauge)
    pub quorum_success_rate: GaugeVec,

    // ========== Network Metrics ==========
    
    /// Bytes sent (counter)
    pub network_bytes_sent_total: IntCounterVec,
    
    /// Bytes received (counter)
    pub network_bytes_received_total: IntCounterVec,
    
    /// RPC request latency (histogram)
    pub rpc_latency_seconds: HistogramVec,
}

impl GreyMetrics {
    /// Create and register all Grey metrics.
    pub fn new() -> Result<Self, prometheus::Error> {
        let registry = Registry::new();

        // ========== Task Metrics ==========

        let tasks_submitted_total = IntCounterVec::new(
            Opts::new("grey_tasks_submitted_total", "Total number of tasks submitted")
                .namespace("grey"),
            &["tenant_id", "task_type"],
        )?;
        registry.register(Box::new(tasks_submitted_total.clone()))?;

        let tasks_completed_total = IntCounterVec::new(
            Opts::new("grey_tasks_completed_total", "Total number of tasks completed successfully")
                .namespace("grey"),
            &["tenant_id", "task_type"],
        )?;
        registry.register(Box::new(tasks_completed_total.clone()))?;

        let tasks_failed_total = IntCounterVec::new(
            Opts::new("grey_tasks_failed_total", "Total number of failed tasks")
                .namespace("grey"),
            &["tenant_id", "task_type", "error_type"],
        )?;
        registry.register(Box::new(tasks_failed_total.clone()))?;

        let tasks_queued = IntGaugeVec::new(
            Opts::new("grey_tasks_queued", "Current number of tasks in queue")
                .namespace("grey"),
            &["tenant_id", "priority"],
        )?;
        registry.register(Box::new(tasks_queued.clone()))?;

        let tasks_running = IntGaugeVec::new(
            Opts::new("grey_tasks_running", "Current number of running tasks")
                .namespace("grey"),
            &["tenant_id", "node_id"],
        )?;
        registry.register(Box::new(tasks_running.clone()))?;

        // Task duration histogram with buckets: 10ms, 50ms, 100ms, 500ms, 1s, 5s, 30s, 60s, 300s
        let task_duration_seconds = HistogramVec::new(
            HistogramOpts::new(
                "grey_task_duration_seconds",
                "Task execution duration in seconds",
            )
            .namespace("grey")
            .buckets(vec![0.01, 0.05, 0.1, 0.5, 1.0, 5.0, 30.0, 60.0, 300.0]),
            &["tenant_id", "task_type", "status"],
        )?;
        registry.register(Box::new(task_duration_seconds.clone()))?;

        let task_queue_wait_seconds = HistogramVec::new(
            HistogramOpts::new(
                "grey_task_queue_wait_seconds",
                "Time tasks spend waiting in queue",
            )
            .namespace("grey")
            .buckets(vec![0.001, 0.01, 0.1, 0.5, 1.0, 5.0, 30.0]),
            &["tenant_id", "priority"],
        )?;
        registry.register(Box::new(task_queue_wait_seconds.clone()))?;

        // ========== Cluster Metrics ==========

        let cluster_nodes_total = IntGauge::new(
            "grey_cluster_nodes_total",
            "Total number of nodes in the cluster",
        )?;
        registry.register(Box::new(cluster_nodes_total.clone()))?;

        let cluster_nodes_healthy = IntGauge::new(
            "grey_cluster_nodes_healthy",
            "Number of healthy nodes in the cluster",
        )?;
        registry.register(Box::new(cluster_nodes_healthy.clone()))?;

        let cluster_nodes_quarantined = IntGauge::new(
            "grey_cluster_nodes_quarantined",
            "Number of quarantined nodes",
        )?;
        registry.register(Box::new(cluster_nodes_quarantined.clone()))?;

        let raft_term = IntGauge::new("grey_raft_term", "Current Raft term")?;
        registry.register(Box::new(raft_term.clone()))?;

        let raft_is_leader = IntGauge::new(
            "grey_raft_is_leader",
            "Whether this node is the Raft leader (0 or 1)",
        )?;
        registry.register(Box::new(raft_is_leader.clone()))?;

        let raft_entries_applied_total = IntCounter::new(
            "grey_raft_entries_applied_total",
            "Total Raft log entries applied",
        )?;
        registry.register(Box::new(raft_entries_applied_total.clone()))?;

        let cluster_cpu_percent = Gauge::new(
            "grey_cluster_cpu_percent",
            "Cluster-wide CPU utilization percentage",
        )?;
        registry.register(Box::new(cluster_cpu_percent.clone()))?;

        let cluster_memory_percent = Gauge::new(
            "grey_cluster_memory_percent",
            "Cluster-wide memory utilization percentage",
        )?;
        registry.register(Box::new(cluster_memory_percent.clone()))?;

        // ========== Tenant Metrics ==========

        let tenant_tasks_total = IntCounterVec::new(
            Opts::new("grey_tenant_tasks_total", "Total tasks per tenant")
                .namespace("grey"),
            &["tenant_id"],
        )?;
        registry.register(Box::new(tenant_tasks_total.clone()))?;

        let tenant_quota_used_percent = GaugeVec::new(
            Opts::new("grey_tenant_quota_used_percent", "Quota usage percentage per tenant")
                .namespace("grey"),
            &["tenant_id", "resource_type"],
        )?;
        registry.register(Box::new(tenant_quota_used_percent.clone()))?;

        let tenant_throttle_events_total = IntCounterVec::new(
            Opts::new("grey_tenant_throttle_events_total", "Throttle events per tenant")
                .namespace("grey"),
            &["tenant_id", "reason"],
        )?;
        registry.register(Box::new(tenant_throttle_events_total.clone()))?;

        let tenant_connections = IntGaugeVec::new(
            Opts::new("grey_tenant_connections", "Active connections per tenant")
                .namespace("grey"),
            &["tenant_id"],
        )?;
        registry.register(Box::new(tenant_connections.clone()))?;

        // ========== Security Metrics ==========

        let tee_attestation_total = IntCounterVec::new(
            Opts::new("grey_tee_attestation_total", "TEE attestation attempts")
                .namespace("grey"),
            &["node_id", "status"],
        )?;
        registry.register(Box::new(tee_attestation_total.clone()))?;

        let auth_attempts_total = IntCounterVec::new(
            Opts::new("grey_auth_attempts_total", "Authentication attempts")
                .namespace("grey"),
            &["method", "status"],
        )?;
        registry.register(Box::new(auth_attempts_total.clone()))?;

        let active_sessions = IntGaugeVec::new(
            Opts::new("grey_active_sessions", "Active authenticated sessions")
                .namespace("grey"),
            &["tenant_id"],
        )?;
        registry.register(Box::new(active_sessions.clone()))?;

        // ========== Storage Metrics ==========

        let storage_writes_total = IntCounterVec::new(
            Opts::new("grey_storage_writes_total", "Storage write operations")
                .namespace("grey"),
            &["backend", "quorum"],
        )?;
        registry.register(Box::new(storage_writes_total.clone()))?;

        let storage_reads_total = IntCounterVec::new(
            Opts::new("grey_storage_reads_total", "Storage read operations")
                .namespace("grey"),
            &["backend", "consistency"],
        )?;
        registry.register(Box::new(storage_reads_total.clone()))?;

        let storage_latency_seconds = HistogramVec::new(
            HistogramOpts::new(
                "grey_storage_latency_seconds",
                "Storage operation latency",
            )
            .namespace("grey")
            .buckets(vec![0.001, 0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 1.0]),
            &["backend", "operation"],
        )?;
        registry.register(Box::new(storage_latency_seconds.clone()))?;

        let quorum_success_rate = GaugeVec::new(
            Opts::new("grey_quorum_success_rate", "Quorum operation success rate")
                .namespace("grey"),
            &["operation"],
        )?;
        registry.register(Box::new(quorum_success_rate.clone()))?;

        // ========== Network Metrics ==========

        let network_bytes_sent_total = IntCounterVec::new(
            Opts::new("grey_network_bytes_sent_total", "Total bytes sent")
                .namespace("grey"),
            &["peer"],
        )?;
        registry.register(Box::new(network_bytes_sent_total.clone()))?;

        let network_bytes_received_total = IntCounterVec::new(
            Opts::new("grey_network_bytes_received_total", "Total bytes received")
                .namespace("grey"),
            &["peer"],
        )?;
        registry.register(Box::new(network_bytes_received_total.clone()))?;

        let rpc_latency_seconds = HistogramVec::new(
            HistogramOpts::new("grey_rpc_latency_seconds", "RPC request latency")
                .namespace("grey")
                .buckets(vec![0.001, 0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5]),
            &["method", "peer"],
        )?;
        registry.register(Box::new(rpc_latency_seconds.clone()))?;

        Ok(Self {
            registry,
            tasks_submitted_total,
            tasks_completed_total,
            tasks_failed_total,
            tasks_queued,
            tasks_running,
            task_duration_seconds,
            task_queue_wait_seconds,
            cluster_nodes_total,
            cluster_nodes_healthy,
            cluster_nodes_quarantined,
            raft_term,
            raft_is_leader,
            raft_entries_applied_total,
            cluster_cpu_percent,
            cluster_memory_percent,
            tenant_tasks_total,
            tenant_quota_used_percent,
            tenant_throttle_events_total,
            tenant_connections,
            tee_attestation_total,
            auth_attempts_total,
            active_sessions,
            storage_writes_total,
            storage_reads_total,
            storage_latency_seconds,
            quorum_success_rate,
            network_bytes_sent_total,
            network_bytes_received_total,
            rpc_latency_seconds,
        })
    }

    /// Export metrics in Prometheus text format.
    pub fn export(&self) -> String {
        let encoder = TextEncoder::new();
        let metric_families = self.registry.gather();
        let mut buffer = String::new();
        
        for mf in metric_families {
            if let Err(e) = encoder.encode_utf8(&[mf], &mut buffer) {
                error!("Failed to encode metric: {}", e);
            }
        }
        
        buffer
    }
}

// ============================================================================
// Metric Recording Helpers
// ============================================================================

impl GreyMetrics {
    /// Record task submission.
    pub fn record_task_submitted(&self, tenant_id: &str, task_type: &str) {
        self.tasks_submitted_total
            .with_label_values(&[tenant_id, task_type])
            .inc();
        self.tenant_tasks_total
            .with_label_values(&[tenant_id])
            .inc();
    }

    /// Record task completion with duration.
    pub fn record_task_completed(
        &self,
        tenant_id: &str,
        task_type: &str,
        duration: Duration,
        queue_wait: Duration,
    ) {
        self.tasks_completed_total
            .with_label_values(&[tenant_id, task_type])
            .inc();
        self.task_duration_seconds
            .with_label_values(&[tenant_id, task_type, "success"])
            .observe(duration.as_secs_f64());
        self.task_queue_wait_seconds
            .with_label_values(&[tenant_id, "normal"])
            .observe(queue_wait.as_secs_f64());
    }

    /// Record task failure.
    pub fn record_task_failed(
        &self,
        tenant_id: &str,
        task_type: &str,
        error_type: &str,
        duration: Duration,
    ) {
        self.tasks_failed_total
            .with_label_values(&[tenant_id, task_type, error_type])
            .inc();
        self.task_duration_seconds
            .with_label_values(&[tenant_id, task_type, "failed"])
            .observe(duration.as_secs_f64());
    }

    /// Update queue depth.
    pub fn set_queue_depth(&self, tenant_id: &str, priority: &str, depth: i64) {
        self.tasks_queued
            .with_label_values(&[tenant_id, priority])
            .set(depth);
    }

    /// Record storage operation.
    pub fn record_storage_operation(
        &self,
        backend: &str,
        operation: &str,
        duration: Duration,
        quorum: &str,
    ) {
        match operation {
            "write" => {
                self.storage_writes_total
                    .with_label_values(&[backend, quorum])
                    .inc();
            }
            "read" => {
                self.storage_reads_total
                    .with_label_values(&[backend, quorum])
                    .inc();
            }
            _ => {}
        }
        self.storage_latency_seconds
            .with_label_values(&[backend, operation])
            .observe(duration.as_secs_f64());
    }

    /// Record TEE attestation.
    pub fn record_attestation(&self, node_id: &str, success: bool) {
        let status = if success { "success" } else { "failure" };
        self.tee_attestation_total
            .with_label_values(&[node_id, status])
            .inc();
    }

    /// Record authentication attempt.
    pub fn record_auth_attempt(&self, method: &str, success: bool) {
        let status = if success { "success" } else { "failure" };
        self.auth_attempts_total
            .with_label_values(&[method, status])
            .inc();
    }

    /// Update cluster state.
    pub fn update_cluster_state(
        &self,
        total_nodes: i64,
        healthy_nodes: i64,
        quarantined_nodes: i64,
        cpu_percent: f64,
        memory_percent: f64,
    ) {
        self.cluster_nodes_total.set(total_nodes);
        self.cluster_nodes_healthy.set(healthy_nodes);
        self.cluster_nodes_quarantined.set(quarantined_nodes);
        self.cluster_cpu_percent.set(cpu_percent);
        self.cluster_memory_percent.set(memory_percent);
    }

    /// Update Raft state.
    pub fn update_raft_state(&self, term: i64, is_leader: bool) {
        self.raft_term.set(term);
        self.raft_is_leader.set(if is_leader { 1 } else { 0 });
    }
}

// ============================================================================
// HTTP Exporter
// ============================================================================

/// Configuration for the Prometheus exporter.
#[derive(Debug, Clone)]
pub struct ExporterConfig {
    /// Address to bind the HTTP server.
    pub bind_address: SocketAddr,
    
    /// Path for metrics endpoint.
    pub metrics_path: String,
    
    /// Enable gzip compression.
    pub enable_compression: bool,
}

impl Default for ExporterConfig {
    fn default() -> Self {
        Self {
            bind_address: ([0, 0, 0, 0], 9090).into(),
            metrics_path: "/metrics".into(),
            enable_compression: true,
        }
    }
}

/// Start the Prometheus HTTP exporter.
///
/// This creates an HTTP server that exposes the /metrics endpoint
/// for Prometheus to scrape.
pub async fn start_exporter(metrics: Arc<GreyMetrics>, config: ExporterConfig) {
    let metrics = warp::any().map(move || Arc::clone(&metrics));

    let metrics_route = warp::path("metrics")
        .and(warp::get())
        .and(metrics)
        .map(|metrics: Arc<GreyMetrics>| {
            let body = metrics.export();
            warp::reply::with_header(
                body,
                "Content-Type",
                "text/plain; version=0.0.4; charset=utf-8",
            )
        });

    let health_route = warp::path("health")
        .and(warp::get())
        .map(|| warp::reply::json(&serde_json::json!({"status": "healthy"})));

    let routes = metrics_route.or(health_route);

    info!(
        "Starting Prometheus exporter on {}{}",
        config.bind_address, config.metrics_path
    );

    warp::serve(routes).run(config.bind_address).await;
}

// ============================================================================
// Metric Timer
// ============================================================================

/// RAII timer for automatic duration recording.
pub struct MetricTimer<F>
where
    F: FnOnce(Duration),
{
    start: Instant,
    on_drop: Option<F>,
}

impl<F> MetricTimer<F>
where
    F: FnOnce(Duration),
{
    pub fn new(on_drop: F) -> Self {
        Self {
            start: Instant::now(),
            on_drop: Some(on_drop),
        }
    }

    pub fn elapsed(&self) -> Duration {
        self.start.elapsed()
    }
}

impl<F> Drop for MetricTimer<F>
where
    F: FnOnce(Duration),
{
    fn drop(&mut self) {
        if let Some(f) = self.on_drop.take() {
            f(self.start.elapsed());
        }
    }
}

// ============================================================================
// Push Gateway Support
// ============================================================================

/// Push metrics to a Prometheus Pushgateway.
///
/// Useful for batch jobs or short-lived processes that can't be scraped.
///
/// # Trade-offs
///
/// Push vs Pull:
/// - Push: Simpler for ephemeral jobs, but loses service discovery benefits
/// - Pull: Better for long-running services, HA, and aggregation
///
/// Use push for Grey's batch task workers that terminate after completion.
pub async fn push_to_gateway(
    metrics: &GreyMetrics,
    gateway_url: &str,
    job_name: &str,
    instance: &str,
) -> Result<(), Box<dyn std::error::Error>> {
    let url = format!("{}/metrics/job/{}/instance/{}", gateway_url, job_name, instance);
    let body = metrics.export();

    let client = reqwest::Client::new();
    let response = client
        .post(&url)
        .header("Content-Type", "text/plain")
        .body(body)
        .send()
        .await?;

    if !response.status().is_success() {
        let status = response.status();
        let text = response.text().await.unwrap_or_default();
        return Err(format!("Push failed: {} - {}", status, text).into());
    }

    debug!("Pushed metrics to gateway: {}", gateway_url);
    Ok(())
}

// ============================================================================
// Example Prometheus Configuration
// ============================================================================

/// Example prometheus.yml configuration.
///
/// ```yaml
/// global:
///   scrape_interval: 15s
///   evaluation_interval: 15s
///
/// scrape_configs:
///   - job_name: 'grey-distributed'
///     static_configs:
///       - targets: ['grey-coordinator-0:9090', 'grey-coordinator-1:9090']
///     relabel_configs:
///       - source_labels: [__address__]
///         target_label: instance
///         regex: '(.+):\d+'
///         replacement: '${1}'
///
///   - job_name: 'grey-workers'
///     kubernetes_sd_configs:
///       - role: pod
///         namespaces:
///           names: ['grey-system']
///     relabel_configs:
///       - source_labels: [__meta_kubernetes_pod_label_app]
///         regex: grey-worker
///         action: keep
///       - source_labels: [__meta_kubernetes_pod_ip]
///         target_label: __address__
///         replacement: '${1}:9090'
///
/// alerting:
///   alertmanagers:
///     - static_configs:
///         - targets: ['alertmanager:9093']
///
/// rule_files:
///   - '/etc/prometheus/rules/*.yml'
/// ```
pub const EXAMPLE_PROMETHEUS_CONFIG: &str = include_str!("prometheus_example.yml");

// ============================================================================
// Alert Rules
// ============================================================================

/// Example alerting rules for Grey metrics.
///
/// Save as /etc/prometheus/rules/grey_alerts.yml
pub const ALERT_RULES: &str = r#"
groups:
  - name: grey_cluster
    interval: 30s
    rules:
      - alert: GreyClusterUnhealthy
        expr: grey_cluster_nodes_healthy < grey_cluster_nodes_total * 0.5
        for: 5m
        labels:
          severity: critical
        annotations:
          summary: "Grey cluster has less than 50% healthy nodes"
          description: "Healthy nodes: {{ $value }}"

      - alert: GreyNoLeader
        expr: sum(grey_raft_is_leader) == 0
        for: 1m
        labels:
          severity: critical
        annotations:
          summary: "Grey cluster has no Raft leader"

      - alert: GreyHighQueueDepth
        expr: sum(grey_tasks_queued) > 10000
        for: 10m
        labels:
          severity: warning
        annotations:
          summary: "Grey task queue depth is high"
          description: "Queue depth: {{ $value }}"

  - name: grey_tenants
    interval: 30s
    rules:
      - alert: GreyTenantThrottled
        expr: increase(grey_tenant_throttle_events_total[5m]) > 100
        labels:
          severity: warning
        annotations:
          summary: "Tenant {{ $labels.tenant_id }} is being throttled"

      - alert: GreyTenantQuotaExhausted
        expr: grey_tenant_quota_used_percent > 95
        for: 5m
        labels:
          severity: warning
        annotations:
          summary: "Tenant {{ $labels.tenant_id }} quota nearly exhausted"

  - name: grey_performance
    interval: 30s
    rules:
      - alert: GreyHighTaskLatency
        expr: histogram_quantile(0.99, rate(grey_task_duration_seconds_bucket[5m])) > 60
        for: 10m
        labels:
          severity: warning
        annotations:
          summary: "Grey p99 task latency is above 60 seconds"

      - alert: GreyHighErrorRate
        expr: |
          sum(rate(grey_tasks_failed_total[5m])) /
          sum(rate(grey_tasks_completed_total[5m]) + rate(grey_tasks_failed_total[5m])) > 0.05
        for: 5m
        labels:
          severity: warning
        annotations:
          summary: "Grey task error rate is above 5%"
"#;

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_metrics_creation() {
        let metrics = GreyMetrics::new().expect("Metrics should be created");
        
        // Record some metrics
        metrics.record_task_submitted("tenant-1", "stateless");
        metrics.record_task_completed(
            "tenant-1",
            "stateless",
            Duration::from_millis(100),
            Duration::from_millis(10),
        );
        
        let output = metrics.export();
        assert!(output.contains("grey_tasks_submitted_total"));
        assert!(output.contains("grey_tasks_completed_total"));
        assert!(output.contains("tenant_id=\"tenant-1\""));
    }

    #[test]
    fn test_metric_timer() {
        use std::sync::atomic::AtomicBool;
        
        let recorded = Arc::new(AtomicBool::new(false));
        let recorded_clone = Arc::clone(&recorded);
        
        {
            let _timer = MetricTimer::new(move |_duration| {
                recorded_clone.store(true, Ordering::SeqCst);
            });
            std::thread::sleep(Duration::from_millis(10));
        }
        
        assert!(recorded.load(Ordering::SeqCst));
    }
}
