import { useState } from 'react';
import { PageHeader, StatCard, Badge, DataTable, Tabs, ProgressBar } from '../../components/ui';
import {
    Shield, Lock, Key, Eye, FileText, AlertTriangle,
    CheckCircle, Clock, Server, RefreshCw, Users,
    Globe, Fingerprint, Activity, XCircle, Radio,
    GitBranch, Zap, Cpu, ArrowRight, BarChart3
} from 'lucide-react';

const controls = [
    { id: 'CC-01', name: 'Access Control', framework: 'SOC2', status: 'Compliant', evidence: 14, lastReview: '5 days ago' },
    { id: 'CC-02', name: 'Data Encryption at Rest', framework: 'SOC2', status: 'Compliant', evidence: 8, lastReview: '12 days ago' },
    { id: 'CC-03', name: 'Data Encryption in Transit', framework: 'SOC2', status: 'Compliant', evidence: 6, lastReview: '12 days ago' },
    { id: 'CC-04', name: 'Change Management', framework: 'SOC2', status: 'Compliant', evidence: 22, lastReview: '3 days ago' },
    { id: 'CC-05', name: 'Incident Response', framework: 'SOC2', status: 'Compliant', evidence: 18, lastReview: '1 week ago' },
    { id: 'CC-06', name: 'Vulnerability Management', framework: 'SOC2', status: 'Needs Review', evidence: 4, lastReview: '28 days ago' },
    { id: 'CC-07', name: 'Vendor Risk Assessment', framework: 'SOC2', status: 'Compliant', evidence: 12, lastReview: '2 weeks ago' },
    { id: 'CC-08', name: 'Business Continuity', framework: 'SOC2', status: 'Compliant', evidence: 9, lastReview: '10 days ago' },
];

const secrets = [
    { name: 'db_master_password', path: '/grey-suite/prod/db', engine: 'PostgreSQL', rotated: '12 days ago', nextRotation: '78 days', status: 'Current' },
    { name: 'jwt_signing_key', path: '/grey-suite/prod/auth', engine: 'HMAC', rotated: '18 days ago', nextRotation: '72 days', status: 'Current' },
    { name: 'stripe_api_key', path: '/grey-suite/prod/payments', engine: 'API Key', rotated: '42 days ago', nextRotation: '48 days', status: 'Current' },
    { name: 'smtp_credentials', path: '/grey-suite/prod/email', engine: 'OAuth2', rotated: '68 days ago', nextRotation: '22 days', status: 'Expiring Soon' },
    { name: 'aws_kms_key', path: '/grey-suite/prod/encryption', engine: 'KMS', rotated: '5 days ago', nextRotation: '85 days', status: 'Current' },
    { name: 'okta_client_secret', path: '/grey-suite/prod/sso', engine: 'OIDC', rotated: '30 days ago', nextRotation: '60 days', status: 'Current' },
];

const incidentPlaybooks = [
    { name: 'Data Breach Response', severity: 'Critical', steps: 12, lastDrill: '2 weeks ago', owner: 'Security Team', sla: '15 min' },
    { name: 'DDoS Mitigation', severity: 'High', steps: 8, lastDrill: '1 month ago', owner: 'SRE Team', sla: '5 min' },
    { name: 'Credential Compromise', severity: 'Critical', steps: 10, lastDrill: '3 weeks ago', owner: 'Security Team', sla: '10 min' },
    { name: 'Service Outage', severity: 'High', steps: 14, lastDrill: '1 week ago', owner: 'SRE Team', sla: '5 min' },
    { name: 'Compliance Violation', severity: 'Medium', steps: 6, lastDrill: '2 months ago', owner: 'Legal + Security', sla: '1 hour' },
    { name: 'Insider Threat', severity: 'Critical', steps: 9, lastDrill: '6 weeks ago', owner: 'Security Team', sla: '15 min' },
];

const auditLog = [
    { time: '2 min ago', actor: 'system', event: 'mTLS certificate renewed — api-gateway', severity: 'Info' },
    { time: '15 min ago', actor: 'admin@acme.com', event: 'Access review completed — Finance team (24 users)', severity: 'Info' },
    { time: '1h ago', actor: 'system', event: 'Blocked: 3 failed login attempts from 185.42.x.x', severity: 'Warning' },
    { time: '2h ago', actor: 'system', event: 'Vulnerability scan completed — 0 critical, 2 medium', severity: 'Info' },
    { time: '4h ago', actor: 'system', event: 'Key rotation: smtp_credentials approaching expiry (22d)', severity: 'Warning' },
    { time: '6h ago', actor: 'security@grey.io', event: 'Incident playbook drill: Credential Compromise', severity: 'Info' },
    { time: '12h ago', actor: 'system', event: 'WAF blocked 142 malicious requests from CIDR 45.x.x.x/24', severity: 'Warning' },
];

const traces = [
    { traceId: 'abc12def', operation: 'POST /api/v1/crm/deals', service: 'api-gateway', duration: '248ms', spans: 14, status: 'OK', depth: 5 },
    { traceId: 'ef34ab56', operation: 'GET /api/v1/erp/invoices', service: 'api-gateway', duration: '82ms', spans: 8, status: 'OK', depth: 3 },
    { traceId: '78cd90ef', operation: 'POST /api/v1/ai/summarize', service: 'ai-orchestrator', duration: '1.2s', spans: 22, status: 'OK', depth: 7 },
    { traceId: 'gh56ij78', operation: 'PUT /api/v1/hcm/employees/:id', service: 'api-gateway', duration: '186ms', spans: 11, status: 'OK', depth: 4 },
    { traceId: 'kl90mn12', operation: 'POST /api/v1/workflows/trigger', service: 'workflow-engine', duration: '3.4s', spans: 38, status: 'ERROR', depth: 9 },
    { traceId: 'op34qr56', operation: 'GET /api/v1/bi/dashboards/:id', service: 'bi-platform', duration: '420ms', spans: 16, status: 'OK', depth: 6 },
];

const spanWaterfall = [
    { service: 'api-gateway', operation: 'HTTP POST /crm/deals', start: 0, duration: 248, level: 0, status: 'ok' },
    { service: 'auth-service', operation: 'JWT validate + RBAC check', start: 2, duration: 8, level: 1, status: 'ok' },
    { service: 'crm-service', operation: 'createDeal()', start: 12, duration: 180, level: 1, status: 'ok' },
    { service: 'crm-service', operation: 'DB: INSERT INTO deals', start: 18, duration: 24, level: 2, status: 'ok' },
    { service: 'crm-service', operation: 'DB: UPDATE pipeline_stage', start: 44, duration: 12, level: 2, status: 'ok' },
    { service: 'event-bus', operation: 'Kafka: publish deal.created', start: 60, duration: 6, level: 2, status: 'ok' },
    { service: 'notification-svc', operation: 'sendSlackNotification()', start: 70, duration: 42, level: 3, status: 'ok' },
    { service: 'ai-orchestrator', operation: 'enrichDealInsights()', start: 80, duration: 108, level: 3, status: 'ok' },
    { service: 'ai-orchestrator', operation: 'LLM: gpt-4o inference', start: 86, duration: 94, level: 4, status: 'ok' },
    { service: 'search-indexer', operation: 'indexDocument(deal)', start: 192, duration: 18, level: 2, status: 'ok' },
    { service: 'audit-logger', operation: 'appendAuditLog()', start: 212, duration: 4, level: 2, status: 'ok' },
];

const circuitBreakers = [
    { service: 'ai-inference', target: 'OpenAI API', state: 'Closed', failRate: '0.2%', threshold: '5%', timeout: '30s', halfOpenAfter: '60s', consecutiveFails: 0, requests: '12.4K/hr' },
    { service: 'email-service', target: 'SendGrid SMTP', state: 'Closed', failRate: '0.8%', threshold: '10%', timeout: '15s', halfOpenAfter: '30s', consecutiveFails: 0, requests: '2.1K/hr' },
    { service: 'payment-service', target: 'Stripe API', state: 'Half-Open', failRate: '4.2%', threshold: '5%', timeout: '10s', halfOpenAfter: '45s', consecutiveFails: 3, requests: '840/hr' },
    { service: 'erp-sync', target: 'SAP Gateway', state: 'Open', failRate: '42%', threshold: '20%', timeout: '60s', halfOpenAfter: '120s', consecutiveFails: 18, requests: '0/hr' },
    { service: 'crm-enrichment', target: 'Clearbit API', state: 'Closed', failRate: '1.1%', threshold: '15%', timeout: '20s', halfOpenAfter: '60s', consecutiveFails: 0, requests: '3.6K/hr' },
    { service: 'data-warehouse', target: 'Snowflake', state: 'Closed', failRate: '0.0%', threshold: '5%', timeout: '45s', halfOpenAfter: '90s', consecutiveFails: 0, requests: '18.2K/hr' },
];

export function SecurityCompliance() {
    const [activeTab, setActiveTab] = useState('Zero Trust');

    return (
        <div>
            <PageHeader
                title="Security, Compliance & Governance"
                subtitle="Zero-trust networking, secrets management, SOC2 controls & incident response"
                actions={
                    <div className="flex gap-2">
                        <button className="gs-btn-secondary flex items-center gap-2"><FileText size={16} /> Export Audit</button>
                        <button className="gs-btn-primary flex items-center gap-2"><Shield size={16} /> Run Scan</button>
                    </div>
                }
            />

            <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-4 gap-4 mb-6">
                <StatCard label="Security Score" value="94/100" change="+2 this month" changeType="positive" icon={<Shield size={18} />} />
                <StatCard label="SOC2 Controls" value="7/8" change="1 needs review" changeType="negative" icon={<CheckCircle size={18} />} />
                <StatCard label="Secrets Managed" value="6" change="0 expired" changeType="positive" icon={<Key size={18} />} />
                <StatCard label="Threats Blocked (24h)" value="287" change="WAF + IDS" changeType="neutral" icon={<AlertTriangle size={18} />} />
            </div>

            <Tabs tabs={['Zero Trust', 'Secrets', 'Key Rotation', 'SOC2 Controls', 'Incident Playbooks', 'Audit Log', 'Access Reviews', 'Distributed Tracing', 'Resilience']} active={activeTab} onChange={setActiveTab} />

            {activeTab === 'Zero Trust' && (
                <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
                    <div className="gs-card p-5">
                        <h3 className="gs-section-title mb-4 flex items-center gap-2"><Globe size={16} /> Zero-Trust Network Architecture</h3>
                        <div className="space-y-3">
                            {[
                                { layer: 'mTLS Between Services', desc: 'All inter-service communication encrypted with mutual TLS', status: 'Enforced', icon: <Lock size={14} /> },
                                { layer: 'Network Segmentation', desc: 'Microsegmented VPCs per module with security groups', status: 'Enforced', icon: <Server size={14} /> },
                                { layer: 'Identity-Aware Proxy', desc: 'Every request authenticated at the edge, no implicit trust', status: 'Enforced', icon: <Fingerprint size={14} /> },
                                { layer: 'Least Privilege Access', desc: 'RBAC + ABAC policies enforce minimum required permissions', status: 'Enforced', icon: <Users size={14} /> },
                                { layer: 'Continuous Verification', desc: 'Session re-validation every 15 min for sensitive operations', status: 'Enforced', icon: <RefreshCw size={14} /> },
                                { layer: 'WAF + DDoS Protection', desc: 'Layer 7 filtering with rate limiting and geo-blocking', status: 'Active', icon: <Shield size={14} /> },
                            ].map(l => (
                                <div key={l.layer} className="p-3 rounded-lg border border-grey-800 flex items-center gap-3">
                                    <span className="text-accent-emerald">{l.icon}</span>
                                    <div className="flex-1">
                                        <p className="text-sm font-medium text-grey-200">{l.layer}</p>
                                        <p className="text-xs text-grey-500">{l.desc}</p>
                                    </div>
                                    <Badge variant="success">{l.status}</Badge>
                                </div>
                            ))}
                        </div>
                    </div>
                    <div className="gs-card p-5">
                        <h3 className="gs-section-title mb-4">mTLS Certificate Status</h3>
                        <div className="space-y-3">
                            {[
                                { service: 'api-gateway', issuer: 'Internal CA', expiry: '88 days', status: 'Valid' },
                                { service: 'auth-service', issuer: 'Internal CA', expiry: '88 days', status: 'Valid' },
                                { service: 'crm-service', issuer: 'Internal CA', expiry: '88 days', status: 'Valid' },
                                { service: 'erp-service', issuer: 'Internal CA', expiry: '88 days', status: 'Valid' },
                                { service: 'ai-inference', issuer: 'Internal CA', expiry: '88 days', status: 'Valid' },
                                { service: 'data-warehouse', issuer: 'Internal CA', expiry: '88 days', status: 'Valid' },
                            ].map(c => (
                                <div key={c.service} className="flex items-center justify-between p-2 bg-grey-800/30 rounded">
                                    <div className="flex items-center gap-2">
                                        <CheckCircle size={12} className="text-accent-emerald" />
                                        <span className="font-mono text-xs text-grey-300">{c.service}</span>
                                    </div>
                                    <span className="text-xs text-grey-500">Expires in {c.expiry}</span>
                                </div>
                            ))}
                        </div>
                    </div>
                </div>
            )}

            {activeTab === 'Secrets' && (
                <div className="gs-card p-5">
                    <h3 className="gs-section-title mb-4 flex items-center gap-2"><Key size={16} /> Vault-Style Secrets Management</h3>
                    <DataTable
                        columns={['Secret', 'Path', 'Engine', 'Last Rotated', 'Next Rotation', 'Status']}
                        rows={secrets.map(s => [
                            <span className="font-mono text-xs text-grey-200">{s.name}</span>,
                            <span className="font-mono text-xs text-grey-400">{s.path}</span>,
                            <Badge variant="info">{s.engine}</Badge>,
                            <span className="text-xs text-grey-500">{s.rotated}</span>,
                            <span className={`text-xs ${parseInt(s.nextRotation) < 30 ? 'text-accent-amber' : 'text-grey-400'}`}>{s.nextRotation}</span>,
                            <Badge variant={s.status === 'Current' ? 'success' : 'warning'}>{s.status}</Badge>,
                        ])}
                    />
                </div>
            )}

            {activeTab === 'Key Rotation' && (
                <div className="gs-card p-5">
                    <h3 className="gs-section-title mb-4 flex items-center gap-2"><RefreshCw size={16} /> Automated Key Rotation (90-Day Policy)</h3>
                    <div className="space-y-4">
                        {secrets.map(s => (
                            <div key={s.name} className="p-4 rounded-lg border border-grey-800">
                                <div className="flex items-center justify-between mb-2">
                                    <div>
                                        <p className="text-sm font-medium text-grey-200 font-mono">{s.name}</p>
                                        <p className="text-xs text-grey-500">{s.engine} · Last rotated {s.rotated}</p>
                                    </div>
                                    <button className="gs-btn-ghost text-xs flex items-center gap-1"><RefreshCw size={12} /> Rotate Now</button>
                                </div>
                                <div className="flex items-center gap-2">
                                    <div className="flex-1">
                                        <ProgressBar value={90 - parseInt(s.nextRotation)} max={90} color={parseInt(s.nextRotation) < 30 ? 'bg-accent-amber' : 'bg-accent-emerald'} size="sm" />
                                    </div>
                                    <span className="text-xs text-grey-400 w-20 text-right">{s.nextRotation} left</span>
                                </div>
                            </div>
                        ))}
                    </div>
                </div>
            )}

            {activeTab === 'SOC2 Controls' && (
                <div className="gs-card p-5">
                    <h3 className="gs-section-title mb-4 flex items-center gap-2"><FileText size={16} /> SOC2 Type II Controls</h3>
                    <DataTable
                        columns={['Control ID', 'Name', 'Framework', 'Evidence', 'Last Review', 'Status']}
                        rows={controls.map(c => [
                            <span className="font-mono text-xs text-grey-400">{c.id}</span>,
                            <span className="text-grey-200">{c.name}</span>,
                            <Badge variant="purple">{c.framework}</Badge>,
                            <span className="text-xs">{c.evidence} artifacts</span>,
                            <span className="text-xs text-grey-500">{c.lastReview}</span>,
                            <Badge variant={c.status === 'Compliant' ? 'success' : 'warning'}>{c.status}</Badge>,
                        ])}
                    />
                </div>
            )}

            {activeTab === 'Incident Playbooks' && (
                <div className="gs-card p-5">
                    <h3 className="gs-section-title mb-4 flex items-center gap-2"><AlertTriangle size={16} /> Incident Response Playbooks</h3>
                    <div className="space-y-3">
                        {incidentPlaybooks.map(p => (
                            <div key={p.name} className="p-4 rounded-lg border border-grey-800 flex items-center justify-between">
                                <div className="flex-1">
                                    <p className="text-sm font-medium text-grey-200">{p.name}</p>
                                    <p className="text-xs text-grey-500">{p.steps} steps · Owner: {p.owner} · SLA: {p.sla} · Last drill: {p.lastDrill}</p>
                                </div>
                                <div className="flex items-center gap-2">
                                    <Badge variant={p.severity === 'Critical' ? 'danger' : p.severity === 'High' ? 'warning' : 'info'}>{p.severity}</Badge>
                                    <button className="gs-btn-ghost text-xs">Run Drill</button>
                                </div>
                            </div>
                        ))}
                    </div>
                </div>
            )}

            {activeTab === 'Audit Log' && (
                <div className="gs-card p-5">
                    <h3 className="gs-section-title mb-4 flex items-center gap-2"><Eye size={16} /> Immutable Audit Log</h3>
                    <p className="text-xs text-grey-500 mb-4">Append-only, tamper-proof log with cryptographic hashing (SHA-256 chain)</p>
                    <div className="space-y-2">
                        {auditLog.map((e, i) => (
                            <div key={i} className="flex items-center gap-3 p-3 rounded-lg hover:bg-grey-800/30 transition-colors">
                                <span className="text-xs text-grey-600 w-16 flex-shrink-0">{e.time}</span>
                                {e.severity === 'Warning' ? <AlertTriangle size={14} className="text-accent-amber flex-shrink-0" /> : <CheckCircle size={14} className="text-grey-600 flex-shrink-0" />}
                                <span className="font-mono text-xs text-grey-500 w-32 flex-shrink-0">{e.actor}</span>
                                <span className="text-sm text-grey-300 flex-1">{e.event}</span>
                            </div>
                        ))}
                    </div>
                </div>
            )}

            {activeTab === 'Access Reviews' && (
                <div className="gs-card p-5">
                    <h3 className="gs-section-title mb-4 flex items-center gap-2"><Users size={16} /> Periodic Access Review Workflows</h3>
                    <div className="space-y-3">
                        {[
                            { team: 'Engineering', users: 142, reviewer: 'VP Engineering', due: '5 days', status: 'In Progress', progress: 68 },
                            { team: 'Finance', users: 24, reviewer: 'CFO', due: '12 days', status: 'In Progress', progress: 42 },
                            { team: 'HR', users: 12, reviewer: 'CHRO', due: '2 days', status: 'In Progress', progress: 88 },
                            { team: 'Sales', users: 86, reviewer: 'VP Sales', due: '18 days', status: 'Not Started', progress: 0 },
                            { team: 'Executive', users: 8, reviewer: 'CEO', due: '8 days', status: 'In Progress', progress: 50 },
                        ].map(r => (
                            <div key={r.team} className="p-4 rounded-lg border border-grey-800">
                                <div className="flex items-center justify-between mb-2">
                                    <div>
                                        <p className="text-sm font-medium text-grey-200">{r.team} Team</p>
                                        <p className="text-xs text-grey-500">{r.users} users · Reviewer: {r.reviewer} · Due in {r.due}</p>
                                    </div>
                                    <Badge variant={r.status === 'In Progress' ? 'info' : 'default'}>{r.status}</Badge>
                                </div>
                                <ProgressBar value={r.progress} color="bg-brand-500" size="sm" />
                            </div>
                        ))}
                    </div>
                </div>
            )}

            {activeTab === 'Distributed Tracing' && (
                <div className="space-y-6">
                    <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-4 gap-4">
                        <div className="gs-card p-4 text-center">
                            <p className="text-2xl font-bold text-grey-100">2.4M</p>
                            <p className="text-xs text-grey-500">Traces / hour</p>
                        </div>
                        <div className="gs-card p-4 text-center">
                            <p className="text-2xl font-bold text-accent-emerald">142ms</p>
                            <p className="text-xs text-grey-500">P50 Latency</p>
                        </div>
                        <div className="gs-card p-4 text-center">
                            <p className="text-2xl font-bold text-accent-amber">480ms</p>
                            <p className="text-xs text-grey-500">P99 Latency</p>
                        </div>
                        <div className="gs-card p-4 text-center">
                            <p className="text-2xl font-bold text-accent-rose">0.12%</p>
                            <p className="text-xs text-grey-500">Error Rate</p>
                        </div>
                    </div>

                    {/* OpenTelemetry Config */}
                    <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
                        <div className="gs-card p-5">
                            <h3 className="gs-section-title mb-4 flex items-center gap-2"><Radio size={16} /> OpenTelemetry Configuration</h3>
                            <div className="bg-grey-900/50 rounded-lg p-4 font-mono text-xs space-y-1">
                                <div className="text-grey-500">// otel-config.yaml — Collector pipeline</div>
                                <div className="text-accent-cyan">receivers:</div>
                                <div className="pl-3 text-grey-300">otlp:</div>
                                <div className="pl-6 text-grey-400">protocols:</div>
                                <div className="pl-9 text-grey-400">grpc: <span className="text-accent-amber">{"endpoint: 0.0.0.0:4317"}</span></div>
                                <div className="pl-9 text-grey-400">http: <span className="text-accent-amber">{"endpoint: 0.0.0.0:4318"}</span></div>
                                <div className="text-accent-cyan mt-2">processors:</div>
                                <div className="pl-3 text-grey-300">batch:</div>
                                <div className="pl-6 text-grey-400">send_batch_size: <span className="text-accent-amber">512</span></div>
                                <div className="pl-6 text-grey-400">timeout: <span className="text-accent-amber">5s</span></div>
                                <div className="pl-3 text-grey-300">tail_sampling:</div>
                                <div className="pl-6 text-grey-400">policies:</div>
                                <div className="pl-9 text-grey-400">- type: <span className="text-accent-emerald">latency</span> <span className="text-grey-600"># sample slow traces</span></div>
                                <div className="pl-12 text-grey-400">threshold_ms: <span className="text-accent-amber">500</span></div>
                                <div className="pl-9 text-grey-400">- type: <span className="text-accent-emerald">status_code</span> <span className="text-grey-600"># always sample errors</span></div>
                                <div className="pl-12 text-grey-400">status_codes: [<span className="text-accent-rose">ERROR</span>]</div>
                                <div className="text-accent-cyan mt-2">exporters:</div>
                                <div className="pl-3 text-grey-300">otlp/jaeger:</div>
                                <div className="pl-6 text-grey-400">endpoint: <span className="text-accent-amber">jaeger:4317</span></div>
                                <div className="pl-3 text-grey-300">prometheus:</div>
                                <div className="pl-6 text-grey-400">endpoint: <span className="text-accent-amber">0.0.0.0:8889</span></div>
                            </div>
                        </div>

                        <div className="gs-card p-5">
                            <h3 className="gs-section-title mb-4 flex items-center gap-2"><BarChart3 size={16} /> Service Dependency Map</h3>
                            <div className="space-y-2">
                                {[
                                    { from: 'api-gateway', to: 'auth-service', calls: '48K/hr', p99: '12ms', errors: '0.0%' },
                                    { from: 'api-gateway', to: 'crm-service', calls: '18K/hr', p99: '86ms', errors: '0.1%' },
                                    { from: 'api-gateway', to: 'erp-service', calls: '12K/hr', p99: '124ms', errors: '0.2%' },
                                    { from: 'crm-service', to: 'event-bus', calls: '8.4K/hr', p99: '8ms', errors: '0.0%' },
                                    { from: 'event-bus', to: 'notification-svc', calls: '6.2K/hr', p99: '220ms', errors: '0.4%' },
                                    { from: 'event-bus', to: 'ai-orchestrator', calls: '4.8K/hr', p99: '1.8s', errors: '0.8%' },
                                    { from: 'ai-orchestrator', to: 'openai-api', calls: '4.2K/hr', p99: '2.4s', errors: '1.2%' },
                                    { from: 'crm-service', to: 'data-warehouse', calls: '2.1K/hr', p99: '42ms', errors: '0.0%' },
                                ].map((e, i) => (
                                    <div key={i} className="flex items-center gap-2 p-2 rounded bg-grey-800/30 text-xs">
                                        <span className="font-mono text-accent-cyan w-28 truncate">{e.from}</span>
                                        <ArrowRight size={10} className="text-grey-600 flex-shrink-0" />
                                        <span className="font-mono text-accent-emerald w-28 truncate">{e.to}</span>
                                        <span className="text-grey-500 w-16 text-right">{e.calls}</span>
                                        <span className="text-grey-400 w-12 text-right">{e.p99}</span>
                                        <span className={`w-10 text-right ${parseFloat(e.errors) > 0.5 ? 'text-accent-rose' : parseFloat(e.errors) > 0 ? 'text-accent-amber' : 'text-grey-600'}`}>{e.errors}</span>
                                    </div>
                                ))}
                            </div>
                        </div>
                    </div>

                    {/* Trace List */}
                    <div className="gs-card p-5">
                        <h3 className="gs-section-title mb-4 flex items-center gap-2"><Eye size={16} /> Recent Traces</h3>
                        <DataTable
                            columns={['Trace ID', 'Operation', 'Root Service', 'Duration', 'Spans', 'Depth', 'Status']}
                            rows={traces.map(t => [
                                <span className="font-mono text-xs text-brand-400">{t.traceId}</span>,
                                <span className="font-mono text-xs text-grey-200">{t.operation}</span>,
                                <span className="text-xs text-grey-400">{t.service}</span>,
                                <span className={`font-mono text-xs ${parseFloat(t.duration) > 1000 || t.duration.includes('s') && !t.duration.includes('ms') ? 'text-accent-amber' : 'text-grey-300'}`}>{t.duration}</span>,
                                t.spans,
                                t.depth,
                                <Badge variant={t.status === 'OK' ? 'success' : 'danger'}>{t.status}</Badge>,
                            ])}
                        />
                    </div>

                    {/* Span Waterfall */}
                    <div className="gs-card p-5">
                        <h3 className="gs-section-title mb-4 flex items-center gap-2"><GitBranch size={16} /> Span Waterfall — trace:abc12def (POST /crm/deals)</h3>
                        <div className="space-y-1">
                            {spanWaterfall.map((s, i) => {
                                const maxDuration = 248;
                                const leftPct = (s.start / maxDuration) * 100;
                                const widthPct = Math.max((s.duration / maxDuration) * 100, 2);
                                const colors = ['bg-brand-500', 'bg-accent-violet', 'bg-accent-emerald', 'bg-accent-cyan', 'bg-accent-amber', 'bg-accent-rose'];
                                return (
                                    <div key={i} className="flex items-center gap-2">
                                        <span className="font-mono text-[10px] text-grey-500 w-28 truncate" style={{ paddingLeft: `${s.level * 12}px` }}>
                                            {s.service}
                                        </span>
                                        <div className="flex-1 h-5 relative bg-grey-900/50 rounded overflow-hidden">
                                            <div
                                                className={`absolute top-0.5 bottom-0.5 rounded ${colors[s.level % colors.length]} opacity-80`}
                                                style={{ left: `${leftPct}%`, width: `${widthPct}%` }}
                                            />
                                            <span className="absolute inset-0 flex items-center text-[9px] text-grey-400 pl-1 truncate" style={{ paddingLeft: `${leftPct + widthPct + 1}%` }}>
                                                {s.operation} ({s.duration}ms)
                                            </span>
                                        </div>
                                        <span className="text-[10px] text-grey-600 w-12 text-right">{s.duration}ms</span>
                                    </div>
                                );
                            })}
                        </div>
                        <div className="mt-4 flex items-center gap-4 text-[10px] text-grey-500">
                            <span className="flex items-center gap-1"><span className="w-2 h-2 rounded bg-brand-500" /> api-gateway</span>
                            <span className="flex items-center gap-1"><span className="w-2 h-2 rounded bg-accent-violet" /> auth / notification</span>
                            <span className="flex items-center gap-1"><span className="w-2 h-2 rounded bg-accent-emerald" /> crm-service</span>
                            <span className="flex items-center gap-1"><span className="w-2 h-2 rounded bg-accent-cyan" /> event-bus / DB</span>
                            <span className="flex items-center gap-1"><span className="w-2 h-2 rounded bg-accent-amber" /> ai-orchestrator</span>
                        </div>
                    </div>

                    {/* Context Propagation */}
                    <div className="gs-card p-5">
                        <h3 className="gs-section-title mb-4">W3C Trace Context Propagation</h3>
                        <div className="bg-grey-900/50 rounded-lg p-4 font-mono text-xs space-y-2">
                            <div className="text-grey-500">// W3C traceparent header propagated across all services</div>
                            <div className="text-accent-cyan">traceparent: 00-<span className="text-accent-amber">abc12def00000000abc12def00000000</span>-<span className="text-accent-emerald">1234567890abcdef</span>-<span className="text-accent-violet">01</span></div>
                            <div className="text-grey-600 mt-2">       version ─┘  trace-id (128-bit) ──────────────────────┘  parent-id (64-bit) ────────┘  flags ─┘</div>
                            <div className="mt-3 text-grey-500">// Baggage propagation for tenant context</div>
                            <div className="text-accent-cyan">baggage: <span className="text-accent-amber">tenant_id=TNT-001</span>,<span className="text-accent-amber">user_id=usr_a1b2c3d4</span>,<span className="text-accent-amber">deployment=canary-v2.4</span></div>
                        </div>
                    </div>
                </div>
            )}

            {activeTab === 'Resilience' && (
                <div className="space-y-6">
                    <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-4 gap-4">
                        <div className="gs-card p-4 text-center">
                            <p className="text-2xl font-bold text-accent-emerald">99.97%</p>
                            <p className="text-xs text-grey-500">Uptime (30d)</p>
                        </div>
                        <div className="gs-card p-4 text-center">
                            <p className="text-2xl font-bold text-grey-100">6</p>
                            <p className="text-xs text-grey-500">Circuit Breakers</p>
                        </div>
                        <div className="gs-card p-4 text-center">
                            <p className="text-2xl font-bold text-accent-rose">1</p>
                            <p className="text-xs text-grey-500">Open Circuits</p>
                        </div>
                        <div className="gs-card p-4 text-center">
                            <p className="text-2xl font-bold text-accent-amber">284</p>
                            <p className="text-xs text-grey-500">Retries (1h)</p>
                        </div>
                    </div>

                    {/* Circuit Breaker Dashboard */}
                    <div className="gs-card p-5">
                        <h3 className="gs-section-title mb-4 flex items-center gap-2"><Zap size={16} /> Circuit Breaker Dashboard</h3>
                        <div className="space-y-3">
                            {circuitBreakers.map(cb => (
                                <div key={cb.service} className="p-4 rounded-lg border border-grey-800">
                                    <div className="flex items-center justify-between mb-3">
                                        <div className="flex items-center gap-3">
                                            <div className={`w-3 h-3 rounded-full ${cb.state === 'Closed' ? 'bg-accent-emerald' : cb.state === 'Half-Open' ? 'bg-accent-amber animate-pulse' : 'bg-accent-rose animate-pulse'}`} />
                                            <div>
                                                <p className="text-sm font-medium text-grey-200">{cb.service} → {cb.target}</p>
                                                <p className="text-xs text-grey-500">{cb.requests} · {cb.consecutiveFails} consecutive failures</p>
                                            </div>
                                        </div>
                                        <Badge variant={cb.state === 'Closed' ? 'success' : cb.state === 'Half-Open' ? 'warning' : 'danger'}>{cb.state}</Badge>
                                    </div>
                                    <div className="grid grid-cols-4 gap-3 text-xs">
                                        <div>
                                            <span className="text-grey-500">Fail Rate</span>
                                            <p className={`font-mono ${parseFloat(cb.failRate) > 5 ? 'text-accent-rose' : parseFloat(cb.failRate) > 1 ? 'text-accent-amber' : 'text-grey-300'}`}>{cb.failRate}</p>
                                        </div>
                                        <div>
                                            <span className="text-grey-500">Threshold</span>
                                            <p className="font-mono text-grey-400">{cb.threshold}</p>
                                        </div>
                                        <div>
                                            <span className="text-grey-500">Timeout</span>
                                            <p className="font-mono text-grey-400">{cb.timeout}</p>
                                        </div>
                                        <div>
                                            <span className="text-grey-500">Half-Open After</span>
                                            <p className="font-mono text-grey-400">{cb.halfOpenAfter}</p>
                                        </div>
                                    </div>
                                </div>
                            ))}
                        </div>
                    </div>

                    {/* Retry Policies + Backpressure */}
                    <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
                        <div className="gs-card p-5">
                            <h3 className="gs-section-title mb-4 flex items-center gap-2"><RefreshCw size={16} /> Retry Policy Configuration</h3>
                            <div className="bg-grey-900/50 rounded-lg p-4 font-mono text-xs space-y-1 mb-4">
                                <div className="text-grey-500">// Exponential backoff with jitter + deadletter</div>
                                <div className="text-accent-cyan">{'{'}</div>
                                <div className="pl-3 text-grey-300">"strategy": <span className="text-accent-emerald">"exponential_backoff"</span>,</div>
                                <div className="pl-3 text-grey-300">"initialDelay": <span className="text-accent-amber">100</span>, <span className="text-grey-600">// ms</span></div>
                                <div className="pl-3 text-grey-300">"maxDelay": <span className="text-accent-amber">30000</span>, <span className="text-grey-600">// 30s cap</span></div>
                                <div className="pl-3 text-grey-300">"multiplier": <span className="text-accent-amber">2.0</span>,</div>
                                <div className="pl-3 text-grey-300">"jitter": <span className="text-accent-amber">0.25</span>, <span className="text-grey-600">// ±25%</span></div>
                                <div className="pl-3 text-grey-300">"maxAttempts": <span className="text-accent-amber">5</span>,</div>
                                <div className="pl-3 text-grey-300">"retryableErrors": [</div>
                                <div className="pl-6 text-accent-emerald">"ECONNRESET",</div>
                                <div className="pl-6 text-accent-emerald">"ETIMEDOUT",</div>
                                <div className="pl-6 text-accent-emerald">"HTTP_429",</div>
                                <div className="pl-6 text-accent-emerald">"HTTP_503"</div>
                                <div className="pl-3 text-grey-300">],</div>
                                <div className="pl-3 text-grey-300">"deadLetterQueue": <span className="text-accent-emerald">"dlq.failed-requests"</span></div>
                                <div className="text-accent-cyan">{'}'}</div>
                            </div>
                            <div className="space-y-2">
                                <h4 className="text-xs font-semibold text-grey-500 uppercase">Retry Attempt Timeline</h4>
                                {[
                                    { attempt: 1, delay: '100ms', cumulative: '100ms' },
                                    { attempt: 2, delay: '200ms', cumulative: '300ms' },
                                    { attempt: 3, delay: '400ms', cumulative: '700ms' },
                                    { attempt: 4, delay: '800ms', cumulative: '1.5s' },
                                    { attempt: 5, delay: '1.6s', cumulative: '3.1s → DLQ' },
                                ].map(r => (
                                    <div key={r.attempt} className="flex items-center gap-3 text-xs p-2 bg-grey-800/30 rounded">
                                        <span className="w-6 h-6 rounded-full flex items-center justify-center bg-grey-800 text-grey-400 font-bold">{r.attempt}</span>
                                        <span className="text-grey-400 w-16">+{r.delay}</span>
                                        <div className="flex-1"><ProgressBar value={r.attempt * 20} color={r.attempt >= 5 ? 'bg-accent-rose' : 'bg-brand-500'} size="sm" /></div>
                                        <span className="text-grey-500 w-20 text-right">{r.cumulative}</span>
                                    </div>
                                ))}
                            </div>
                        </div>

                        <div className="space-y-6">
                            <div className="gs-card p-5">
                                <h3 className="gs-section-title mb-4 flex items-center gap-2"><Activity size={16} /> Backpressure Mechanisms</h3>
                                <div className="space-y-3">
                                    {[
                                        { mechanism: 'Token Bucket Rate Limiter', scope: 'Per-tenant', config: '1000 req/s, burst: 2000', status: 'Active', icon: <Cpu size={14} /> },
                                        { mechanism: 'Sliding Window Counter', scope: 'Per-API-key', config: '100 req/min per endpoint', status: 'Active', icon: <Clock size={14} /> },
                                        { mechanism: 'Semaphore Bulkhead', scope: 'Per-service', config: 'max 50 concurrent requests', status: 'Active', icon: <Server size={14} /> },
                                        { mechanism: 'Queue Depth Monitor', scope: 'Kafka partitions', config: 'lag threshold: 10K msgs', status: 'Active', icon: <Activity size={14} /> },
                                        { mechanism: 'Adaptive Load Shedding', scope: 'Global', config: 'shed at 85% CPU / 90% mem', status: 'Armed', icon: <AlertTriangle size={14} /> },
                                    ].map(m => (
                                        <div key={m.mechanism} className="p-3 rounded-lg border border-grey-800 flex items-center gap-3">
                                            <span className="text-accent-violet">{m.icon}</span>
                                            <div className="flex-1">
                                                <p className="text-sm font-medium text-grey-200">{m.mechanism}</p>
                                                <p className="text-xs text-grey-500">{m.scope} · {m.config}</p>
                                            </div>
                                            <Badge variant={m.status === 'Active' ? 'success' : 'warning'}>{m.status}</Badge>
                                        </div>
                                    ))}
                                </div>
                            </div>

                            <div className="gs-card p-5">
                                <h3 className="gs-section-title mb-4 flex items-center gap-2"><Shield size={16} /> Bulkhead Isolation</h3>
                                <div className="space-y-3">
                                    {[
                                        { pool: 'Critical Path', services: 'auth, payments', threads: 100, used: 34, priority: 'P0' },
                                        { pool: 'Core Business', services: 'crm, erp, hcm', threads: 200, used: 142, priority: 'P1' },
                                        { pool: 'AI / Enrichment', services: 'ai-orchestrator', threads: 50, used: 38, priority: 'P2' },
                                        { pool: 'Background Jobs', services: 'search, sync, reports', threads: 80, used: 22, priority: 'P3' },
                                    ].map(p => (
                                        <div key={p.pool} className="p-3 bg-grey-800/30 rounded-lg">
                                            <div className="flex items-center justify-between mb-2">
                                                <div>
                                                    <p className="text-sm font-medium text-grey-200">{p.pool}</p>
                                                    <p className="text-xs text-grey-500">{p.services} · {p.used}/{p.threads} threads</p>
                                                </div>
                                                <Badge variant={p.priority === 'P0' ? 'danger' : p.priority === 'P1' ? 'warning' : 'info'}>{p.priority}</Badge>
                                            </div>
                                            <ProgressBar value={(p.used / p.threads) * 100} color={p.used / p.threads > 0.8 ? 'bg-accent-rose' : p.used / p.threads > 0.6 ? 'bg-accent-amber' : 'bg-accent-emerald'} size="sm" />
                                        </div>
                                    ))}
                                </div>
                            </div>
                        </div>
                    </div>

                    {/* Chaos Engineering + SLOs */}
                    <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
                        <div className="gs-card p-5">
                            <h3 className="gs-section-title mb-4 flex items-center gap-2"><Zap size={16} /> Chaos Engineering Experiments</h3>
                            <div className="space-y-3">
                                {[
                                    { name: 'Kill random pod', target: 'any service', frequency: 'Daily', lastRun: '6h ago', result: 'Passed', recovery: '< 2s' },
                                    { name: 'Network partition', target: 'crm ↔ db', frequency: 'Weekly', lastRun: '3 days ago', result: 'Passed', recovery: '< 5s' },
                                    { name: 'CPU spike (90%)', target: 'ai-orchestrator', frequency: 'Weekly', lastRun: '5 days ago', result: 'Passed', recovery: '< 10s' },
                                    { name: 'Dependency failure', target: 'OpenAI API', frequency: 'Bi-weekly', lastRun: '8 days ago', result: 'Passed', recovery: '< 1s (circuit breaker)' },
                                    { name: 'Zone failover', target: 'us-east-1a', frequency: 'Monthly', lastRun: '18 days ago', result: 'Passed', recovery: '< 30s' },
                                    { name: 'Data corruption', target: 'bronze layer', frequency: 'Monthly', lastRun: '22 days ago', result: 'Passed', recovery: '< 2 min (rollback)' },
                                ].map(e => (
                                    <div key={e.name} className="flex items-center justify-between p-3 rounded-lg border border-grey-800">
                                        <div className="flex-1">
                                            <p className="text-sm font-medium text-grey-200">{e.name}</p>
                                            <p className="text-xs text-grey-500">{e.target} · {e.frequency} · Recovery: {e.recovery}</p>
                                        </div>
                                        <div className="flex items-center gap-2">
                                            <span className="text-xs text-grey-500">{e.lastRun}</span>
                                            <Badge variant="success">{e.result}</Badge>
                                        </div>
                                    </div>
                                ))}
                            </div>
                        </div>

                        <div className="gs-card p-5">
                            <h3 className="gs-section-title mb-4 flex items-center gap-2"><BarChart3 size={16} /> SLO Error Budget Tracking</h3>
                            <div className="space-y-4">
                                {[
                                    { slo: 'Availability', target: '99.95%', actual: '99.97%', budget: '21.6 min/mo', remaining: '15.2 min', burnRate: 0.3 },
                                    { slo: 'Latency (P99)', target: '< 500ms', actual: '480ms', budget: '1% of requests', remaining: '0.88%', burnRate: 0.12 },
                                    { slo: 'Error Rate', target: '< 0.1%', actual: '0.04%', budget: '0.1% of requests', remaining: '0.06%', burnRate: 0.4 },
                                    { slo: 'Data Freshness', target: '< 5 min', actual: '2.4 min', budget: '30 min/day staleness', remaining: '18 min', burnRate: 0.4 },
                                ].map(s => (
                                    <div key={s.slo} className="p-3 rounded-lg border border-grey-800">
                                        <div className="flex items-center justify-between mb-2">
                                            <div>
                                                <p className="text-sm font-medium text-grey-200">{s.slo}</p>
                                                <p className="text-xs text-grey-500">Target: {s.target} · Actual: <span className="text-accent-emerald">{s.actual}</span></p>
                                            </div>
                                            <Badge variant={s.burnRate < 0.5 ? 'success' : s.burnRate < 0.8 ? 'warning' : 'danger'}>
                                                {(s.burnRate * 100).toFixed(0)}% burned
                                            </Badge>
                                        </div>
                                        <ProgressBar value={s.burnRate * 100} color={s.burnRate < 0.5 ? 'bg-accent-emerald' : s.burnRate < 0.8 ? 'bg-accent-amber' : 'bg-accent-rose'} size="sm" />
                                        <p className="text-xs text-grey-500 mt-1">Budget remaining: {s.remaining}</p>
                                    </div>
                                ))}
                            </div>
                        </div>
                    </div>
                </div>
            )}
        </div>
    );
}
