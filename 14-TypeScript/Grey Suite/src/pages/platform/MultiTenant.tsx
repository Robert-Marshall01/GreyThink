import { useState } from 'react';
import { PageHeader, StatCard, Badge, DataTable, ProgressBar, Tabs, MiniSparkline } from '../../components/ui';
import {
    Building2, Shield, Key, Flag, Activity, Database,
    Plus, RefreshCw, Settings, Globe, Lock, Server,
    CheckCircle, AlertTriangle, Layers, Cpu
} from 'lucide-react';

const tenants = [
    { id: 'TNT-001', name: 'Acme Corporation', plan: 'Enterprise', users: 1284, storage: '2.4 TB', status: 'Active', region: 'us-east-1', schema: 'acme_corp', mrr: '$48,500' },
    { id: 'TNT-002', name: 'Globex Industries', plan: 'Enterprise', users: 876, storage: '1.8 TB', status: 'Active', region: 'eu-west-1', schema: 'globex_ind', mrr: '$36,200' },
    { id: 'TNT-003', name: 'Initech LLC', plan: 'Business', users: 342, storage: '680 GB', status: 'Active', region: 'us-west-2', schema: 'initech_llc', mrr: '$12,800' },
    { id: 'TNT-004', name: 'Umbrella Corp', plan: 'Enterprise', users: 2140, storage: '4.1 TB', status: 'Active', region: 'ap-southeast-1', schema: 'umbrella_corp', mrr: '$72,000' },
    { id: 'TNT-005', name: 'Stark Industries', plan: 'Enterprise', users: 1560, storage: '3.2 TB', status: 'Provisioning', region: 'us-east-1', schema: 'stark_ind', mrr: '$55,000' },
    { id: 'TNT-006', name: 'Wayne Enterprises', plan: 'Business', users: 520, storage: '920 GB', status: 'Active', region: 'eu-central-1', schema: 'wayne_ent', mrr: '$18,400' },
];

const provisioningSteps = [
    { step: 'Create DB Schema', status: 'complete', duration: '2.4s' },
    { step: 'Seed Default Config', status: 'complete', duration: '1.1s' },
    { step: 'Generate Encryption Keys', status: 'complete', duration: '0.8s' },
    { step: 'Assign Feature Flags', status: 'complete', duration: '0.3s' },
    { step: 'Configure Service Mesh', status: 'running', duration: '—' },
    { step: 'Initialize Metering', status: 'pending', duration: '—' },
    { step: 'Health Check', status: 'pending', duration: '—' },
];

const featureFlags = [
    { flag: 'ai_orchestration', description: 'AI Orchestration Layer', tenants: 4, global: true },
    { flag: 'advanced_analytics', description: 'Advanced BI & Analytics', tenants: 6, global: true },
    { flag: 'workflow_engine_v2', description: 'Workflow Engine v2 (beta)', tenants: 2, global: false },
    { flag: 'knowledge_graph', description: 'Knowledge Graph Module', tenants: 3, global: false },
    { flag: 'simulation_engine', description: 'Simulation Engine', tenants: 1, global: false },
    { flag: 'custom_branding', description: 'White-label Branding', tenants: 4, global: false },
];

export function MultiTenant() {
    const [activeTab, setActiveTab] = useState('Registry');

    return (
        <div>
            <PageHeader
                title="Multi-Tenant Architecture"
                subtitle="Tenant isolation, provisioning, and cross-tenant analytics"
                actions={
                    <button className="gs-btn-primary flex items-center gap-2">
                        <Plus size={16} /> Provision Tenant
                    </button>
                }
            />

            <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-4 gap-4 mb-6">
                <StatCard label="Active Tenants" value="6" change="+2 this quarter" changeType="positive" icon={<Building2 size={18} />} />
                <StatCard label="Total Users" value="6,722" change="+18% MoM" changeType="positive" icon={<Globe size={18} />} />
                <StatCard label="Total MRR" value="$242,900" change="+12.4%" changeType="positive" icon={<Activity size={18} />} />
                <StatCard label="Isolation Score" value="98.7%" change="SOC2 compliant" changeType="positive" icon={<Shield size={18} />} />
            </div>

            <Tabs tabs={['Registry', 'Provisioning', 'Feature Flags', 'Metering', 'Encryption', 'Service Mesh']} active={activeTab} onChange={setActiveTab} />

            {activeTab === 'Registry' && (
                <div className="space-y-6">
                    <div className="gs-card p-5">
                        <h3 className="gs-section-title mb-4 flex items-center gap-2"><Database size={16} /> Tenant Registry</h3>
                        <DataTable
                            columns={['Tenant ID', 'Organization', 'Plan', 'Users', 'Storage', 'Region', 'Schema', 'MRR', 'Status']}
                            rows={tenants.map(t => [
                                <span className="font-mono text-xs text-grey-400">{t.id}</span>,
                                <span className="font-medium text-grey-200">{t.name}</span>,
                                <Badge variant={t.plan === 'Enterprise' ? 'purple' : 'info'}>{t.plan}</Badge>,
                                t.users.toLocaleString(),
                                t.storage,
                                <span className="font-mono text-xs">{t.region}</span>,
                                <span className="font-mono text-xs text-accent-cyan">{t.schema}</span>,
                                <span className="text-accent-emerald font-medium">{t.mrr}</span>,
                                <Badge variant={t.status === 'Active' ? 'success' : 'warning'}>{t.status}</Badge>,
                            ])}
                        />
                    </div>

                    {/* Tenant-Aware Routing */}
                    <div className="gs-card p-5">
                        <h3 className="gs-section-title mb-4 flex items-center gap-2"><Globe size={16} /> Tenant-Aware Routing</h3>
                        <div className="bg-grey-900/50 rounded-lg p-4 font-mono text-sm space-y-2">
                            <div className="text-grey-500">// Request flow with tenant_id propagation</div>
                            <div className="text-accent-cyan">GET /api/v1/crm/leads</div>
                            <div className="text-grey-400">Headers:</div>
                            <div className="pl-4 text-accent-amber">X-Tenant-ID: TNT-001</div>
                            <div className="pl-4 text-accent-amber">Authorization: Bearer {'<jwt with tenant claim>'}</div>
                            <div className="text-grey-400 mt-2">→ API Gateway validates tenant_id in JWT</div>
                            <div className="text-grey-400">→ Service mesh routes to tenant-scoped partition</div>
                            <div className="text-grey-400">→ DB query scoped: <span className="text-accent-emerald">WHERE tenant_id = 'TNT-001'</span></div>
                        </div>
                    </div>
                </div>
            )}

            {activeTab === 'Provisioning' && (
                <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
                    <div className="gs-card p-5">
                        <h3 className="gs-section-title mb-4 flex items-center gap-2"><Layers size={16} /> Provisioning Pipeline</h3>
                        <div className="space-y-3">
                            {provisioningSteps.map((s, i) => (
                                <div key={i} className="flex items-center gap-3 p-3 rounded-lg border border-grey-800">
                                    <div className="w-8 h-8 rounded-full flex items-center justify-center text-sm font-bold bg-grey-800 text-grey-400">
                                        {i + 1}
                                    </div>
                                    <div className="flex-1">
                                        <p className="text-sm font-medium text-grey-200">{s.step}</p>
                                        <p className="text-xs text-grey-500">{s.duration}</p>
                                    </div>
                                    {s.status === 'complete' && <CheckCircle size={16} className="text-accent-emerald" />}
                                    {s.status === 'running' && <RefreshCw size={16} className="text-accent-amber animate-spin" />}
                                    {s.status === 'pending' && <div className="w-4 h-4 rounded-full border-2 border-grey-700" />}
                                </div>
                            ))}
                        </div>
                    </div>

                    <div className="space-y-6">
                        <div className="gs-card p-5">
                            <h3 className="gs-section-title mb-4">Schema Strategy</h3>
                            <div className="space-y-3">
                                {[
                                    { module: 'ERP / Finance', strategy: 'Schema-per-tenant', isolation: 'Full', icon: <Database size={14} /> },
                                    { module: 'CRM', strategy: 'Row-level security', isolation: 'Logical', icon: <Lock size={14} /> },
                                    { module: 'HCM / Payroll', strategy: 'Schema-per-tenant', isolation: 'Full', icon: <Database size={14} /> },
                                    { module: 'Analytics', strategy: 'Aggregated read-replica', isolation: 'Shared', icon: <Server size={14} /> },
                                    { module: 'Chat / Collab', strategy: 'Row-level security', isolation: 'Logical', icon: <Lock size={14} /> },
                                ].map(s => (
                                    <div key={s.module} className="flex items-center justify-between p-3 rounded-lg bg-grey-800/30">
                                        <div className="flex items-center gap-2 text-sm">
                                            <span className="text-grey-500">{s.icon}</span>
                                            <span className="text-grey-300">{s.module}</span>
                                        </div>
                                        <div className="flex items-center gap-2">
                                            <Badge variant={s.isolation === 'Full' ? 'success' : s.isolation === 'Logical' ? 'info' : 'warning'}>{s.isolation}</Badge>
                                            <span className="text-xs text-grey-500">{s.strategy}</span>
                                        </div>
                                    </div>
                                ))}
                            </div>
                        </div>

                        <div className="gs-card p-5">
                            <h3 className="gs-section-title mb-4">Cross-Tenant Analytics</h3>
                            <div className="text-xs text-grey-500 mb-3">Read-only aggregated warehouse for platform-level insights</div>
                            <MiniSparkline data={[120, 145, 162, 178, 195, 210, 228, 245, 262, 280, 298, 320]} height={80} />
                            <div className="grid grid-cols-3 gap-3 mt-3 text-center">
                                <div className="p-2 bg-grey-800/30 rounded"><p className="text-sm font-bold text-grey-200">6</p><p className="text-xs text-grey-500">Tenants</p></div>
                                <div className="p-2 bg-grey-800/30 rounded"><p className="text-sm font-bold text-grey-200">14.2B</p><p className="text-xs text-grey-500">Events/mo</p></div>
                                <div className="p-2 bg-grey-800/30 rounded"><p className="text-sm font-bold text-grey-200">320ms</p><p className="text-xs text-grey-500">Avg Query</p></div>
                            </div>
                        </div>
                    </div>
                </div>
            )}

            {activeTab === 'Feature Flags' && (
                <div className="gs-card p-5">
                    <h3 className="gs-section-title mb-4 flex items-center gap-2"><Flag size={16} /> Tenant-Level Feature Flags</h3>
                    <DataTable
                        columns={['Flag Key', 'Description', 'Enabled Tenants', 'Scope', 'Status']}
                        rows={featureFlags.map(f => [
                            <span className="font-mono text-xs text-accent-cyan">{f.flag}</span>,
                            <span className="text-grey-300">{f.description}</span>,
                            <span className="font-medium">{f.tenants} / 6</span>,
                            <Badge variant={f.global ? 'success' : 'warning'}>{f.global ? 'GA' : 'Beta'}</Badge>,
                            <button className="text-xs text-brand-400 hover:text-brand-300">Configure</button>,
                        ])}
                    />
                </div>
            )}

            {activeTab === 'Metering' && (
                <div className="space-y-6">
                    <div className="gs-card p-5">
                        <h3 className="gs-section-title mb-4 flex items-center gap-2"><Activity size={16} /> Usage Metering by Tenant</h3>
                        <div className="space-y-4">
                            {tenants.filter(t => t.status === 'Active').map(t => (
                                <div key={t.id} className="p-4 rounded-lg border border-grey-800">
                                    <div className="flex items-center justify-between mb-3">
                                        <div>
                                            <p className="text-sm font-medium text-grey-200">{t.name}</p>
                                            <p className="text-xs text-grey-500">{t.id} · {t.region}</p>
                                        </div>
                                        <span className="text-sm font-bold text-accent-emerald">{t.mrr}</span>
                                    </div>
                                    <div className="grid grid-cols-4 gap-3">
                                        {[
                                            { label: 'API Calls', value: `${(Math.random() * 50 + 10).toFixed(1)}M`, pct: Math.random() * 80 + 20 },
                                            { label: 'Storage', value: t.storage, pct: Math.random() * 60 + 30 },
                                            { label: 'Compute', value: `${(Math.random() * 200 + 50).toFixed(0)} vCPU-hrs`, pct: Math.random() * 70 + 20 },
                                            { label: 'AI Tokens', value: `${(Math.random() * 5 + 1).toFixed(1)}M`, pct: Math.random() * 90 + 10 },
                                        ].map(m => (
                                            <div key={m.label}>
                                                <div className="flex justify-between text-xs mb-1">
                                                    <span className="text-grey-500">{m.label}</span>
                                                    <span className="text-grey-400">{m.value}</span>
                                                </div>
                                                <ProgressBar value={m.pct} color={m.pct > 80 ? 'bg-accent-rose' : m.pct > 60 ? 'bg-accent-amber' : 'bg-brand-500'} size="sm" />
                                            </div>
                                        ))}
                                    </div>
                                </div>
                            ))}
                        </div>
                    </div>
                </div>
            )}

            {activeTab === 'Encryption' && (
                <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
                    <div className="gs-card p-5">
                        <h3 className="gs-section-title mb-4 flex items-center gap-2"><Key size={16} /> Per-Tenant Encryption Keys</h3>
                        <div className="space-y-3">
                            {tenants.map(t => (
                                <div key={t.id} className="flex items-center justify-between p-3 rounded-lg border border-grey-800">
                                    <div className="flex items-center gap-3">
                                        <Lock size={14} className="text-accent-emerald" />
                                        <div>
                                            <p className="text-sm text-grey-200">{t.name}</p>
                                            <p className="text-xs text-grey-500 font-mono">AES-256-GCM · Rotated 12d ago</p>
                                        </div>
                                    </div>
                                    <div className="flex items-center gap-2">
                                        <Badge variant="success">Active</Badge>
                                        <button className="text-xs text-brand-400 hover:text-brand-300 flex items-center gap-1">
                                            <RefreshCw size={12} /> Rotate
                                        </button>
                                    </div>
                                </div>
                            ))}
                        </div>
                    </div>
                    <div className="gs-card p-5">
                        <h3 className="gs-section-title mb-4">Key Management Policy</h3>
                        <div className="space-y-3">
                            {[
                                { policy: 'Rotation Interval', value: '90 days', status: 'Enforced' },
                                { policy: 'Algorithm', value: 'AES-256-GCM', status: 'Enforced' },
                                { policy: 'Key Derivation', value: 'HKDF-SHA256', status: 'Enforced' },
                                { policy: 'HSM Backing', value: 'AWS CloudHSM', status: 'Enabled' },
                                { policy: 'Envelope Encryption', value: 'DEK + KEK', status: 'Enforced' },
                                { policy: 'Cross-Tenant Access', value: 'Denied', status: 'Enforced' },
                            ].map(p => (
                                <div key={p.policy} className="flex items-center justify-between p-2 rounded bg-grey-800/30">
                                    <span className="text-sm text-grey-300">{p.policy}</span>
                                    <div className="flex items-center gap-2">
                                        <span className="text-xs font-mono text-grey-400">{p.value}</span>
                                        <Badge variant="success">{p.status}</Badge>
                                    </div>
                                </div>
                            ))}
                        </div>
                    </div>
                </div>
            )}

            {activeTab === 'Service Mesh' && (
                <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
                    <div className="gs-card p-5">
                        <h3 className="gs-section-title mb-4 flex items-center gap-2"><Cpu size={16} /> Tenant-Aware Service Mesh</h3>
                        <div className="bg-grey-900/50 rounded-lg p-4 text-sm space-y-3">
                            <div className="flex items-center gap-2 text-accent-cyan"><Server size={14} /> Istio / Linkerd Sidecar Pattern</div>
                            <div className="pl-6 space-y-2 text-grey-400">
                                <p>• mTLS between all services with tenant-scoped certificates</p>
                                <p>• Traffic routing based on <span className="font-mono text-accent-amber">X-Tenant-ID</span> header</p>
                                <p>• Per-tenant rate limiting and circuit breaking</p>
                                <p>• Tenant-level observability with distributed tracing</p>
                                <p>• Automatic retry with exponential backoff per tenant SLA</p>
                            </div>
                        </div>
                        <div className="mt-4 grid grid-cols-2 gap-3">
                            {[
                                { label: 'Active Sidecars', value: '248' },
                                { label: 'mTLS Coverage', value: '100%' },
                                { label: 'Avg Latency Overhead', value: '1.2ms' },
                                { label: 'Policy Rules', value: '86' },
                            ].map(m => (
                                <div key={m.label} className="p-3 bg-grey-800/30 rounded-lg text-center">
                                    <p className="text-lg font-bold text-grey-100">{m.value}</p>
                                    <p className="text-xs text-grey-500">{m.label}</p>
                                </div>
                            ))}
                        </div>
                    </div>
                    <div className="gs-card p-5">
                        <h3 className="gs-section-title mb-4">Tenant Traffic Distribution</h3>
                        <MiniSparkline data={[42, 58, 52, 68, 62, 74, 71, 82, 78, 88, 84, 92]} height={100} />
                        <div className="space-y-3 mt-4">
                            {tenants.filter(t => t.status === 'Active').slice(0, 4).map((t, i) => (
                                <div key={t.id} className="flex items-center gap-3">
                                    <span className="text-xs text-grey-500 w-20 truncate">{t.name}</span>
                                    <div className="flex-1"><ProgressBar value={[35, 25, 15, 12][i]} color={['bg-brand-500', 'bg-accent-emerald', 'bg-accent-violet', 'bg-accent-cyan'][i]} size="sm" /></div>
                                    <span className="text-xs text-grey-400 w-8 text-right">{[35, 25, 15, 12][i]}%</span>
                                </div>
                            ))}
                        </div>
                    </div>
                </div>
            )}
        </div>
    );
}
