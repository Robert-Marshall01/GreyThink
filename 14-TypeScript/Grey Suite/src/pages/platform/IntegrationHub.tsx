import { useState } from 'react';
import { PageHeader, StatCard, Badge, DataTable, Tabs, ProgressBar, MiniSparkline } from '../../components/ui';
import {
    Plug, Webhook, Key, Activity, Settings, Plus,
    CheckCircle, AlertTriangle, Clock, ArrowUpRight,
    Code, Globe, Zap, RefreshCw, Shield, Server
} from 'lucide-react';

const connectors = [
    { name: 'Salesforce', category: 'CRM', direction: 'Bi-directional', events: '4.2M/mo', latency: '120ms', status: 'Healthy', version: 'v3.2' },
    { name: 'Workday', category: 'HCM', direction: 'Bi-directional', events: '1.8M/mo', latency: '180ms', status: 'Healthy', version: 'v2.8' },
    { name: 'Slack', category: 'Collaboration', direction: 'Bi-directional', events: '12M/mo', latency: '45ms', status: 'Healthy', version: 'v4.1' },
    { name: 'Stripe', category: 'Payments', direction: 'Inbound', events: '2.4M/mo', latency: '90ms', status: 'Healthy', version: 'v3.0' },
    { name: 'Jira', category: 'Project Mgmt', direction: 'Bi-directional', events: '800K/mo', latency: '150ms', status: 'Degraded', version: 'v2.4' },
    { name: 'HubSpot', category: 'Marketing', direction: 'Outbound', events: '1.2M/mo', latency: '200ms', status: 'Healthy', version: 'v2.1' },
    { name: 'Snowflake', category: 'Data', direction: 'Outbound', events: '3.6M/mo', latency: '250ms', status: 'Healthy', version: 'v1.8' },
    { name: 'GitHub', category: 'DevOps', direction: 'Inbound', events: '420K/mo', latency: '80ms', status: 'Healthy', version: 'v3.5' },
];

const webhooks = [
    { endpoint: 'https://api.acme.com/webhooks/gs', events: ['lead.created', 'deal.closed'], signing: 'HMAC-SHA256', retries: 3, status: 'Active', successRate: 99.4 },
    { endpoint: 'https://hooks.globex.io/ingest', events: ['invoice.sent', 'payment.received'], signing: 'HMAC-SHA256', retries: 3, status: 'Active', successRate: 98.1 },
    { endpoint: 'https://n8n.initech.dev/webhook/gs', events: ['employee.onboarded'], signing: 'HMAC-SHA256', retries: 5, status: 'Active', successRate: 99.8 },
    { endpoint: 'https://zapier.com/hooks/catch/12345', events: ['ticket.resolved'], signing: 'None', retries: 3, status: 'Paused', successRate: 95.2 },
];

const oauthApps = [
    { name: 'Analytics Dashboard Pro', developer: 'DataViz Inc.', scopes: ['read:analytics', 'read:reports'], users: 842, status: 'Approved' },
    { name: 'HR Sync Tool', developer: 'PeopleOps Co.', scopes: ['read:hcm', 'write:hcm'], users: 124, status: 'Approved' },
    { name: 'Sales Accelerator', developer: 'RevOps Ltd.', scopes: ['read:crm', 'write:crm'], users: 268, status: 'Approved' },
    { name: 'Custom Reporting', developer: 'Internal', scopes: ['read:*'], users: 42, status: 'Approved' },
    { name: 'Expense Bot', developer: 'FinTech Corp.', scopes: ['read:finance', 'write:finance'], users: 0, status: 'In Review' },
];

export function IntegrationHub() {
    const [activeTab, setActiveTab] = useState('Connectors');

    return (
        <div>
            <PageHeader
                title="Integration Hub"
                subtitle="Connectors, webhooks, OAuth marketplace & ETL pipelines"
                actions={
                    <div className="flex gap-2">
                        <button className="gs-btn-secondary flex items-center gap-2"><Code size={16} /> Connector SDK</button>
                        <button className="gs-btn-primary flex items-center gap-2"><Plus size={16} /> Add Integration</button>
                    </div>
                }
            />

            <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-4 gap-4 mb-6">
                <StatCard label="Active Connectors" value="8" change="7 healthy" changeType="positive" icon={<Plug size={18} />} />
                <StatCard label="Events Processed" value="26.4M/mo" change="+18% MoM" changeType="positive" icon={<Activity size={18} />} />
                <StatCard label="Webhook Success" value="98.6%" change="Across all endpoints" changeType="positive" icon={<Webhook size={18} />} />
                <StatCard label="OAuth Apps" value="5" change="1 in review" changeType="neutral" icon={<Key size={18} />} />
            </div>

            <Tabs tabs={['Connectors', 'Webhooks', 'OAuth Marketplace', 'ETL Pipelines', 'Rate Limits', 'Health']} active={activeTab} onChange={setActiveTab} />

            {activeTab === 'Connectors' && (
                <div className="gs-card p-5">
                    <h3 className="gs-section-title mb-4 flex items-center gap-2"><Plug size={16} /> Prebuilt Connectors</h3>
                    <DataTable
                        columns={['Connector', 'Category', 'Direction', 'Events/mo', 'Latency', 'Version', 'Status']}
                        rows={connectors.map(c => [
                            <span className="font-medium text-grey-200">{c.name}</span>,
                            <Badge variant="info">{c.category}</Badge>,
                            <Badge variant={c.direction === 'Bi-directional' ? 'purple' : c.direction === 'Inbound' ? 'success' : 'warning'}>{c.direction}</Badge>,
                            c.events,
                            <span className="font-mono text-xs">{c.latency}</span>,
                            <span className="text-xs text-grey-400">{c.version}</span>,
                            <Badge variant={c.status === 'Healthy' ? 'success' : 'warning'}>{c.status}</Badge>,
                        ])}
                    />
                </div>
            )}

            {activeTab === 'Webhooks' && (
                <div className="space-y-6">
                    <div className="gs-card p-5">
                        <h3 className="gs-section-title mb-4 flex items-center gap-2"><Webhook size={16} /> Webhook Endpoints</h3>
                        <div className="space-y-3">
                            {webhooks.map((w, i) => (
                                <div key={i} className="p-4 rounded-lg border border-grey-800">
                                    <div className="flex items-center justify-between mb-2">
                                        <span className="font-mono text-xs text-accent-cyan">{w.endpoint}</span>
                                        <Badge variant={w.status === 'Active' ? 'success' : 'warning'}>{w.status}</Badge>
                                    </div>
                                    <div className="flex items-center gap-4 text-xs text-grey-500">
                                        <span>Events: {w.events.join(', ')}</span>
                                        <span>Signing: {w.signing}</span>
                                        <span>Retries: {w.retries}</span>
                                        <span className={w.successRate >= 99 ? 'text-accent-emerald' : 'text-accent-amber'}>Success: {w.successRate}%</span>
                                    </div>
                                </div>
                            ))}
                        </div>
                    </div>
                    <div className="gs-card p-5">
                        <h3 className="gs-section-title mb-3">Webhook Signing & Retry Policy</h3>
                        <div className="bg-grey-900/50 rounded-lg p-4 font-mono text-xs space-y-1">
                            <div className="text-grey-500">// Webhook delivery with HMAC signing</div>
                            <div className="text-accent-cyan">POST {'{endpoint}'}</div>
                            <div className="text-grey-400">Headers:</div>
                            <div className="pl-4 text-accent-amber">X-GS-Signature: sha256=hmac(payload, secret)</div>
                            <div className="pl-4 text-accent-amber">X-GS-Timestamp: 1739180400</div>
                            <div className="pl-4 text-accent-amber">X-GS-Event: lead.created</div>
                            <div className="text-grey-400 mt-2">Retry: exponential backoff (1s, 5s, 25s) up to 3 attempts</div>
                            <div className="text-grey-400">Dead letter queue after max retries</div>
                        </div>
                    </div>
                </div>
            )}

            {activeTab === 'OAuth Marketplace' && (
                <div className="gs-card p-5">
                    <h3 className="gs-section-title mb-4 flex items-center gap-2"><Key size={16} /> OAuth App Marketplace</h3>
                    <div className="space-y-3">
                        {oauthApps.map(app => (
                            <div key={app.name} className="p-4 rounded-lg border border-grey-800 flex items-center justify-between">
                                <div className="flex-1">
                                    <div className="flex items-center gap-2">
                                        <p className="text-sm font-medium text-grey-200">{app.name}</p>
                                        <Badge variant={app.status === 'Approved' ? 'success' : 'warning'}>{app.status}</Badge>
                                    </div>
                                    <p className="text-xs text-grey-500 mt-1">Developer: {app.developer} · {app.users} users</p>
                                    <div className="flex gap-1 mt-2">
                                        {app.scopes.map(s => (
                                            <span key={s} className="font-mono text-xs bg-grey-800/50 text-grey-400 px-2 py-0.5 rounded">{s}</span>
                                        ))}
                                    </div>
                                </div>
                                <button className="gs-btn-ghost text-xs flex items-center gap-1"><Settings size={12} /> Manage</button>
                            </div>
                        ))}
                    </div>
                </div>
            )}

            {activeTab === 'ETL Pipelines' && (
                <div className="gs-card p-5">
                    <h3 className="gs-section-title mb-4 flex items-center gap-2"><Zap size={16} /> Integration ETL Pipelines</h3>
                    <div className="space-y-3">
                        {[
                            { name: 'Salesforce → CRM Sync', schedule: 'Every 15 min', lastRun: '4 min ago', records: '2,480', duration: '12s', status: 'Success' },
                            { name: 'Workday → HCM Import', schedule: 'Every 1 hour', lastRun: '22 min ago', records: '842', duration: '8s', status: 'Success' },
                            { name: 'Stripe → Finance Reconciliation', schedule: 'Every 30 min', lastRun: '8 min ago', records: '1,240', duration: '6s', status: 'Success' },
                            { name: 'Slack → Activity Log', schedule: 'Real-time', lastRun: '1 min ago', records: '4,200', duration: '< 1s', status: 'Running' },
                            { name: 'HubSpot → Marketing Export', schedule: 'Daily 2AM', lastRun: '8h ago', records: '18,400', duration: '2m 14s', status: 'Success' },
                            { name: 'Snowflake → BI Export', schedule: 'Every 6 hours', lastRun: '2h ago', records: '84,000', duration: '4m 32s', status: 'Success' },
                        ].map(p => (
                            <div key={p.name} className="p-3 rounded-lg border border-grey-800 flex items-center justify-between">
                                <div>
                                    <p className="text-sm font-medium text-grey-200">{p.name}</p>
                                    <p className="text-xs text-grey-500">{p.schedule} · Last: {p.lastRun} · {p.records} records · {p.duration}</p>
                                </div>
                                <Badge variant={p.status === 'Success' ? 'success' : p.status === 'Running' ? 'info' : 'danger'}>{p.status}</Badge>
                            </div>
                        ))}
                    </div>
                </div>
            )}

            {activeTab === 'Rate Limits' && (
                <div className="gs-card p-5">
                    <h3 className="gs-section-title mb-4 flex items-center gap-2"><Shield size={16} /> Rate Limiting per Integration</h3>
                    <div className="space-y-4">
                        {connectors.map(c => (
                            <div key={c.name} className="p-3 rounded-lg bg-grey-800/30">
                                <div className="flex items-center justify-between mb-2">
                                    <span className="text-sm text-grey-200">{c.name}</span>
                                    <span className="text-xs text-grey-500">{c.events} events/mo</span>
                                </div>
                                <ProgressBar value={Math.random() * 60 + 20} color="bg-brand-500" size="sm" />
                                <div className="flex justify-between mt-1 text-xs text-grey-500">
                                    <span>1,000 req/min limit</span>
                                    <span>{Math.floor(Math.random() * 400 + 200)} req/min current</span>
                                </div>
                            </div>
                        ))}
                    </div>
                </div>
            )}

            {activeTab === 'Health' && (
                <div className="space-y-6">
                    <div className="gs-card p-5">
                        <h3 className="gs-section-title mb-4 flex items-center gap-2"><Activity size={16} /> Integration Health Dashboard</h3>
                        <MiniSparkline data={[96, 97, 98, 97, 99, 98, 99, 98, 99, 97, 98, 99]} height={80} />
                        <div className="grid grid-cols-4 gap-4 mt-4 text-center">
                            <div><p className="text-lg font-bold text-accent-emerald">99.1%</p><p className="text-xs text-grey-500">Avg Uptime</p></div>
                            <div><p className="text-lg font-bold text-grey-100">142ms</p><p className="text-xs text-grey-500">Avg Latency</p></div>
                            <div><p className="text-lg font-bold text-grey-100">26.4M</p><p className="text-xs text-grey-500">Events/mo</p></div>
                            <div><p className="text-lg font-bold text-grey-100">0</p><p className="text-xs text-grey-500">Dead Letters</p></div>
                        </div>
                    </div>
                    <div className="gs-card p-5">
                        <h3 className="gs-section-title mb-4">Connector Status</h3>
                        <div className="grid grid-cols-2 lg:grid-cols-4 gap-3">
                            {connectors.map(c => (
                                <div key={c.name} className="p-3 rounded-lg border border-grey-800 text-center">
                                    <div className={`w-3 h-3 rounded-full mx-auto mb-2 ${c.status === 'Healthy' ? 'bg-accent-emerald' : 'bg-accent-amber dot-pulse'}`} />
                                    <p className="text-sm font-medium text-grey-200">{c.name}</p>
                                    <p className="text-xs text-grey-500">{c.latency}</p>
                                </div>
                            ))}
                        </div>
                    </div>
                </div>
            )}
        </div>
    );
}
