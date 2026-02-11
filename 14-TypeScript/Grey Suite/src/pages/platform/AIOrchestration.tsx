import { useState } from 'react';
import { PageHeader, StatCard, Badge, DataTable, Tabs, ProgressBar, MiniSparkline } from '../../components/ui';
import {
    Bot, Zap, Eye, Shield, Users, FileText,
    Play, Pause, AlertTriangle, CheckCircle, Clock,
    GitBranch, Cpu, DollarSign, TrendingUp, Activity
} from 'lucide-react';

const agents = [
    { name: 'Lead Qualifier', model: 'GPT-4o', triggers: 'New lead created', runs: 12842, success: 98.4, cost: '$284', status: 'Active', tenant: 'All', drift: 0.02 },
    { name: 'Invoice Processor', model: 'Claude 3.5', triggers: 'Invoice uploaded', runs: 8421, success: 99.1, cost: '$168', status: 'Active', tenant: 'All', drift: 0.01 },
    { name: 'Meeting Summarizer', model: 'GPT-4o', triggers: 'Meeting ended', runs: 4218, success: 97.8, cost: '$126', status: 'Active', tenant: 'All', drift: 0.03 },
    { name: 'Resume Screener', model: 'Claude 3.5', triggers: 'Application received', runs: 2841, success: 96.2, cost: '$85', status: 'Active', tenant: 'TNT-001', drift: 0.05 },
    { name: 'Anomaly Detector', model: 'Custom LSTM', triggers: 'Metric threshold', runs: 142000, success: 99.8, cost: '$42', status: 'Active', tenant: 'All', drift: 0.01 },
    { name: 'Contract Analyzer', model: 'GPT-4o', triggers: 'Manual', runs: 842, success: 94.6, cost: '$210', status: 'Paused', tenant: 'TNT-002', drift: 0.08 },
];

const promptTemplates = [
    { id: 'PT-001', name: 'Lead Qualification', version: 'v3.2', model: 'GPT-4o', tokens: '~1,200', lastUpdated: '3 days ago', uses: 12842 },
    { id: 'PT-002', name: 'Invoice Extraction', version: 'v2.8', model: 'Claude 3.5', tokens: '~800', lastUpdated: '1 week ago', uses: 8421 },
    { id: 'PT-003', name: 'Meeting Summary', version: 'v4.1', model: 'GPT-4o', tokens: '~2,400', lastUpdated: '2 days ago', uses: 4218 },
    { id: 'PT-004', name: 'Resume Screening', version: 'v1.6', model: 'Claude 3.5', tokens: '~1,600', lastUpdated: '5 days ago', uses: 2841 },
    { id: 'PT-005', name: 'Contract Review', version: 'v2.0', model: 'GPT-4o', tokens: '~3,200', lastUpdated: '1 day ago', uses: 842 },
];

const guardrails = [
    { name: 'PII Detection', type: 'Input Filter', status: 'Active', blocks: 142, lastTriggered: '4h ago' },
    { name: 'Toxicity Filter', type: 'Output Filter', status: 'Active', blocks: 28, lastTriggered: '12h ago' },
    { name: 'Cost Limit', type: 'Budget Guard', status: 'Active', blocks: 3, lastTriggered: '2d ago' },
    { name: 'Hallucination Check', type: 'Output Validator', status: 'Active', blocks: 84, lastTriggered: '1h ago' },
    { name: 'Schema Validator', type: 'Output Filter', status: 'Active', blocks: 16, lastTriggered: '6h ago' },
    { name: 'Rate Limiter', type: 'Throttle', status: 'Active', blocks: 412, lastTriggered: '30m ago' },
];

export function AIOrchestration() {
    const [activeTab, setActiveTab] = useState('Agents');

    return (
        <div>
            <PageHeader
                title="AI Orchestration Layer"
                subtitle="Multi-agent workflows, prompt management, guardrails & observability"
                actions={
                    <div className="flex gap-2">
                        <button className="gs-btn-secondary flex items-center gap-2"><Eye size={16} /> Observability</button>
                        <button className="gs-btn-primary flex items-center gap-2"><Bot size={16} /> Deploy Agent</button>
                    </div>
                }
            />

            <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-4 gap-4 mb-6">
                <StatCard label="Active Agents" value="5" change="6 total" changeType="neutral" icon={<Bot size={18} />} />
                <StatCard label="Runs Today" value="4,218" change="+22% vs yesterday" changeType="positive" icon={<Zap size={18} />} />
                <StatCard label="Avg Success Rate" value="97.6%" change="Above 95% SLA" changeType="positive" icon={<CheckCircle size={18} />} />
                <StatCard label="AI Spend (MTD)" value="$915" change="$0.04 avg/run" changeType="neutral" icon={<DollarSign size={18} />} />
            </div>

            <Tabs tabs={['Agents', 'Prompt Registry', 'Guardrails', 'Human-in-Loop', 'Observability', 'Cost Tracking']} active={activeTab} onChange={setActiveTab} />

            {activeTab === 'Agents' && (
                <div className="space-y-6">
                    <div className="gs-card p-5">
                        <h3 className="gs-section-title mb-4 flex items-center gap-2"><Bot size={16} /> Multi-Agent Registry</h3>
                        <DataTable
                            columns={['Agent', 'Model', 'Trigger', 'Runs', 'Success', 'Drift', 'Tenant', 'Cost (30d)', 'Status']}
                            rows={agents.map(a => [
                                <span className="font-medium text-grey-200">{a.name}</span>,
                                <Badge variant="purple">{a.model}</Badge>,
                                <span className="text-xs text-grey-400">{a.triggers}</span>,
                                a.runs.toLocaleString(),
                                <span className={a.success >= 97 ? 'text-accent-emerald' : 'text-accent-amber'}>{a.success}%</span>,
                                <span className={`text-xs ${a.drift > 0.05 ? 'text-accent-rose' : 'text-grey-400'}`}>{(a.drift * 100).toFixed(1)}%</span>,
                                <span className="font-mono text-xs">{a.tenant}</span>,
                                a.cost,
                                <Badge variant={a.status === 'Active' ? 'success' : 'warning'}>{a.status}</Badge>,
                            ])}
                        />
                    </div>

                    <div className="gs-card p-5">
                        <h3 className="gs-section-title mb-4">Agent Schema Definition</h3>
                        <div className="bg-grey-900/50 rounded-lg p-4 font-mono text-xs space-y-1">
                            <div className="text-grey-500">// Each agent declares its contract</div>
                            <div className="text-accent-cyan">{'{'}</div>
                            <div className="pl-3 text-grey-400">"agent_id": <span className="text-accent-emerald">"lead_qualifier_v3"</span>,</div>
                            <div className="pl-3 text-grey-400">"capabilities": [<span className="text-accent-emerald">"classify"</span>, <span className="text-accent-emerald">"score"</span>, <span className="text-accent-emerald">"enrich"</span>],</div>
                            <div className="pl-3 text-grey-400">"triggers": [<span className="text-accent-emerald">"event:lead.created"</span>],</div>
                            <div className="pl-3 text-grey-400">"required_context": [<span className="text-accent-emerald">"lead.company"</span>, <span className="text-accent-emerald">"lead.source"</span>],</div>
                            <div className="pl-3 text-grey-400">"output_schema": {'{'}</div>
                            <div className="pl-6 text-grey-400">"score": <span className="text-accent-amber">"number(0-100)"</span>,</div>
                            <div className="pl-6 text-grey-400">"tier": <span className="text-accent-amber">"enum(hot,warm,cold)"</span>,</div>
                            <div className="pl-6 text-grey-400">"notes": <span className="text-accent-amber">"string"</span></div>
                            <div className="pl-3 text-grey-400">{'}'},</div>
                            <div className="pl-3 text-grey-400">"communication": <span className="text-accent-emerald">"event_bus"</span></div>
                            <div className="text-accent-cyan">{'}'}</div>
                        </div>
                    </div>
                </div>
            )}

            {activeTab === 'Prompt Registry' && (
                <div className="gs-card p-5">
                    <h3 className="gs-section-title mb-4 flex items-center gap-2"><FileText size={16} /> Versioned Prompt Templates</h3>
                    <DataTable
                        columns={['ID', 'Template', 'Version', 'Model', 'Avg Tokens', 'Uses (30d)', 'Updated']}
                        rows={promptTemplates.map(p => [
                            <span className="font-mono text-xs text-grey-400">{p.id}</span>,
                            <span className="font-medium text-grey-200">{p.name}</span>,
                            <Badge variant="info">{p.version}</Badge>,
                            <Badge variant="purple">{p.model}</Badge>,
                            <span className="font-mono text-xs">{p.tokens}</span>,
                            p.uses.toLocaleString(),
                            <span className="text-xs text-grey-500">{p.lastUpdated}</span>,
                        ])}
                    />
                    <div className="mt-4 p-4 bg-grey-800/30 rounded-lg">
                        <div className="flex items-center gap-2 text-sm text-grey-300 mb-2"><GitBranch size={14} className="text-brand-400" /> Version Control</div>
                        <p className="text-xs text-grey-500">Each prompt template is versioned with semantic numbering. Old versions are retained for rollback. A/B testing supported across versions. Deployment requires approval for production prompts.</p>
                    </div>
                </div>
            )}

            {activeTab === 'Guardrails' && (
                <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
                    <div className="gs-card p-5">
                        <h3 className="gs-section-title mb-4 flex items-center gap-2"><Shield size={16} /> Guardrails & Policy Enforcement</h3>
                        <div className="space-y-3">
                            {guardrails.map(g => (
                                <div key={g.name} className="p-3 rounded-lg border border-grey-800 flex items-center justify-between">
                                    <div>
                                        <p className="text-sm font-medium text-grey-200">{g.name}</p>
                                        <p className="text-xs text-grey-500">{g.type} · Last triggered {g.lastTriggered}</p>
                                    </div>
                                    <div className="flex items-center gap-3">
                                        <span className="text-xs text-grey-400">{g.blocks} blocks</span>
                                        <Badge variant="success">{g.status}</Badge>
                                    </div>
                                </div>
                            ))}
                        </div>
                    </div>
                    <div className="gs-card p-5">
                        <h3 className="gs-section-title mb-4">Drift Detection</h3>
                        <p className="text-xs text-grey-500 mb-4">Monitor output quality degradation across agents</p>
                        <div className="space-y-3">
                            {agents.map(a => (
                                <div key={a.name} className="flex items-center gap-3">
                                    <span className="text-sm text-grey-300 w-36 truncate">{a.name}</span>
                                    <div className="flex-1">
                                        <ProgressBar value={a.drift * 100} max={10} color={a.drift > 0.05 ? 'bg-accent-rose' : a.drift > 0.03 ? 'bg-accent-amber' : 'bg-accent-emerald'} size="sm" />
                                    </div>
                                    <span className={`text-xs w-12 text-right ${a.drift > 0.05 ? 'text-accent-rose' : 'text-grey-400'}`}>{(a.drift * 100).toFixed(1)}%</span>
                                    {a.drift > 0.05 && <AlertTriangle size={12} className="text-accent-rose" />}
                                </div>
                            ))}
                        </div>
                    </div>
                </div>
            )}

            {activeTab === 'Human-in-Loop' && (
                <div className="gs-card p-5">
                    <h3 className="gs-section-title mb-4 flex items-center gap-2"><Users size={16} /> Human-in-the-Loop Approvals</h3>
                    <div className="space-y-3">
                        {[
                            { task: 'Contract clause flagged for legal review', agent: 'Contract Analyzer', confidence: 72, time: '5 min ago', status: 'Pending' },
                            { task: 'High-value deal auto-approved ($128K)', agent: 'Lead Qualifier', confidence: 94, time: '12 min ago', status: 'Pending' },
                            { task: 'Resume shortlist for VP Engineering', agent: 'Resume Screener', confidence: 88, time: '1h ago', status: 'Pending' },
                            { task: 'Anomaly detected in Q4 revenue data', agent: 'Anomaly Detector', confidence: 96, time: '2h ago', status: 'Approved' },
                            { task: 'Marketing copy generated for Product X', agent: 'Content Generator', confidence: 91, time: '3h ago', status: 'Approved' },
                        ].map((item, i) => (
                            <div key={i} className="p-4 rounded-lg border border-grey-800 flex items-center justify-between">
                                <div className="flex-1">
                                    <p className="text-sm font-medium text-grey-200">{item.task}</p>
                                    <p className="text-xs text-grey-500">{item.agent} · {item.time} · Confidence: <span className={item.confidence >= 90 ? 'text-accent-emerald' : 'text-accent-amber'}>{item.confidence}%</span></p>
                                </div>
                                {item.status === 'Pending' ? (
                                    <div className="flex gap-2">
                                        <button className="gs-btn-primary text-xs px-3 py-1">Approve</button>
                                        <button className="gs-btn-ghost text-xs px-3 py-1 text-accent-rose">Reject</button>
                                    </div>
                                ) : (
                                    <Badge variant="success">Approved</Badge>
                                )}
                            </div>
                        ))}
                    </div>
                </div>
            )}

            {activeTab === 'Observability' && (
                <div className="space-y-6">
                    <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
                        <div className="gs-card p-5">
                            <h3 className="gs-section-title mb-4 flex items-center gap-2"><Activity size={16} /> Agent Performance (30d)</h3>
                            <MiniSparkline data={[3200, 3800, 4100, 3900, 4400, 4800, 4200, 4600, 5100, 4800, 5200, 4218]} height={100} />
                            <div className="grid grid-cols-3 gap-3 mt-4 text-center">
                                <div><p className="text-lg font-bold text-grey-100">52.4K</p><p className="text-xs text-grey-500">Total Runs</p></div>
                                <div><p className="text-lg font-bold text-accent-emerald">97.6%</p><p className="text-xs text-grey-500">Avg Success</p></div>
                                <div><p className="text-lg font-bold text-grey-100">240ms</p><p className="text-xs text-grey-500">P50 Latency</p></div>
                            </div>
                        </div>
                        <div className="gs-card p-5">
                            <h3 className="gs-section-title mb-4 flex items-center gap-2"><Cpu size={16} /> Resource Utilization</h3>
                            <div className="space-y-4">
                                {[
                                    { resource: 'GPU Inference Cluster', usage: 68, max: '8x A100' },
                                    { resource: 'Event Bus Throughput', usage: 42, max: '50K msg/s' },
                                    { resource: 'Vector DB', usage: 54, max: '2TB index' },
                                    { resource: 'Prompt Cache Hit Rate', usage: 82, max: '10K entries' },
                                ].map(r => (
                                    <div key={r.resource}>
                                        <div className="flex justify-between text-xs mb-1">
                                            <span className="text-grey-300">{r.resource}</span>
                                            <span className="text-grey-500">{r.usage}% · {r.max}</span>
                                        </div>
                                        <ProgressBar value={r.usage} color={r.usage > 80 ? 'bg-accent-amber' : 'bg-brand-500'} size="md" />
                                    </div>
                                ))}
                            </div>
                        </div>
                    </div>
                </div>
            )}

            {activeTab === 'Cost Tracking' && (
                <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
                    <div className="gs-card p-5">
                        <h3 className="gs-section-title mb-4 flex items-center gap-2"><DollarSign size={16} /> Cost per Agent (30d)</h3>
                        <div className="space-y-3">
                            {agents.map(a => (
                                <div key={a.name} className="flex items-center justify-between p-3 rounded-lg bg-grey-800/30">
                                    <div className="flex items-center gap-2">
                                        <Bot size={14} className="text-accent-violet" />
                                        <span className="text-sm text-grey-300">{a.name}</span>
                                    </div>
                                    <div className="flex items-center gap-4">
                                        <span className="text-xs text-grey-500">{a.runs.toLocaleString()} runs</span>
                                        <span className="text-sm font-medium text-grey-200">{a.cost}</span>
                                    </div>
                                </div>
                            ))}
                            <div className="flex items-center justify-between p-3 rounded-lg bg-brand-500/10 border border-brand-500/20">
                                <span className="text-sm font-semibold text-grey-200">Total</span>
                                <span className="text-sm font-bold text-brand-400">$915</span>
                            </div>
                        </div>
                    </div>
                    <div className="gs-card p-5">
                        <h3 className="gs-section-title mb-4 flex items-center gap-2"><TrendingUp size={16} /> Cost per Tenant (30d)</h3>
                        <div className="space-y-3">
                            {[
                                { tenant: 'Acme Corp (TNT-001)', cost: '$340', runs: 18400 },
                                { tenant: 'Globex Industries (TNT-002)', cost: '$245', runs: 12800 },
                                { tenant: 'Umbrella Corp (TNT-004)', cost: '$180', runs: 9600 },
                                { tenant: 'Initech LLC (TNT-003)', cost: '$92', runs: 4200 },
                                { tenant: 'Wayne Enterprises (TNT-006)', cost: '$58', runs: 2800 },
                            ].map(t => (
                                <div key={t.tenant} className="flex items-center justify-between p-3 rounded-lg bg-grey-800/30">
                                    <span className="text-sm text-grey-300">{t.tenant}</span>
                                    <div className="flex items-center gap-4">
                                        <span className="text-xs text-grey-500">{t.runs.toLocaleString()} runs</span>
                                        <span className="text-sm font-medium text-grey-200">{t.cost}</span>
                                    </div>
                                </div>
                            ))}
                        </div>
                    </div>
                </div>
            )}
        </div>
    );
}
