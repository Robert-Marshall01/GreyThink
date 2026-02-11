import { useState } from 'react';
import { PageHeader, StatCard, Badge, DataTable, Tabs, ProgressBar, MiniSparkline } from '../../components/ui';
import {
    GitBranch, Play, Pause, Clock, Users, CheckCircle,
    AlertTriangle, Settings, Plus, Bot, Zap, ArrowRight,
    RotateCcw, Eye, Layers, Activity
} from 'lucide-react';

const workflows = [
    { id: 'WF-001', name: 'New Employee Onboarding', module: 'HCM', steps: 14, sla: '3 days', runs: 842, avgDuration: '2.1 days', version: 'v4.2', status: 'Active' },
    { id: 'WF-002', name: 'Purchase Order Approval', module: 'ERP', steps: 8, sla: '4 hours', runs: 2480, avgDuration: '2.8 hours', version: 'v3.1', status: 'Active' },
    { id: 'WF-003', name: 'Lead to Opportunity', module: 'CRM', steps: 6, sla: '24 hours', runs: 4210, avgDuration: '18 hours', version: 'v2.8', status: 'Active' },
    { id: 'WF-004', name: 'Security Incident Response', module: 'Security', steps: 12, sla: '15 min', runs: 142, avgDuration: '8.4 min', version: 'v5.0', status: 'Active' },
    { id: 'WF-005', name: 'Invoice Processing', module: 'Finance', steps: 10, sla: '2 hours', runs: 1840, avgDuration: '1.4 hours', version: 'v3.6', status: 'Active' },
    { id: 'WF-006', name: 'Data Quality Review', module: 'Data', steps: 7, sla: '1 hour', runs: 420, avgDuration: '42 min', version: 'v1.2', status: 'Draft' },
];

const workflowSteps = [
    { step: 'Trigger: HR submits new hire form', type: 'trigger', status: 'complete' },
    { step: 'AI Agent: Verify documents', type: 'ai', status: 'complete' },
    { step: 'Conditional: Salary > $150K?', type: 'condition', status: 'complete' },
    { step: 'Human Approval: VP sign-off', type: 'approval', status: 'complete' },
    { step: 'Action: Create accounts (Email, Slack, GitHub)', type: 'action', status: 'running' },
    { step: 'Action: Assign hardware', type: 'action', status: 'pending' },
    { step: 'Action: Schedule orientation', type: 'action', status: 'pending' },
    { step: 'AI Agent: Generate personalized onboarding plan', type: 'ai', status: 'pending' },
    { step: 'Human Approval: Manager confirms', type: 'approval', status: 'pending' },
    { step: 'Action: Send welcome email', type: 'action', status: 'pending' },
];

const slaTracking = [
    { workflow: 'Security Incident Response', sla: '15 min', p50: '6.2 min', p95: '12.8 min', breaches: 0, compliance: 100 },
    { workflow: 'Purchase Order Approval', sla: '4 hours', p50: '1.8 hours', p95: '3.4 hours', breaches: 4, compliance: 99.2 },
    { workflow: 'Invoice Processing', sla: '2 hours', p50: '48 min', p95: '1.6 hours', breaches: 2, compliance: 99.6 },
    { workflow: 'Lead to Opportunity', sla: '24 hours', p50: '12 hours', p95: '20 hours', breaches: 18, compliance: 97.8 },
    { workflow: 'Employee Onboarding', sla: '3 days', p50: '2.1 days', p95: '2.8 days', breaches: 1, compliance: 99.8 },
];

export function WorkflowEngine() {
    const [activeTab, setActiveTab] = useState('Workflows');

    return (
        <div>
            <PageHeader
                title="Workflow Engine"
                subtitle="BPMN-style workflow builder with SLA tracking, approvals & AI-powered steps"
                actions={
                    <div className="flex gap-2">
                        <button className="gs-btn-secondary flex items-center gap-2"><Settings size={16} /> Templates</button>
                        <button className="gs-btn-primary flex items-center gap-2"><Plus size={16} /> New Workflow</button>
                    </div>
                }
            />

            <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-4 gap-4 mb-6">
                <StatCard label="Active Workflows" value="5" change="6 total" changeType="neutral" icon={<GitBranch size={18} />} />
                <StatCard label="Executions (30d)" value="9,934" change="+24% MoM" changeType="positive" icon={<Zap size={18} />} />
                <StatCard label="Avg SLA Compliance" value="99.3%" change="25 breaches total" changeType="positive" icon={<Clock size={18} />} />
                <StatCard label="Human Approvals" value="1,284" change="Avg 2.4h response" changeType="neutral" icon={<Users size={18} />} />
            </div>

            <Tabs tabs={['Workflows', 'Builder', 'SLA Tracking', 'Versioning', 'Execution Log']} active={activeTab} onChange={setActiveTab} />

            {activeTab === 'Workflows' && (
                <div className="gs-card p-5">
                    <h3 className="gs-section-title mb-4 flex items-center gap-2"><GitBranch size={16} /> Workflow Registry</h3>
                    <DataTable
                        columns={['ID', 'Workflow', 'Module', 'Steps', 'SLA', 'Runs (30d)', 'Avg Duration', 'Version', 'Status']}
                        rows={workflows.map(w => [
                            <span className="font-mono text-xs text-grey-400">{w.id}</span>,
                            <span className="font-medium text-grey-200">{w.name}</span>,
                            <Badge variant="info">{w.module}</Badge>,
                            w.steps,
                            w.sla,
                            w.runs.toLocaleString(),
                            w.avgDuration,
                            <Badge variant="purple">{w.version}</Badge>,
                            <Badge variant={w.status === 'Active' ? 'success' : 'default'}>{w.status}</Badge>,
                        ])}
                    />
                </div>
            )}

            {activeTab === 'Builder' && (
                <div className="grid grid-cols-1 lg:grid-cols-3 gap-6">
                    <div className="lg:col-span-2 gs-card p-5">
                        <h3 className="gs-section-title mb-4 flex items-center gap-2"><Layers size={16} /> Workflow Builder — Employee Onboarding</h3>
                        <div className="space-y-2">
                            {workflowSteps.map((s, i) => (
                                <div key={i} className="flex items-center gap-3">
                                    <div className={`w-8 h-8 rounded-full flex items-center justify-center text-xs font-bold flex-shrink-0
                    ${s.status === 'complete' ? 'bg-accent-emerald/20 text-accent-emerald' :
                                            s.status === 'running' ? 'bg-brand-500/20 text-brand-400' :
                                                'bg-grey-800 text-grey-500'}`}>
                                        {s.status === 'complete' ? <CheckCircle size={14} /> : i + 1}
                                    </div>
                                    <div className={`flex-1 p-3 rounded-lg border transition-colors
                    ${s.status === 'complete' ? 'border-accent-emerald/30 bg-accent-emerald/5' :
                                            s.status === 'running' ? 'border-brand-500/30 bg-brand-500/5' :
                                                'border-grey-800'}`}>
                                        <div className="flex items-center justify-between">
                                            <div className="flex items-center gap-2">
                                                {s.type === 'ai' && <Bot size={14} className="text-accent-violet" />}
                                                {s.type === 'approval' && <Users size={14} className="text-accent-amber" />}
                                                {s.type === 'condition' && <GitBranch size={14} className="text-accent-cyan" />}
                                                {s.type === 'trigger' && <Zap size={14} className="text-accent-emerald" />}
                                                {s.type === 'action' && <Play size={14} className="text-brand-400" />}
                                                <span className="text-sm text-grey-300">{s.step}</span>
                                            </div>
                                            <Badge variant={
                                                s.type === 'ai' ? 'purple' :
                                                    s.type === 'approval' ? 'warning' :
                                                        s.type === 'condition' ? 'info' :
                                                            s.type === 'trigger' ? 'success' : 'default'
                                            }>{s.type}</Badge>
                                        </div>
                                    </div>
                                </div>
                            ))}
                        </div>
                    </div>
                    <div className="space-y-6">
                        <div className="gs-card p-5">
                            <h3 className="gs-section-title mb-3">Step Types</h3>
                            <div className="space-y-2">
                                {[
                                    { type: 'Trigger', icon: <Zap size={14} />, desc: 'Event or schedule start', color: 'text-accent-emerald' },
                                    { type: 'Action', icon: <Play size={14} />, desc: 'Automated system action', color: 'text-brand-400' },
                                    { type: 'Condition', icon: <GitBranch size={14} />, desc: 'If/else branching logic', color: 'text-accent-cyan' },
                                    { type: 'Approval', icon: <Users size={14} />, desc: 'Human sign-off gate', color: 'text-accent-amber' },
                                    { type: 'AI Step', icon: <Bot size={14} />, desc: 'AI agent execution', color: 'text-accent-violet' },
                                    { type: 'Compensation', icon: <RotateCcw size={14} />, desc: 'Rollback on failure', color: 'text-accent-rose' },
                                ].map(t => (
                                    <div key={t.type} className="flex items-center gap-2 p-2 rounded-lg hover:bg-grey-800/30 cursor-pointer transition-colors">
                                        <span className={t.color}>{t.icon}</span>
                                        <div>
                                            <p className="text-sm text-grey-300">{t.type}</p>
                                            <p className="text-xs text-grey-500">{t.desc}</p>
                                        </div>
                                    </div>
                                ))}
                            </div>
                        </div>
                        <div className="gs-card p-5">
                            <h3 className="gs-section-title mb-3">JSON Definition</h3>
                            <div className="bg-grey-900/50 rounded-lg p-3 font-mono text-xs space-y-1">
                                <div className="text-accent-cyan">{'{'}</div>
                                <div className="pl-2 text-grey-400">"id": <span className="text-accent-emerald">"WF-001"</span>,</div>
                                <div className="pl-2 text-grey-400">"version": <span className="text-accent-emerald">"4.2"</span>,</div>
                                <div className="pl-2 text-grey-400">"state_machine": {'{'}</div>
                                <div className="pl-4 text-grey-400">"initial": <span className="text-accent-emerald">"trigger"</span>,</div>
                                <div className="pl-4 text-grey-400">"states": {'{ ... }'}</div>
                                <div className="pl-2 text-grey-400">{'}'},</div>
                                <div className="pl-2 text-grey-400">"retry": {'{ "max": 3, "backoff": "exp" }'},</div>
                                <div className="pl-2 text-grey-400">"compensation": <span className="text-accent-amber">true</span></div>
                                <div className="text-accent-cyan">{'}'}</div>
                            </div>
                        </div>
                    </div>
                </div>
            )}

            {activeTab === 'SLA Tracking' && (
                <div className="gs-card p-5">
                    <h3 className="gs-section-title mb-4 flex items-center gap-2"><Clock size={16} /> SLA Compliance</h3>
                    <DataTable
                        columns={['Workflow', 'SLA Target', 'P50', 'P95', 'Breaches (30d)', 'Compliance']}
                        rows={slaTracking.map(s => [
                            <span className="font-medium text-grey-200">{s.workflow}</span>,
                            <span className="font-mono text-xs text-accent-amber">{s.sla}</span>,
                            <span className="font-mono text-xs text-accent-emerald">{s.p50}</span>,
                            <span className="font-mono text-xs text-grey-300">{s.p95}</span>,
                            <span className={s.breaches > 0 ? 'text-accent-rose' : 'text-accent-emerald'}>{s.breaches}</span>,
                            <span className={`font-bold ${s.compliance >= 99.5 ? 'text-accent-emerald' : s.compliance >= 98 ? 'text-accent-amber' : 'text-accent-rose'}`}>{s.compliance}%</span>,
                        ])}
                    />
                </div>
            )}

            {activeTab === 'Versioning' && (
                <div className="gs-card p-5">
                    <h3 className="gs-section-title mb-4 flex items-center gap-2"><GitBranch size={16} /> Workflow Version History</h3>
                    <div className="space-y-3">
                        {[
                            { wf: 'Employee Onboarding', from: 'v4.1', to: 'v4.2', change: 'Added AI-powered onboarding plan generation step', author: 'admin@grey.io', date: '3 days ago' },
                            { wf: 'Purchase Order Approval', from: 'v3.0', to: 'v3.1', change: 'Added conditional bypass for POs under $1K', author: 'finance@grey.io', date: '1 week ago' },
                            { wf: 'Security Incident Response', from: 'v4.8', to: 'v5.0', change: 'Major: added automated containment + rollback compensation', author: 'security@grey.io', date: '2 weeks ago' },
                            { wf: 'Invoice Processing', from: 'v3.5', to: 'v3.6', change: 'Updated AI extraction model from GPT-4 to GPT-4o', author: 'admin@grey.io', date: '3 weeks ago' },
                            { wf: 'Lead to Opportunity', from: 'v2.7', to: 'v2.8', change: 'Added Slack notification on stage transition', author: 'sales@grey.io', date: '1 month ago' },
                        ].map((v, i) => (
                            <div key={i} className="p-4 rounded-lg border border-grey-800 flex items-center justify-between">
                                <div>
                                    <p className="text-sm font-medium text-grey-200">{v.wf}</p>
                                    <p className="text-xs text-grey-500">{v.change}</p>
                                    <p className="text-xs text-grey-600 mt-1">{v.author} · {v.date}</p>
                                </div>
                                <div className="flex items-center gap-1">
                                    <Badge variant="default">{v.from}</Badge>
                                    <ArrowRight size={12} className="text-grey-600" />
                                    <Badge variant="success">{v.to}</Badge>
                                </div>
                            </div>
                        ))}
                    </div>
                </div>
            )}

            {activeTab === 'Execution Log' && (
                <div className="gs-card p-5">
                    <h3 className="gs-section-title mb-4 flex items-center gap-2"><Activity size={16} /> Recent Executions</h3>
                    <DataTable
                        columns={['Execution ID', 'Workflow', 'Started', 'Duration', 'Steps', 'Status']}
                        rows={[
                            ['EXC-84210', 'Purchase Order Approval', '4 min ago', '1.2 hours', '8/8', 'Completed'],
                            ['EXC-84209', 'Employee Onboarding', '18 min ago', '—', '5/14', 'Running'],
                            ['EXC-84208', 'Invoice Processing', '25 min ago', '42 min', '10/10', 'Completed'],
                            ['EXC-84207', 'Lead to Opportunity', '1h ago', '14 hours', '6/6', 'Completed'],
                            ['EXC-84206', 'Security Incident', '2h ago', '6.8 min', '12/12', 'Completed'],
                            ['EXC-84205', 'Purchase Order Approval', '3h ago', '—', '4/8', 'Failed'],
                        ].map(r => [
                            <span className="font-mono text-xs text-grey-400">{r[0]}</span>,
                            <span className="text-grey-200">{r[1]}</span>,
                            <span className="text-xs text-grey-500">{r[2]}</span>,
                            <span className="font-mono text-xs">{r[3]}</span>,
                            r[4],
                            <Badge variant={r[5] === 'Completed' ? 'success' : r[5] === 'Running' ? 'info' : 'danger'}>{r[5]}</Badge>,
                        ])}
                    />
                </div>
            )}
        </div>
    );
}
