import { useState } from 'react';
import {
    Workflow, Play, Pause, RefreshCw, Clock, Zap, AlertTriangle,
    CheckCircle2, ArrowRight, GitBranch, Settings, Shield, Target,
    Plus, Layers, Database, Mail, MessageSquare, Users, Bot
} from 'lucide-react';
import { PageHeader, StatCard, DataTable, Badge, Tabs, ProgressBar, MiniSparkline } from '../../components/ui';

export function WorkflowAutomation() {
    const [activeTab, setActiveTab] = useState('Builder');

    const tabs = ['Builder', 'Templates', 'Executions', 'SLA Tracking', 'Error Handling'];

    return (
        <div className="p-6 space-y-6">
            <PageHeader
                title="Workflow Automation Platform"
                subtitle="Drag-and-drop workflow builder with triggers, actions, conditions, cross-module automation, and compensation logic"
                actions={
                    <div className="flex gap-2">
                        <button className="gs-btn-secondary">
                            <Settings size={14} /> Settings
                        </button>
                        <button className="gs-btn-primary">
                            <Plus size={14} /> New Flow
                        </button>
                    </div>
                }
            />

            <div className="grid grid-cols-4 gap-4">
                <StatCard label="Active Flows" value="284" change="+24 this month" changeType="positive" icon={<Workflow size={18} />} />
                <StatCard label="Executions (24h)" value="48,291" change="+12.4%" changeType="positive" icon={<Zap size={18} />} />
                <StatCard label="Success Rate" value="99.2%" change="+0.3%" changeType="positive" icon={<CheckCircle2 size={18} />} />
                <StatCard label="Avg Duration" value="4.2s" change="-0.8s" changeType="positive" icon={<Clock size={18} />} />
            </div>

            <Tabs tabs={tabs} active={activeTab} onChange={setActiveTab} />

            {activeTab === 'Builder' && (
                <div className="space-y-6">
                    <div className="gs-card p-6">
                        <div className="flex items-center justify-between mb-4">
                            <div>
                                <h3 className="text-sm font-semibold text-grey-300">Visual Workflow Builder</h3>
                                <p className="text-xs text-grey-500 mt-1">Employee Offboarding Automation</p>
                            </div>
                            <div className="flex gap-2">
                                <button className="gs-btn-ghost text-xs"><Play size={12} /> Test Run</button>
                                <button className="gs-btn-primary text-xs"><CheckCircle2 size={12} /> Publish</button>
                            </div>
                        </div>

                        {/* Visual Workflow */}
                        <div className="bg-grey-900 rounded-lg p-6 border border-grey-700/50">
                            <div className="flex flex-col items-center gap-3">
                                {[
                                    { type: 'trigger', label: 'HR System: Employee Termination', icon: <Zap size={14} />, color: 'border-amber-500 bg-amber-500/10', detail: 'When status → "Terminated"' },
                                    { type: 'condition', label: 'Check: Has Company Equipment?', icon: <GitBranch size={14} />, color: 'border-violet-500 bg-violet-500/10', detail: 'Query asset registry' },
                                    { type: 'parallel', label: 'Parallel Branch', icon: <Layers size={14} />, color: 'border-brand-500 bg-brand-500/10', detail: 'Execute simultaneously' },
                                ].map((step, i) => (
                                    <div key={i} className="flex flex-col items-center gap-3 w-full">
                                        <div className={`px-6 py-3 rounded-lg border-2 ${step.color} w-full max-w-lg`}>
                                            <div className="flex items-center gap-2 mb-1">
                                                <span className="text-grey-300">{step.icon}</span>
                                                <span className="text-sm font-semibold text-grey-200">{step.label}</span>
                                            </div>
                                            <p className="text-xs text-grey-500">{step.detail}</p>
                                        </div>
                                        {i < 2 && <ArrowRight size={16} className="text-grey-600 rotate-90" />}
                                    </div>
                                ))}

                                {/* Parallel branches */}
                                <div className="grid grid-cols-3 gap-4 w-full">
                                    {[
                                        {
                                            actions: [
                                                { label: 'Disable AD Account', icon: <Shield size={12} />, service: 'Identity' },
                                                { label: 'Revoke OAuth Tokens', icon: <Shield size={12} />, service: 'Auth' },
                                                { label: 'Remove from Groups', icon: <Users size={12} />, service: 'Directory' },
                                            ]
                                        },
                                        {
                                            actions: [
                                                { label: 'Create IT Ticket', icon: <Settings size={12} />, service: 'ITSM' },
                                                { label: 'Schedule Equipment Return', icon: <RefreshCw size={12} />, service: 'Asset Mgmt' },
                                                { label: 'Wipe Mobile Device', icon: <Shield size={12} />, service: 'MDM' },
                                            ]
                                        },
                                        {
                                            actions: [
                                                { label: 'Notify Manager', icon: <Mail size={12} />, service: 'Email' },
                                                { label: 'Post to HR Channel', icon: <MessageSquare size={12} />, service: 'Chat' },
                                                { label: 'Update Payroll', icon: <Database size={12} />, service: 'HCM' },
                                            ]
                                        },
                                    ].map((branch, bi) => (
                                        <div key={bi} className="space-y-2">
                                            {branch.actions.map((a, ai) => (
                                                <div key={ai} className="bg-grey-800/80 rounded-lg p-2.5 border border-grey-700/50">
                                                    <div className="flex items-center gap-2">
                                                        <span className="text-brand-400">{a.icon}</span>
                                                        <div>
                                                            <p className="text-xs text-grey-200">{a.label}</p>
                                                            <p className="text-[10px] text-grey-500">{a.service}</p>
                                                        </div>
                                                    </div>
                                                </div>
                                            ))}
                                        </div>
                                    ))}
                                </div>

                                <ArrowRight size={16} className="text-grey-600 rotate-90" />

                                <div className="px-6 py-3 rounded-lg border-2 border-emerald-500 bg-emerald-500/10 w-full max-w-lg">
                                    <div className="flex items-center gap-2 mb-1">
                                        <CheckCircle2 size={14} className="text-emerald-400" />
                                        <span className="text-sm font-semibold text-grey-200">Complete: Log Audit Record</span>
                                    </div>
                                    <p className="text-xs text-grey-500">Write immutable audit entry + send compliance report</p>
                                </div>
                            </div>
                        </div>
                    </div>

                    <div className="gs-card p-6">
                        <h3 className="text-sm font-semibold text-grey-300 mb-4">Action Palette</h3>
                        <div className="grid grid-cols-4 gap-3">
                            {[
                                { category: 'Triggers', items: ['HTTP Webhook', 'Schedule (CRON)', 'Database Change', 'Form Submit', 'Email Received', 'Event Bus'] },
                                { category: 'Logic', items: ['If/Else Condition', 'Switch/Case', 'Loop (forEach)', 'Parallel Branch', 'Wait/Delay', 'Variable Set'] },
                                { category: 'Actions', items: ['Send Email', 'Post to Chat', 'Create Record', 'HTTP Request', 'Run Script', 'AI Prompt'] },
                                { category: 'Integration', items: ['Salesforce', 'Workday', 'Slack', 'Jira', 'GitHub', 'Custom API'] },
                            ].map(cat => (
                                <div key={cat.category}>
                                    <p className="text-xs font-semibold text-grey-400 uppercase mb-2">{cat.category}</p>
                                    <div className="space-y-1">
                                        {cat.items.map(item => (
                                            <div key={item} className="text-xs px-2 py-1.5 bg-grey-800/50 rounded text-grey-400 border border-grey-700/50 cursor-pointer hover:border-brand-500/50 transition-colors">{item}</div>
                                        ))}
                                    </div>
                                </div>
                            ))}
                        </div>
                    </div>
                </div>
            )}

            {activeTab === 'Templates' && (
                <div className="space-y-6">
                    <DataTable
                        columns={['Template', 'Category', 'Steps', 'Trigger', 'Modules Used', 'Installs', 'Rating']}
                        rows={[
                            ['Employee Onboarding', <Badge variant="info">HCM</Badge>, '14', 'New hire created', '5 (HCM, IT, Chat, Email, PM)', '842', '⭐ 4.8'],
                            ['Lead to Opportunity', <Badge variant="purple">CRM</Badge>, '8', 'Lead score ≥ 80', '3 (CRM, Email, Chat)', '624', '⭐ 4.7'],
                            ['Invoice Approval', <Badge variant="warning">Finance</Badge>, '6', 'Invoice uploaded', '3 (ERP, Email, Chat)', '1,204', '⭐ 4.9'],
                            ['Incident Response', <Badge variant="danger">Security</Badge>, '12', 'Alert triggered', '6 (SIEM, Chat, PagerDuty, Jira, Email, Audit)', '284', '⭐ 4.6'],
                            ['Content Publishing', <Badge variant="info">CMS</Badge>, '8', 'Draft approved', '4 (CMS, Email, Chat, Social)', '421', '⭐ 4.5'],
                            ['Contract Renewal', <Badge variant="purple">Legal</Badge>, '10', '90 days before expiry', '4 (CRM, Legal, Email, DocSign)', '184', '⭐ 4.7'],
                            ['Support Escalation', <Badge variant="warning">Support</Badge>, '6', 'SLA breach risk', '4 (Support, Chat, Email, CRM)', '892', '⭐ 4.8'],
                            ['Expense Approval', <Badge variant="info">Finance</Badge>, '5', 'Expense submitted', '3 (ERP, Email, Chat)', '1,842', '⭐ 4.9'],
                        ]}
                    />
                </div>
            )}

            {activeTab === 'Executions' && (
                <div className="space-y-6">
                    <div className="grid grid-cols-3 gap-4 mb-4">
                        {[
                            { label: 'Executions (24h)', data: [42100, 44200, 43800, 45200, 44800, 46100, 48291], color: '#1a8fe0' },
                            { label: 'Failures (24h)', data: [120, 98, 142, 104, 88, 110, 96], color: '#f43f5e' },
                            { label: 'Avg Duration', data: [5.1, 4.8, 5.2, 4.6, 4.4, 4.3, 4.2], color: '#10b981' },
                        ].map(m => (
                            <div key={m.label} className="gs-card p-4">
                                <p className="text-xs text-grey-500 mb-2">{m.label}</p>
                                <MiniSparkline data={m.data} color={m.color} height={36} />
                                <p className="text-lg font-bold text-grey-200 mt-1">{m.data[m.data.length - 1].toLocaleString()}{m.label.includes('Duration') ? 's' : ''}</p>
                            </div>
                        ))}
                    </div>

                    <DataTable
                        columns={['Execution ID', 'Flow', 'Trigger', 'Duration', 'Steps', 'Status', 'Timestamp']}
                        rows={[
                            ['exec-a8f2c1', 'Employee Onboarding', 'HR System', '12.4s', '14/14', <Badge variant="success">Completed</Badge>, '2026-02-10 09:42:18'],
                            ['exec-b3d4e5', 'Invoice Approval', 'Email Webhook', '3.2s', '6/6', <Badge variant="success">Completed</Badge>, '2026-02-10 09:41:52'],
                            ['exec-c6f7g8', 'Lead Scoring', 'CRM Event', '1.8s', '5/5', <Badge variant="success">Completed</Badge>, '2026-02-10 09:41:30'],
                            ['exec-d9h0i1', 'Incident Response', 'Alert Trigger', '—', '4/12', <Badge variant="warning">Running</Badge>, '2026-02-10 09:41:15'],
                            ['exec-e2j3k4', 'Contract Renewal', 'Scheduler', '8.6s', '8/10', <Badge variant="danger">Failed</Badge>, '2026-02-10 09:40:42'],
                            ['exec-f5l6m7', 'Support Escalation', 'SLA Monitor', '2.1s', '6/6', <Badge variant="success">Completed</Badge>, '2026-02-10 09:40:18'],
                            ['exec-g8n9o0', 'Expense Approval', 'Form Submit', '4.8s', '5/5', <Badge variant="success">Completed</Badge>, '2026-02-10 09:39:55'],
                        ]}
                    />
                </div>
            )}

            {activeTab === 'SLA Tracking' && (
                <div className="space-y-6">
                    <div className="gs-card p-6">
                        <h3 className="text-sm font-semibold text-grey-300 mb-4">SLA Compliance Dashboard</h3>
                        <DataTable
                            columns={['Flow', 'SLA Target', 'P50', 'P95', 'P99', 'Compliance', 'Breaches (30d)', 'Status']}
                            rows={[
                                ['Employee Onboarding', '< 30s', '12.4s', '18.2s', '24.8s', <><ProgressBar value={99.2} color="bg-accent-emerald" /><span className="text-xs text-grey-400">99.2%</span></>, '4', <Badge variant="success">Healthy</Badge>],
                                ['Invoice Approval', '< 10s', '3.2s', '6.8s', '8.4s', <><ProgressBar value={99.8} color="bg-accent-emerald" /><span className="text-xs text-grey-400">99.8%</span></>, '1', <Badge variant="success">Healthy</Badge>],
                                ['Incident Response', '< 60s', '8.2s', '22.4s', '45.6s', <><ProgressBar value={98.4} color="bg-accent-emerald" /><span className="text-xs text-grey-400">98.4%</span></>, '12', <Badge variant="success">Healthy</Badge>],
                                ['Lead Processing', '< 5s', '1.8s', '3.2s', '4.1s', <><ProgressBar value={99.9} color="bg-accent-emerald" /><span className="text-xs text-grey-400">99.9%</span></>, '0', <Badge variant="success">Healthy</Badge>],
                                ['Contract Renewal', '< 15s', '8.6s', '12.4s', '18.2s', <><ProgressBar value={94.2} color="bg-amber-500" /><span className="text-xs text-grey-400">94.2%</span></>, '42', <Badge variant="warning">Degraded</Badge>],
                            ]}
                        />
                    </div>
                </div>
            )}

            {activeTab === 'Error Handling' && (
                <div className="space-y-6">
                    <div className="gs-card p-6">
                        <h3 className="text-sm font-semibold text-grey-300 mb-4">Retry & Compensation Logic</h3>
                        <div className="bg-grey-900 rounded-lg p-4 font-mono text-xs text-grey-400 mb-6">
                            <pre>{`// Error Handling Strategy
interface RetryPolicy {
  max_attempts: 3;
  backoff: 'exponential';    // 1s, 2s, 4s
  jitter: true;              // ±200ms random
  timeout_per_attempt: 30_000; // 30s
  retryable_errors: [
    'TIMEOUT', 'RATE_LIMITED',
    'SERVICE_UNAVAILABLE', '5XX'
  ];
  non_retryable: [
    'AUTH_FAILED', 'VALIDATION_ERROR',
    'NOT_FOUND', '4XX'
  ];
}

// Compensation (Saga Pattern)
// When step N fails after steps 1..N-1 succeeded:
// → Execute compensating actions in reverse
// → Example: Onboarding failure at step 8
//   - Step 7 comp: Remove from Slack channels
//   - Step 6 comp: Revoke VPN access
//   - Step 5 comp: Delete AD account
//   - Step 4 comp: Cancel equipment order
//   ...
// → All compensations logged to audit trail
// → Dead Letter Queue for unrecoverable failures`}</pre>
                        </div>
                    </div>

                    <DataTable
                        columns={['Error Type', 'Occurrences (30d)', 'Retry Success', 'Compensation', 'Dead Letter', 'Resolution']}
                        rows={[
                            ['Timeout (external API)', '284', <Badge variant="success">92% resolved</Badge>, '8 flows', '0', 'Auto-retry'],
                            ['Rate Limited', '142', <Badge variant="success">100% resolved</Badge>, '0', '0', 'Backoff + retry'],
                            ['Auth Token Expired', '48', <Badge variant="success">100% resolved</Badge>, '0', '0', 'Auto-refresh + retry'],
                            ['Schema Validation', '24', <Badge variant="danger">0% (not retryable)</Badge>, '12 flows', '12', 'Manual fix required'],
                            ['Service Unavailable', '18', <Badge variant="success">94% resolved</Badge>, '1 flow', '0', 'Auto-retry'],
                            ['Data Conflict', '8', <Badge variant="warning">50% resolved</Badge>, '4 flows', '4', 'Manual review'],
                        ]}
                    />
                </div>
            )}
        </div>
    );
}
