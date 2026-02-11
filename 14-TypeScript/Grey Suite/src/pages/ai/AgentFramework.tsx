import { PageHeader, StatCard, Badge } from '../../components/ui';
import { Bot, Zap, Play, Pause, Plus, ArrowRight, Clock, CheckCircle } from 'lucide-react';
import { useState } from 'react';

export function AgentFramework() {
    const [activeTab, setActiveTab] = useState('agents');

    const agents = [
        { name: 'Lead Qualifier', status: 'Active', runs: 1284, success: 97.2, trigger: 'New CRM lead', lastRun: '2m ago', desc: 'Scores and routes inbound leads based on ICP match' },
        { name: 'Invoice Processor', status: 'Active', runs: 842, success: 99.1, trigger: 'Email attachment', lastRun: '14m ago', desc: 'Extracts data from invoices and creates AP entries' },
        { name: 'Onboarding Assistant', status: 'Active', runs: 56, success: 100, trigger: 'New hire created', lastRun: '1d ago', desc: 'Provisions accounts, sends welcome emails, schedules training' },
        { name: 'Support Triage', status: 'Paused', runs: 3210, success: 94.8, trigger: 'Support ticket', lastRun: '4h ago', desc: 'Classifies, prioritizes and routes support requests' },
        { name: 'Expense Auditor', status: 'Active', runs: 428, success: 98.5, trigger: 'Expense submitted', lastRun: '32m ago', desc: 'Validates expenses against policy and flags anomalies' },
        { name: 'Meeting Scheduler', status: 'Active', runs: 2140, success: 96.3, trigger: 'Calendar request', lastRun: '8m ago', desc: 'Finds optimal times and auto-schedules meetings' },
    ];

    const workflows = [
        { name: 'Deal Closed → Invoice', steps: 4, runs: 89, status: 'Active' },
        { name: 'New Employee → Full Provisioning', steps: 12, runs: 56, status: 'Active' },
        { name: 'Support Escalation Chain', steps: 6, runs: 342, status: 'Active' },
        { name: 'Quarterly Report Generation', steps: 8, runs: 4, status: 'Scheduled' },
    ];

    return (
        <div>
            <PageHeader
                title="Agent Framework"
                subtitle="AI agents, triggers, actions & automated workflows"
                actions={<button className="gs-btn-primary text-sm flex items-center gap-2"><Plus size={14} /> Create Agent</button>}
            />

            <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-4 gap-4 mb-6">
                <StatCard label="Active Agents" value={12} icon={<Bot size={18} />} />
                <StatCard label="Total Runs Today" value="1,847" change="+22% vs avg" changeType="positive" icon={<Zap size={18} />} />
                <StatCard label="Success Rate" value="97.4%" change="+0.3%" changeType="positive" icon={<CheckCircle size={18} />} />
                <StatCard label="Time Saved" value="142h" change="This month" changeType="neutral" icon={<Clock size={18} />} />
            </div>

            {/* Tabs */}
            <div className="flex gap-1 border-b border-grey-800 mb-6">
                {['agents', 'workflows'].map(tab => (
                    <button key={tab} onClick={() => setActiveTab(tab)} className={`px-4 py-2.5 text-sm font-medium border-b-2 transition-colors capitalize ${activeTab === tab ? 'border-brand-400 text-brand-400' : 'border-transparent text-grey-500 hover:text-grey-300'
                        }`}>{tab}</button>
                ))}
            </div>

            {activeTab === 'agents' ? (
                <div className="grid grid-cols-1 lg:grid-cols-2 gap-4">
                    {agents.map(agent => (
                        <div key={agent.name} className="gs-card p-5">
                            <div className="flex items-start justify-between mb-3">
                                <div className="flex items-center gap-3">
                                    <div className={`w-10 h-10 rounded-xl flex items-center justify-center ${agent.status === 'Active' ? 'bg-accent-emerald/15 text-accent-emerald' : 'bg-grey-800 text-grey-500'}`}>
                                        <Bot size={20} />
                                    </div>
                                    <div>
                                        <h4 className="text-sm font-semibold text-grey-200">{agent.name}</h4>
                                        <p className="text-xs text-grey-500 mt-0.5">{agent.desc}</p>
                                    </div>
                                </div>
                                <button className={`p-1.5 rounded-lg ${agent.status === 'Active' ? 'text-accent-emerald hover:bg-accent-emerald/10' : 'text-grey-500 hover:bg-grey-800'}`}>
                                    {agent.status === 'Active' ? <Pause size={14} /> : <Play size={14} />}
                                </button>
                            </div>
                            <div className="flex items-center gap-4 text-xs">
                                <div className="flex items-center gap-1 text-grey-500">
                                    <Zap size={12} /> <span className="text-grey-400">{agent.trigger}</span>
                                </div>
                                <span className="text-grey-600">•</span>
                                <span className="text-grey-500">{agent.runs.toLocaleString()} runs</span>
                                <span className="text-grey-600">•</span>
                                <span className="text-accent-emerald">{agent.success}% success</span>
                                <span className="text-grey-600">•</span>
                                <span className="text-grey-500">{agent.lastRun}</span>
                            </div>
                        </div>
                    ))}
                </div>
            ) : (
                <div className="space-y-4">
                    {workflows.map(wf => (
                        <div key={wf.name} className="gs-card p-5">
                            <div className="flex items-center justify-between">
                                <div className="flex items-center gap-4">
                                    <div className="w-10 h-10 rounded-xl bg-accent-violet/15 text-accent-violet flex items-center justify-center">
                                        <Zap size={20} />
                                    </div>
                                    <div>
                                        <h4 className="text-sm font-semibold text-grey-200">{wf.name}</h4>
                                        <p className="text-xs text-grey-500">{wf.steps} steps · {wf.runs} runs</p>
                                    </div>
                                </div>
                                <Badge variant={wf.status === 'Active' ? 'success' : 'info'}>{wf.status}</Badge>
                            </div>
                            {/* Visual workflow steps */}
                            <div className="flex items-center gap-2 mt-4 overflow-x-auto pb-2">
                                {Array.from({ length: wf.steps }, (_, i) => (
                                    <div key={i} className="flex items-center gap-2">
                                        <div className="w-8 h-8 rounded-lg bg-grey-800 flex items-center justify-center text-xs text-grey-400 flex-shrink-0">{i + 1}</div>
                                        {i < wf.steps - 1 && <ArrowRight size={14} className="text-grey-600 flex-shrink-0" />}
                                    </div>
                                ))}
                            </div>
                        </div>
                    ))}
                </div>
            )}
        </div>
    );
}
