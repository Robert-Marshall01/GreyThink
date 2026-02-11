import { PageHeader, StatCard, Badge, DataTable } from '../../components/ui';
import { Search, Send, Target, Clock, Plus, Bot, Eye } from 'lucide-react';
import { useState } from 'react';

export function JobSearch() {
    const [activeTab, setActiveTab] = useState('tracker');

    return (
        <div>
            <PageHeader
                title="Job Search & Career Tools"
                subtitle="Application tracking, ATS optimization, outreach & automation"
                actions={<button className="gs-btn-primary text-sm flex items-center gap-2"><Plus size={14} /> Add Application</button>}
            />

            <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-4 gap-4 mb-6">
                <StatCard label="Active Applications" value={24} icon={<Search size={18} />} />
                <StatCard label="Interviews Scheduled" value={5} change="This week" changeType="positive" icon={<Target size={18} />} />
                <StatCard label="Outreach Sent" value={42} change="18 replies (43%)" changeType="positive" icon={<Send size={18} />} />
                <StatCard label="Avg Response Time" value="3.2 days" change="-1.8 days" changeType="positive" icon={<Clock size={18} />} />
            </div>

            <div className="flex gap-1 border-b border-grey-800 mb-6">
                {['tracker', 'ats', 'outreach', 'automation'].map(tab => (
                    <button key={tab} onClick={() => setActiveTab(tab)} className={`px-4 py-2.5 text-sm font-medium border-b-2 capitalize transition-colors ${activeTab === tab ? 'border-brand-400 text-brand-400' : 'border-transparent text-grey-500 hover:text-grey-300'
                        }`}>{tab === 'ats' ? 'ATS Optimizer' : tab}</button>
                ))}
            </div>

            {activeTab === 'tracker' && (
                <>
                    {/* Pipeline */}
                    <div className="grid grid-cols-2 md:grid-cols-5 gap-3 mb-6">
                        {[
                            { stage: 'Applied', count: 8, color: 'border-grey-600' },
                            { stage: 'Screening', count: 6, color: 'border-brand-500' },
                            { stage: 'Interview', count: 5, color: 'border-accent-amber' },
                            { stage: 'Offer', count: 2, color: 'border-accent-emerald' },
                            { stage: 'Rejected', count: 3, color: 'border-accent-rose' },
                        ].map(s => (
                            <div key={s.stage} className={`gs-card p-3 border-t-2 ${s.color} text-center`}>
                                <p className="text-2xl font-bold text-grey-100">{s.count}</p>
                                <p className="text-xs text-grey-500">{s.stage}</p>
                            </div>
                        ))}
                    </div>

                    <div className="gs-card p-5">
                        <DataTable
                            columns={['Company', 'Position', 'Applied', 'Stage', 'Contact', 'Next Step']}
                            rows={[
                                ['Stripe', 'Staff Engineer', 'Feb 8', <Badge variant="info">Interview</Badge>, 'J. Martinez', 'System Design — Feb 12'],
                                ['Vercel', 'Principal Engineer', 'Feb 5', <Badge variant="info">Interview</Badge>, 'L. Park', 'Team Round — Feb 14'],
                                ['Figma', 'Engineering Manager', 'Feb 3', <Badge variant="warning">Screening</Badge>, 'M. Taylor', 'Awaiting response'],
                                ['Notion', 'Sr. Engineer', 'Feb 1', <Badge variant="success">Offer</Badge>, 'S. Williams', 'Review package'],
                                ['Linear', 'Founding Engineer', 'Jan 28', <Badge variant="danger">Rejected</Badge>, '—', 'Closed'],
                            ]}
                        />
                    </div>
                </>
            )}

            {activeTab === 'ats' && (
                <div className="gs-card p-6">
                    <div className="flex items-center gap-3 mb-4">
                        <Eye size={20} className="text-brand-400" />
                        <h3 className="gs-section-title">ATS Resume Optimizer</h3>
                    </div>
                    <p className="text-sm text-grey-400 mb-4">Paste a job description to analyze keyword match and optimize your resume for ATS systems.</p>
                    <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
                        <div>
                            <label className="text-xs font-semibold text-grey-400 uppercase tracking-wider">Job Description</label>
                            <textarea className="gs-input w-full mt-2 h-48 resize-none text-sm" placeholder="Paste the job description here..." />
                        </div>
                        <div>
                            <label className="text-xs font-semibold text-grey-400 uppercase tracking-wider">Your Resume</label>
                            <textarea className="gs-input w-full mt-2 h-48 resize-none text-sm" placeholder="Paste your resume text here..." />
                        </div>
                    </div>
                    <div className="flex justify-end mt-4">
                        <button className="gs-btn-primary text-sm flex items-center gap-2"><Bot size={14} /> Analyze & Optimize</button>
                    </div>
                </div>
            )}

            {activeTab === 'outreach' && (
                <div className="gs-card p-5">
                    <h3 className="gs-section-title mb-4">Outreach Campaigns</h3>
                    <div className="space-y-3">
                        {[
                            { to: 'Jane Martinez — Hiring Manager, Stripe', template: 'Warm Intro', sent: 'Feb 8', status: 'Replied', reply: 'Scheduled call' },
                            { to: 'Leo Park — Recruiter, Vercel', template: 'Follow-up', sent: 'Feb 6', status: 'Replied', reply: 'Moving to next round' },
                            { to: 'Maria Taylor — Head of Eng, Figma', template: 'Cold Outreach', sent: 'Feb 4', status: 'Opened', reply: '—' },
                            { to: 'Sam Williams — VP Eng, Notion', template: 'Warm Intro', sent: 'Feb 2', status: 'Replied', reply: 'Offer discussion' },
                            { to: 'Chris Lee — CTO, Linear', template: 'Cold Outreach', sent: 'Jan 30', status: 'No Response', reply: '—' },
                        ].map(o => (
                            <div key={o.to} className="flex items-center justify-between p-3 rounded-lg hover:bg-grey-800/40 transition-colors">
                                <div>
                                    <p className="text-sm font-medium text-grey-200">{o.to}</p>
                                    <p className="text-xs text-grey-500">{o.template} · Sent {o.sent}</p>
                                </div>
                                <div className="flex items-center gap-3">
                                    <Badge variant={o.status === 'Replied' ? 'success' : o.status === 'Opened' ? 'info' : 'default'}>{o.status}</Badge>
                                </div>
                            </div>
                        ))}
                    </div>
                </div>
            )}

            {activeTab === 'automation' && (
                <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
                    <div className="gs-card p-5">
                        <h3 className="gs-section-title mb-4 flex items-center gap-2"><Bot size={16} /> Auto Search Agents</h3>
                        <div className="space-y-3">
                            {[
                                { name: 'Staff Engineer — Remote', sources: 'LinkedIn, Lever, Greenhouse', matches: 12, active: true },
                                { name: 'Engineering Manager', sources: 'LinkedIn, Indeed', matches: 8, active: true },
                                { name: 'Founding Engineer — AI', sources: 'YC Work at a Startup, LinkedIn', matches: 5, active: false },
                            ].map(a => (
                                <div key={a.name} className="p-3 rounded-lg border border-grey-800 hover:border-grey-700 transition-colors">
                                    <div className="flex items-center justify-between mb-2">
                                        <p className="text-sm font-medium text-grey-200">{a.name}</p>
                                        <Badge variant={a.active ? 'success' : 'default'}>{a.active ? 'Active' : 'Paused'}</Badge>
                                    </div>
                                    <p className="text-xs text-grey-500">Sources: {a.sources}</p>
                                    <p className="text-xs text-grey-400 mt-1">{a.matches} new matches this week</p>
                                </div>
                            ))}
                        </div>
                    </div>
                    <div className="gs-card p-5">
                        <h3 className="gs-section-title mb-4">Automation Rules</h3>
                        <div className="space-y-3">
                            {[
                                { rule: 'Auto-apply to matching roles', trigger: 'New job match', action: 'Submit application', active: true },
                                { rule: 'Follow-up reminder', trigger: '5 days no response', action: 'Send follow-up', active: true },
                                { rule: 'Track company news', trigger: 'Company in pipeline posts news', action: 'Notify', active: true },
                                { rule: 'ATS optimization', trigger: 'New application', action: 'Optimize resume', active: false },
                            ].map(r => (
                                <div key={r.rule} className="flex items-center justify-between p-3 rounded-lg hover:bg-grey-800/40 transition-colors">
                                    <div>
                                        <p className="text-sm text-grey-200">{r.rule}</p>
                                        <p className="text-xs text-grey-500">{r.trigger} → {r.action}</p>
                                    </div>
                                    <div className={`w-8 h-4 rounded-full ${r.active ? 'bg-accent-emerald' : 'bg-grey-700'} relative cursor-pointer`}>
                                        <div className={`w-3 h-3 rounded-full bg-white absolute top-0.5 transition-all ${r.active ? 'right-0.5' : 'left-0.5'}`} />
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
