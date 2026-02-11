import { useState } from 'react';
import { PageHeader, StatCard, Badge, DataTable, Tabs, ProgressBar } from '../../components/ui';
import {
    Briefcase, MapPin, DollarSign, Clock, Building2, Users,
    Search, Bookmark, Send, Filter, ChevronRight, Star,
    CheckCircle, XCircle, ArrowRight, Eye, FileText, Zap
} from 'lucide-react';

const jobPostings = [
    { id: 'JOB-001', title: 'Staff Platform Engineer', company: 'Acme Corporation', location: 'San Francisco, CA (Hybrid)', type: 'Full-time', level: 'Staff', salary: '$220K–$280K', skills: ['TypeScript', 'Kubernetes', 'System Design'], posted: '2d ago', applicants: 42, match: 94, dept: 'Platform Engineering' },
    { id: 'JOB-002', title: 'Senior ML Engineer', company: 'Stark Industries', location: 'Seattle, WA (Remote)', type: 'Full-time', level: 'Senior', salary: '$195K–$245K', skills: ['Python', 'PyTorch', 'MLOps'], posted: '3d ago', applicants: 68, match: 72, dept: 'AI/ML' },
    { id: 'JOB-003', title: 'Engineering Manager, Data', company: 'Globex Industries', location: 'New York, NY (On-site)', type: 'Full-time', level: 'Manager', salary: '$240K–$300K', skills: ['Leadership', 'Data Engineering', 'Spark'], posted: '1d ago', applicants: 24, match: 68, dept: 'Data Platform' },
    { id: 'JOB-004', title: 'Principal Architect', company: 'Umbrella Corp', location: 'London, UK (Remote)', type: 'Full-time', level: 'Principal', salary: '£150K–£200K', skills: ['System Design', 'Microservices', 'Cloud'], posted: '5d ago', applicants: 18, match: 88, dept: 'Architecture' },
    { id: 'JOB-005', title: 'Senior Frontend Engineer', company: 'Initech LLC', location: 'Austin, TX (Hybrid)', type: 'Full-time', level: 'Senior', salary: '$170K–$210K', skills: ['React', 'TypeScript', 'GraphQL'], posted: '1d ago', applicants: 86, match: 82, dept: 'Product Engineering' },
    { id: 'JOB-006', title: 'SRE Lead', company: 'Wayne Enterprises', location: 'Chicago, IL (Remote)', type: 'Full-time', level: 'Lead', salary: '$200K–$250K', skills: ['Kubernetes', 'Terraform', 'Observability'], posted: '4d ago', applicants: 32, match: 76, dept: 'Infrastructure' },
];

const applications = [
    { job: 'Staff Platform Engineer', company: 'Acme Corporation', applied: 'Jan 28, 2026', status: 'Interview', stage: 'Technical Round 2', lastUpdate: '2d ago' },
    { job: 'Principal Architect', company: 'Umbrella Corp', applied: 'Jan 22, 2026', status: 'Screening', stage: 'Recruiter Call', lastUpdate: '5d ago' },
    { job: 'Senior Staff Engineer', company: 'Stripe', applied: 'Jan 15, 2026', status: 'Offer', stage: 'Offer Review', lastUpdate: '1d ago' },
    { job: 'Staff Engineer, Platform', company: 'Datadog', applied: 'Jan 10, 2026', status: 'Rejected', stage: 'After Final Round', lastUpdate: '1w ago' },
];

const savedJobs = [
    { title: 'VP Engineering', company: 'SeriesA Startup', salary: '$300K–$400K + 1.5% equity', saved: '3d ago', deadline: '14d left' },
    { title: 'Distinguished Engineer', company: 'Google Cloud', salary: '$450K–$600K TC', saved: '1w ago', deadline: '21d left' },
    { title: 'CTO', company: 'Climate Tech Startup', salary: '$250K + 4% equity', saved: '2w ago', deadline: '7d left' },
];

const recruiterPipeline = [
    { candidate: 'Alex Turner', role: 'Staff Platform Engineer', stage: 'Phone Screen', score: 88, applied: '2d ago', source: 'Inbound' },
    { candidate: 'Jordan Lee', role: 'Staff Platform Engineer', stage: 'Technical 1', score: 92, applied: '5d ago', source: 'Referral' },
    { candidate: 'Sam Mitchell', role: 'Staff Platform Engineer', stage: 'Technical 2', score: 86, applied: '1w ago', source: 'Search' },
    { candidate: 'Casey Park', role: 'Senior ML Engineer', stage: 'Onsite', score: 94, applied: '2w ago', source: 'Inbound' },
    { candidate: 'Morgan Chen', role: 'Senior ML Engineer', stage: 'Offer', score: 96, applied: '3w ago', source: 'Referral' },
];

const internalMobility = [
    { title: 'Staff Engineer → Engineering Manager', dept: 'Platform → Platform', type: 'Vertical', match: 92, status: 'Open' },
    { title: 'Staff Engineer → Principal Architect', dept: 'Platform → Architecture', type: 'Vertical', match: 88, status: 'Open' },
    { title: 'Staff Engineer → Staff Data Engineer', dept: 'Platform → Data', type: 'Lateral', match: 74, status: 'Open' },
];

export function NetworkJobs() {
    const [activeTab, setActiveTab] = useState('Job Board');

    return (
        <div>
            <PageHeader
                title="Jobs & Recruiting"
                subtitle="Job postings, applications, recruiter tools, and internal mobility"
                actions={
                    <div className="flex gap-2">
                        <button className="gs-btn-secondary flex items-center gap-2"><Filter size={16} /> Filters</button>
                        <button className="gs-btn-primary flex items-center gap-2"><Briefcase size={16} /> Post Job</button>
                    </div>
                }
            />

            <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-4 gap-4 mb-6">
                <StatCard label="Job Matches" value="248" change="94% top match" changeType="positive" icon={<Briefcase size={18} />} />
                <StatCard label="Applications" value="4" change="1 offer stage" changeType="positive" icon={<Send size={18} />} />
                <StatCard label="Saved Jobs" value="3" change="1 deadline soon" changeType="negative" icon={<Bookmark size={18} />} />
                <StatCard label="Recruiter Views" value="18" change="This week" changeType="neutral" icon={<Eye size={18} />} />
            </div>

            <Tabs tabs={['Job Board', 'Applications', 'Saved Jobs', 'Recruiter Tools', 'Internal Mobility', 'Jobs API']} active={activeTab} onChange={setActiveTab} />

            {activeTab === 'Job Board' && (
                <div className="space-y-4">
                    {jobPostings.map(job => (
                        <div key={job.id} className="gs-card p-5 hover:border-grey-700 transition-colors cursor-pointer">
                            <div className="flex items-start justify-between">
                                <div className="flex gap-4">
                                    <div className="w-12 h-12 rounded-xl bg-grey-800 flex items-center justify-center text-sm font-bold text-grey-400">
                                        {job.company.split(' ').map(w => w[0]).join('').slice(0, 2)}
                                    </div>
                                    <div>
                                        <h3 className="text-sm font-semibold text-grey-100 mb-1">{job.title}</h3>
                                        <p className="text-xs text-grey-400 mb-2">{job.company} · {job.dept}</p>
                                        <div className="flex items-center gap-3 text-xs text-grey-500 mb-3">
                                            <span className="flex items-center gap-1"><MapPin size={11} />{job.location}</span>
                                            <span className="flex items-center gap-1"><DollarSign size={11} />{job.salary}</span>
                                            <span className="flex items-center gap-1"><Clock size={11} />{job.posted}</span>
                                        </div>
                                        <div className="flex gap-1.5 flex-wrap">
                                            {job.skills.map(s => (
                                                <span key={s} className="px-2 py-0.5 text-[10px] rounded bg-grey-800 text-grey-400 border border-grey-700">{s}</span>
                                            ))}
                                            <Badge variant={job.level === 'Staff' || job.level === 'Principal' ? 'purple' : job.level === 'Manager' ? 'warning' : 'info'}>{job.level}</Badge>
                                            <Badge variant="default">{job.type}</Badge>
                                        </div>
                                    </div>
                                </div>
                                <div className="text-right flex-shrink-0">
                                    <div className={`text-lg font-bold ${job.match >= 90 ? 'text-accent-emerald' : job.match >= 80 ? 'text-accent-amber' : 'text-grey-400'}`}>{job.match}%</div>
                                    <p className="text-[10px] text-grey-500">match</p>
                                    <p className="text-xs text-grey-600 mt-2">{job.applicants} applicants</p>
                                    <div className="flex gap-2 mt-3">
                                        <button className="gs-btn-ghost text-xs p-1.5"><Bookmark size={14} /></button>
                                        <button className="gs-btn-primary text-xs px-3 py-1.5">Apply</button>
                                    </div>
                                </div>
                            </div>
                        </div>
                    ))}
                </div>
            )}

            {activeTab === 'Applications' && (
                <div className="gs-card p-5">
                    <h3 className="gs-section-title mb-4 flex items-center gap-2"><Send size={16} /> Your Applications</h3>
                    <div className="space-y-3">
                        {applications.map((a, i) => (
                            <div key={i} className="p-4 rounded-lg border border-grey-800">
                                <div className="flex items-center justify-between mb-2">
                                    <div>
                                        <p className="text-sm font-medium text-grey-200">{a.job}</p>
                                        <p className="text-xs text-grey-500">{a.company} · Applied {a.applied}</p>
                                    </div>
                                    <Badge variant={a.status === 'Offer' ? 'success' : a.status === 'Interview' ? 'info' : a.status === 'Screening' ? 'warning' : 'danger'}>{a.status}</Badge>
                                </div>
                                <div className="flex items-center justify-between">
                                    <div className="flex items-center gap-2">
                                        <span className="text-xs text-grey-500">Stage:</span>
                                        <span className="text-xs text-grey-300">{a.stage}</span>
                                    </div>
                                    <span className="text-xs text-grey-600">Updated {a.lastUpdate}</span>
                                </div>
                                {/* Stage Progress */}
                                <div className="flex items-center gap-1 mt-3">
                                    {['Applied', 'Screening', 'Interview', 'Offer'].map((stage, j) => {
                                        const stageMap: Record<string, number> = { 'Rejected': -1, 'Screening': 1, 'Interview': 2, 'Offer': 3 };
                                        const currentStage = stageMap[a.status] ?? 0;
                                        const isComplete = j <= currentStage;
                                        const isRejected = a.status === 'Rejected';
                                        return (
                                            <div key={stage} className="flex items-center gap-1 flex-1">
                                                <div className={`w-full h-1.5 rounded-full ${isRejected && j > 0 ? 'bg-accent-rose/30' : isComplete ? 'bg-brand-500' : 'bg-grey-800'}`} />
                                            </div>
                                        );
                                    })}
                                </div>
                            </div>
                        ))}
                    </div>
                </div>
            )}

            {activeTab === 'Saved Jobs' && (
                <div className="gs-card p-5">
                    <h3 className="gs-section-title mb-4 flex items-center gap-2"><Bookmark size={16} /> Saved Jobs</h3>
                    <div className="space-y-3">
                        {savedJobs.map((j, i) => (
                            <div key={i} className="flex items-center justify-between p-4 rounded-lg border border-grey-800">
                                <div>
                                    <p className="text-sm font-medium text-grey-200">{j.title}</p>
                                    <p className="text-xs text-grey-500">{j.company} · {j.salary}</p>
                                    <p className="text-xs text-grey-600">Saved {j.saved}</p>
                                </div>
                                <div className="flex items-center gap-3">
                                    <Badge variant={j.deadline.includes('7d') ? 'danger' : 'default'}>{j.deadline}</Badge>
                                    <button className="gs-btn-primary text-xs px-3 py-1.5">Apply</button>
                                </div>
                            </div>
                        ))}
                    </div>
                </div>
            )}

            {activeTab === 'Recruiter Tools' && (
                <div className="space-y-6">
                    <div className="gs-card p-5">
                        <h3 className="gs-section-title mb-4 flex items-center gap-2"><Users size={16} /> Recruiting Pipeline</h3>
                        <DataTable
                            columns={['Candidate', 'Role', 'Stage', 'Match Score', 'Applied', 'Source', 'Actions']}
                            rows={recruiterPipeline.map(c => [
                                <span className="font-medium text-grey-200">{c.candidate}</span>,
                                <span className="text-xs text-grey-400">{c.role}</span>,
                                <Badge variant={c.stage === 'Offer' ? 'success' : c.stage === 'Onsite' ? 'purple' : c.stage.includes('Technical') ? 'info' : 'default'}>{c.stage}</Badge>,
                                <span className={`font-bold ${c.score >= 90 ? 'text-accent-emerald' : 'text-accent-amber'}`}>{c.score}%</span>,
                                <span className="text-xs text-grey-500">{c.applied}</span>,
                                <Badge variant={c.source === 'Referral' ? 'success' : c.source === 'Search' ? 'purple' : 'info'}>{c.source}</Badge>,
                                <div className="flex gap-2">
                                    <button className="text-xs text-brand-400 hover:text-brand-300">Review</button>
                                    <button className="text-xs text-grey-500 hover:text-grey-300">Advance</button>
                                </div>,
                            ])}
                        />
                    </div>

                    {/* Pipeline Metrics */}
                    <div className="grid grid-cols-4 gap-4">
                        {[
                            { label: 'Time to Hire', value: '28 days', trend: '-4 days' },
                            { label: 'Offer Accept Rate', value: '78%', trend: '+6%' },
                            { label: 'Pipeline Conversion', value: '12%', trend: '+2%' },
                            { label: 'Referral Rate', value: '34%', trend: '+8%' },
                        ].map(m => (
                            <div key={m.label} className="gs-card p-4 text-center">
                                <p className="text-xl font-bold text-grey-100">{m.value}</p>
                                <p className="text-xs text-grey-500">{m.label}</p>
                                <p className="text-xs text-accent-emerald">{m.trend}</p>
                            </div>
                        ))}
                    </div>
                </div>
            )}

            {activeTab === 'Internal Mobility' && (
                <div className="gs-card p-5">
                    <h3 className="gs-section-title mb-4 flex items-center gap-2"><ArrowRight size={16} /> Internal Mobility Recommendations</h3>
                    <p className="text-xs text-grey-500 mb-4">Powered by HCM integration — recommended internal moves based on skills, performance, and career goals</p>
                    <div className="space-y-3">
                        {internalMobility.map((m, i) => (
                            <div key={i} className="p-4 rounded-lg border border-grey-800 flex items-center justify-between">
                                <div className="flex-1">
                                    <p className="text-sm font-medium text-grey-200">{m.title}</p>
                                    <p className="text-xs text-grey-500">{m.dept} · {m.type} move</p>
                                </div>
                                <div className="flex items-center gap-3">
                                    <span className={`text-sm font-bold ${m.match >= 90 ? 'text-accent-emerald' : m.match >= 80 ? 'text-accent-amber' : 'text-grey-400'}`}>{m.match}% match</span>
                                    <Badge variant="success">{m.status}</Badge>
                                    <button className="text-xs text-brand-400 hover:text-brand-300">Express Interest</button>
                                </div>
                            </div>
                        ))}
                    </div>
                </div>
            )}

            {activeTab === 'Jobs API' && (
                <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
                    <div className="gs-card p-5">
                        <h3 className="gs-section-title mb-4 flex items-center gap-2"><FileText size={16} /> Jobs Service Endpoints</h3>
                        <div className="space-y-2">
                            {[
                                { method: 'POST', path: '/api/v1/jobs', desc: 'Create job posting' },
                                { method: 'GET', path: '/api/v1/jobs/search', desc: 'Search jobs with filters' },
                                { method: 'GET', path: '/api/v1/jobs/{id}', desc: 'Get job posting details' },
                                { method: 'POST', path: '/api/v1/jobs/{id}/apply', desc: 'Apply to job' },
                                { method: 'GET', path: '/api/v1/jobs/{id}/applications', desc: 'List applications (recruiter)' },
                                { method: 'PUT', path: '/api/v1/applications/{id}/stage', desc: 'Advance application stage' },
                                { method: 'GET', path: '/api/v1/jobs/saved', desc: 'Saved jobs for current user' },
                                { method: 'POST', path: '/api/v1/jobs/{id}/save', desc: 'Save/bookmark a job' },
                                { method: 'GET', path: '/api/v1/jobs/recommended', desc: 'AI-matched job recommendations' },
                                { method: 'GET', path: '/api/v1/mobility/internal', desc: 'Internal mobility suggestions (HCM)' },
                            ].map((e, i) => (
                                <div key={i} className="flex items-center gap-3 p-2.5 rounded-lg bg-grey-800/30">
                                    <Badge variant={e.method === 'GET' ? 'info' : e.method === 'POST' ? 'success' : 'warning'}>{e.method}</Badge>
                                    <span className="font-mono text-xs text-accent-cyan flex-1">{e.path}</span>
                                    <span className="text-xs text-grey-500">{e.desc}</span>
                                </div>
                            ))}
                        </div>
                    </div>
                    <div className="gs-card p-5">
                        <h3 className="gs-section-title mb-4">HCM + Career Integration</h3>
                        <div className="bg-grey-900/50 rounded-lg p-4 font-mono text-xs space-y-2">
                            <div className="text-grey-500">// Job recommendation pipeline</div>
                            <div className="text-accent-cyan">Sources:</div>
                            <div className="pl-3 text-grey-300">1. <span className="text-accent-emerald">Profile Skills</span> → skill_match_score</div>
                            <div className="pl-3 text-grey-300">2. <span className="text-accent-emerald">Work History</span> → title_similarity</div>
                            <div className="pl-3 text-grey-300">3. <span className="text-accent-emerald">Career Goals</span> → trajectory_alignment</div>
                            <div className="pl-3 text-grey-300">4. <span className="text-accent-emerald">Network Graph</span> → company_connections</div>
                            <div className="pl-3 text-grey-300">5. <span className="text-accent-emerald">HCM Performance</span> → readiness_score</div>
                            <div className="mt-3 text-accent-cyan">Ranking:</div>
                            <div className="pl-3 text-grey-400">composite = 0.35 * skill_match</div>
                            <div className="pl-3 text-grey-400">          + 0.25 * title_similarity</div>
                            <div className="pl-3 text-grey-400">          + 0.20 * trajectory_alignment</div>
                            <div className="pl-3 text-grey-400">          + 0.10 * company_connections</div>
                            <div className="pl-3 text-grey-400">          + 0.10 * readiness_score</div>
                        </div>
                    </div>
                </div>
            )}
        </div>
    );
}
