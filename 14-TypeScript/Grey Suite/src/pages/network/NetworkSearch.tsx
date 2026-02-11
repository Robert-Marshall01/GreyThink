import { useState } from 'react';
import { PageHeader, StatCard, Badge, DataTable, Tabs, ProgressBar, MiniSparkline } from '../../components/ui';
import {
    Search, Users, Briefcase, Building2, Award, Zap,
    MapPin, Filter, Star, TrendingUp, Brain, Share2,
    ArrowRight, Lightbulb, Target, Sparkles, Eye
} from 'lucide-react';

const peopleResults = [
    { name: 'Rachel Wong', headline: 'Staff Engineer @ Netflix', location: 'Los Gatos, CA', mutual: 22, skills: ['Distributed Systems', 'Java', 'Kafka'], match: 92, avatar: 'RW' },
    { name: 'Carlos Rivera', headline: 'Engineering Manager @ Stripe', location: 'San Francisco, CA', mutual: 8, skills: ['Leadership', 'Payments', 'Go'], match: 84, avatar: 'CR' },
    { name: 'Lisa Su', headline: 'Principal Engineer @ AMD', location: 'Santa Clara, CA', mutual: 4, skills: ['Hardware', 'Chip Design', 'C++'], match: 68, avatar: 'LS' },
    { name: 'Tom Zhang', headline: 'Staff SRE @ Google', location: 'Mountain View, CA', mutual: 14, skills: ['SRE', 'Kubernetes', 'Go'], match: 88, avatar: 'TZ' },
    { name: 'Eva Müller', headline: 'VP Architecture @ SAP', location: 'Berlin, Germany', mutual: 6, skills: ['Enterprise', 'ABAP', 'Cloud'], match: 72, avatar: 'EM' },
];

const jobResults = [
    { title: 'Staff Platform Engineer', company: 'Acme Corp', location: 'SF (Hybrid)', salary: '$220K–$280K', match: 94, posted: '2d ago', skills: ['TypeScript', 'K8s', 'System Design'] },
    { title: 'Principal Architect', company: 'Umbrella Corp', location: 'London (Remote)', salary: '£150K–£200K', match: 88, posted: '5d ago', skills: ['Architecture', 'Cloud', 'Microservices'] },
    { title: 'Senior Frontend Engineer', company: 'Initech', location: 'Austin (Hybrid)', salary: '$170K–$210K', match: 82, posted: '1d ago', skills: ['React', 'TypeScript', 'GraphQL'] },
];

const companyResults = [
    { name: 'Acme Corporation', industry: 'Enterprise Software', size: '5K-10K', employees: 42, openRoles: 42, connections: 18 },
    { name: 'Stark Industries', industry: 'Aerospace & Defense', size: '10K+', employees: 124, openRoles: 124, connections: 8 },
    { name: 'Globex Industries', industry: 'Cloud Infrastructure', size: '1K-5K', employees: 18, openRoles: 18, connections: 12 },
];

const pymk = [
    { name: 'Emily Rodriguez', headline: 'Staff SRE @ Datadog', reason: '8 mutual connections + similar skills', mutual: 8, match: 92, avatar: 'ER' },
    { name: 'Alex Turner', headline: 'CTO @ SeriesA Startup', reason: 'Connected to 3 of your close contacts', mutual: 3, match: 86, avatar: 'AT' },
    { name: 'Jordan Lee', headline: 'Senior Engineer @ Vercel', reason: 'Same alma mater + overlapping skills', mutual: 5, match: 84, avatar: 'JL' },
    { name: 'Sam Mitchell', headline: 'Staff Engineer @ Cloudflare', reason: 'Works with 4 of your connections', mutual: 4, match: 82, avatar: 'SM' },
    { name: 'Casey Park', headline: 'Principal PM @ Figma', reason: 'Shared interests: System Design, DX', mutual: 6, match: 78, avatar: 'CP' },
    { name: 'Morgan Chen', headline: 'Engineering Manager @ Airbnb', reason: 'Former colleague at Globex Industries', mutual: 12, match: 94, avatar: 'MC' },
];

const jobRecommendations = [
    { title: 'Staff Platform Engineer', company: 'Acme Corp', match: 94, reasons: ['24/24 skills match', 'Title alignment', '18 connections there'] },
    { title: 'Principal Architect', company: 'Umbrella Corp', match: 88, reasons: ['20/24 skills match', 'Graph-based rec', 'Growing team'] },
    { title: 'Senior Staff Engineer', company: 'Stripe', match: 86, reasons: ['22/24 skills match', 'Past applicant', 'Referral available'] },
];

const skillRecommendations = [
    { skill: 'Rust', category: 'Languages', reason: 'Growing demand (+56%) in your network', currentLevel: 'Intermediate', targetLevel: 'Advanced', demand: 'Very High' },
    { skill: 'gRPC', category: 'APIs', reason: 'Used by 62% of your 1st-degree connections', currentLevel: 'Beginner', targetLevel: 'Intermediate', demand: 'High' },
    { skill: 'Observability', category: 'Infrastructure', reason: 'Required in 4 of your saved jobs', currentLevel: 'Intermediate', targetLevel: 'Advanced', demand: 'High' },
    { skill: 'Team Leadership', category: 'Management', reason: 'Aligns with your Staff→EM career goal', currentLevel: 'Beginner', targetLevel: 'Intermediate', demand: 'Very High' },
];

export function NetworkSearch() {
    const [activeTab, setActiveTab] = useState('People Search');
    const [searchQuery, setSearchQuery] = useState('');

    return (
        <div>
            <PageHeader
                title="Search & Recommendations"
                subtitle="People, job, company, and skill search with graph-based recommendations"
                actions={
                    <button className="gs-btn-secondary flex items-center gap-2"><Filter size={16} /> Advanced Filters</button>
                }
            />

            {/* Universal Search Bar */}
            <div className="gs-card p-4 mb-6">
                <div className="flex items-center gap-3">
                    <Search size={18} className="text-grey-500" />
                    <input
                        type="text"
                        placeholder="Search people, jobs, companies, skills..."
                        value={searchQuery}
                        onChange={e => setSearchQuery(e.target.value)}
                        className="flex-1 bg-transparent text-sm text-grey-200 placeholder-grey-600 outline-none"
                    />
                    <div className="flex gap-2">
                        <button className="px-3 py-1.5 text-xs rounded-lg bg-grey-800 text-grey-400 hover:text-grey-200">People</button>
                        <button className="px-3 py-1.5 text-xs rounded-lg bg-grey-800 text-grey-400 hover:text-grey-200">Jobs</button>
                        <button className="px-3 py-1.5 text-xs rounded-lg bg-grey-800 text-grey-400 hover:text-grey-200">Companies</button>
                        <button className="px-3 py-1.5 text-xs rounded-lg bg-grey-800 text-grey-400 hover:text-grey-200">Skills</button>
                    </div>
                </div>
            </div>

            <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-4 gap-4 mb-6">
                <StatCard label="Search Index" value="156K" change="People, jobs, skills" changeType="neutral" icon={<Search size={18} />} />
                <StatCard label="PYMK Candidates" value="2,480" change="Graph-matched" changeType="neutral" icon={<Users size={18} />} />
                <StatCard label="Job Matches" value="248" change="Skill-aligned" changeType="positive" icon={<Briefcase size={18} />} />
                <StatCard label="Skill Suggestions" value="4" change="Based on goals" changeType="neutral" icon={<Lightbulb size={18} />} />
            </div>

            <Tabs tabs={['People Search', 'Job Search', 'Company Search', 'People You May Know', 'Job Recommendations', 'Skill Recs', 'Search API']} active={activeTab} onChange={setActiveTab} />

            {activeTab === 'People Search' && (
                <div className="gs-card p-5">
                    <h3 className="gs-section-title mb-4 flex items-center gap-2"><Users size={16} /> People Results</h3>
                    <div className="space-y-3">
                        {peopleResults.map((p, i) => (
                            <div key={i} className="flex items-center justify-between p-4 rounded-lg border border-grey-800 hover:border-grey-700 transition-colors">
                                <div className="flex items-center gap-3">
                                    <div className="w-12 h-12 rounded-full bg-grey-800 flex items-center justify-center text-sm font-bold text-grey-400">{p.avatar}</div>
                                    <div>
                                        <p className="text-sm font-medium text-grey-200">{p.name}</p>
                                        <p className="text-xs text-grey-500">{p.headline}</p>
                                        <div className="flex items-center gap-2 mt-1">
                                            <span className="text-xs text-grey-600 flex items-center gap-1"><MapPin size={10} />{p.location}</span>
                                            <span className="text-xs text-grey-600">{p.mutual} mutual</span>
                                        </div>
                                        <div className="flex gap-1 mt-1.5">
                                            {p.skills.map(s => <span key={s} className="text-[10px] px-1.5 py-0.5 rounded bg-grey-800 text-grey-400">{s}</span>)}
                                        </div>
                                    </div>
                                </div>
                                <div className="flex items-center gap-3">
                                    <span className={`text-sm font-bold ${p.match >= 90 ? 'text-accent-emerald' : p.match >= 80 ? 'text-accent-amber' : 'text-grey-400'}`}>{p.match}%</span>
                                    <button className="gs-btn-primary text-xs px-3 py-1.5">Connect</button>
                                </div>
                            </div>
                        ))}
                    </div>
                </div>
            )}

            {activeTab === 'Job Search' && (
                <div className="gs-card p-5">
                    <h3 className="gs-section-title mb-4 flex items-center gap-2"><Briefcase size={16} /> Job Results</h3>
                    <DataTable
                        columns={['Role', 'Company', 'Location', 'Salary', 'Skills', 'Match', 'Posted']}
                        rows={jobResults.map(j => [
                            <span className="font-medium text-grey-200">{j.title}</span>,
                            <span className="text-grey-300">{j.company}</span>,
                            <span className="text-xs text-grey-400 flex items-center gap-1"><MapPin size={10} />{j.location}</span>,
                            <span className="text-xs text-accent-emerald">{j.salary}</span>,
                            <div className="flex gap-1">{j.skills.map(s => <span key={s} className="text-[10px] px-1.5 py-0.5 rounded bg-grey-800 text-grey-400">{s}</span>)}</div>,
                            <span className={`font-bold ${j.match >= 90 ? 'text-accent-emerald' : 'text-accent-amber'}`}>{j.match}%</span>,
                            <span className="text-xs text-grey-500">{j.posted}</span>,
                        ])}
                    />
                </div>
            )}

            {activeTab === 'Company Search' && (
                <div className="gs-card p-5">
                    <h3 className="gs-section-title mb-4 flex items-center gap-2"><Building2 size={16} /> Company Results</h3>
                    <DataTable
                        columns={['Company', 'Industry', 'Size', 'Open Roles', 'Your Connections', 'Actions']}
                        rows={companyResults.map(c => [
                            <span className="font-medium text-grey-200">{c.name}</span>,
                            <Badge variant="info">{c.industry}</Badge>,
                            <span className="text-xs text-grey-400">{c.size}</span>,
                            <span className="text-accent-emerald font-medium">{c.openRoles}</span>,
                            <span className="text-xs text-grey-400">{c.connections} people</span>,
                            <button className="text-xs text-brand-400 hover:text-brand-300">Follow</button>,
                        ])}
                    />
                </div>
            )}

            {activeTab === 'People You May Know' && (
                <div className="gs-card p-5">
                    <h3 className="gs-section-title mb-4 flex items-center gap-2"><Sparkles size={16} /> People You May Know</h3>
                    <p className="text-xs text-grey-500 mb-4">Graph-based recommendations using mutual connections, shared skills, and network proximity</p>
                    <div className="space-y-3">
                        {pymk.map((p, i) => (
                            <div key={i} className="flex items-center justify-between p-4 rounded-lg border border-grey-800">
                                <div className="flex items-center gap-3">
                                    <div className="w-12 h-12 rounded-full bg-grey-800 flex items-center justify-center text-sm font-bold text-grey-400">{p.avatar}</div>
                                    <div>
                                        <p className="text-sm font-medium text-grey-200">{p.name}</p>
                                        <p className="text-xs text-grey-500">{p.headline}</p>
                                        <p className="text-xs text-brand-400 mt-1 flex items-center gap-1"><Lightbulb size={10} />{p.reason}</p>
                                    </div>
                                </div>
                                <div className="flex items-center gap-3">
                                    <div className="text-right">
                                        <span className="text-sm font-bold text-accent-emerald">{p.match}%</span>
                                        <p className="text-[10px] text-grey-500">{p.mutual} mutual</p>
                                    </div>
                                    <button className="gs-btn-primary text-xs px-3 py-1.5">Connect</button>
                                </div>
                            </div>
                        ))}
                    </div>
                </div>
            )}

            {activeTab === 'Job Recommendations' && (
                <div className="gs-card p-5">
                    <h3 className="gs-section-title mb-4 flex items-center gap-2"><Target size={16} /> Recommended Jobs For You</h3>
                    <div className="space-y-3">
                        {jobRecommendations.map((j, i) => (
                            <div key={i} className="p-4 rounded-lg border border-grey-800">
                                <div className="flex items-center justify-between mb-2">
                                    <div>
                                        <p className="text-sm font-medium text-grey-200">{j.title}</p>
                                        <p className="text-xs text-grey-500">{j.company}</p>
                                    </div>
                                    <span className="text-lg font-bold text-accent-emerald">{j.match}%</span>
                                </div>
                                <div className="flex gap-2 flex-wrap">
                                    {j.reasons.map((r, k) => (
                                        <span key={k} className="text-[10px] px-2 py-0.5 rounded bg-accent-emerald/10 text-accent-emerald border border-accent-emerald/20">{r}</span>
                                    ))}
                                </div>
                            </div>
                        ))}
                    </div>
                </div>
            )}

            {activeTab === 'Skill Recs' && (
                <div className="gs-card p-5">
                    <h3 className="gs-section-title mb-4 flex items-center gap-2"><Award size={16} /> Skills To Learn</h3>
                    <div className="space-y-3">
                        {skillRecommendations.map((s, i) => (
                            <div key={i} className="p-4 rounded-lg border border-grey-800">
                                <div className="flex items-center justify-between mb-2">
                                    <div className="flex items-center gap-2">
                                        <span className="font-medium text-grey-200">{s.skill}</span>
                                        <Badge variant="purple">{s.category}</Badge>
                                    </div>
                                    <Badge variant={s.demand === 'Very High' ? 'danger' : 'warning'}>{s.demand} demand</Badge>
                                </div>
                                <p className="text-xs text-brand-400 mb-2 flex items-center gap-1"><Lightbulb size={10} />{s.reason}</p>
                                <div className="flex items-center gap-2 text-xs">
                                    <span className="text-grey-500">Current: <span className="text-grey-300">{s.currentLevel}</span></span>
                                    <ArrowRight size={10} className="text-grey-600" />
                                    <span className="text-grey-500">Target: <span className="text-accent-emerald">{s.targetLevel}</span></span>
                                </div>
                            </div>
                        ))}
                    </div>
                </div>
            )}

            {activeTab === 'Search API' && (
                <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
                    <div className="gs-card p-5">
                        <h3 className="gs-section-title mb-4 flex items-center gap-2"><Search size={16} /> Search Service API</h3>
                        <div className="space-y-2">
                            {[
                                { method: 'GET', path: '/api/v1/search/people', desc: 'Search people by name, skills, location' },
                                { method: 'GET', path: '/api/v1/search/jobs', desc: 'Search jobs with filters' },
                                { method: 'GET', path: '/api/v1/search/companies', desc: 'Search companies by name, industry' },
                                { method: 'GET', path: '/api/v1/search/skills', desc: 'Search skill directory' },
                                { method: 'GET', path: '/api/v1/recommendations/pymk', desc: 'People you may know' },
                                { method: 'GET', path: '/api/v1/recommendations/jobs', desc: 'Recommended jobs' },
                                { method: 'GET', path: '/api/v1/recommendations/companies', desc: 'Companies to follow' },
                                { method: 'GET', path: '/api/v1/recommendations/skills', desc: 'Skills to learn' },
                            ].map((e, i) => (
                                <div key={i} className="flex items-center gap-3 p-2.5 rounded-lg bg-grey-800/30">
                                    <Badge variant="info">{e.method}</Badge>
                                    <span className="font-mono text-xs text-accent-cyan flex-1">{e.path}</span>
                                    <span className="text-xs text-grey-500">{e.desc}</span>
                                </div>
                            ))}
                        </div>
                    </div>
                    <div className="gs-card p-5">
                        <h3 className="gs-section-title mb-4">Recommendation Engine</h3>
                        <div className="bg-grey-900/50 rounded-lg p-4 font-mono text-xs space-y-2">
                            <div className="text-grey-500">// PYMK recommendation pipeline (v1: rule-based)</div>
                            <div className="text-accent-cyan">Phase 1: Candidate Generation</div>
                            <div className="pl-3 text-grey-300">→ 2-hop neighbors in connection graph</div>
                            <div className="pl-3 text-grey-300">→ Same company employees (via employment edges)</div>
                            <div className="pl-3 text-grey-300">→ Same skill cluster members</div>
                            <div className="text-accent-cyan mt-2">Phase 2: Scoring</div>
                            <div className="pl-3 text-grey-300">mutual_connections * <span className="text-accent-amber">0.40</span></div>
                            <div className="pl-3 text-grey-300">+ skill_overlap * <span className="text-accent-amber">0.25</span></div>
                            <div className="pl-3 text-grey-300">+ company_affinity * <span className="text-accent-amber">0.20</span></div>
                            <div className="pl-3 text-grey-300">+ profile_completeness * <span className="text-accent-amber">0.15</span></div>
                            <div className="text-accent-cyan mt-2">Phase 3: Filtering</div>
                            <div className="pl-3 text-grey-300">→ Remove blocked / muted users</div>
                            <div className="pl-3 text-grey-300">→ Remove recently declined</div>
                            <div className="pl-3 text-grey-300">→ Diversity sampling</div>
                            <div className="text-accent-cyan mt-2">Phase 4: Ranking</div>
                            <div className="pl-3 text-grey-300">→ Top-K by composite score</div>
                            <div className="pl-3 text-grey-300">→ Interleave with explore candidates (10%)</div>
                            <div className="mt-3 text-grey-600">// v2: Replace scoring with embedding cosine similarity</div>
                            <div className="text-grey-600">// v3: Add GNN-based link prediction</div>
                        </div>
                    </div>
                </div>
            )}
        </div>
    );
}
