import { useState } from 'react';
import { PageHeader, StatCard, Badge, DataTable, Tabs, ProgressBar } from '../../components/ui';
import {
    Share2, Users, Building2, Award, Briefcase, Heart,
    Link2, GitBranch, ArrowRight, Eye, Plus, UserPlus,
    CheckCircle, Clock, XCircle, Zap, Network, Search
} from 'lucide-react';

const connectionRequests = [
    { from: 'Marcus Johnson', headline: 'VP Engineering @ Globex', mutual: 12, status: 'pending', sent: '2h ago', avatar: 'MJ' },
    { from: 'Emily Rodriguez', headline: 'Staff SRE @ Datadog', mutual: 8, status: 'pending', sent: '5h ago', avatar: 'ER' },
    { from: 'Alex Turner', headline: 'CTO @ SeriesA Startup', mutual: 3, status: 'pending', sent: '1d ago', avatar: 'AT' },
];

const connections = [
    { name: 'James O\'Brien', headline: 'Principal Architect @ Umbrella', since: 'Mar 2022', mutual: 42, degree: 1, avatar: 'JO' },
    { name: 'Anika Gupta', headline: 'Director of Data Science @ Stark', since: 'Jun 2023', mutual: 28, degree: 1, avatar: 'AG' },
    { name: 'Priya Patel', headline: 'Senior PM @ Initech', since: 'Sep 2023', mutual: 18, degree: 1, avatar: 'PP' },
    { name: 'David Kim', headline: 'Senior SRE @ Wayne Enterprises', since: 'Jan 2024', mutual: 14, degree: 1, avatar: 'DK' },
    { name: 'Rachel Wong', headline: 'Staff Engineer @ Netflix', since: 'Apr 2024', mutual: 22, degree: 1, avatar: 'RW' },
    { name: 'Carlos Rivera', headline: 'Engineering Manager @ Stripe', since: 'Jul 2024', mutual: 8, degree: 1, avatar: 'CR' },
];

const employmentEdges = [
    { person: 'Sarah Chen', company: 'Acme Corporation', role: 'Staff Engineer', type: 'WORKS_AT', since: 'Jan 2024', status: 'Current' },
    { person: 'Marcus Johnson', company: 'Globex Industries', role: 'VP Engineering', type: 'WORKS_AT', since: 'Mar 2020', status: 'Current' },
    { person: 'James O\'Brien', company: 'Umbrella Corp', role: 'Principal Architect', type: 'WORKS_AT', since: 'Sep 2021', status: 'Current' },
    { person: 'Sarah Chen', company: 'Globex Industries', role: 'Senior Engineer', type: 'WORKED_AT', since: 'Mar 2021', status: 'Past' },
    { person: 'Sarah Chen', company: 'Initech LLC', role: 'Software Engineer II', type: 'WORKED_AT', since: 'Jun 2019', status: 'Past' },
];

const skillEdges = [
    { person: 'Sarah Chen', skill: 'TypeScript', endorsers: 86, type: 'HAS_SKILL', proficiency: 'Expert' },
    { person: 'Sarah Chen', skill: 'System Design', endorsers: 64, type: 'HAS_SKILL', proficiency: 'Expert' },
    { person: 'Sarah Chen', skill: 'Kubernetes', endorsers: 48, type: 'HAS_SKILL', proficiency: 'Advanced' },
    { person: 'Sarah Chen', skill: 'React', endorsers: 72, type: 'HAS_SKILL', proficiency: 'Expert' },
    { person: 'Sarah Chen', skill: 'Rust', endorsers: 24, type: 'HAS_SKILL', proficiency: 'Intermediate' },
    { person: 'Sarah Chen', skill: 'PostgreSQL', endorsers: 58, type: 'HAS_SKILL', proficiency: 'Advanced' },
];

const followEdges = [
    { follower: 'Sarah Chen', target: 'Acme Corporation', type: 'FOLLOWS_COMPANY', since: 'Jan 2024' },
    { follower: 'Sarah Chen', target: 'Distributed Systems', type: 'FOLLOWS_TOPIC', since: 'Mar 2022' },
    { follower: 'Sarah Chen', target: 'Stark Industries', type: 'FOLLOWS_COMPANY', since: 'Jun 2023' },
    { follower: 'Sarah Chen', target: 'Platform Engineering', type: 'FOLLOWS_TOPIC', since: 'Sep 2023' },
    { follower: 'Sarah Chen', target: 'Rust Programming', type: 'FOLLOWS_TOPIC', since: 'Feb 2024' },
    { follower: 'Sarah Chen', target: 'Umbrella Corp', type: 'FOLLOWS_COMPANY', since: 'Nov 2023' },
];

const mutualConnections = [
    { name: 'Priya Patel', mutualWith: 'James O\'Brien', sharedConnections: ['Rachel Wong', 'David Kim', 'Carlos Rivera'], sharedSkills: ['System Design', 'TypeScript'] },
    { name: 'Emily Rodriguez', mutualWith: 'James O\'Brien', sharedConnections: ['Anika Gupta', 'David Kim'], sharedSkills: ['Kubernetes', 'SRE'] },
];

const shortestPaths = [
    { from: 'Sarah Chen', to: 'Satya Nadella', hops: 3, path: ['Sarah Chen', 'James O\'Brien', 'Lisa Su', 'Satya Nadella'], via: ['CONNECTED_TO', 'CONNECTED_TO', 'CONNECTED_TO'] },
    { from: 'Sarah Chen', to: 'Kelsey Hightower', hops: 2, path: ['Sarah Chen', 'Rachel Wong', 'Kelsey Hightower'], via: ['CONNECTED_TO', 'CONNECTED_TO'] },
];

export function NetworkGraph() {
    const [activeTab, setActiveTab] = useState('Connections');

    return (
        <div>
            <PageHeader
                title="Professional Graph"
                subtitle="Connection graph, employment edges, skill associations, and relationship traversal"
                actions={
                    <button className="gs-btn-primary flex items-center gap-2"><UserPlus size={16} /> Connect</button>
                }
            />

            <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-4 gap-4 mb-6">
                <StatCard label="Connections" value="842" change="1st degree" changeType="neutral" icon={<Users size={18} />} />
                <StatCard label="2nd Degree Reach" value="48,240" change="Via mutual" changeType="neutral" icon={<Share2 size={18} />} />
                <StatCard label="Graph Edges" value="126K" change="6 edge types" changeType="neutral" icon={<GitBranch size={18} />} />
                <StatCard label="Network Strength" value="8.4/10" change="Top 12%" changeType="positive" icon={<Zap size={18} />} />
            </div>

            <Tabs tabs={['Connections', 'Employment', 'Skills Graph', 'Follows', 'Mutual Analysis', 'Graph API']} active={activeTab} onChange={setActiveTab} />

            {activeTab === 'Connections' && (
                <div className="space-y-6">
                    {/* Pending Requests */}
                    <div className="gs-card p-5">
                        <h3 className="gs-section-title mb-4 flex items-center gap-2"><Clock size={16} /> Pending Requests ({connectionRequests.length})</h3>
                        <div className="space-y-3">
                            {connectionRequests.map((r, i) => (
                                <div key={i} className="flex items-center justify-between p-3 rounded-lg border border-grey-800">
                                    <div className="flex items-center gap-3">
                                        <div className="w-10 h-10 rounded-full bg-grey-800 flex items-center justify-center text-sm font-bold text-grey-400">{r.avatar}</div>
                                        <div>
                                            <p className="text-sm font-medium text-grey-200">{r.from}</p>
                                            <p className="text-xs text-grey-500">{r.headline}</p>
                                            <p className="text-xs text-grey-600">{r.mutual} mutual connections · {r.sent}</p>
                                        </div>
                                    </div>
                                    <div className="flex gap-2">
                                        <button className="gs-btn-primary text-xs px-3 py-1.5 flex items-center gap-1"><CheckCircle size={12} /> Accept</button>
                                        <button className="gs-btn-ghost text-xs px-3 py-1.5 flex items-center gap-1"><XCircle size={12} /> Decline</button>
                                    </div>
                                </div>
                            ))}
                        </div>
                    </div>

                    {/* Connection List */}
                    <div className="gs-card p-5">
                        <h3 className="gs-section-title mb-4 flex items-center gap-2"><Users size={16} /> Your Connections</h3>
                        <DataTable
                            columns={['Person', 'Headline', 'Connected Since', 'Mutual', 'Degree', 'Actions']}
                            rows={connections.map(c => [
                                <div className="flex items-center gap-2">
                                    <div className="w-8 h-8 rounded-full bg-grey-800 flex items-center justify-center text-xs font-bold text-grey-400">{c.avatar}</div>
                                    <span className="font-medium text-grey-200">{c.name}</span>
                                </div>,
                                <span className="text-xs text-grey-400">{c.headline}</span>,
                                <span className="text-xs text-grey-500">{c.since}</span>,
                                <span className="text-xs text-grey-400">{c.mutual} shared</span>,
                                <Badge variant="success">{c.degree}°</Badge>,
                                <button className="text-xs text-brand-400 hover:text-brand-300">Message</button>,
                            ])}
                        />
                    </div>
                </div>
            )}

            {activeTab === 'Employment' && (
                <div className="gs-card p-5">
                    <h3 className="gs-section-title mb-4 flex items-center gap-2"><Building2 size={16} /> Employment Edges (WORKS_AT / WORKED_AT)</h3>
                    <DataTable
                        columns={['Person', 'Edge Type', 'Company', 'Role', 'Since', 'Status']}
                        rows={employmentEdges.map(e => [
                            <span className="font-medium text-grey-200">{e.person}</span>,
                            <span className="font-mono text-xs text-accent-cyan">{e.type}</span>,
                            <span className="text-grey-300">{e.company}</span>,
                            <span className="text-xs text-grey-400">{e.role}</span>,
                            <span className="text-xs text-grey-500">{e.since}</span>,
                            <Badge variant={e.status === 'Current' ? 'success' : 'default'}>{e.status}</Badge>,
                        ])}
                    />
                </div>
            )}

            {activeTab === 'Skills Graph' && (
                <div className="gs-card p-5">
                    <h3 className="gs-section-title mb-4 flex items-center gap-2"><Award size={16} /> Skill Edges (HAS_SKILL / ENDORSED_FOR)</h3>
                    <div className="space-y-3">
                        {skillEdges.map((s, i) => (
                            <div key={i} className="p-3 rounded-lg border border-grey-800">
                                <div className="flex items-center justify-between mb-2">
                                    <div className="flex items-center gap-3">
                                        <span className="font-mono text-xs text-accent-cyan bg-grey-800/50 px-2 py-0.5 rounded">{s.person}</span>
                                        <ArrowRight size={12} className="text-grey-600" />
                                        <span className="font-mono text-xs text-accent-violet">HAS_SKILL</span>
                                        <ArrowRight size={12} className="text-grey-600" />
                                        <span className="font-mono text-xs text-accent-emerald bg-grey-800/50 px-2 py-0.5 rounded">{s.skill}</span>
                                    </div>
                                    <Badge variant={s.proficiency === 'Expert' ? 'success' : s.proficiency === 'Advanced' ? 'info' : 'default'}>{s.proficiency}</Badge>
                                </div>
                                <div className="flex items-center gap-2">
                                    <span className="text-xs text-grey-500">{s.endorsers} endorsements</span>
                                    <div className="flex-1"><ProgressBar value={s.endorsers} max={100} color="bg-accent-emerald" size="sm" /></div>
                                </div>
                            </div>
                        ))}
                    </div>
                </div>
            )}

            {activeTab === 'Follows' && (
                <div className="gs-card p-5">
                    <h3 className="gs-section-title mb-4 flex items-center gap-2"><Heart size={16} /> Follow Edges</h3>
                    <DataTable
                        columns={['Follower', 'Edge Type', 'Target', 'Since']}
                        rows={followEdges.map(f => [
                            <span className="text-grey-200">{f.follower}</span>,
                            <span className="font-mono text-xs text-accent-amber">{f.type}</span>,
                            <span className="text-grey-300">{f.target}</span>,
                            <span className="text-xs text-grey-500">{f.since}</span>,
                        ])}
                    />
                </div>
            )}

            {activeTab === 'Mutual Analysis' && (
                <div className="space-y-6">
                    {/* Mutual Connections */}
                    <div className="gs-card p-5">
                        <h3 className="gs-section-title mb-4 flex items-center gap-2"><Share2 size={16} /> Mutual Connection Analysis</h3>
                        {mutualConnections.map((m, i) => (
                            <div key={i} className="p-4 rounded-lg border border-grey-800 mb-3">
                                <div className="flex items-center gap-2 mb-2">
                                    <span className="text-sm font-medium text-grey-200">{m.name}</span>
                                    <span className="text-xs text-grey-600">↔</span>
                                    <span className="text-sm font-medium text-grey-200">{m.mutualWith}</span>
                                </div>
                                <div className="grid grid-cols-2 gap-3">
                                    <div>
                                        <p className="text-xs text-grey-500 mb-1">Shared Connections</p>
                                        <div className="flex gap-1 flex-wrap">
                                            {m.sharedConnections.map(c => <span key={c} className="text-[10px] px-1.5 py-0.5 rounded bg-grey-800 text-grey-400">{c}</span>)}
                                        </div>
                                    </div>
                                    <div>
                                        <p className="text-xs text-grey-500 mb-1">Shared Skills</p>
                                        <div className="flex gap-1 flex-wrap">
                                            {m.sharedSkills.map(s => <span key={s} className="text-[10px] px-1.5 py-0.5 rounded bg-brand-500/10 text-brand-400">{s}</span>)}
                                        </div>
                                    </div>
                                </div>
                            </div>
                        ))}
                    </div>

                    {/* Shortest Path */}
                    <div className="gs-card p-5">
                        <h3 className="gs-section-title mb-4 flex items-center gap-2"><Network size={16} /> Shortest Path Queries</h3>
                        {shortestPaths.map((sp, i) => (
                            <div key={i} className="p-4 rounded-lg border border-grey-800 mb-3">
                                <div className="flex items-center justify-between mb-3">
                                    <span className="text-sm font-medium text-grey-200">{sp.from} → {sp.to}</span>
                                    <Badge variant="info">{sp.hops} hops</Badge>
                                </div>
                                <div className="flex items-center gap-2 flex-wrap">
                                    {sp.path.map((node, j) => (
                                        <span key={j} className="flex items-center gap-2">
                                            <span className={`text-xs px-2 py-1 rounded ${j === 0 || j === sp.path.length - 1 ? 'bg-brand-500/10 text-brand-400 border border-brand-500/20' : 'bg-grey-800 text-grey-300'}`}>{node}</span>
                                            {j < sp.path.length - 1 && (
                                                <span className="flex items-center gap-1">
                                                    <span className="text-[9px] text-grey-600 font-mono">{sp.via[j]}</span>
                                                    <ArrowRight size={10} className="text-grey-600" />
                                                </span>
                                            )}
                                        </span>
                                    ))}
                                </div>
                            </div>
                        ))}
                    </div>
                </div>
            )}

            {activeTab === 'Graph API' && (
                <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
                    <div className="gs-card p-5">
                        <h3 className="gs-section-title mb-4 flex items-center gap-2"><GitBranch size={16} /> Graph Service API</h3>
                        <div className="space-y-2">
                            {[
                                { method: 'POST', path: '/api/v1/connections/request', desc: 'Send connection request' },
                                { method: 'POST', path: '/api/v1/connections/accept', desc: 'Accept connection request' },
                                { method: 'DELETE', path: '/api/v1/connections/{id}', desc: 'Remove connection' },
                                { method: 'GET', path: '/api/v1/graph/neighbors', desc: '2-3 hop neighbor traversal' },
                                { method: 'GET', path: '/api/v1/graph/mutual', desc: 'Mutual connections between two people' },
                                { method: 'GET', path: '/api/v1/graph/shortest-path', desc: 'Shortest path between two people' },
                                { method: 'GET', path: '/api/v1/recommendations/pymk', desc: 'People you may know (mutual-based)' },
                            ].map((e, i) => (
                                <div key={i} className="flex items-center gap-3 p-2.5 rounded-lg bg-grey-800/30">
                                    <Badge variant={e.method === 'GET' ? 'info' : e.method === 'POST' ? 'success' : 'danger'}>{e.method}</Badge>
                                    <span className="font-mono text-xs text-accent-cyan flex-1">{e.path}</span>
                                    <span className="text-xs text-grey-500">{e.desc}</span>
                                </div>
                            ))}
                        </div>
                    </div>
                    <div className="gs-card p-5">
                        <h3 className="gs-section-title mb-4">Node / Edge Schema</h3>
                        <div className="bg-grey-900/50 rounded-lg p-4 font-mono text-xs space-y-2">
                            <div className="text-grey-500">// Graph DB schema (Neo4j-compatible)</div>
                            <div className="text-accent-cyan mt-2">Nodes:</div>
                            <div className="pl-3 text-grey-300">(:Person {'{'} id, name, headline, location {'}'})</div>
                            <div className="pl-3 text-grey-300">(:Company {'{'} id, name, industry, size {'}'})</div>
                            <div className="pl-3 text-grey-300">(:Skill {'{'} id, name, category {'}'})</div>
                            <div className="pl-3 text-grey-300">(:Job {'{'} id, title, level, location {'}'})</div>
                            <div className="pl-3 text-grey-300">(:Post {'{'} id, content, created_at {'}'})</div>
                            <div className="text-accent-cyan mt-3">Edges:</div>
                            <div className="pl-3 text-accent-emerald">[:CONNECTED_TO] <span className="text-grey-500">Person ↔ Person</span></div>
                            <div className="pl-3 text-accent-emerald">[:WORKS_AT] <span className="text-grey-500">Person → Company</span></div>
                            <div className="pl-3 text-accent-emerald">[:HAS_SKILL] <span className="text-grey-500">Person → Skill</span></div>
                            <div className="pl-3 text-accent-emerald">[:ENDORSED_FOR] <span className="text-grey-500">Person → Person×Skill</span></div>
                            <div className="pl-3 text-accent-emerald">[:APPLIED_TO] <span className="text-grey-500">Person → Job</span></div>
                            <div className="pl-3 text-accent-emerald">[:FOLLOWS] <span className="text-grey-500">Person → Company|Topic</span></div>
                            <div className="pl-3 text-accent-emerald">[:POSTED] <span className="text-grey-500">Person → Post</span></div>
                        </div>
                        <div className="mt-4 space-y-2">
                            <h4 className="text-xs font-semibold text-grey-500 uppercase">Graph Stats</h4>
                            {[
                                { label: 'Person Nodes', value: '6,722', growth: '+18%' },
                                { label: 'Company Nodes', value: '1,284', growth: '+8%' },
                                { label: 'Skill Nodes', value: '842', growth: '+12%' },
                                { label: 'Total Edges', value: '126,480', growth: '+24%' },
                            ].map(s => (
                                <div key={s.label} className="flex items-center justify-between p-2 bg-grey-800/30 rounded">
                                    <span className="text-xs text-grey-300">{s.label}</span>
                                    <div className="flex items-center gap-2">
                                        <span className="font-mono text-xs text-grey-200">{s.value}</span>
                                        <span className="text-xs text-accent-emerald">{s.growth}</span>
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
