import { useState } from 'react';
import { PageHeader, StatCard, Badge, DataTable, Tabs, ProgressBar, MiniSparkline } from '../../components/ui';
import {
    User, Building2, Award, Briefcase, MapPin, Link2,
    Edit3, Eye, Plus, GraduationCap, Github, Globe,
    CheckCircle, Star, Clock, Users, Camera, FileText
} from 'lucide-react';

const people = [
    { id: 'P-001', name: 'Sarah Chen', headline: 'Staff Engineer @ Acme Corp', location: 'San Francisco, CA', connections: 842, skills: 24, completeness: 96, avatar: 'SC', verified: true },
    { id: 'P-002', name: 'Marcus Johnson', headline: 'VP Engineering @ Globex', location: 'New York, NY', connections: 1284, skills: 18, completeness: 92, avatar: 'MJ', verified: true },
    { id: 'P-003', name: 'Priya Patel', headline: 'Senior PM @ Initech', location: 'Austin, TX', connections: 567, skills: 16, completeness: 88, avatar: 'PP', verified: false },
    { id: 'P-004', name: 'James O\'Brien', headline: 'Principal Architect @ Umbrella', location: 'London, UK', connections: 2140, skills: 32, completeness: 98, avatar: 'JO', verified: true },
    { id: 'P-005', name: 'Anika Gupta', headline: 'Director of Data Science @ Stark', location: 'Seattle, WA', connections: 1560, skills: 28, completeness: 94, avatar: 'AG', verified: true },
    { id: 'P-006', name: 'David Kim', headline: 'Senior SRE @ Wayne Enterprises', location: 'Chicago, IL', connections: 420, skills: 21, completeness: 82, avatar: 'DK', verified: false },
];

const companies = [
    { id: 'C-001', name: 'Acme Corporation', industry: 'Enterprise Software', size: '5,000-10,000', hq: 'San Francisco, CA', openRoles: 42, followers: 28400, logo: 'AC' },
    { id: 'C-002', name: 'Globex Industries', industry: 'Cloud Infrastructure', size: '1,000-5,000', hq: 'New York, NY', openRoles: 18, followers: 14200, logo: 'GI' },
    { id: 'C-003', name: 'Initech LLC', industry: 'FinTech', size: '500-1,000', hq: 'Austin, TX', openRoles: 8, followers: 6800, logo: 'IL' },
    { id: 'C-004', name: 'Umbrella Corp', industry: 'AI / ML', size: '10,000+', hq: 'London, UK', openRoles: 86, followers: 52000, logo: 'UC' },
    { id: 'C-005', name: 'Stark Industries', industry: 'Aerospace & Defense', size: '10,000+', hq: 'Seattle, WA', openRoles: 124, followers: 98000, logo: 'SI' },
];

const skills = [
    { name: 'TypeScript', category: 'Languages', endorsements: 486, related: ['JavaScript', 'React', 'Node.js'], demand: 'Very High', growth: '+28%' },
    { name: 'System Design', category: 'Architecture', endorsements: 324, related: ['Distributed Systems', 'Microservices', 'Cloud'], demand: 'Very High', growth: '+34%' },
    { name: 'React', category: 'Frameworks', endorsements: 412, related: ['TypeScript', 'Next.js', 'Redux'], demand: 'High', growth: '+18%' },
    { name: 'Kubernetes', category: 'Infrastructure', endorsements: 268, related: ['Docker', 'Helm', 'AWS EKS'], demand: 'High', growth: '+42%' },
    { name: 'Machine Learning', category: 'AI/ML', endorsements: 186, related: ['Python', 'TensorFlow', 'PyTorch'], demand: 'Very High', growth: '+56%' },
    { name: 'PostgreSQL', category: 'Databases', endorsements: 348, related: ['SQL', 'TimescaleDB', 'pgvector'], demand: 'High', growth: '+12%' },
    { name: 'GraphQL', category: 'APIs', endorsements: 142, related: ['REST', 'Apollo', 'Federation'], demand: 'Medium', growth: '+8%' },
    { name: 'Go', category: 'Languages', endorsements: 224, related: ['Microservices', 'gRPC', 'Kubernetes'], demand: 'High', growth: '+22%' },
];

const workHistory = [
    { company: 'Acme Corporation', title: 'Staff Engineer', dates: 'Jan 2024 – Present', duration: '2 yr 1 mo', desc: 'Leading platform engineering for multi-tenant SaaS architecture. Designed graph-based permission system serving 6M+ authorization checks/day.' },
    { company: 'Globex Industries', title: 'Senior Engineer', dates: 'Mar 2021 – Dec 2023', duration: '2 yr 10 mo', desc: 'Built real-time data pipeline (CDC + Kafka) processing 84M events/day. Reduced P99 latency from 2.4s to 480ms.' },
    { company: 'Initech LLC', title: 'Software Engineer II', dates: 'Jun 2019 – Feb 2021', duration: '1 yr 9 mo', desc: 'Full-stack development on React + Node.js. Shipped payment processing integration handling $12M/mo ARR.' },
    { company: 'FreshStart Inc.', title: 'Junior Engineer', dates: 'Aug 2017 – May 2019', duration: '1 yr 10 mo', desc: 'First engineering role. Built internal tools, automated CI/CD pipelines, wrote technical documentation.' },
];

const education = [
    { school: 'Stanford University', degree: 'M.S. Computer Science', dates: '2015 – 2017', field: 'Distributed Systems' },
    { school: 'UC Berkeley', degree: 'B.S. Computer Science', dates: '2011 – 2015', field: 'Software Engineering' },
];

const projects = [
    { name: 'Graph Permission Engine', type: 'Open Source', tech: ['Rust', 'Neo4j', 'gRPC'], stars: 1240, desc: 'High-performance authorization engine using graph traversal for RBAC/ABAC policy evaluation.' },
    { name: 'Real-Time Analytics SDK', type: 'Work Project', tech: ['TypeScript', 'Kafka', 'ClickHouse'], stars: 0, desc: 'Client-side SDK for streaming clickstream events with batching, retry, and offline support.' },
    { name: 'ML Feature Store', type: 'Open Source', tech: ['Python', 'Redis', 'FastAPI'], stars: 680, desc: 'Lightweight feature store for ML pipelines with versioning, lineage tracking, and serving layer.' },
];

export function NetworkProfiles() {
    const [activeTab, setActiveTab] = useState('Person Profile');

    return (
        <div>
            <PageHeader
                title="Professional Profiles"
                subtitle="Person, company, and skill profiles — the identity layer of the professional network"
                actions={
                    <div className="flex gap-2">
                        <button className="gs-btn-secondary flex items-center gap-2"><Eye size={16} /> Preview</button>
                        <button className="gs-btn-primary flex items-center gap-2"><Edit3 size={16} /> Edit Profile</button>
                    </div>
                }
            />

            <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-4 gap-4 mb-6">
                <StatCard label="Network Size" value="842" change="+38 this month" changeType="positive" icon={<Users size={18} />} />
                <StatCard label="Profile Views (30d)" value="1,247" change="+22% MoM" changeType="positive" icon={<Eye size={18} />} />
                <StatCard label="Search Appearances" value="3,841" change="+15% MoM" changeType="positive" icon={<Globe size={18} />} />
                <StatCard label="Profile Completeness" value="96%" change="Top 5%" changeType="positive" icon={<CheckCircle size={18} />} />
            </div>

            <Tabs tabs={['Person Profile', 'Company Profiles', 'Skill Directory', 'Portfolio', 'Profile API']} active={activeTab} onChange={setActiveTab} />

            {activeTab === 'Person Profile' && (
                <div className="space-y-6">
                    {/* Profile Card */}
                    <div className="gs-card overflow-hidden">
                        <div className="h-32 bg-gradient-to-r from-brand-600/30 to-accent-violet/20" />
                        <div className="p-5 -mt-12">
                            <div className="flex items-end gap-4 mb-4">
                                <div className="w-24 h-24 rounded-full bg-brand-500 border-4 border-grey-950 flex items-center justify-center text-2xl font-bold text-white">SC</div>
                                <div className="pb-1">
                                    <div className="flex items-center gap-2">
                                        <h2 className="text-xl font-bold text-grey-100">Sarah Chen</h2>
                                        <CheckCircle size={16} className="text-brand-400" />
                                    </div>
                                    <p className="text-sm text-grey-400">Staff Engineer @ Acme Corporation</p>
                                    <p className="text-xs text-grey-500 flex items-center gap-1 mt-1"><MapPin size={12} /> San Francisco, CA · 842 connections</p>
                                </div>
                            </div>

                            <p className="text-sm text-grey-300 leading-relaxed mb-4">
                                Platform engineer specializing in multi-tenant SaaS architecture, distributed systems, and graph-based authorization. Passionate about building scalable infrastructure that empowers product teams. Open source contributor with 1.9K+ GitHub stars.
                            </p>

                            <div className="flex flex-wrap gap-2 mb-4">
                                {['TypeScript', 'Rust', 'System Design', 'Kubernetes', 'PostgreSQL', 'Graph DBs'].map(s => (
                                    <span key={s} className="px-2.5 py-1 text-xs rounded-full bg-brand-500/10 text-brand-400 border border-brand-500/20">{s}</span>
                                ))}
                                <span className="px-2.5 py-1 text-xs rounded-full bg-grey-800 text-grey-400">+18 more</span>
                            </div>

                            <div className="flex items-center gap-4 text-xs text-grey-500">
                                <span className="flex items-center gap-1"><Github size={12} /> github.com/sarahchen</span>
                                <span className="flex items-center gap-1"><Globe size={12} /> sarahchen.dev</span>
                                <span className="flex items-center gap-1"><Link2 size={12} /> Portfolio</span>
                            </div>
                        </div>
                    </div>

                    {/* Work History */}
                    <div className="grid grid-cols-1 lg:grid-cols-3 gap-6">
                        <div className="lg:col-span-2 gs-card p-5">
                            <h3 className="gs-section-title mb-4 flex items-center gap-2"><Briefcase size={16} /> Work Experience</h3>
                            <div className="space-y-4">
                                {workHistory.map((w, i) => (
                                    <div key={i} className="flex gap-3 p-3 rounded-lg border border-grey-800 hover:border-grey-700 transition-colors">
                                        <div className="w-10 h-10 rounded-lg bg-grey-800 flex items-center justify-center text-xs font-bold text-grey-400 flex-shrink-0">
                                            {w.company.split(' ').map(x => x[0]).join('').slice(0, 2)}
                                        </div>
                                        <div>
                                            <p className="text-sm font-medium text-grey-200">{w.title}</p>
                                            <p className="text-xs text-grey-400">{w.company} · {w.duration}</p>
                                            <p className="text-xs text-grey-500 mt-1">{w.dates}</p>
                                            <p className="text-xs text-grey-400 mt-2 leading-relaxed">{w.desc}</p>
                                        </div>
                                    </div>
                                ))}
                            </div>
                        </div>

                        <div className="space-y-6">
                            <div className="gs-card p-5">
                                <h3 className="gs-section-title mb-4 flex items-center gap-2"><GraduationCap size={16} /> Education</h3>
                                <div className="space-y-3">
                                    {education.map((e, i) => (
                                        <div key={i} className="p-3 rounded-lg bg-grey-800/30">
                                            <p className="text-sm font-medium text-grey-200">{e.school}</p>
                                            <p className="text-xs text-grey-400">{e.degree}</p>
                                            <p className="text-xs text-grey-500">{e.dates} · {e.field}</p>
                                        </div>
                                    ))}
                                </div>
                            </div>

                            <div className="gs-card p-5">
                                <h3 className="gs-section-title mb-4">Profile Analytics</h3>
                                <MiniSparkline data={[28, 42, 38, 56, 62, 48, 72, 68, 84, 92, 78, 96]} height={60} />
                                <div className="grid grid-cols-2 gap-3 mt-3 text-center">
                                    <div className="p-2 bg-grey-800/30 rounded"><p className="text-sm font-bold text-grey-200">1,247</p><p className="text-xs text-grey-500">Views</p></div>
                                    <div className="p-2 bg-grey-800/30 rounded"><p className="text-sm font-bold text-grey-200">3,841</p><p className="text-xs text-grey-500">Searches</p></div>
                                </div>
                            </div>
                        </div>
                    </div>
                </div>
            )}

            {activeTab === 'Company Profiles' && (
                <div className="space-y-6">
                    <div className="gs-card p-5">
                        <h3 className="gs-section-title mb-4 flex items-center gap-2"><Building2 size={16} /> Company Directory</h3>
                        <DataTable
                            columns={['Company', 'Industry', 'Size', 'Headquarters', 'Open Roles', 'Followers', 'Actions']}
                            rows={companies.map(c => [
                                <div className="flex items-center gap-2">
                                    <div className="w-8 h-8 rounded-lg bg-grey-800 flex items-center justify-center text-xs font-bold text-grey-400">{c.logo}</div>
                                    <span className="font-medium text-grey-200">{c.name}</span>
                                </div>,
                                <Badge variant="info">{c.industry}</Badge>,
                                <span className="text-xs text-grey-400">{c.size}</span>,
                                <span className="text-xs text-grey-400 flex items-center gap-1"><MapPin size={10} />{c.hq}</span>,
                                <span className="text-accent-emerald font-medium">{c.openRoles}</span>,
                                <span className="text-xs text-grey-400">{c.followers.toLocaleString()}</span>,
                                <div className="flex gap-2">
                                    <button className="text-xs text-brand-400 hover:text-brand-300">Follow</button>
                                    <button className="text-xs text-grey-500 hover:text-grey-300">View</button>
                                </div>,
                            ])}
                        />
                    </div>

                    {/* Company Detail Preview */}
                    <div className="gs-card overflow-hidden">
                        <div className="h-24 bg-gradient-to-r from-accent-emerald/20 to-accent-cyan/10" />
                        <div className="p-5 -mt-8">
                            <div className="flex items-end gap-4 mb-4">
                                <div className="w-16 h-16 rounded-xl bg-grey-800 border-4 border-grey-950 flex items-center justify-center text-lg font-bold text-grey-300">AC</div>
                                <div>
                                    <h3 className="text-lg font-bold text-grey-100">Acme Corporation</h3>
                                    <p className="text-xs text-grey-400">Enterprise Software · 5,000-10,000 employees · San Francisco, CA</p>
                                </div>
                            </div>
                            <p className="text-sm text-grey-300 mb-4">
                                Leading enterprise software company building the next generation of multi-tenant SaaS platforms. We are on a mission to make enterprise software delightful, secure, and scalable.
                            </p>
                            <div className="grid grid-cols-4 gap-4 text-center">
                                <div className="p-3 bg-grey-800/30 rounded-lg"><p className="text-lg font-bold text-grey-100">42</p><p className="text-xs text-grey-500">Open Roles</p></div>
                                <div className="p-3 bg-grey-800/30 rounded-lg"><p className="text-lg font-bold text-grey-100">28.4K</p><p className="text-xs text-grey-500">Followers</p></div>
                                <div className="p-3 bg-grey-800/30 rounded-lg"><p className="text-lg font-bold text-grey-100">6.8K</p><p className="text-xs text-grey-500">Employees</p></div>
                                <div className="p-3 bg-grey-800/30 rounded-lg"><p className="text-lg font-bold text-grey-100">12</p><p className="text-xs text-grey-500">Locations</p></div>
                            </div>
                        </div>
                    </div>
                </div>
            )}

            {activeTab === 'Skill Directory' && (
                <div className="gs-card p-5">
                    <h3 className="gs-section-title mb-4 flex items-center gap-2"><Award size={16} /> Skill Entities & Market Demand</h3>
                    <DataTable
                        columns={['Skill', 'Category', 'Endorsements', 'Related Skills', 'Market Demand', 'Growth']}
                        rows={skills.map(s => [
                            <span className="font-medium text-grey-200">{s.name}</span>,
                            <Badge variant="purple">{s.category}</Badge>,
                            <span className="text-grey-300">{s.endorsements}</span>,
                            <div className="flex gap-1 flex-wrap">{s.related.map(r => <span key={r} className="text-[10px] px-1.5 py-0.5 rounded bg-grey-800 text-grey-400">{r}</span>)}</div>,
                            <Badge variant={s.demand === 'Very High' ? 'danger' : s.demand === 'High' ? 'warning' : 'info'}>{s.demand}</Badge>,
                            <span className="text-accent-emerald text-xs font-medium">{s.growth}</span>,
                        ])}
                    />
                </div>
            )}

            {activeTab === 'Portfolio' && (
                <div className="space-y-6">
                    <div className="gs-card p-5">
                        <h3 className="gs-section-title mb-4 flex items-center gap-2"><FileText size={16} /> Projects & Portfolio Items</h3>
                        <div className="space-y-4">
                            {projects.map((p, i) => (
                                <div key={i} className="p-4 rounded-lg border border-grey-800 hover:border-grey-700 transition-colors">
                                    <div className="flex items-center justify-between mb-2">
                                        <div className="flex items-center gap-2">
                                            <h4 className="text-sm font-medium text-grey-200">{p.name}</h4>
                                            <Badge variant={p.type === 'Open Source' ? 'success' : 'info'}>{p.type}</Badge>
                                        </div>
                                        {p.stars > 0 && (
                                            <span className="text-xs text-grey-400 flex items-center gap-1"><Star size={12} className="text-accent-amber" />{p.stars.toLocaleString()}</span>
                                        )}
                                    </div>
                                    <p className="text-xs text-grey-400 mb-3">{p.desc}</p>
                                    <div className="flex gap-1.5">
                                        {p.tech.map(t => (
                                            <span key={t} className="px-2 py-0.5 text-[10px] rounded bg-grey-800 text-grey-400 border border-grey-700">{t}</span>
                                        ))}
                                    </div>
                                </div>
                            ))}
                        </div>
                    </div>
                </div>
            )}

            {activeTab === 'Profile API' && (
                <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
                    <div className="gs-card p-5">
                        <h3 className="gs-section-title mb-4 flex items-center gap-2"><FileText size={16} /> Profile Service Endpoints</h3>
                        <div className="space-y-2">
                            {[
                                { method: 'GET', path: '/api/v1/profiles/{id}', desc: 'Retrieve person profile with work history, education, skills' },
                                { method: 'PUT', path: '/api/v1/profiles/{id}', desc: 'Update person profile fields' },
                                { method: 'GET', path: '/api/v1/profiles/{id}/timeline', desc: 'Combined work + education timeline' },
                                { method: 'GET', path: '/api/v1/profiles/{id}/skills', desc: 'Skills with endorsement counts' },
                                { method: 'POST', path: '/api/v1/profiles/{id}/endorsements', desc: 'Endorse a skill for a person' },
                                { method: 'GET', path: '/api/v1/companies/{id}', desc: 'Company profile with open roles count' },
                                { method: 'GET', path: '/api/v1/companies/{id}/employees', desc: 'List company employees (paginated)' },
                                { method: 'GET', path: '/api/v1/skills', desc: 'Browse/search skill directory' },
                                { method: 'GET', path: '/api/v1/skills/{id}/related', desc: 'Related skills by graph distance' },
                            ].map((e, i) => (
                                <div key={i} className="flex items-center gap-3 p-2.5 rounded-lg bg-grey-800/30">
                                    <Badge variant={e.method === 'GET' ? 'info' : e.method === 'POST' ? 'success' : 'warning'}>{e.method}</Badge>
                                    <span className="font-mono text-xs text-accent-cyan flex-1">{e.path}</span>
                                    <span className="text-xs text-grey-500 max-w-xs truncate">{e.desc}</span>
                                </div>
                            ))}
                        </div>
                    </div>
                    <div className="gs-card p-5">
                        <h3 className="gs-section-title mb-4">HCM Integration Mapping</h3>
                        <div className="bg-grey-900/50 rounded-lg p-4 font-mono text-xs space-y-1">
                            <div className="text-grey-500">// Auto-sync between HCM employees and network profiles</div>
                            <div className="text-accent-cyan">{'{'}</div>
                            <div className="pl-3 text-grey-300">"source": <span className="text-accent-emerald">"hcm.employees"</span>,</div>
                            <div className="pl-3 text-grey-300">"target": <span className="text-accent-emerald">"network.profiles"</span>,</div>
                            <div className="pl-3 text-grey-300">"sync": <span className="text-accent-emerald">"bidirectional"</span>,</div>
                            <div className="pl-3 text-grey-300">"mappings": {'{'}</div>
                            <div className="pl-6 text-grey-400">"employee.name" → "profile.name",</div>
                            <div className="pl-6 text-grey-400">"employee.title" → "profile.headline",</div>
                            <div className="pl-6 text-grey-400">"employee.department" → "profile.current_role",</div>
                            <div className="pl-6 text-grey-400">"employee.skills" → "profile.skills[]",</div>
                            <div className="pl-6 text-grey-400">"employee.manager_id" → "graph.REPORTS_TO",</div>
                            <div className="pl-6 text-grey-400">"employee.company" → "graph.WORKS_AT"</div>
                            <div className="pl-3 text-grey-300">{'}'}</div>
                            <div className="text-accent-cyan">{'}'}</div>
                        </div>
                        <div className="mt-4 space-y-2">
                            {[
                                { sync: 'HCM → Person Profiles', status: 'Real-time', records: '6,722 synced' },
                                { sync: 'HCM → Org Graph (REPORTS_TO)', status: 'Real-time', records: '6,718 edges' },
                                { sync: 'Performance → Skills Graph', status: '15 min', records: '24K endorsements' },
                                { sync: 'Career → Portfolio Items', status: 'On change', records: '1,842 projects' },
                            ].map(s => (
                                <div key={s.sync} className="flex items-center justify-between p-2 bg-grey-800/30 rounded">
                                    <span className="text-xs text-grey-300">{s.sync}</span>
                                    <div className="flex items-center gap-2">
                                        <span className="text-xs text-grey-500">{s.records}</span>
                                        <Badge variant="success">{s.status}</Badge>
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
