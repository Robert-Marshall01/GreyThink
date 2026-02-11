import { PageHeader, StatCard, Badge, ProgressBar } from '../../components/ui';
import { GraduationCap, Award, BookOpen, Code, Trophy } from 'lucide-react';

export function SkillDevelopment() {
    return (
        <div>
            <PageHeader
                title="Skill Development"
                subtitle="Learning paths, certifications & proof of work"
            />

            <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-4 gap-4 mb-6">
                <StatCard label="Courses Completed" value={24} change="+3 this month" changeType="positive" icon={<BookOpen size={18} />} />
                <StatCard label="Certifications" value={8} change="2 expiring soon" changeType="neutral" icon={<Award size={18} />} />
                <StatCard label="Learning Streak" value="14 days" change="Personal best!" changeType="positive" icon={<Trophy size={18} />} />
                <StatCard label="Skills Tracked" value={32} icon={<Code size={18} />} />
            </div>

            <div className="grid grid-cols-1 lg:grid-cols-2 gap-6 mb-6">
                {/* Active Learning */}
                <div className="gs-card p-5">
                    <h3 className="gs-section-title mb-4">Active Courses</h3>
                    <div className="space-y-4">
                        {[
                            { name: 'Advanced System Design', provider: 'GreyLearn', progress: 72, hours: '8/12h', category: 'Engineering' },
                            { name: 'AI/ML Engineering', provider: 'Coursera', progress: 45, hours: '14/32h', category: 'AI/ML' },
                            { name: 'Executive Leadership', provider: 'Maven', progress: 88, hours: '4/6h', category: 'Leadership' },
                            { name: 'Cloud Architecture (AWS)', provider: 'A Cloud Guru', progress: 60, hours: '18/30h', category: 'Cloud' },
                        ].map(c => (
                            <div key={c.name} className="p-3 rounded-lg border border-grey-800 hover:border-grey-700 transition-colors cursor-pointer">
                                <div className="flex items-center justify-between mb-2">
                                    <div>
                                        <p className="text-sm font-medium text-grey-200">{c.name}</p>
                                        <p className="text-xs text-grey-500">{c.provider} · {c.hours}</p>
                                    </div>
                                    <Badge variant="info">{c.category}</Badge>
                                </div>
                                <ProgressBar value={c.progress} color="bg-brand-500" size="md" />
                                <p className="text-xs text-grey-500 mt-1">{c.progress}% complete</p>
                            </div>
                        ))}
                    </div>
                </div>

                {/* Certifications */}
                <div className="gs-card p-5">
                    <h3 className="gs-section-title mb-4 flex items-center gap-2"><Award size={16} /> Certifications</h3>
                    <div className="space-y-3">
                        {[
                            { name: 'AWS Solutions Architect — Professional', issuer: 'Amazon Web Services', earned: 'Jan 2026', expires: 'Jan 2029', status: 'Active' },
                            { name: 'Google Cloud Professional Cloud Architect', issuer: 'Google', earned: 'Nov 2025', expires: 'Nov 2027', status: 'Active' },
                            { name: 'Certified Kubernetes Administrator', issuer: 'CNCF', earned: 'Sep 2025', expires: 'Sep 2028', status: 'Active' },
                            { name: 'PMP — Project Management Professional', issuer: 'PMI', earned: 'Jul 2024', expires: 'Jul 2027', status: 'Active' },
                            { name: 'Terraform Associate', issuer: 'HashiCorp', earned: 'Mar 2024', expires: 'Mar 2026', status: 'Expiring Soon' },
                        ].map(cert => (
                            <div key={cert.name} className="flex items-center justify-between p-3 rounded-lg hover:bg-grey-800/40 transition-colors">
                                <div className="flex items-center gap-3">
                                    <div className="w-10 h-10 rounded-xl bg-accent-amber/15 text-accent-amber flex items-center justify-center">
                                        <Award size={18} />
                                    </div>
                                    <div>
                                        <p className="text-sm font-medium text-grey-200">{cert.name}</p>
                                        <p className="text-xs text-grey-500">{cert.issuer} · Earned {cert.earned}</p>
                                    </div>
                                </div>
                                <Badge variant={cert.status === 'Active' ? 'success' : 'warning'}>{cert.status}</Badge>
                            </div>
                        ))}
                    </div>
                </div>
            </div>

            {/* Skill Matrix */}
            <div className="gs-card p-5 mb-6">
                <h3 className="gs-section-title mb-4">Skill Matrix</h3>
                <div className="grid grid-cols-2 md:grid-cols-3 lg:grid-cols-4 gap-3">
                    {[
                        { skill: 'TypeScript', level: 95, category: 'Languages' },
                        { skill: 'React', level: 92, category: 'Frontend' },
                        { skill: 'Node.js', level: 88, category: 'Backend' },
                        { skill: 'Python', level: 82, category: 'Languages' },
                        { skill: 'AWS', level: 90, category: 'Cloud' },
                        { skill: 'Kubernetes', level: 78, category: 'DevOps' },
                        { skill: 'System Design', level: 85, category: 'Architecture' },
                        { skill: 'PostgreSQL', level: 80, category: 'Data' },
                        { skill: 'GraphQL', level: 75, category: 'API' },
                        { skill: 'Terraform', level: 72, category: 'IaC' },
                        { skill: 'Machine Learning', level: 65, category: 'AI/ML' },
                        { skill: 'Leadership', level: 82, category: 'Soft Skills' },
                    ].map(s => (
                        <div key={s.skill} className="p-3 bg-grey-800/30 rounded-lg">
                            <div className="flex items-center justify-between mb-2">
                                <span className="text-sm font-medium text-grey-200">{s.skill}</span>
                                <span className="text-xs text-grey-500">{s.level}%</span>
                            </div>
                            <ProgressBar
                                value={s.level}
                                color={s.level >= 90 ? 'bg-accent-emerald' : s.level >= 75 ? 'bg-brand-500' : s.level >= 60 ? 'bg-accent-amber' : 'bg-grey-600'}
                                size="sm"
                            />
                            <p className="text-xs text-grey-600 mt-1">{s.category}</p>
                        </div>
                    ))}
                </div>
            </div>

            {/* Portfolio */}
            <div className="gs-card p-5">
                <h3 className="gs-section-title mb-4 flex items-center gap-2"><Code size={16} /> Proof of Work — Portfolio</h3>
                <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4">
                    {[
                        { name: 'Grey Suite Platform', desc: 'Enterprise-grade unified corporate OS', tech: ['React', 'TypeScript', 'Node.js'], stars: 248, url: '#' },
                        { name: 'ML Pipeline Framework', desc: 'Production ML orchestration library', tech: ['Python', 'Kubernetes', 'Airflow'], stars: 142, url: '#' },
                        { name: 'Real-time Analytics Engine', desc: 'High-throughput event processing', tech: ['Go', 'Kafka', 'ClickHouse'], stars: 89, url: '#' },
                    ].map(p => (
                        <div key={p.name} className="p-4 rounded-lg border border-grey-800 hover:border-brand-500/30 transition-colors cursor-pointer">
                            <h4 className="text-sm font-semibold text-grey-200">{p.name}</h4>
                            <p className="text-xs text-grey-500 mt-1">{p.desc}</p>
                            <div className="flex gap-1 mt-3">{p.tech.map(t => <Badge key={t}>{t}</Badge>)}</div>
                            <p className="text-xs text-accent-amber mt-2">★ {p.stars}</p>
                        </div>
                    ))}
                </div>
            </div>
        </div>
    );
}
