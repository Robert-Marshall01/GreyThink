import { PageHeader, StatCard, Badge, DataTable } from '../../components/ui';
import { Users, UserPlus, Clock, CheckCircle } from 'lucide-react';

export function HCMRecruiting() {
    return (
        <div>
            <PageHeader
                title="Recruiting"
                subtitle="Job postings, applicants & hiring pipeline"
                actions={<button className="gs-btn-primary text-sm">+ Post Job</button>}
            />

            <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-4 gap-4 mb-6">
                <StatCard label="Open Positions" value={24} change="6 urgent" changeType="neutral" icon={<Users size={18} />} />
                <StatCard label="Applicants" value={342} change="+128 this week" changeType="positive" icon={<UserPlus size={18} />} />
                <StatCard label="Avg Time to Hire" value="28 days" change="-4 days" changeType="positive" icon={<Clock size={18} />} />
                <StatCard label="Offers Extended" value={8} change="5 accepted" changeType="positive" icon={<CheckCircle size={18} />} />
            </div>

            <div className="grid grid-cols-1 lg:grid-cols-4 gap-4 mb-6">
                {[
                    { stage: 'Applied', count: 186, color: 'border-grey-600' },
                    { stage: 'Screening', count: 64, color: 'border-brand-500' },
                    { stage: 'Interview', count: 32, color: 'border-accent-amber' },
                    { stage: 'Offer', count: 8, color: 'border-accent-emerald' },
                ].map(s => (
                    <div key={s.stage} className={`gs-card p-4 border-t-2 ${s.color}`}>
                        <p className="text-xs text-grey-500 uppercase tracking-wider">{s.stage}</p>
                        <p className="text-2xl font-bold text-grey-100 mt-1">{s.count}</p>
                    </div>
                ))}
            </div>

            <div className="gs-card p-5 mb-6">
                <h3 className="gs-section-title mb-4">Open Positions</h3>
                <DataTable
                    columns={['Position', 'Department', 'Location', 'Applicants', 'Priority', 'Posted']}
                    rows={[
                        ['Sr. Software Engineer', 'Engineering', 'Remote', '48', <Badge variant="danger">Urgent</Badge>, 'Jan 28'],
                        ['Product Manager', 'Product', 'New York', '32', <Badge variant="warning">High</Badge>, 'Feb 1'],
                        ['Data Scientist', 'Analytics', 'Remote', '64', <Badge variant="info">Normal</Badge>, 'Jan 15'],
                        ['UX Designer', 'Design', 'San Francisco', '28', <Badge variant="info">Normal</Badge>, 'Feb 5'],
                        ['Sales Executive', 'Sales', 'Chicago', '22', <Badge variant="warning">High</Badge>, 'Jan 20'],
                        ['DevOps Engineer', 'Infrastructure', 'Remote', '36', <Badge variant="danger">Urgent</Badge>, 'Jan 10'],
                    ]}
                />
            </div>

            <div className="gs-card p-5">
                <h3 className="gs-section-title mb-4">Recent Candidates</h3>
                <div className="space-y-3">
                    {[
                        { name: 'Alex Morgan', role: 'Sr. Software Engineer', stage: 'Final Interview', rating: 4.5 },
                        { name: 'Priya Sharma', role: 'Data Scientist', stage: 'Technical Screen', rating: 4.2 },
                        { name: 'Tom Baker', role: 'Product Manager', stage: 'Offer Extended', rating: 4.8 },
                        { name: 'Lisa Wang', role: 'UX Designer', stage: 'Portfolio Review', rating: 4.0 },
                    ].map(c => (
                        <div key={c.name} className="flex items-center justify-between p-3 rounded-lg hover:bg-grey-800/40 transition-colors">
                            <div className="flex items-center gap-3">
                                <div className="w-8 h-8 rounded-full bg-grey-700 flex items-center justify-center text-xs font-bold text-grey-300">
                                    {c.name.split(' ').map(n => n[0]).join('')}
                                </div>
                                <div>
                                    <p className="text-sm font-medium text-grey-200">{c.name}</p>
                                    <p className="text-xs text-grey-500">{c.role}</p>
                                </div>
                            </div>
                            <div className="flex items-center gap-3">
                                <Badge variant="info">{c.stage}</Badge>
                                <span className="text-sm text-accent-amber">★ {c.rating}</span>
                            </div>
                        </div>
                    ))}
                </div>
            </div>
        </div>
    );
}
