import { PageHeader, StatCard, Badge, DataTable, MiniSparkline } from '../../components/ui';
import { Briefcase, DollarSign, TrendingUp, Calendar } from 'lucide-react';

export function CRMOpportunities() {
    return (
        <div>
            <PageHeader
                title="Opportunities"
                subtitle="Deal pipeline, forecasting & revenue tracking"
                actions={<button className="gs-btn-primary text-sm">+ New Opportunity</button>}
            />

            <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-4 gap-4 mb-6">
                <StatCard label="Open Deals" value={67} change="+5 this week" changeType="positive" icon={<Briefcase size={18} />} />
                <StatCard label="Pipeline Value" value="$4.8M" change="+12% MoM" changeType="positive" icon={<DollarSign size={18} />} />
                <StatCard label="Win Rate" value="34%" change="+3% vs LQ" changeType="positive" icon={<TrendingUp size={18} />} />
                <StatCard label="Avg Close Time" value="42 days" change="-5 days" changeType="positive" icon={<Calendar size={18} />} />
            </div>

            <div className="grid grid-cols-1 lg:grid-cols-5 gap-4 mb-6">
                {[
                    { stage: 'Discovery', count: 18, value: '$890K', color: 'bg-grey-600' },
                    { stage: 'Qualification', count: 14, value: '$1.2M', color: 'bg-brand-500' },
                    { stage: 'Proposal', count: 12, value: '$1.4M', color: 'bg-accent-amber' },
                    { stage: 'Negotiation', count: 8, value: '$720K', color: 'bg-accent-violet' },
                    { stage: 'Closed Won', count: 15, value: '$580K', color: 'bg-accent-emerald' },
                ].map(s => (
                    <div key={s.stage} className="gs-card p-4 text-center">
                        <div className={`w-3 h-3 rounded-full ${s.color} mx-auto mb-2`} />
                        <p className="text-xs text-grey-500 uppercase tracking-wider">{s.stage}</p>
                        <p className="text-xl font-bold text-grey-100 mt-1">{s.count}</p>
                        <p className="text-sm text-grey-400">{s.value}</p>
                    </div>
                ))}
            </div>

            <div className="gs-card p-5 mb-6">
                <h3 className="gs-section-title mb-4">Pipeline Forecast</h3>
                <MiniSparkline data={[320, 480, 520, 610, 580, 720, 680, 810, 780, 920, 880, 1050]} height={100} color="#8b5cf6" />
                <div className="flex gap-6 mt-4 text-sm">
                    <div><span className="text-grey-500">Best Case</span> <span className="font-semibold text-grey-200 ml-1">$5.2M</span></div>
                    <div><span className="text-grey-500">Committed</span> <span className="font-semibold text-accent-emerald ml-1">$3.1M</span></div>
                    <div><span className="text-grey-500">Target</span> <span className="font-semibold text-accent-amber ml-1">$4.0M</span></div>
                </div>
            </div>

            <div className="gs-card p-5">
                <h3 className="gs-section-title mb-4">Active Opportunities</h3>
                <DataTable
                    columns={['Opportunity', 'Account', 'Value', 'Stage', 'Probability', 'Close Date']}
                    rows={[
                        ['Enterprise Platform Deal', 'Acme Corp', '$320K', <Badge variant="purple">Negotiation</Badge>, '75%', 'Mar 15'],
                        ['Cloud Migration Project', 'GlobalTech', '$180K', <Badge variant="info">Proposal</Badge>, '60%', 'Mar 28'],
                        ['Annual License Renewal', 'Pinnacle Group', '$95K', <Badge variant="success">Closed Won</Badge>, '100%', 'Feb 10'],
                        ['Data Analytics Suite', 'Vertex Labs', '$240K', <Badge variant="info">Qualification</Badge>, '40%', 'Apr 12'],
                        ['Security Audit Package', 'Nova Systems', '$65K', <Badge variant="warning">Discovery</Badge>, '25%', 'Apr 30'],
                    ]}
                />
            </div>
        </div>
    );
}
