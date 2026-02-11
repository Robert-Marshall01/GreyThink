import { PageHeader, StatCard, Badge, MiniSparkline } from '../../components/ui';
import { BarChart3, Users, TrendingUp, Eye, Plus } from 'lucide-react';
import { useState } from 'react';

export function BIDashboards() {
    const [activeDashboard, setActiveDashboard] = useState('executive');

    return (
        <div>
            <PageHeader
                title="BI Dashboards"
                subtitle="Business intelligence, reporting & data visualization"
                actions={<button className="gs-btn-primary text-sm flex items-center gap-2"><Plus size={14} /> New Dashboard</button>}
            />

            {/* Dashboard Tabs */}
            <div className="flex gap-2 mb-6 overflow-x-auto pb-2">
                {[
                    { id: 'executive', name: 'Executive Overview' },
                    { id: 'sales', name: 'Sales Performance' },
                    { id: 'marketing', name: 'Marketing Analytics' },
                    { id: 'operations', name: 'Operations' },
                ].map(d => (
                    <button
                        key={d.id}
                        onClick={() => setActiveDashboard(d.id)}
                        className={`px-4 py-2 rounded-lg text-sm font-medium whitespace-nowrap transition-colors ${activeDashboard === d.id ? 'bg-brand-500/15 text-brand-400 border border-brand-500/30' : 'bg-grey-900 text-grey-400 border border-grey-800 hover:border-grey-700'
                            }`}
                    >{d.name}</button>
                ))}
            </div>

            {/* KPIs */}
            <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-4 gap-4 mb-6">
                <StatCard label="Revenue" value="$14.8M" change="+12.3%" changeType="positive" icon={<TrendingUp size={18} />} />
                <StatCard label="Active Users" value="24.8K" change="+8.4%" changeType="positive" icon={<Users size={18} />} />
                <StatCard label="Conversion Rate" value="3.42%" change="+0.28%" changeType="positive" icon={<BarChart3 size={18} />} />
                <StatCard label="Page Views" value="1.2M" change="+15.7%" changeType="positive" icon={<Eye size={18} />} />
            </div>

            {/* Charts Area */}
            <div className="grid grid-cols-1 lg:grid-cols-2 gap-6 mb-6">
                <div className="gs-card p-5">
                    <h3 className="gs-section-title mb-1">Revenue Trend</h3>
                    <p className="text-xs text-grey-500 mb-4">Monthly revenue over last 12 months</p>
                    <MiniSparkline data={[8.2, 8.8, 9.1, 9.6, 10.2, 10.8, 11.4, 11.9, 12.6, 13.2, 14.0, 14.8]} height={140} />
                    <div className="grid grid-cols-3 gap-4 mt-4 pt-4 border-t border-grey-800">
                        <div><p className="text-xs text-grey-500">Q1 Target</p><p className="text-sm font-semibold text-grey-200">$16.0M</p></div>
                        <div><p className="text-xs text-grey-500">Run Rate</p><p className="text-sm font-semibold text-accent-emerald">$17.8M</p></div>
                        <div><p className="text-xs text-grey-500">YoY Growth</p><p className="text-sm font-semibold text-grey-200">+18.4%</p></div>
                    </div>
                </div>

                <div className="gs-card p-5">
                    <h3 className="gs-section-title mb-1">Revenue by Segment</h3>
                    <p className="text-xs text-grey-500 mb-4">Breakdown by customer segment</p>
                    <div className="space-y-4 mt-2">
                        {[
                            { segment: 'Enterprise', revenue: '$8.2M', pct: 55, color: 'bg-brand-500' },
                            { segment: 'Mid-Market', revenue: '$3.8M', pct: 26, color: 'bg-accent-violet' },
                            { segment: 'SMB', revenue: '$2.1M', pct: 14, color: 'bg-accent-amber' },
                            { segment: 'Self-Serve', revenue: '$0.7M', pct: 5, color: 'bg-accent-emerald' },
                        ].map(s => (
                            <div key={s.segment}>
                                <div className="flex justify-between text-sm mb-1">
                                    <span className="text-grey-300">{s.segment}</span>
                                    <span className="text-grey-400">{s.revenue} ({s.pct}%)</span>
                                </div>
                                <div className="w-full bg-grey-800 rounded-full h-3">
                                    <div className={`h-3 rounded-full ${s.color}`} style={{ width: `${s.pct}%` }} />
                                </div>
                            </div>
                        ))}
                    </div>
                </div>
            </div>

            <div className="grid grid-cols-1 lg:grid-cols-3 gap-6">
                <div className="gs-card p-5">
                    <h3 className="gs-section-title mb-4">Top Products</h3>
                    <div className="space-y-3">
                        {[
                            { name: 'Grey Suite Enterprise', mrr: '$420K' },
                            { name: 'Grey Suite Pro', mrr: '$280K' },
                            { name: 'Grey Analytics', mrr: '$180K' },
                            { name: 'Grey AI Platform', mrr: '$140K' },
                            { name: 'Grey Security', mrr: '$95K' },
                        ].map((p, i) => (
                            <div key={p.name} className="flex items-center justify-between">
                                <div className="flex items-center gap-3">
                                    <span className="text-xs text-grey-600 w-4">{i + 1}</span>
                                    <span className="text-sm text-grey-300">{p.name}</span>
                                </div>
                                <span className="text-sm font-medium text-grey-200">{p.mrr}</span>
                            </div>
                        ))}
                    </div>
                </div>

                <div className="gs-card p-5">
                    <h3 className="gs-section-title mb-4">Geo Distribution</h3>
                    <div className="space-y-3">
                        {[
                            { region: 'North America', pct: 52, revenue: '$7.7M' },
                            { region: 'Europe', pct: 28, revenue: '$4.1M' },
                            { region: 'Asia Pacific', pct: 14, revenue: '$2.1M' },
                            { region: 'Latin America', pct: 4, revenue: '$0.6M' },
                            { region: 'MEA', pct: 2, revenue: '$0.3M' },
                        ].map(g => (
                            <div key={g.region} className="flex items-center justify-between text-sm">
                                <span className="text-grey-300">{g.region}</span>
                                <span className="text-grey-400">{g.revenue} ({g.pct}%)</span>
                            </div>
                        ))}
                    </div>
                </div>

                <div className="gs-card p-5">
                    <h3 className="gs-section-title mb-4">Recent Reports</h3>
                    <div className="space-y-2">
                        {[
                            { name: 'Monthly Revenue Report', date: 'Feb 10', auto: true },
                            { name: 'Churn Analysis Q1', date: 'Feb 8', auto: false },
                            { name: 'Pipeline Coverage', date: 'Feb 6', auto: true },
                            { name: 'Cohort Analysis', date: 'Feb 4', auto: false },
                        ].map(r => (
                            <div key={r.name} className="flex items-center justify-between p-2 rounded-lg hover:bg-grey-800/40 transition-colors cursor-pointer">
                                <span className="text-sm text-grey-300">{r.name}</span>
                                <div className="flex items-center gap-2">
                                    {r.auto && <Badge variant="info">Auto</Badge>}
                                    <span className="text-xs text-grey-500">{r.date}</span>
                                </div>
                            </div>
                        ))}
                    </div>
                </div>
            </div>
        </div>
    );
}
