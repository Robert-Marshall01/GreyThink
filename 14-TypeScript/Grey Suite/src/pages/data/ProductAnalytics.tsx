import { PageHeader, StatCard, Badge, MiniSparkline } from '../../components/ui';
import { Activity, Users, MousePointer, Clock } from 'lucide-react';

export function ProductAnalytics() {
    return (
        <div>
            <PageHeader
                title="Product Analytics"
                subtitle="Events, funnels, retention & user behavior"
            />

            <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-4 gap-4 mb-6">
                <StatCard label="DAU" value="24.8K" change="+8.4%" changeType="positive" icon={<Users size={18} />} />
                <StatCard label="Events Today" value="1.4M" change="+12%" changeType="positive" icon={<Activity size={18} />} />
                <StatCard label="Avg Session" value="12.4m" change="+1.2m" changeType="positive" icon={<Clock size={18} />} />
                <StatCard label="Feature Adoption" value="68%" change="+4%" changeType="positive" icon={<MousePointer size={18} />} />
            </div>

            <div className="grid grid-cols-1 lg:grid-cols-2 gap-6 mb-6">
                {/* Active Users */}
                <div className="gs-card p-5">
                    <h3 className="gs-section-title mb-1">Active Users</h3>
                    <p className="text-xs text-grey-500 mb-4">Daily active users — last 30 days</p>
                    <MiniSparkline data={[18200, 19400, 20100, 19800, 21200, 22400, 23100, 22800, 23600, 24100, 23800, 24500, 24200, 24800, 25100]} height={120} color="#10b981" />
                    <div className="grid grid-cols-3 gap-4 mt-4 pt-4 border-t border-grey-800 text-sm">
                        <div><p className="text-xs text-grey-500">DAU</p><p className="font-semibold text-grey-200">24.8K</p></div>
                        <div><p className="text-xs text-grey-500">WAU</p><p className="font-semibold text-grey-200">42.1K</p></div>
                        <div><p className="text-xs text-grey-500">MAU</p><p className="font-semibold text-grey-200">68.4K</p></div>
                    </div>
                </div>

                {/* Conversion Funnel */}
                <div className="gs-card p-5">
                    <h3 className="gs-section-title mb-4">Conversion Funnel</h3>
                    <div className="space-y-3">
                        {[
                            { step: 'Homepage Visit', count: '48,200', pct: 100, drop: null },
                            { step: 'Sign Up Started', count: '12,400', pct: 25.7, drop: '74.3%' },
                            { step: 'Profile Complete', count: '8,200', pct: 17.0, drop: '33.9%' },
                            { step: 'First Action', count: '6,800', pct: 14.1, drop: '17.1%' },
                            { step: 'Converted (Paid)', count: '2,400', pct: 5.0, drop: '64.7%' },
                        ].map((s, i) => (
                            <div key={s.step}>
                                <div className="flex items-center justify-between text-sm mb-1">
                                    <div className="flex items-center gap-2">
                                        <span className="w-5 h-5 rounded-full bg-grey-800 flex items-center justify-center text-xs text-grey-400">{i + 1}</span>
                                        <span className="text-grey-300">{s.step}</span>
                                    </div>
                                    <div className="flex items-center gap-3">
                                        <span className="text-grey-200 font-medium">{s.count}</span>
                                        {s.drop && <span className="text-xs text-accent-rose">↓ {s.drop}</span>}
                                    </div>
                                </div>
                                <div className="ml-7 w-full bg-grey-800 rounded-full h-2">
                                    <div className="h-2 rounded-full bg-brand-500" style={{ width: `${s.pct}%` }} />
                                </div>
                            </div>
                        ))}
                    </div>
                </div>
            </div>

            {/* Retention */}
            <div className="gs-card p-5 mb-6">
                <h3 className="gs-section-title mb-4">Weekly Retention Cohorts</h3>
                <div className="overflow-x-auto">
                    <table className="gs-table w-full text-center">
                        <thead>
                            <tr>
                                <th className="text-left">Cohort</th>
                                <th>Week 0</th><th>Week 1</th><th>Week 2</th><th>Week 3</th><th>Week 4</th><th>Week 5</th><th>Week 6</th><th>Week 7</th>
                            </tr>
                        </thead>
                        <tbody>
                            {[
                                { week: 'Jan 6', data: [100, 64, 52, 45, 41, 38, 36, 34] },
                                { week: 'Jan 13', data: [100, 68, 56, 48, 43, 40, 37, null] },
                                { week: 'Jan 20', data: [100, 71, 58, 50, 45, 42, null, null] },
                                { week: 'Jan 27', data: [100, 66, 54, 47, 42, null, null, null] },
                                { week: 'Feb 3', data: [100, 72, 59, 51, null, null, null, null] },
                            ].map(row => (
                                <tr key={row.week}>
                                    <td className="text-left font-medium">{row.week}</td>
                                    {row.data.map((val, i) => (
                                        <td key={i}>
                                            {val !== null ? (
                                                <span className={`inline-block px-2 py-0.5 rounded text-xs font-medium ${val >= 60 ? 'bg-accent-emerald/20 text-accent-emerald' :
                                                        val >= 40 ? 'bg-brand-500/20 text-brand-400' :
                                                            val >= 20 ? 'bg-accent-amber/20 text-accent-amber' :
                                                                'bg-grey-800 text-grey-400'
                                                    }`}>{val}%</span>
                                            ) : <span className="text-grey-700">—</span>}
                                        </td>
                                    ))}
                                </tr>
                            ))}
                        </tbody>
                    </table>
                </div>
            </div>

            {/* Top Events */}
            <div className="gs-card p-5">
                <h3 className="gs-section-title mb-4">Top Events (Today)</h3>
                <div className="grid grid-cols-1 md:grid-cols-2 gap-2">
                    {[
                        { event: 'page_view', count: '482K', trend: '+8%' },
                        { event: 'button_click', count: '124K', trend: '+12%' },
                        { event: 'form_submit', count: '48K', trend: '+3%' },
                        { event: 'search_query', count: '36K', trend: '+15%' },
                        { event: 'file_upload', count: '8.4K', trend: '-2%' },
                        { event: 'api_call', count: '248K', trend: '+22%' },
                    ].map(e => (
                        <div key={e.event} className="flex items-center justify-between p-3 rounded-lg hover:bg-grey-800/40 transition-colors">
                            <span className="text-sm font-mono text-grey-300">{e.event}</span>
                            <div className="flex items-center gap-3">
                                <span className="text-sm font-medium text-grey-200">{e.count}</span>
                                <span className={`text-xs ${e.trend.startsWith('+') ? 'text-accent-emerald' : 'text-accent-rose'}`}>{e.trend}</span>
                            </div>
                        </div>
                    ))}
                </div>
            </div>
        </div>
    );
}
