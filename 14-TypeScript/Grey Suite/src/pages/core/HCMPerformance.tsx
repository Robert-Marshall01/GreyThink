import { PageHeader, StatCard, Badge, ProgressBar } from '../../components/ui';
import { Activity, Award, Target, TrendingUp } from 'lucide-react';

export function HCMPerformance() {
    return (
        <div>
            <PageHeader
                title="Performance"
                subtitle="Reviews, goals, OKRs & employee development"
                actions={<button className="gs-btn-primary text-sm">+ New Review Cycle</button>}
            />

            <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-4 gap-4 mb-6">
                <StatCard label="Review Completion" value="78%" change="Q1 cycle" changeType="neutral" icon={<Activity size={18} />} />
                <StatCard label="Avg Rating" value="4.2/5" change="+0.1 vs LQ" changeType="positive" icon={<Award size={18} />} />
                <StatCard label="Goals On Track" value="84%" change="268 of 320" changeType="positive" icon={<Target size={18} />} />
                <StatCard label="Growth Plans" value={156} change="Active" changeType="neutral" icon={<TrendingUp size={18} />} />
            </div>

            <div className="grid grid-cols-1 lg:grid-cols-2 gap-6 mb-6">
                <div className="gs-card p-5">
                    <h3 className="gs-section-title mb-4">Company OKRs — Q1 2026</h3>
                    <div className="space-y-4">
                        {[
                            { objective: 'Increase ARR to $60M', progress: 72, status: 'On Track' },
                            { objective: 'Achieve NPS score of 75+', progress: 88, status: 'Ahead' },
                            { objective: 'Reduce churn to < 3%', progress: 65, status: 'At Risk' },
                            { objective: 'Launch 3 new products', progress: 33, status: 'On Track' },
                            { objective: 'Hire 50 engineers', progress: 56, status: 'Behind' },
                        ].map(okr => (
                            <div key={okr.objective}>
                                <div className="flex justify-between text-sm mb-2">
                                    <span className="text-grey-200">{okr.objective}</span>
                                    <Badge variant={okr.status === 'Ahead' ? 'success' : okr.status === 'On Track' ? 'info' : okr.status === 'At Risk' ? 'warning' : 'danger'}>
                                        {okr.status}
                                    </Badge>
                                </div>
                                <ProgressBar value={okr.progress} color={okr.progress >= 80 ? 'bg-accent-emerald' : okr.progress >= 50 ? 'bg-brand-500' : 'bg-accent-amber'} size="md" />
                                <p className="text-xs text-grey-500 mt-1">{okr.progress}% complete</p>
                            </div>
                        ))}
                    </div>
                </div>

                <div className="gs-card p-5">
                    <h3 className="gs-section-title mb-4">Rating Distribution</h3>
                    <div className="space-y-3">
                        {[
                            { rating: 'Exceptional (5)', count: 28, pct: 8 },
                            { rating: 'Exceeds Expectations (4)', count: 124, pct: 36 },
                            { rating: 'Meets Expectations (3)', count: 148, pct: 43 },
                            { rating: 'Needs Improvement (2)', count: 34, pct: 10 },
                            { rating: 'Unsatisfactory (1)', count: 8, pct: 3 },
                        ].map(r => (
                            <div key={r.rating}>
                                <div className="flex justify-between text-sm mb-1">
                                    <span className="text-grey-300">{r.rating}</span>
                                    <span className="text-grey-500">{r.count} ({r.pct}%)</span>
                                </div>
                                <div className="w-full bg-grey-800 rounded-full h-2">
                                    <div className="h-2 rounded-full bg-brand-500" style={{ width: `${r.pct}%` }} />
                                </div>
                            </div>
                        ))}
                    </div>
                </div>
            </div>

            {/* Top Performers */}
            <div className="gs-card p-5">
                <h3 className="gs-section-title mb-4">Top Performers</h3>
                <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-3">
                    {[
                        { name: 'Rachel Torres', dept: 'Engineering', rating: 5.0, highlight: '3 patents filed' },
                        { name: 'Kevin Liu', dept: 'Sales', rating: 4.9, highlight: '180% quota attained' },
                        { name: 'Maria Santos', dept: 'Product', rating: 4.8, highlight: 'Led 2 launches' },
                        { name: 'James Park', dept: 'Marketing', rating: 4.8, highlight: '42% lead increase' },
                    ].map(p => (
                        <div key={p.name} className="p-4 bg-grey-800/40 rounded-lg border border-grey-800">
                            <div className="flex items-center gap-3 mb-2">
                                <div className="w-10 h-10 rounded-full bg-gradient-to-br from-brand-400 to-accent-violet flex items-center justify-center text-xs font-bold text-white">
                                    {p.name.split(' ').map(n => n[0]).join('')}
                                </div>
                                <div>
                                    <p className="text-sm font-medium text-grey-200">{p.name}</p>
                                    <p className="text-xs text-grey-500">{p.dept}</p>
                                </div>
                            </div>
                            <div className="flex justify-between text-xs">
                                <span className="text-accent-amber">★ {p.rating}</span>
                                <span className="text-grey-400">{p.highlight}</span>
                            </div>
                        </div>
                    ))}
                </div>
            </div>
        </div>
    );
}
