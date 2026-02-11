import { useState } from 'react';
import { PageHeader, StatCard, Badge, DataTable, Tabs, ProgressBar, MiniSparkline } from '../../components/ui';
import {
    FlaskConical, TrendingUp, Users, Truck, Play,
    Download, Settings, Plus, BarChart3, Activity,
    DollarSign, AlertTriangle, CheckCircle, Layers
} from 'lucide-react';

const scenarios = [
    { id: 'SCN-001', name: 'Base Case — FY2026', type: 'Financial', iterations: 10000, status: 'Completed', confidence: '95%', lastRun: '2h ago' },
    { id: 'SCN-002', name: 'Bull Case — 130% Growth', type: 'Financial', iterations: 10000, status: 'Completed', confidence: '95%', lastRun: '2h ago' },
    { id: 'SCN-003', name: 'Bear Case — Recession', type: 'Financial', iterations: 10000, status: 'Completed', confidence: '95%', lastRun: '2h ago' },
    { id: 'SCN-004', name: 'Headcount Plan — Q3 2026', type: 'Workforce', iterations: 5000, status: 'Completed', confidence: '90%', lastRun: '1d ago' },
    { id: 'SCN-005', name: 'Supply Chain Disruption — APAC', type: 'Supply Chain', iterations: 8000, status: 'Running', confidence: '—', lastRun: 'In progress' },
    { id: 'SCN-006', name: 'AI Cost Projection', type: 'Financial', iterations: 10000, status: 'Completed', confidence: '95%', lastRun: '3d ago' },
];

export function SimulationEngine() {
    const [activeTab, setActiveTab] = useState('Overview');

    return (
        <div>
            <PageHeader
                title="Simulation Engine"
                subtitle="Monte Carlo simulations for finance, workforce planning & supply chain stress testing"
                actions={
                    <div className="flex gap-2">
                        <button className="gs-btn-secondary flex items-center gap-2"><Download size={16} /> Export</button>
                        <button className="gs-btn-primary flex items-center gap-2"><Plus size={16} /> New Scenario</button>
                    </div>
                }
            />

            <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-4 gap-4 mb-6">
                <StatCard label="Scenarios" value="6" change="1 running" changeType="neutral" icon={<FlaskConical size={18} />} />
                <StatCard label="Total Iterations" value="53K" change="Monte Carlo" changeType="neutral" icon={<Activity size={18} />} />
                <StatCard label="Avg Confidence" value="95%" change="10K iterations" changeType="positive" icon={<CheckCircle size={18} />} />
                <StatCard label="Variables Modeled" value="42" change="Across all scenarios" changeType="neutral" icon={<BarChart3 size={18} />} />
            </div>

            <Tabs tabs={['Overview', 'Financial', 'Workforce', 'Supply Chain', 'Parameters']} active={activeTab} onChange={setActiveTab} />

            {activeTab === 'Overview' && (
                <div className="gs-card p-5">
                    <h3 className="gs-section-title mb-4 flex items-center gap-2"><FlaskConical size={16} /> Scenario Registry</h3>
                    <DataTable
                        columns={['ID', 'Scenario', 'Type', 'Iterations', 'Confidence', 'Last Run', 'Status']}
                        rows={scenarios.map(s => [
                            <span className="font-mono text-xs text-grey-400">{s.id}</span>,
                            <span className="font-medium text-grey-200">{s.name}</span>,
                            <Badge variant={s.type === 'Financial' ? 'info' : s.type === 'Workforce' ? 'purple' : 'warning'}>{s.type}</Badge>,
                            s.iterations.toLocaleString(),
                            <span className="font-mono text-xs">{s.confidence}</span>,
                            <span className="text-xs text-grey-500">{s.lastRun}</span>,
                            <Badge variant={s.status === 'Completed' ? 'success' : 'info'}>{s.status}</Badge>,
                        ])}
                    />
                </div>
            )}

            {activeTab === 'Financial' && (
                <div className="space-y-6">
                    <div className="gs-card p-5">
                        <h3 className="gs-section-title mb-4 flex items-center gap-2"><DollarSign size={16} /> Financial Scenario Modeling — FY2026</h3>
                        <div className="grid grid-cols-3 gap-4 mb-6">
                            {[
                                { scenario: 'Bear Case', revenue: '$12.4M', ebitda: '$1.8M', margin: '14.5%', color: 'border-accent-rose/30 bg-accent-rose/5', textColor: 'text-accent-rose' },
                                { scenario: 'Base Case', revenue: '$18.2M', ebitda: '$4.2M', margin: '23.1%', color: 'border-brand-500/30 bg-brand-500/5', textColor: 'text-brand-400' },
                                { scenario: 'Bull Case', revenue: '$24.8M', ebitda: '$7.4M', margin: '29.8%', color: 'border-accent-emerald/30 bg-accent-emerald/5', textColor: 'text-accent-emerald' },
                            ].map(s => (
                                <div key={s.scenario} className={`p-5 rounded-lg border ${s.color}`}>
                                    <h4 className={`text-sm font-semibold ${s.textColor} mb-3`}>{s.scenario}</h4>
                                    <div className="space-y-2">
                                        <div className="flex justify-between"><span className="text-xs text-grey-500">Revenue</span><span className={`text-lg font-bold ${s.textColor}`}>{s.revenue}</span></div>
                                        <div className="flex justify-between"><span className="text-xs text-grey-500">EBITDA</span><span className="text-sm text-grey-300">{s.ebitda}</span></div>
                                        <div className="flex justify-between"><span className="text-xs text-grey-500">Margin</span><span className="text-sm text-grey-300">{s.margin}</span></div>
                                    </div>
                                </div>
                            ))}
                        </div>
                        <h4 className="text-xs font-semibold text-grey-500 uppercase mb-3">Revenue Distribution (10K iterations)</h4>
                        <MiniSparkline data={[2, 5, 12, 28, 52, 78, 95, 88, 62, 38, 18, 8, 3]} height={100} />
                        <div className="flex justify-between text-xs text-grey-500 mt-2">
                            <span>$10M (5th percentile)</span>
                            <span>$18.2M (median)</span>
                            <span>$28M (95th percentile)</span>
                        </div>
                    </div>

                    <div className="gs-card p-5">
                        <h3 className="gs-section-title mb-4">Sensitivity Analysis — Key Drivers</h3>
                        <div className="space-y-3">
                            {[
                                { driver: 'Customer Churn Rate', impact: 'High', range: '1.5% — 4.2%', baseline: '2.1%', sensitivity: 85 },
                                { driver: 'Average Contract Value', impact: 'High', range: '$8K — $18K', baseline: '$12K', sensitivity: 78 },
                                { driver: 'Sales Cycle Length', impact: 'Medium', range: '28 — 65 days', baseline: '42 days', sensitivity: 62 },
                                { driver: 'CAC Payback Period', impact: 'Medium', range: '8 — 18 months', baseline: '12 months', sensitivity: 55 },
                                { driver: 'Engineering Velocity', impact: 'Low', range: '0.8x — 1.4x', baseline: '1.0x', sensitivity: 38 },
                                { driver: 'Market Growth Rate', impact: 'Medium', range: '12% — 28%', baseline: '18%', sensitivity: 48 },
                            ].map(d => (
                                <div key={d.driver} className="p-3 rounded-lg bg-grey-800/30">
                                    <div className="flex items-center justify-between mb-1">
                                        <div className="flex items-center gap-2">
                                            <span className="text-sm text-grey-200">{d.driver}</span>
                                            <Badge variant={d.impact === 'High' ? 'danger' : d.impact === 'Medium' ? 'warning' : 'info'}>{d.impact}</Badge>
                                        </div>
                                        <span className="font-mono text-xs text-grey-400">{d.range}</span>
                                    </div>
                                    <ProgressBar value={d.sensitivity} color={d.sensitivity > 70 ? 'bg-accent-rose' : d.sensitivity > 50 ? 'bg-accent-amber' : 'bg-brand-500'} size="sm" />
                                </div>
                            ))}
                        </div>
                    </div>
                </div>
            )}

            {activeTab === 'Workforce' && (
                <div className="space-y-6">
                    <div className="gs-card p-5">
                        <h3 className="gs-section-title mb-4 flex items-center gap-2"><Users size={16} /> Workforce Planning Simulation — Q3 2026</h3>
                        <div className="grid grid-cols-4 gap-4 mb-6">
                            {[
                                { label: 'Current Headcount', value: '342', detail: 'FTE' },
                                { label: 'Projected (Q3)', value: '398', detail: '+56 hires' },
                                { label: 'Attrition Rate', value: '8.4%', detail: 'Annualized' },
                                { label: 'Total Comp Cost', value: '$42.8M', detail: 'Annual projection' },
                            ].map(m => (
                                <div key={m.label} className="p-4 bg-grey-800/30 rounded-lg text-center">
                                    <p className="text-xl font-bold text-grey-100">{m.value}</p>
                                    <p className="text-xs text-grey-400">{m.detail}</p>
                                    <p className="text-xs text-grey-500 mt-1">{m.label}</p>
                                </div>
                            ))}
                        </div>
                        <h4 className="text-xs font-semibold text-grey-500 uppercase mb-3">Headcount Projections by Department</h4>
                        <div className="space-y-3">
                            {[
                                { dept: 'Engineering', current: 142, projected: 168, change: '+26' },
                                { dept: 'Sales', current: 86, projected: 102, change: '+16' },
                                { dept: 'Product', current: 24, projected: 30, change: '+6' },
                                { dept: 'Finance', current: 24, projected: 28, change: '+4' },
                                { dept: 'HR', current: 12, projected: 14, change: '+2' },
                                { dept: 'Other', current: 54, projected: 56, change: '+2' },
                            ].map(d => (
                                <div key={d.dept} className="flex items-center gap-3">
                                    <span className="text-sm text-grey-300 w-24">{d.dept}</span>
                                    <div className="flex-1">
                                        <ProgressBar value={d.projected} max={180} color="bg-brand-500" size="sm" />
                                    </div>
                                    <span className="text-xs text-grey-400 w-12 text-right">{d.current}</span>
                                    <span className="text-xs text-accent-emerald w-8 text-right">{d.change}</span>
                                </div>
                            ))}
                        </div>
                    </div>
                </div>
            )}

            {activeTab === 'Supply Chain' && (
                <div className="space-y-6">
                    <div className="gs-card p-5">
                        <h3 className="gs-section-title mb-4 flex items-center gap-2"><Truck size={16} /> Supply Chain Stress Test — APAC Disruption</h3>
                        <div className="grid grid-cols-3 gap-4 mb-6">
                            {[
                                { label: 'Impact: Revenue Delay', value: '$2.4M', severity: 'High', desc: 'Expected revenue pushed to Q4' },
                                { label: 'Impact: COGS Increase', value: '+18%', severity: 'Medium', desc: 'Alt supplier premium pricing' },
                                { label: 'Recovery Time', value: '6-8 weeks', severity: 'Medium', desc: 'Estimated recovery period' },
                            ].map(i => (
                                <div key={i.label} className="p-4 rounded-lg border border-grey-800">
                                    <Badge variant={i.severity === 'High' ? 'danger' : 'warning'}>{i.severity}</Badge>
                                    <p className="text-lg font-bold text-grey-100 mt-2">{i.value}</p>
                                    <p className="text-xs text-grey-500">{i.label}</p>
                                    <p className="text-xs text-grey-600 mt-1">{i.desc}</p>
                                </div>
                            ))}
                        </div>

                        <h4 className="text-xs font-semibold text-grey-500 uppercase mb-3">Mitigation Strategies</h4>
                        <div className="space-y-2">
                            {[
                                { strategy: 'Activate secondary supplier (Vietnam)', cost: '+$180K', time: '2 weeks', effectiveness: 82 },
                                { strategy: 'Air freight for critical components', cost: '+$420K', time: '3 days', effectiveness: 94 },
                                { strategy: 'Buffer inventory increase (30 days)', cost: '+$640K', time: 'Preventive', effectiveness: 88 },
                                { strategy: 'Customer communication + delivery reschedule', cost: '$0', time: '1 day', effectiveness: 65 },
                            ].map(s => (
                                <div key={s.strategy} className="p-3 rounded-lg bg-grey-800/30 flex items-center justify-between">
                                    <div>
                                        <p className="text-sm text-grey-200">{s.strategy}</p>
                                        <p className="text-xs text-grey-500">Cost: {s.cost} · Lead time: {s.time}</p>
                                    </div>
                                    <div className="flex items-center gap-2">
                                        <span className={`text-sm font-bold ${s.effectiveness >= 90 ? 'text-accent-emerald' : s.effectiveness >= 80 ? 'text-accent-amber' : 'text-grey-400'}`}>
                                            {s.effectiveness}%
                                        </span>
                                        <span className="text-xs text-grey-500">effective</span>
                                    </div>
                                </div>
                            ))}
                        </div>
                    </div>
                </div>
            )}

            {activeTab === 'Parameters' && (
                <div className="gs-card p-5">
                    <h3 className="gs-section-title mb-4 flex items-center gap-2"><Settings size={16} /> Parameterized Scenario Inputs</h3>
                    <div className="bg-grey-900/50 rounded-lg p-4 font-mono text-xs mb-6">
                        <div className="text-grey-500">// Monte Carlo parameters for financial scenario</div>
                        <div className="text-accent-cyan">{'{'}</div>
                        <div className="pl-3 text-grey-400">"engine": <span className="text-accent-emerald">"monte_carlo"</span>,</div>
                        <div className="pl-3 text-grey-400">"iterations": <span className="text-accent-amber">10000</span>,</div>
                        <div className="pl-3 text-grey-400">"confidence_interval": <span className="text-accent-amber">0.95</span>,</div>
                        <div className="pl-3 text-grey-400">"variables": {'{'}</div>
                        <div className="pl-6 text-grey-400">"revenue_growth": {'{'} "dist": <span className="text-accent-emerald">"normal"</span>, "mean": <span className="text-accent-amber">0.18</span>, "std": <span className="text-accent-amber">0.06</span> {'}'},</div>
                        <div className="pl-6 text-grey-400">"churn_rate": {'{'} "dist": <span className="text-accent-emerald">"beta"</span>, "alpha": <span className="text-accent-amber">2</span>, "beta": <span className="text-accent-amber">50</span> {'}'},</div>
                        <div className="pl-6 text-grey-400">"acv": {'{'} "dist": <span className="text-accent-emerald">"lognormal"</span>, "mu": <span className="text-accent-amber">9.4</span>, "sigma": <span className="text-accent-amber">0.3</span> {'}'},</div>
                        <div className="pl-6 text-grey-400">"sales_cycle": {'{'} "dist": <span className="text-accent-emerald">"uniform"</span>, "min": <span className="text-accent-amber">28</span>, "max": <span className="text-accent-amber">65</span> {'}'}</div>
                        <div className="pl-3 text-grey-400">{'}'},</div>
                        <div className="pl-3 text-grey-400">"output": [<span className="text-accent-emerald">"revenue"</span>, <span className="text-accent-emerald">"ebitda"</span>, <span className="text-accent-emerald">"margin"</span>]</div>
                        <div className="text-accent-cyan">{'}'}</div>
                    </div>
                    <div className="flex gap-2">
                        <button className="gs-btn-primary flex items-center gap-2"><Play size={16} /> Run Simulation</button>
                        <button className="gs-btn-secondary flex items-center gap-2"><Download size={16} /> Export Config</button>
                    </div>
                </div>
            )}
        </div>
    );
}
