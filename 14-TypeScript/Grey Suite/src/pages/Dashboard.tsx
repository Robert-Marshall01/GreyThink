import { StatCard, Badge, MiniSparkline } from '../components/ui';
import { Link } from 'react-router-dom';
import {
    DollarSign, Users, Briefcase, TrendingUp, MessageSquare, Bot,
    BarChart3, Shield, Activity, Zap, CheckCircle, Clock,
    ArrowUpRight, Building2, Hexagon
} from 'lucide-react';

export function Dashboard() {
    return (
        <div>
            {/* Welcome Banner */}
            <div className="gs-card p-6 mb-6 bg-gradient-to-r from-brand-500/10 via-grey-900 to-accent-violet/10 border-brand-500/20">
                <div className="flex items-center justify-between">
                    <div>
                        <div className="flex items-center gap-3 mb-2">
                            <div className="w-10 h-10 rounded-xl bg-brand-500 flex items-center justify-center">
                                <Hexagon size={20} className="text-white" />
                            </div>
                            <div>
                                <h1 className="text-2xl font-bold text-grey-100">Good morning, Admin</h1>
                                <p className="text-sm text-grey-400">Monday, February 10, 2026 · Grey Suite Enterprise</p>
                            </div>
                        </div>
                    </div>
                    <div className="hidden lg:flex items-center gap-4 text-sm">
                        <div className="flex items-center gap-2 text-accent-emerald">
                            <span className="w-2 h-2 rounded-full bg-accent-emerald dot-pulse" />
                            All systems operational
                        </div>
                    </div>
                </div>
            </div>

            {/* Executive KPIs */}
            <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-4 gap-4 mb-6">
                <StatCard label="Revenue (MTD)" value="$14.8M" change="+12.3% vs LM" changeType="positive" icon={<DollarSign size={18} />} />
                <StatCard label="Active Customers" value="1,284" change="+42 this month" changeType="positive" icon={<Users size={18} />} />
                <StatCard label="Open Deals" value="67" change="$4.8M pipeline" changeType="neutral" icon={<Briefcase size={18} />} />
                <StatCard label="Employee Count" value="342" change="+8 hires" changeType="positive" icon={<Building2 size={18} />} />
            </div>

            {/* Main Grid */}
            <div className="grid grid-cols-1 lg:grid-cols-3 gap-6 mb-6">
                {/* Revenue Chart */}
                <div className="lg:col-span-2 gs-card p-5">
                    <div className="flex items-center justify-between mb-4">
                        <h3 className="gs-section-title">Revenue Overview</h3>
                        <Link to="/erp/finance" className="text-xs text-brand-400 hover:text-brand-300 flex items-center gap-1">
                            View Details <ArrowUpRight size={12} />
                        </Link>
                    </div>
                    <MiniSparkline data={[8.2, 8.8, 9.1, 9.6, 10.2, 10.8, 11.4, 11.9, 12.6, 13.2, 14.0, 14.8]} height={140} />
                    <div className="grid grid-cols-4 gap-4 mt-4 pt-4 border-t border-grey-800 text-sm">
                        <div><p className="text-xs text-grey-500">MRR</p><p className="font-semibold text-grey-200">$1.24M</p></div>
                        <div><p className="text-xs text-grey-500">ARR</p><p className="font-semibold text-grey-200">$14.8M</p></div>
                        <div><p className="text-xs text-grey-500">NRR</p><p className="font-semibold text-accent-emerald">118%</p></div>
                        <div><p className="text-xs text-grey-500">Churn</p><p className="font-semibold text-grey-200">2.1%</p></div>
                    </div>
                </div>

                {/* Quick Actions */}
                <div className="gs-card p-5">
                    <h3 className="gs-section-title mb-4">Quick Actions</h3>
                    <div className="space-y-2">
                        {[
                            { label: 'View Sales Pipeline', path: '/crm/opportunities', icon: <TrendingUp size={16} />, color: 'text-brand-400' },
                            { label: 'Open Chat', path: '/collaboration/chat', icon: <MessageSquare size={16} />, color: 'text-accent-emerald' },
                            { label: 'Project Board', path: '/ops/projects', icon: <CheckCircle size={16} />, color: 'text-accent-amber' },
                            { label: 'AI Agents', path: '/ai/agent-framework', icon: <Bot size={16} />, color: 'text-accent-violet' },
                            { label: 'BI Dashboard', path: '/data/bi', icon: <BarChart3 size={16} />, color: 'text-accent-cyan' },
                            { label: 'Security Center', path: '/ops/security', icon: <Shield size={16} />, color: 'text-accent-rose' },
                        ].map(action => (
                            <Link
                                key={action.label}
                                to={action.path}
                                className="flex items-center gap-3 p-3 rounded-lg hover:bg-grey-800/60 transition-colors group"
                            >
                                <div className={action.color}>{action.icon}</div>
                                <span className="text-sm text-grey-300 group-hover:text-grey-100 transition-colors">{action.label}</span>
                                <ArrowUpRight size={12} className="ml-auto text-grey-600 group-hover:text-grey-400 transition-colors" />
                            </Link>
                        ))}
                    </div>
                </div>
            </div>

            {/* Module Status Row */}
            <div className="grid grid-cols-1 lg:grid-cols-3 gap-6 mb-6">
                {/* Recent Activity */}
                <div className="gs-card p-5">
                    <h3 className="gs-section-title mb-4">Recent Activity</h3>
                    <div className="space-y-3">
                        {[
                            { action: 'Deal closed — Acme Corp', module: 'CRM', time: '12m ago', icon: <Briefcase size={14} /> },
                            { action: 'Sprint 24 started', module: 'Projects', time: '1h ago', icon: <Activity size={14} /> },
                            { action: 'New hire onboarded', module: 'HCM', time: '2h ago', icon: <Users size={14} /> },
                            { action: 'AI Agent triggered 842 times', module: 'AI', time: '3h ago', icon: <Bot size={14} /> },
                            { action: 'Security scan completed', module: 'Security', time: '4h ago', icon: <Shield size={14} /> },
                            { action: 'Invoice #2026-0284 sent', module: 'Finance', time: '5h ago', icon: <DollarSign size={14} /> },
                        ].map((a, i) => (
                            <div key={i} className="flex items-center gap-3 text-sm">
                                <div className="text-grey-500">{a.icon}</div>
                                <div className="flex-1">
                                    <span className="text-grey-300">{a.action}</span>
                                </div>
                                <span className="text-xs text-grey-600">{a.time}</span>
                            </div>
                        ))}
                    </div>
                </div>

                {/* AI Automation */}
                <div className="gs-card p-5">
                    <div className="flex items-center justify-between mb-4">
                        <h3 className="gs-section-title flex items-center gap-2"><Zap size={16} className="text-accent-amber" /> AI Activity</h3>
                        <Link to="/ai/agent-framework" className="text-xs text-brand-400 hover:text-brand-300">View All</Link>
                    </div>
                    <div className="grid grid-cols-2 gap-3 mb-4">
                        <div className="p-3 bg-grey-800/30 rounded-lg">
                            <p className="text-xs text-grey-500">Runs Today</p>
                            <p className="text-xl font-bold text-grey-100">1,847</p>
                            <p className="text-xs text-accent-emerald mt-1">+22%</p>
                        </div>
                        <div className="p-3 bg-grey-800/30 rounded-lg">
                            <p className="text-xs text-grey-500">Time Saved</p>
                            <p className="text-xl font-bold text-grey-100">142h</p>
                            <p className="text-xs text-grey-400 mt-1">This month</p>
                        </div>
                    </div>
                    <div className="space-y-2">
                        {[
                            { agent: 'Lead Qualifier', runs: 284, status: 'Active' },
                            { agent: 'Invoice Processor', runs: 142, status: 'Active' },
                            { agent: 'Meeting Scheduler', runs: 86, status: 'Active' },
                        ].map(a => (
                            <div key={a.agent} className="flex items-center justify-between p-2 rounded-lg hover:bg-grey-800/40 transition-colors text-sm">
                                <div className="flex items-center gap-2">
                                    <Bot size={14} className="text-accent-violet" />
                                    <span className="text-grey-300">{a.agent}</span>
                                </div>
                                <span className="text-xs text-grey-500">{a.runs} runs</span>
                            </div>
                        ))}
                    </div>
                </div>

                {/* Infrastructure Status */}
                <div className="gs-card p-5">
                    <div className="flex items-center justify-between mb-4">
                        <h3 className="gs-section-title">System Health</h3>
                        <Link to="/infra" className="text-xs text-brand-400 hover:text-brand-300">Details</Link>
                    </div>
                    <div className="space-y-3">
                        {[
                            { service: 'API Gateway', latency: '12ms', status: 'Healthy' },
                            { service: 'Auth Service', latency: '8ms', status: 'Healthy' },
                            { service: 'CRM Service', latency: '24ms', status: 'Healthy' },
                            { service: 'Analytics Engine', latency: '48ms', status: 'Healthy' },
                            { service: 'AI Inference', latency: '120ms', status: 'Degraded' },
                        ].map(s => (
                            <div key={s.service} className="flex items-center justify-between text-sm">
                                <div className="flex items-center gap-2">
                                    <div className={`w-1.5 h-1.5 rounded-full ${s.status === 'Healthy' ? 'bg-accent-emerald' : 'bg-accent-amber dot-pulse'}`} />
                                    <span className="text-grey-300">{s.service}</span>
                                </div>
                                <span className="text-xs text-grey-500">{s.latency}</span>
                            </div>
                        ))}
                    </div>
                    <div className="mt-4 pt-4 border-t border-grey-800 grid grid-cols-3 gap-2 text-center">
                        <div>
                            <p className="text-lg font-bold text-grey-100">99.97%</p>
                            <p className="text-xs text-grey-500">Uptime</p>
                        </div>
                        <div>
                            <p className="text-lg font-bold text-grey-100">48</p>
                            <p className="text-xs text-grey-500">Services</p>
                        </div>
                        <div>
                            <p className="text-lg font-bold text-grey-100">42</p>
                            <p className="text-xs text-grey-500">Deploys/wk</p>
                        </div>
                    </div>
                </div>
            </div>

            {/* Upcoming & Tasks */}
            <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
                <div className="gs-card p-5">
                    <h3 className="gs-section-title mb-4 flex items-center gap-2"><Clock size={16} /> Upcoming</h3>
                    <div className="space-y-3">
                        {[
                            { event: 'Sprint Planning', time: '10:00 AM', type: 'Meeting', attendees: 12 },
                            { event: 'Design Review', time: '11:30 AM', type: 'Review', attendees: 6 },
                            { event: '1:1 with Sarah', time: '2:00 PM', type: '1:1', attendees: 2 },
                            { event: 'All Hands', time: '3:30 PM', type: 'Company', attendees: 342 },
                        ].map(e => (
                            <div key={e.event} className="flex items-center justify-between p-3 rounded-lg border border-grey-800 hover:border-grey-700 transition-colors">
                                <div>
                                    <p className="text-sm font-medium text-grey-200">{e.event}</p>
                                    <p className="text-xs text-grey-500">{e.time} · {e.attendees} attendees</p>
                                </div>
                                <Badge variant={e.type === 'Meeting' ? 'info' : e.type === '1:1' ? 'purple' : e.type === 'Review' ? 'warning' : 'default'}>{e.type}</Badge>
                            </div>
                        ))}
                    </div>
                </div>

                <div className="gs-card p-5">
                    <h3 className="gs-section-title mb-4 flex items-center gap-2"><CheckCircle size={16} /> My Tasks</h3>
                    <div className="space-y-3">
                        {[
                            { task: 'Review API design document', project: 'ENG-4521', priority: 'High', due: 'Today' },
                            { task: 'Approve Q1 budget proposal', project: 'FIN-0042', priority: 'High', due: 'Today' },
                            { task: 'Update team roadmap', project: 'PROD-0128', priority: 'Medium', due: 'Tomorrow' },
                            { task: 'Prepare board presentation', project: 'EXEC-0018', priority: 'Medium', due: 'Feb 14' },
                        ].map(t => (
                            <div key={t.task} className="flex items-center gap-3 p-3 rounded-lg border border-grey-800 hover:border-grey-700 transition-colors">
                                <input type="checkbox" className="w-4 h-4 rounded border-grey-600 bg-grey-800 text-brand-500 focus:ring-brand-500" />
                                <div className="flex-1">
                                    <p className="text-sm text-grey-200">{t.task}</p>
                                    <p className="text-xs text-grey-500">{t.project} · Due {t.due}</p>
                                </div>
                                <Badge variant={t.priority === 'High' ? 'danger' : 'warning'}>{t.priority}</Badge>
                            </div>
                        ))}
                    </div>
                </div>
            </div>
        </div>
    );
}
