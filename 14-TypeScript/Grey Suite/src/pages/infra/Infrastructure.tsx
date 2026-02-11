import { PageHeader, StatCard, Badge } from '../../components/ui';
import { Server, Globe, Activity, Shield, GitBranch, Container, Cloud, Terminal } from 'lucide-react';

export function Infrastructure() {
    return (
        <div>
            <PageHeader
                title="Infrastructure"
                subtitle="API gateway, service mesh, monitoring, CI/CD, Kubernetes & Terraform"
            />

            <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-4 gap-4 mb-6">
                <StatCard label="Services" value={48} change="All healthy" changeType="positive" icon={<Server size={18} />} />
                <StatCard label="Uptime" value="99.97%" change="Last 30 days" changeType="positive" icon={<Activity size={18} />} />
                <StatCard label="Deployments (7d)" value={42} change="+8 vs last week" changeType="positive" icon={<GitBranch size={18} />} />
                <StatCard label="Incidents" value={0} change="0 open" changeType="positive" icon={<Shield size={18} />} />
            </div>

            <div className="grid grid-cols-1 lg:grid-cols-2 gap-6 mb-6">
                {/* Service Health */}
                <div className="gs-card p-5">
                    <h3 className="gs-section-title mb-4 flex items-center gap-2"><Globe size={16} /> Service Health</h3>
                    <div className="space-y-2">
                        {[
                            { service: 'API Gateway', status: 'Healthy', latency: '12ms', rps: '4,200', cpu: '34%' },
                            { service: 'Auth Service', status: 'Healthy', latency: '8ms', rps: '1,800', cpu: '22%' },
                            { service: 'CRM Service', status: 'Healthy', latency: '24ms', rps: '890', cpu: '45%' },
                            { service: 'Analytics Engine', status: 'Healthy', latency: '48ms', rps: '2,400', cpu: '68%' },
                            { service: 'AI Inference', status: 'Degraded', latency: '120ms', rps: '340', cpu: '89%' },
                            { service: 'Notification Service', status: 'Healthy', latency: '6ms', rps: '560', cpu: '18%' },
                            { service: 'File Storage', status: 'Healthy', latency: '18ms', rps: '1,200', cpu: '28%' },
                            { service: 'Search Index', status: 'Healthy', latency: '32ms', rps: '780', cpu: '52%' },
                        ].map(s => (
                            <div key={s.service} className="flex items-center justify-between p-2 rounded-lg hover:bg-grey-800/40 transition-colors">
                                <div className="flex items-center gap-3">
                                    <div className={`w-2 h-2 rounded-full ${s.status === 'Healthy' ? 'bg-accent-emerald' : 'bg-accent-amber dot-pulse'}`} />
                                    <span className="text-sm text-grey-200 w-36">{s.service}</span>
                                </div>
                                <div className="flex items-center gap-4 text-xs text-grey-400">
                                    <span className="w-16">{s.latency}</span>
                                    <span className="w-16">{s.rps} rps</span>
                                    <span className={`w-12 ${parseInt(s.cpu) > 80 ? 'text-accent-amber' : ''}`}>{s.cpu}</span>
                                    <Badge variant={s.status === 'Healthy' ? 'success' : 'warning'}>{s.status}</Badge>
                                </div>
                            </div>
                        ))}
                    </div>
                </div>

                {/* Kubernetes */}
                <div className="gs-card p-5">
                    <h3 className="gs-section-title mb-4 flex items-center gap-2"><Container size={16} /> Kubernetes Clusters</h3>
                    <div className="space-y-4">
                        {[
                            { name: 'prod-us-east', nodes: 12, pods: 148, cpu: '64%', memory: '72%', version: 'v1.29' },
                            { name: 'prod-us-west', nodes: 8, pods: 96, cpu: '52%', memory: '58%', version: 'v1.29' },
                            { name: 'prod-eu-central', nodes: 6, pods: 72, cpu: '48%', memory: '54%', version: 'v1.29' },
                            { name: 'staging', nodes: 4, pods: 42, cpu: '28%', memory: '34%', version: 'v1.30' },
                        ].map(cluster => (
                            <div key={cluster.name} className="p-3 rounded-lg border border-grey-800 hover:border-grey-700 transition-colors">
                                <div className="flex items-center justify-between mb-2">
                                    <div className="flex items-center gap-2">
                                        <span className="w-2 h-2 rounded-full bg-accent-emerald" />
                                        <span className="text-sm font-mono font-medium text-grey-200">{cluster.name}</span>
                                    </div>
                                    <Badge>{cluster.version}</Badge>
                                </div>
                                <div className="grid grid-cols-4 gap-2 text-xs text-grey-400">
                                    <div><span className="text-grey-600">Nodes:</span> {cluster.nodes}</div>
                                    <div><span className="text-grey-600">Pods:</span> {cluster.pods}</div>
                                    <div><span className="text-grey-600">CPU:</span> {cluster.cpu}</div>
                                    <div><span className="text-grey-600">Mem:</span> {cluster.memory}</div>
                                </div>
                            </div>
                        ))}
                    </div>
                </div>
            </div>

            <div className="grid grid-cols-1 lg:grid-cols-2 gap-6 mb-6">
                {/* CI/CD */}
                <div className="gs-card p-5">
                    <h3 className="gs-section-title mb-4 flex items-center gap-2"><GitBranch size={16} /> Recent Deployments</h3>
                    <div className="space-y-2">
                        {[
                            { id: '#4521', branch: 'main', msg: 'feat: auth token refresh', author: 'Marcus J.', time: '10m ago', status: 'Success', duration: '4m 12s' },
                            { id: '#4520', branch: 'main', msg: 'fix: API latency optimization', author: 'Sarah C.', time: '42m ago', status: 'Success', duration: '3m 48s' },
                            { id: '#4519', branch: 'staging', msg: 'feat: new dashboard widgets', author: 'David K.', time: '1h ago', status: 'Success', duration: '5m 02s' },
                            { id: '#4518', branch: 'main', msg: 'chore: upgrade dependencies', author: 'Bot', time: '2h ago', status: 'Success', duration: '6m 14s' },
                            { id: '#4517', branch: 'staging', msg: 'feat: AI summarization v2', author: 'Emily R.', time: '3h ago', status: 'Failed', duration: '2m 38s' },
                        ].map(d => (
                            <div key={d.id} className="flex items-center justify-between p-2 rounded-lg hover:bg-grey-800/40 transition-colors">
                                <div className="flex items-center gap-3">
                                    <div className={`w-2 h-2 rounded-full ${d.status === 'Success' ? 'bg-accent-emerald' : 'bg-accent-rose'}`} />
                                    <div>
                                        <p className="text-sm text-grey-200">
                                            <span className="font-mono text-grey-500">{d.id}</span> {d.msg}
                                        </p>
                                        <p className="text-xs text-grey-500">{d.branch} · {d.author} · {d.time}</p>
                                    </div>
                                </div>
                                <div className="flex items-center gap-2 text-xs">
                                    <span className="text-grey-500">{d.duration}</span>
                                    <Badge variant={d.status === 'Success' ? 'success' : 'danger'}>{d.status}</Badge>
                                </div>
                            </div>
                        ))}
                    </div>
                </div>

                {/* Terraform / IaC */}
                <div className="gs-card p-5">
                    <h3 className="gs-section-title mb-4 flex items-center gap-2"><Cloud size={16} /> Infrastructure as Code</h3>
                    <div className="space-y-3">
                        {[
                            { stack: 'aws-networking', resources: 42, drift: 0, lastApply: '2h ago', status: 'Synced' },
                            { stack: 'aws-compute', resources: 86, drift: 0, lastApply: '4h ago', status: 'Synced' },
                            { stack: 'aws-databases', resources: 18, drift: 1, lastApply: '1d ago', status: 'Drift Detected' },
                            { stack: 'gcp-ai-services', resources: 24, drift: 0, lastApply: '6h ago', status: 'Synced' },
                            { stack: 'monitoring', resources: 32, drift: 0, lastApply: '8h ago', status: 'Synced' },
                        ].map(s => (
                            <div key={s.stack} className="flex items-center justify-between p-3 rounded-lg hover:bg-grey-800/40 transition-colors">
                                <div className="flex items-center gap-3">
                                    <Terminal size={14} className="text-grey-500" />
                                    <div>
                                        <p className="text-sm font-mono text-grey-200">{s.stack}</p>
                                        <p className="text-xs text-grey-500">{s.resources} resources · Applied {s.lastApply}</p>
                                    </div>
                                </div>
                                <Badge variant={s.status === 'Synced' ? 'success' : 'warning'}>{s.status}</Badge>
                            </div>
                        ))}
                    </div>
                </div>
            </div>

            {/* Monitoring */}
            <div className="gs-card p-5">
                <h3 className="gs-section-title mb-4 flex items-center gap-2"><Activity size={16} /> Real-time Monitoring</h3>
                <div className="grid grid-cols-2 md:grid-cols-4 gap-4">
                    {[
                        { metric: 'Requests / sec', value: '12,400', trend: '↑ 8%', color: 'text-accent-emerald' },
                        { metric: 'Error Rate', value: '0.02%', trend: '↓ 0.01%', color: 'text-accent-emerald' },
                        { metric: 'P99 Latency', value: '142ms', trend: '↓ 18ms', color: 'text-accent-emerald' },
                        { metric: 'Active Connections', value: '4,280', trend: '↑ 12%', color: 'text-accent-emerald' },
                        { metric: 'CPU Utilization', value: '52%', trend: '→ stable', color: 'text-grey-400' },
                        { metric: 'Memory Usage', value: '68%', trend: '↑ 2%', color: 'text-accent-amber' },
                        { metric: 'Disk I/O', value: '24 MB/s', trend: '↓ 4%', color: 'text-accent-emerald' },
                        { metric: 'Network In/Out', value: '840 Mbps', trend: '↑ 6%', color: 'text-grey-400' },
                    ].map(m => (
                        <div key={m.metric} className="p-3 bg-grey-800/30 rounded-lg">
                            <p className="text-xs text-grey-500 mb-1">{m.metric}</p>
                            <p className="text-lg font-bold text-grey-100">{m.value}</p>
                            <p className={`text-xs ${m.color} mt-1`}>{m.trend}</p>
                        </div>
                    ))}
                </div>
            </div>
        </div>
    );
}
