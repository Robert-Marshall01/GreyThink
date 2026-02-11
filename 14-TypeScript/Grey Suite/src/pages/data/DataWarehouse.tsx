import { PageHeader, StatCard, Badge, DataTable } from '../../components/ui';
import { Database, HardDrive, GitBranch, Clock, Plus } from 'lucide-react';

export function DataWarehouse() {
    return (
        <div>
            <PageHeader
                title="Data Warehouse"
                subtitle="Schemas, pipelines, transformations & data catalog"
                actions={<button className="gs-btn-primary text-sm flex items-center gap-2"><Plus size={14} /> New Pipeline</button>}
            />

            <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-4 gap-4 mb-6">
                <StatCard label="Total Tables" value={284} icon={<Database size={18} />} />
                <StatCard label="Storage Used" value="4.2 TB" change="+180 GB MoM" changeType="neutral" icon={<HardDrive size={18} />} />
                <StatCard label="Active Pipelines" value={42} change="All healthy" changeType="positive" icon={<GitBranch size={18} />} />
                <StatCard label="Last Refresh" value="8m ago" change="Next in 22m" changeType="neutral" icon={<Clock size={18} />} />
            </div>

            <div className="grid grid-cols-1 lg:grid-cols-2 gap-6 mb-6">
                <div className="gs-card p-5">
                    <h3 className="gs-section-title mb-4">Schemas</h3>
                    <div className="space-y-2">
                        {[
                            { name: 'raw', tables: 86, size: '1.8 TB', status: 'Healthy' },
                            { name: 'staging', tables: 64, size: '920 GB', status: 'Healthy' },
                            { name: 'analytics', tables: 48, size: '680 GB', status: 'Healthy' },
                            { name: 'ml_features', tables: 32, size: '420 GB', status: 'Healthy' },
                            { name: 'reporting', tables: 54, size: '380 GB', status: 'Warning' },
                        ].map(s => (
                            <div key={s.name} className="flex items-center justify-between p-3 rounded-lg hover:bg-grey-800/40 transition-colors cursor-pointer">
                                <div className="flex items-center gap-3">
                                    <Database size={16} className="text-grey-500" />
                                    <div>
                                        <p className="text-sm font-mono font-medium text-grey-200">{s.name}</p>
                                        <p className="text-xs text-grey-500">{s.tables} tables · {s.size}</p>
                                    </div>
                                </div>
                                <Badge variant={s.status === 'Healthy' ? 'success' : 'warning'}>{s.status}</Badge>
                            </div>
                        ))}
                    </div>
                </div>

                <div className="gs-card p-5">
                    <h3 className="gs-section-title mb-4">Pipeline Status</h3>
                    <div className="space-y-2">
                        {[
                            { name: 'CRM Sync', schedule: 'Every 30m', lastRun: '8m ago', duration: '2m 14s', status: 'Success' },
                            { name: 'ERP Extract', schedule: 'Hourly', lastRun: '22m ago', duration: '8m 42s', status: 'Success' },
                            { name: 'Web Analytics', schedule: 'Every 15m', lastRun: '3m ago', duration: '1m 08s', status: 'Success' },
                            { name: 'ML Feature Build', schedule: 'Daily 2 AM', lastRun: '8h ago', duration: '42m 16s', status: 'Success' },
                            { name: 'Financial Close', schedule: 'Monthly', lastRun: '10d ago', duration: '1h 24m', status: 'Queued' },
                        ].map(p => (
                            <div key={p.name} className="flex items-center justify-between p-3 rounded-lg hover:bg-grey-800/40 transition-colors">
                                <div>
                                    <p className="text-sm font-medium text-grey-200">{p.name}</p>
                                    <p className="text-xs text-grey-500">{p.schedule} · Last: {p.lastRun} · Duration: {p.duration}</p>
                                </div>
                                <Badge variant={p.status === 'Success' ? 'success' : 'info'}>{p.status}</Badge>
                            </div>
                        ))}
                    </div>
                </div>
            </div>

            <div className="gs-card p-5">
                <h3 className="gs-section-title mb-4">Data Catalog — Popular Tables</h3>
                <DataTable
                    columns={['Table', 'Schema', 'Rows', 'Size', 'Last Updated', 'Queries/Day']}
                    rows={[
                        [<span className="font-mono text-brand-400">fact_orders</span>, 'analytics', '18.4M', '2.1 GB', '8m ago', '1,284'],
                        [<span className="font-mono text-brand-400">dim_customers</span>, 'analytics', '1.2M', '340 MB', '8m ago', '986'],
                        [<span className="font-mono text-brand-400">fact_events</span>, 'raw', '142M', '48 GB', '3m ago', '642'],
                        [<span className="font-mono text-brand-400">agg_daily_revenue</span>, 'reporting', '4.2K', '12 MB', '1h ago', '524'],
                        [<span className="font-mono text-brand-400">ml_user_features</span>, 'ml_features', '1.2M', '1.8 GB', '8h ago', '312'],
                    ]}
                />
            </div>
        </div>
    );
}
