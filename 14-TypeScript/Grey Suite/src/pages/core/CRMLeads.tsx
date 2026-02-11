import { PageHeader, StatCard, Badge, DataTable } from '../../components/ui';
import { Users, UserPlus, Target, Clock } from 'lucide-react';
import { useState } from 'react';

export function CRMLeads() {
    const [view, setView] = useState<'list' | 'kanban'>('list');

    const leads = [
        { name: 'Sarah Chen', company: 'TechFlow Inc.', value: '$85,000', source: 'Inbound', score: 92, status: 'Hot' },
        { name: 'Marcus Johnson', company: 'DataPrime', value: '$120,000', source: 'Outbound', score: 87, status: 'Hot' },
        { name: 'Emily Rodriguez', company: 'CloudScale', value: '$45,000', source: 'Referral', score: 78, status: 'Warm' },
        { name: 'David Kim', company: 'NexGen Solutions', value: '$200,000', source: 'Event', score: 65, status: 'Warm' },
        { name: 'Amanda Foster', company: 'BrightPath Co.', value: '$35,000', source: 'Inbound', score: 54, status: 'Cold' },
        { name: 'James Wright', company: 'Apex Dynamics', value: '$150,000', source: 'Outbound', score: 71, status: 'Warm' },
    ];

    return (
        <div>
            <PageHeader
                title="Leads"
                subtitle="Pipeline management & lead qualification"
                actions={
                    <div className="flex gap-2">
                        <div className="flex bg-grey-900 border border-grey-800 rounded-lg overflow-hidden">
                            <button onClick={() => setView('list')} className={`px-3 py-1.5 text-sm ${view === 'list' ? 'bg-grey-700 text-grey-100' : 'text-grey-500'}`}>List</button>
                            <button onClick={() => setView('kanban')} className={`px-3 py-1.5 text-sm ${view === 'kanban' ? 'bg-grey-700 text-grey-100' : 'text-grey-500'}`}>Kanban</button>
                        </div>
                        <button className="gs-btn-primary text-sm">+ Add Lead</button>
                    </div>
                }
            />

            <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-4 gap-4 mb-6">
                <StatCard label="Total Leads" value={248} change="+18 this week" changeType="positive" icon={<Users size={18} />} />
                <StatCard label="New Today" value={7} change="+40% vs avg" changeType="positive" icon={<UserPlus size={18} />} />
                <StatCard label="Conversion Rate" value="23.4%" change="+2.1%" changeType="positive" icon={<Target size={18} />} />
                <StatCard label="Avg Response Time" value="2.4h" change="-0.8h improvement" changeType="positive" icon={<Clock size={18} />} />
            </div>

            {view === 'kanban' ? (
                <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
                    {['Hot', 'Warm', 'Cold'].map(status => (
                        <div key={status} className="gs-card p-4">
                            <div className="flex items-center justify-between mb-3">
                                <h3 className="text-sm font-semibold text-grey-300">{status}</h3>
                                <Badge variant={status === 'Hot' ? 'danger' : status === 'Warm' ? 'warning' : 'default'}>
                                    {leads.filter(l => l.status === status).length}
                                </Badge>
                            </div>
                            <div className="space-y-2">
                                {leads.filter(l => l.status === status).map(lead => (
                                    <div key={lead.name} className="p-3 bg-grey-800/50 rounded-lg border border-grey-800 hover:border-grey-700 cursor-pointer transition-colors">
                                        <p className="text-sm font-medium text-grey-200">{lead.name}</p>
                                        <p className="text-xs text-grey-500">{lead.company}</p>
                                        <div className="flex justify-between mt-2">
                                            <span className="text-xs text-grey-400">{lead.value}</span>
                                            <span className="text-xs text-brand-400">Score: {lead.score}</span>
                                        </div>
                                    </div>
                                ))}
                            </div>
                        </div>
                    ))}
                </div>
            ) : (
                <div className="gs-card p-5">
                    <DataTable
                        columns={['Name', 'Company', 'Value', 'Source', 'Score', 'Status']}
                        rows={leads.map(l => [
                            <span className="font-medium text-grey-200">{l.name}</span>,
                            l.company,
                            l.value,
                            <Badge variant="info">{l.source}</Badge>,
                            <span className={l.score >= 80 ? 'text-accent-emerald' : l.score >= 60 ? 'text-accent-amber' : 'text-grey-400'}>{l.score}</span>,
                            <Badge variant={l.status === 'Hot' ? 'danger' : l.status === 'Warm' ? 'warning' : 'default'}>{l.status}</Badge>,
                        ])}
                    />
                </div>
            )}
        </div>
    );
}
