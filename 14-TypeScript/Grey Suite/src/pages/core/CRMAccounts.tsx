import { PageHeader, StatCard, Badge, DataTable } from '../../components/ui';
import { Building2, TrendingUp, Star, Globe } from 'lucide-react';

export function CRMAccounts() {
    return (
        <div>
            <PageHeader
                title="Accounts"
                subtitle="Customer & partner account management"
                actions={<button className="gs-btn-primary text-sm">+ New Account</button>}
            />

            <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-4 gap-4 mb-6">
                <StatCard label="Total Accounts" value={1284} icon={<Building2 size={18} />} />
                <StatCard label="Active This Month" value={342} change="+24" changeType="positive" icon={<TrendingUp size={18} />} />
                <StatCard label="Enterprise Tier" value={86} icon={<Star size={18} />} />
                <StatCard label="Regions" value={12} icon={<Globe size={18} />} />
            </div>

            <div className="grid grid-cols-1 lg:grid-cols-3 gap-6 mb-6">
                <div className="lg:col-span-2 gs-card p-5">
                    <h3 className="gs-section-title mb-4">Top Accounts by Revenue</h3>
                    <DataTable
                        columns={['Account', 'Industry', 'ARR', 'Health', 'Owner']}
                        rows={[
                            [<span className="font-medium text-grey-200">Acme Corporation</span>, 'Technology', '$2.4M', <Badge variant="success">Healthy</Badge>, 'J. Smith'],
                            [<span className="font-medium text-grey-200">GlobalTech Inc.</span>, 'Finance', '$1.8M', <Badge variant="success">Healthy</Badge>, 'M. Chen'],
                            [<span className="font-medium text-grey-200">Pinnacle Group</span>, 'Manufacturing', '$1.2M', <Badge variant="warning">At Risk</Badge>, 'S. Park'],
                            [<span className="font-medium text-grey-200">Vertex Labs</span>, 'Healthcare', '$980K', <Badge variant="success">Healthy</Badge>, 'R. Patel'],
                            [<span className="font-medium text-grey-200">Nova Systems</span>, 'Retail', '$870K', <Badge variant="danger">Critical</Badge>, 'A. Garcia'],
                        ]}
                    />
                </div>
                <div className="gs-card p-5">
                    <h3 className="gs-section-title mb-4">By Industry</h3>
                    <div className="space-y-3">
                        {[
                            { industry: 'Technology', count: 412, pct: 32 },
                            { industry: 'Finance', count: 284, pct: 22 },
                            { industry: 'Healthcare', count: 198, pct: 15 },
                            { industry: 'Manufacturing', count: 156, pct: 12 },
                            { industry: 'Retail', count: 134, pct: 11 },
                            { industry: 'Other', count: 100, pct: 8 },
                        ].map(ind => (
                            <div key={ind.industry} className="flex items-center gap-3">
                                <div className="flex-1">
                                    <div className="flex justify-between text-sm mb-1">
                                        <span className="text-grey-300">{ind.industry}</span>
                                        <span className="text-grey-500">{ind.count}</span>
                                    </div>
                                    <div className="w-full bg-grey-800 rounded-full h-1.5">
                                        <div className="h-1.5 rounded-full bg-brand-500" style={{ width: `${ind.pct}%` }} />
                                    </div>
                                </div>
                            </div>
                        ))}
                    </div>
                </div>
            </div>
        </div>
    );
}
