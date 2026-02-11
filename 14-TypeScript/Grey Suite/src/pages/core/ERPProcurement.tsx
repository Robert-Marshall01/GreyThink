import { PageHeader, StatCard, Badge, DataTable } from '../../components/ui';
import { ShoppingCart, Clock, CheckCircle, AlertTriangle } from 'lucide-react';

export function ERPProcurement() {
    return (
        <div>
            <PageHeader
                title="Procurement"
                subtitle="Purchase orders, vendor management & approvals"
                actions={<button className="gs-btn-primary text-sm">+ New PO</button>}
            />

            <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-4 gap-4 mb-6">
                <StatCard label="Open POs" value={42} change="8 awaiting approval" changeType="neutral" icon={<ShoppingCart size={18} />} />
                <StatCard label="Avg Lead Time" value="12 days" change="-2 days vs avg" changeType="positive" icon={<Clock size={18} />} />
                <StatCard label="Fulfilled This Month" value={127} change="+15% vs last month" changeType="positive" icon={<CheckCircle size={18} />} />
                <StatCard label="Flagged Items" value={3} change="Requires attention" changeType="negative" icon={<AlertTriangle size={18} />} />
            </div>

            <div className="grid grid-cols-1 lg:grid-cols-2 gap-6 mb-6">
                <div className="gs-card p-5">
                    <h3 className="gs-section-title mb-4">Top Vendors by Spend</h3>
                    <div className="space-y-3">
                        {[
                            { name: 'TechParts Inc.', spend: '$284K', reliability: 98 },
                            { name: 'Global Supply Co.', spend: '$192K', reliability: 95 },
                            { name: 'FastShip Logistics', spend: '$156K', reliability: 91 },
                            { name: 'Premium Materials', spend: '$134K', reliability: 97 },
                            { name: 'Digital Solutions', spend: '$98K', reliability: 89 },
                        ].map(v => (
                            <div key={v.name} className="flex items-center justify-between p-3 rounded-lg hover:bg-grey-800/40 transition-colors">
                                <div>
                                    <p className="text-sm font-medium text-grey-200">{v.name}</p>
                                    <p className="text-xs text-grey-500">Reliability: {v.reliability}%</p>
                                </div>
                                <span className="text-sm font-semibold text-grey-300">{v.spend}</span>
                            </div>
                        ))}
                    </div>
                </div>

                <div className="gs-card p-5">
                    <h3 className="gs-section-title mb-4">Pending Approvals</h3>
                    <div className="space-y-3">
                        {[
                            { po: 'PO-2026-0412', vendor: 'TechParts Inc.', amount: '$34,200', urgency: 'High' },
                            { po: 'PO-2026-0413', vendor: 'Global Supply Co.', amount: '$18,500', urgency: 'Medium' },
                            { po: 'PO-2026-0414', vendor: 'FastShip Logistics', amount: '$7,800', urgency: 'Low' },
                            { po: 'PO-2026-0415', vendor: 'Premium Materials', amount: '$52,100', urgency: 'High' },
                        ].map(item => (
                            <div key={item.po} className="flex items-center justify-between p-3 rounded-lg border border-grey-800 hover:border-grey-700 transition-colors">
                                <div>
                                    <p className="text-sm font-medium text-grey-200">{item.po}</p>
                                    <p className="text-xs text-grey-500">{item.vendor} · {item.amount}</p>
                                </div>
                                <div className="flex items-center gap-2">
                                    <Badge variant={item.urgency === 'High' ? 'danger' : item.urgency === 'Medium' ? 'warning' : 'default'}>{item.urgency}</Badge>
                                    <button className="gs-btn-primary text-xs px-3 py-1">Approve</button>
                                </div>
                            </div>
                        ))}
                    </div>
                </div>
            </div>

            <div className="gs-card p-5">
                <h3 className="gs-section-title mb-4">Recent Purchase Orders</h3>
                <DataTable
                    columns={['PO #', 'Vendor', 'Items', 'Total', 'Status', 'ETA']}
                    rows={[
                        ['PO-2026-0410', 'TechParts Inc.', '24 items', '$34,200', <Badge variant="info">In Transit</Badge>, 'Feb 15'],
                        ['PO-2026-0409', 'FastShip Logistics', '8 items', '$12,400', <Badge variant="success">Delivered</Badge>, 'Feb 8'],
                        ['PO-2026-0408', 'Digital Solutions', '3 items', '$8,900', <Badge variant="success">Delivered</Badge>, 'Feb 6'],
                        ['PO-2026-0407', 'Global Supply Co.', '56 items', '$92,100', <Badge variant="warning">Partial</Badge>, 'Feb 12'],
                        ['PO-2026-0406', 'Premium Materials', '12 items', '$21,300', <Badge variant="success">Delivered</Badge>, 'Feb 4'],
                    ]}
                />
            </div>
        </div>
    );
}
