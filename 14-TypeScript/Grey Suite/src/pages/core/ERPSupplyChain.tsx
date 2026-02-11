import { PageHeader, StatCard, Badge, DataTable, MiniSparkline } from '../../components/ui';
import { Truck, Package, MapPin, AlertTriangle } from 'lucide-react';

export function ERPSupplyChain() {
    return (
        <div>
            <PageHeader
                title="Supply Chain"
                subtitle="Inventory, logistics, warehousing & fulfillment"
                actions={<button className="gs-btn-primary text-sm">+ New Shipment</button>}
            />

            <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-4 gap-4 mb-6">
                <StatCard label="Active Shipments" value={89} change="12 arriving today" changeType="positive" icon={<Truck size={18} />} />
                <StatCard label="Inventory Value" value="$8.4M" change="+3.2% MoM" changeType="positive" icon={<Package size={18} />} />
                <StatCard label="Warehouse Utilization" value="78%" change="3 locations" changeType="neutral" icon={<MapPin size={18} />} />
                <StatCard label="Delayed Shipments" value={4} change="Action required" changeType="negative" icon={<AlertTriangle size={18} />} />
            </div>

            <div className="grid grid-cols-1 lg:grid-cols-3 gap-6 mb-6">
                <div className="gs-card p-5 lg:col-span-2">
                    <h3 className="gs-section-title mb-4">Fulfillment Rate (30 Days)</h3>
                    <MiniSparkline data={[92, 94, 91, 96, 93, 95, 97, 94, 96, 98, 95, 97, 96, 98, 97]} height={100} color="#10b981" />
                    <p className="text-sm text-grey-500 mt-2">Average: 95.2% · Target: 96%</p>
                </div>
                <div className="gs-card p-5">
                    <h3 className="gs-section-title mb-4">Warehouse Status</h3>
                    <div className="space-y-4">
                        {[
                            { name: 'US East (NJ)', capacity: 82, items: '14,200' },
                            { name: 'US West (CA)', capacity: 71, items: '11,800' },
                            { name: 'EU Central (DE)', capacity: 68, items: '9,400' },
                        ].map(w => (
                            <div key={w.name}>
                                <div className="flex justify-between text-sm mb-1">
                                    <span className="text-grey-300">{w.name}</span>
                                    <span className="text-grey-500">{w.capacity}%</span>
                                </div>
                                <div className="w-full bg-grey-800 rounded-full h-2">
                                    <div className={`h-2 rounded-full ${w.capacity > 80 ? 'bg-accent-amber' : 'bg-accent-emerald'}`} style={{ width: `${w.capacity}%` }} />
                                </div>
                                <p className="text-xs text-grey-600 mt-1">{w.items} items</p>
                            </div>
                        ))}
                    </div>
                </div>
            </div>

            <div className="gs-card p-5">
                <h3 className="gs-section-title mb-4">Active Shipments</h3>
                <DataTable
                    columns={['Tracking #', 'Route', 'Carrier', 'Items', 'Status', 'ETA']}
                    rows={[
                        ['SHP-88412', 'Shanghai → Newark', 'Maersk', '240 units', <Badge variant="info">In Transit</Badge>, 'Feb 14'],
                        ['SHP-88410', 'Munich → Newark', 'DHL Express', '48 units', <Badge variant="warning">Customs</Badge>, 'Feb 12'],
                        ['SHP-88409', 'Los Angeles → Austin', 'FedEx', '120 units', <Badge variant="success">Out for Delivery</Badge>, 'Feb 10'],
                        ['SHP-88408', 'Newark → London', 'UPS', '86 units', <Badge variant="info">In Transit</Badge>, 'Feb 16'],
                    ]}
                />
            </div>
        </div>
    );
}
