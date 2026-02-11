import { PageHeader, StatCard, Badge, DataTable } from '../../components/ui';
import { CreditCard, FileText, Receipt, TrendingUp, Plus } from 'lucide-react';

export function FinancePayments() {
    return (
        <div>
            <PageHeader
                title="Finance & Payments"
                subtitle="Billing, invoicing, expenses & payouts"
                actions={<button className="gs-btn-primary text-sm flex items-center gap-2"><Plus size={14} /> New Invoice</button>}
            />

            <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-4 gap-4 mb-6">
                <StatCard label="MRR" value="$1.24M" change="+8.4% MoM" changeType="positive" icon={<TrendingUp size={18} />} />
                <StatCard label="Outstanding" value="$284K" change="12 invoices" changeType="neutral" icon={<FileText size={18} />} />
                <StatCard label="Expenses (MTD)" value="$186K" change="Under budget" changeType="positive" icon={<Receipt size={18} />} />
                <StatCard label="Processing" value="$42K" change="4 payouts" changeType="neutral" icon={<CreditCard size={18} />} />
            </div>

            <div className="grid grid-cols-1 lg:grid-cols-2 gap-6 mb-6">
                <div className="gs-card p-5">
                    <h3 className="gs-section-title mb-4">Recent Invoices</h3>
                    <div className="space-y-2">
                        {[
                            { id: 'INV-2026-0284', client: 'Acme Corporation', amount: '$48,000', due: 'Feb 28', status: 'Sent' },
                            { id: 'INV-2026-0283', client: 'GlobalTech Inc.', amount: '$36,000', due: 'Feb 25', status: 'Sent' },
                            { id: 'INV-2026-0282', client: 'Pinnacle Group', amount: '$24,000', due: 'Feb 20', status: 'Overdue' },
                            { id: 'INV-2026-0281', client: 'Vertex Labs', amount: '$18,000', due: 'Feb 15', status: 'Paid' },
                            { id: 'INV-2026-0280', client: 'Nova Systems', amount: '$12,000', due: 'Feb 10', status: 'Paid' },
                        ].map(inv => (
                            <div key={inv.id} className="flex items-center justify-between p-3 rounded-lg hover:bg-grey-800/40 transition-colors cursor-pointer">
                                <div>
                                    <p className="text-sm font-medium text-grey-200">{inv.client}</p>
                                    <p className="text-xs text-grey-500">{inv.id} · Due {inv.due}</p>
                                </div>
                                <div className="flex items-center gap-3">
                                    <span className="text-sm font-semibold text-grey-200">{inv.amount}</span>
                                    <Badge variant={inv.status === 'Paid' ? 'success' : inv.status === 'Overdue' ? 'danger' : 'info'}>{inv.status}</Badge>
                                </div>
                            </div>
                        ))}
                    </div>
                </div>

                <div className="gs-card p-5">
                    <h3 className="gs-section-title mb-4">Pending Expenses</h3>
                    <div className="space-y-2">
                        {[
                            { desc: 'Team Dinner — Engineering', by: 'Sarah Chen', amount: '$420', category: 'Meals', date: 'Feb 9' },
                            { desc: 'AWS Infrastructure', by: 'Auto', amount: '$12,800', category: 'Cloud', date: 'Feb 8' },
                            { desc: 'Conference Tickets — React Summit', by: 'Marcus Johnson', amount: '$2,400', category: 'Events', date: 'Feb 7' },
                            { desc: 'Office Supplies', by: 'Amanda Foster', amount: '$340', category: 'Office', date: 'Feb 6' },
                            { desc: 'Travel — Client Visit', by: 'David Kim', amount: '$1,840', category: 'Travel', date: 'Feb 5' },
                        ].map(exp => (
                            <div key={exp.desc} className="flex items-center justify-between p-3 rounded-lg hover:bg-grey-800/40 transition-colors">
                                <div>
                                    <p className="text-sm font-medium text-grey-200">{exp.desc}</p>
                                    <p className="text-xs text-grey-500">{exp.by} · {exp.date}</p>
                                </div>
                                <div className="flex items-center gap-3">
                                    <Badge>{exp.category}</Badge>
                                    <span className="text-sm font-medium text-grey-200">{exp.amount}</span>
                                    <button className="gs-btn-primary text-xs px-2 py-1">Approve</button>
                                </div>
                            </div>
                        ))}
                    </div>
                </div>
            </div>

            <div className="gs-card p-5">
                <h3 className="gs-section-title mb-4">Payout History</h3>
                <DataTable
                    columns={['Date', 'Description', 'Method', 'Amount', 'Status']}
                    rows={[
                        ['Feb 10', 'Vendor Payout — TechParts Inc.', 'Wire Transfer', '$34,200', <Badge variant="info">Processing</Badge>],
                        ['Feb 8', 'Contractor Payments', 'ACH Batch', '$18,400', <Badge variant="success">Completed</Badge>],
                        ['Feb 5', 'Refund — Nova Systems', 'Credit Card', '$2,400', <Badge variant="success">Completed</Badge>],
                        ['Feb 1', 'Monthly Subscriptions', 'Auto-Pay', '$8,200', <Badge variant="success">Completed</Badge>],
                    ]}
                />
            </div>
        </div>
    );
}
