import { StatCard, PageHeader, MiniSparkline, Badge } from '../../components/ui';
import {
    DollarSign, TrendingUp, TrendingDown, FileText, CreditCard,
    ArrowUpRight, PieChart, Receipt
} from 'lucide-react';
import { useState } from 'react';

export function ERPFinance() {
    const [period] = useState('Q1 2026');

    return (
        <div>
            <PageHeader
                title="Finance"
                subtitle="General Ledger, Accounts, Cash Flow & Budgets"
                actions={
                    <div className="flex gap-2">
                        <select className="gs-input text-sm">
                            <option>Q1 2026</option><option>Q4 2025</option><option>Q3 2025</option>
                        </select>
                        <button className="gs-btn-primary text-sm">+ New Entry</button>
                    </div>
                }
            />

            <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-4 gap-4 mb-6">
                <StatCard label="Total Revenue" value="$14.8M" change="+12.3% vs LQ" changeType="positive" icon={<DollarSign size={18} />} />
                <StatCard label="Net Income" value="$3.2M" change="+8.7% vs LQ" changeType="positive" icon={<TrendingUp size={18} />} />
                <StatCard label="Operating Expenses" value="$9.1M" change="+3.2% vs LQ" changeType="negative" icon={<TrendingDown size={18} />} />
                <StatCard label="Cash on Hand" value="$22.4M" change="-1.1% vs LQ" changeType="neutral" icon={<CreditCard size={18} />} />
            </div>

            <div className="grid grid-cols-1 lg:grid-cols-3 gap-6 mb-6">
                {/* Revenue Trend */}
                <div className="gs-card p-5 lg:col-span-2">
                    <div className="flex items-center justify-between mb-4">
                        <h3 className="gs-section-title">Revenue Trend ({period})</h3>
                        <Badge variant="success">On Track</Badge>
                    </div>
                    <MiniSparkline data={[3200, 3800, 4100, 3900, 4600, 5100, 4900, 5300, 5800, 6200, 5900, 6400]} height={120} />
                    <div className="flex gap-6 mt-4 text-sm">
                        <div><span className="text-grey-500">Avg Monthly</span> <span className="font-semibold text-grey-200 ml-1">$4.9M</span></div>
                        <div><span className="text-grey-500">Peak</span> <span className="font-semibold text-grey-200 ml-1">$6.4M</span></div>
                        <div><span className="text-grey-500">YoY Growth</span> <span className="font-semibold text-accent-emerald ml-1">+18.4%</span></div>
                    </div>
                </div>

                {/* Budget vs Actual */}
                <div className="gs-card p-5">
                    <h3 className="gs-section-title mb-4">Budget Allocation</h3>
                    <div className="space-y-4">
                        {[
                            { dept: 'Engineering', budget: 4200, spent: 3800, pct: 90 },
                            { dept: 'Marketing', budget: 2100, spent: 1900, pct: 88 },
                            { dept: 'Sales', budget: 1800, spent: 1650, pct: 92 },
                            { dept: 'Operations', budget: 1500, spent: 1200, pct: 80 },
                            { dept: 'HR', budget: 800, spent: 720, pct: 90 },
                        ].map(item => (
                            <div key={item.dept}>
                                <div className="flex justify-between text-sm mb-1">
                                    <span className="text-grey-300">{item.dept}</span>
                                    <span className="text-grey-500">${(item.spent / 1000).toFixed(1)}K / ${(item.budget / 1000).toFixed(1)}K</span>
                                </div>
                                <div className="w-full bg-grey-800 rounded-full h-1.5">
                                    <div
                                        className={`h-1.5 rounded-full ${item.pct > 90 ? 'bg-accent-amber' : 'bg-brand-500'}`}
                                        style={{ width: `${item.pct}%` }}
                                    />
                                </div>
                            </div>
                        ))}
                    </div>
                </div>
            </div>

            {/* Recent Transactions */}
            <div className="gs-card p-5">
                <div className="flex items-center justify-between mb-4">
                    <h3 className="gs-section-title">Recent Transactions</h3>
                    <button className="gs-btn-ghost text-sm flex items-center gap-1">View All <ArrowUpRight size={14} /></button>
                </div>
                <div className="overflow-x-auto">
                    <table className="gs-table w-full">
                        <thead>
                            <tr>
                                <th>Date</th><th>Description</th><th>Category</th><th>Amount</th><th>Status</th>
                            </tr>
                        </thead>
                        <tbody>
                            {[
                                { date: 'Feb 10, 2026', desc: 'AWS Infrastructure', cat: 'Operations', amount: '-$48,200', status: 'Completed' },
                                { date: 'Feb 9, 2026', desc: 'Client Payment — Acme Corp', cat: 'Revenue', amount: '+$125,000', status: 'Completed' },
                                { date: 'Feb 8, 2026', desc: 'Office Lease Q1', cat: 'Facilities', amount: '-$32,500', status: 'Pending' },
                                { date: 'Feb 7, 2026', desc: 'Software Licenses', cat: 'IT', amount: '-$15,800', status: 'Completed' },
                                { date: 'Feb 6, 2026', desc: 'Consulting Revenue — Beta Inc', cat: 'Revenue', amount: '+$87,500', status: 'Completed' },
                            ].map((tx, i) => (
                                <tr key={i}>
                                    <td className="text-grey-400">{tx.date}</td>
                                    <td className="font-medium text-grey-200">{tx.desc}</td>
                                    <td><Badge>{tx.cat}</Badge></td>
                                    <td className={tx.amount.startsWith('+') ? 'text-accent-emerald font-medium' : 'text-grey-300'}>{tx.amount}</td>
                                    <td><Badge variant={tx.status === 'Completed' ? 'success' : 'warning'}>{tx.status}</Badge></td>
                                </tr>
                            ))}
                        </tbody>
                    </table>
                </div>
            </div>
        </div>
    );
}
