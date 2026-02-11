import { PageHeader, StatCard, Badge, DataTable } from '../../components/ui';
import { DollarSign, Users, Calendar, FileCheck } from 'lucide-react';

export function HCMPayroll() {
    return (
        <div>
            <PageHeader
                title="Payroll"
                subtitle="Compensation, benefits & tax management"
                actions={
                    <div className="flex gap-2">
                        <button className="gs-btn-secondary text-sm">Run Payroll</button>
                        <button className="gs-btn-primary text-sm">+ Add Employee</button>
                    </div>
                }
            />

            <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-4 gap-4 mb-6">
                <StatCard label="Monthly Payroll" value="$1.84M" change="342 employees" changeType="neutral" icon={<DollarSign size={18} />} />
                <StatCard label="Active Employees" value={342} change="+8 this month" changeType="positive" icon={<Users size={18} />} />
                <StatCard label="Next Pay Date" value="Feb 15" change="In 5 days" changeType="neutral" icon={<Calendar size={18} />} />
                <StatCard label="Tax Compliance" value="100%" change="All filings current" changeType="positive" icon={<FileCheck size={18} />} />
            </div>

            <div className="grid grid-cols-1 lg:grid-cols-2 gap-6 mb-6">
                <div className="gs-card p-5">
                    <h3 className="gs-section-title mb-4">Payroll by Department</h3>
                    <div className="space-y-3">
                        {[
                            { dept: 'Engineering', amount: '$680K', headcount: 98, pct: 37 },
                            { dept: 'Sales', amount: '$320K', headcount: 64, pct: 17 },
                            { dept: 'Marketing', amount: '$240K', headcount: 42, pct: 13 },
                            { dept: 'Operations', amount: '$210K', headcount: 52, pct: 11 },
                            { dept: 'Product', amount: '$180K', headcount: 34, pct: 10 },
                            { dept: 'Other', amount: '$210K', headcount: 52, pct: 12 },
                        ].map(d => (
                            <div key={d.dept} className="flex items-center gap-4">
                                <span className="text-sm text-grey-300 w-24">{d.dept}</span>
                                <div className="flex-1">
                                    <div className="w-full bg-grey-800 rounded-full h-2">
                                        <div className="h-2 rounded-full bg-brand-500" style={{ width: `${d.pct}%` }} />
                                    </div>
                                </div>
                                <span className="text-sm font-medium text-grey-200 w-16 text-right">{d.amount}</span>
                                <span className="text-xs text-grey-500 w-12 text-right">{d.headcount} ppl</span>
                            </div>
                        ))}
                    </div>
                </div>

                <div className="gs-card p-5">
                    <h3 className="gs-section-title mb-4">Benefits Summary</h3>
                    <div className="grid grid-cols-2 gap-3">
                        {[
                            { benefit: 'Health Insurance', enrolled: '96%', cost: '$182K/mo' },
                            { benefit: '401(k) Match', enrolled: '84%', cost: '$92K/mo' },
                            { benefit: 'Dental & Vision', enrolled: '89%', cost: '$34K/mo' },
                            { benefit: 'Life Insurance', enrolled: '72%', cost: '$18K/mo' },
                            { benefit: 'HSA/FSA', enrolled: '61%', cost: '$24K/mo' },
                            { benefit: 'Commuter Benefits', enrolled: '38%', cost: '$12K/mo' },
                        ].map(b => (
                            <div key={b.benefit} className="p-3 bg-grey-800/40 rounded-lg">
                                <p className="text-xs text-grey-500">{b.benefit}</p>
                                <p className="text-sm font-semibold text-grey-200 mt-1">{b.enrolled} enrolled</p>
                                <p className="text-xs text-grey-400">{b.cost}</p>
                            </div>
                        ))}
                    </div>
                </div>
            </div>

            <div className="gs-card p-5">
                <h3 className="gs-section-title mb-4">Recent Payroll History</h3>
                <DataTable
                    columns={['Pay Period', 'Gross Pay', 'Deductions', 'Net Pay', 'Employees', 'Status']}
                    rows={[
                        ['Jan 16–31, 2026', '$920K', '$184K', '$736K', '342', <Badge variant="success">Processed</Badge>],
                        ['Jan 1–15, 2026', '$918K', '$182K', '$736K', '340', <Badge variant="success">Processed</Badge>],
                        ['Dec 16–31, 2025', '$945K', '$189K', '$756K', '338', <Badge variant="success">Processed</Badge>],
                        ['Dec 1–15, 2025', '$942K', '$188K', '$754K', '336', <Badge variant="success">Processed</Badge>],
                    ]}
                />
            </div>
        </div>
    );
}
