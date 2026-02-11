import { useState } from 'react';
import {
    Blocks, Layout, Database, Layers, Settings, Play, Upload,
    Users, Shield, Eye, Code2, Paintbrush, Smartphone, Globe,
    Package, GitBranch, CheckCircle2, AlertTriangle, Zap, Plus,
    Search, Filter, ArrowRight, RefreshCw
} from 'lucide-react';
import { PageHeader, StatCard, DataTable, Badge, Tabs, ProgressBar } from '../../components/ui';

export function LowCodeBuilder() {
    const [activeTab, setActiveTab] = useState('App Builder');

    const tabs = ['App Builder', 'Data Connectors', 'Component Library', 'Publishing', 'Lifecycle'];

    return (
        <div className="p-6 space-y-6">
            <PageHeader
                title="Low-Code Application Builder"
                subtitle="Visual app builder, data connectors, component library, role-based publishing, and app lifecycle management"
                actions={
                    <div className="flex gap-2">
                        <button className="gs-btn-secondary">
                            <Upload size={14} /> Import App
                        </button>
                        <button className="gs-btn-primary">
                            <Plus size={14} /> New App
                        </button>
                    </div>
                }
            />

            <div className="grid grid-cols-4 gap-4">
                <StatCard label="Published Apps" value="142" change="+18 this month" changeType="positive" icon={<Blocks size={18} />} />
                <StatCard label="Active Users" value="4,821" change="+12%" changeType="positive" icon={<Users size={18} />} />
                <StatCard label="Data Sources" value="48" change="+4" changeType="positive" icon={<Database size={18} />} />
                <StatCard label="Citizen Developers" value="284" change="+42" changeType="positive" icon={<Code2 size={18} />} />
            </div>

            <Tabs tabs={tabs} active={activeTab} onChange={setActiveTab} />

            {activeTab === 'App Builder' && (
                <div className="space-y-6">
                    <div className="gs-card p-6">
                        <div className="flex items-center justify-between mb-4">
                            <div>
                                <h3 className="text-sm font-semibold text-grey-300">Visual App Designer</h3>
                                <p className="text-xs text-grey-500">Expense Approval App — v2.3</p>
                            </div>
                            <div className="flex gap-2">
                                <button className="gs-btn-ghost text-xs"><Smartphone size={12} /> Mobile</button>
                                <button className="gs-btn-ghost text-xs bg-grey-700"><Layout size={12} /> Desktop</button>
                                <button className="gs-btn-ghost text-xs"><Eye size={12} /> Preview</button>
                            </div>
                        </div>

                        <div className="bg-grey-900 rounded-lg border border-grey-700/50 overflow-hidden">
                            <div className="flex">
                                {/* Component Palette */}
                                <div className="w-48 border-r border-grey-700/50 p-3">
                                    <p className="text-[10px] font-semibold text-grey-500 uppercase mb-2">Components</p>
                                    <div className="space-y-1">
                                        {[
                                            { name: 'Text Input', icon: '📝' },
                                            { name: 'Dropdown', icon: '📋' },
                                            { name: 'Date Picker', icon: '📅' },
                                            { name: 'File Upload', icon: '📎' },
                                            { name: 'Data Table', icon: '📊' },
                                            { name: 'Button', icon: '🔘' },
                                            { name: 'Gallery', icon: '🖼️' },
                                            { name: 'Form', icon: '📄' },
                                            { name: 'Chart', icon: '📈' },
                                            { name: 'Container', icon: '📦' },
                                            { name: 'Navigation', icon: '🧭' },
                                            { name: 'Camera', icon: '📷' },
                                        ].map(c => (
                                            <div key={c.name} className="flex items-center gap-2 px-2 py-1.5 rounded text-xs text-grey-400 hover:bg-grey-800/50 cursor-grab">
                                                <span>{c.icon}</span>
                                                <span>{c.name}</span>
                                            </div>
                                        ))}
                                    </div>
                                </div>

                                {/* Canvas */}
                                <div className="flex-1 p-6 min-h-[350px]" style={{ backgroundImage: 'radial-gradient(circle, #374151 1px, transparent 1px)', backgroundSize: '20px 20px' }}>
                                    <div className="max-w-md mx-auto space-y-4">
                                        <div className="bg-grey-800/80 rounded-lg p-4 border-2 border-dashed border-brand-500/30">
                                            <p className="text-xs text-grey-400 mb-2">Expense Title</p>
                                            <div className="h-8 bg-grey-700/50 rounded border border-grey-600" />
                                        </div>
                                        <div className="grid grid-cols-2 gap-3">
                                            <div className="bg-grey-800/80 rounded-lg p-4 border border-grey-700/50">
                                                <p className="text-xs text-grey-400 mb-2">Amount</p>
                                                <div className="h-8 bg-grey-700/50 rounded border border-grey-600" />
                                            </div>
                                            <div className="bg-grey-800/80 rounded-lg p-4 border border-grey-700/50">
                                                <p className="text-xs text-grey-400 mb-2">Category</p>
                                                <div className="h-8 bg-grey-700/50 rounded border border-grey-600 flex items-center justify-end px-2">
                                                    <ArrowRight size={10} className="text-grey-500 rotate-90" />
                                                </div>
                                            </div>
                                        </div>
                                        <div className="bg-grey-800/80 rounded-lg p-4 border border-grey-700/50">
                                            <p className="text-xs text-grey-400 mb-2">Receipt Upload</p>
                                            <div className="h-16 bg-grey-700/30 rounded border-2 border-dashed border-grey-600 flex items-center justify-center">
                                                <Upload size={16} className="text-grey-500" />
                                            </div>
                                        </div>
                                        <div className="flex gap-2 justify-end">
                                            <div className="px-4 py-2 bg-grey-700 rounded text-xs text-grey-400">Cancel</div>
                                            <div className="px-4 py-2 bg-brand-500 rounded text-xs text-white">Submit</div>
                                        </div>
                                    </div>
                                </div>

                                {/* Properties Panel */}
                                <div className="w-56 border-l border-grey-700/50 p-3">
                                    <p className="text-[10px] font-semibold text-grey-500 uppercase mb-2">Properties</p>
                                    <div className="space-y-3">
                                        {[
                                            { label: 'Data Source', value: 'Expenses_Table' },
                                            { label: 'On Submit', value: 'SubmitForm()' },
                                            { label: 'Validation', value: 'Required + Range' },
                                            { label: 'Visible', value: 'true' },
                                            { label: 'Role Access', value: 'All Employees' },
                                        ].map(p => (
                                            <div key={p.label}>
                                                <p className="text-[10px] text-grey-500 mb-1">{p.label}</p>
                                                <div className="text-xs px-2 py-1 bg-grey-800/50 rounded text-grey-300 font-mono border border-grey-700/50">{p.value}</div>
                                            </div>
                                        ))}
                                    </div>

                                    <p className="text-[10px] font-semibold text-grey-500 uppercase mt-4 mb-2">Formula Bar</p>
                                    <div className="bg-grey-800/50 rounded p-2 font-mono text-[10px] text-grey-400 border border-grey-700/50">
                                        <code>{`If(Amount.Value > 500,
  Navigate(ApprovalScreen),
  SubmitForm(ExpenseForm)
)`}</code>
                                    </div>
                                </div>
                            </div>
                        </div>
                    </div>

                    <div className="gs-card p-6">
                        <h3 className="text-sm font-semibold text-grey-300 mb-4">Expression Language</h3>
                        <div className="bg-grey-900 rounded-lg p-4 font-mono text-xs text-grey-400">
                            <pre>{`// Grey Suite Expression Language (GSEL)
// Type-safe, Excel-like formula system for citizen developers

// Data Operations
Filter(Expenses, Status = "Pending" && Amount > 100)
Sort(Employees, LastName, Ascending)
LookUp(Accounts, AccountID = ThisItem.AccountID)
GroupBy(Invoices, Department, Sum(Amount))

// Navigation & UI
Navigate(DetailScreen, ScreenTransition.Fade)
Set(varCurrentUser, User())
UpdateContext({showDialog: true})

// Integration
Office365.SendEmail(To, Subject, Body)
Salesforce.CreateRecord("Lead", {Name: Title.Text})
GreySuite.Workflow.Trigger("approval-flow", ThisItem)

// AI Functions (built-in)
AI.Classify(Description.Text, ["Travel", "Office", "Software"])
AI.Summarize(LongText.Text, 100)  // 100 word summary
AI.Extract(Receipt.Image, "amount,date,vendor")`}</pre>
                        </div>
                    </div>
                </div>
            )}

            {activeTab === 'Data Connectors' && (
                <div className="space-y-6">
                    <DataTable
                        columns={['Connector', 'Type', 'Tables/Entities', 'Connection', 'Auth', 'Delegation', 'Status']}
                        rows={[
                            ['Grey Suite Tables', <Badge variant="info">Native</Badge>, '284 tables', 'Direct', 'Inherited', <Badge variant="success">Full</Badge>, <Badge variant="success">Connected</Badge>],
                            ['SQL Server', <Badge variant="default">Database</Badge>, '142 tables', 'On-prem Gateway', 'SQL Auth', <Badge variant="success">Full</Badge>, <Badge variant="success">Connected</Badge>],
                            ['SharePoint', <Badge variant="info">Cloud</Badge>, '48 lists', 'OAuth 2.0', 'AAD', <Badge variant="warning">Partial</Badge>, <Badge variant="success">Connected</Badge>],
                            ['Salesforce', <Badge variant="purple">SaaS</Badge>, '84 objects', 'REST API', 'OAuth 2.0', <Badge variant="warning">Partial</Badge>, <Badge variant="success">Connected</Badge>],
                            ['REST API (Custom)', <Badge variant="default">Custom</Badge>, 'Defined by spec', 'HTTPS', 'API Key / OAuth', <Badge variant="default">None</Badge>, <Badge variant="success">Available</Badge>],
                            ['Excel Online', <Badge variant="info">Cloud</Badge>, 'Worksheets', 'Graph API', 'AAD', <Badge variant="warning">Partial</Badge>, <Badge variant="success">Connected</Badge>],
                            ['Dataverse', <Badge variant="info">Native</Badge>, '196 entities', 'Direct', 'Inherited', <Badge variant="success">Full</Badge>, <Badge variant="success">Connected</Badge>],
                            ['Snowflake', <Badge variant="purple">Data</Badge>, '64 views', 'JDBC', 'Key Pair', <Badge variant="success">Full</Badge>, <Badge variant="success">Connected</Badge>],
                        ]}
                    />

                    <div className="gs-card p-6">
                        <h3 className="text-sm font-semibold text-grey-300 mb-4">Data Delegation & Performance</h3>
                        <div className="bg-grey-900 rounded-lg p-4 font-mono text-xs text-grey-400">
                            <pre>{`// Delegation Strategy
// Delegated queries execute on the data source (server-side)
// Non-delegated queries download data locally (500 row limit)

// Delegable operations by connector:
// SQL Server:    Filter, Sort, Search, Aggregate ✅
// SharePoint:    Filter, Sort ✅ | Search ⚠️ (partial)
// Salesforce:    Filter, Sort ✅ | Aggregate ⚠️
// REST API:      Depends on API capabilities
// Excel Online:  None ❌ (always local)

// Best Practice: Always use delegable queries for > 500 rows
// Grey Suite Tables: Full delegation with server-side indexing
// Auto-warning when non-delegable expression detected`}</pre>
                        </div>
                    </div>
                </div>
            )}

            {activeTab === 'Component Library' && (
                <div className="space-y-6">
                    <div className="gs-card p-6">
                        <h3 className="text-sm font-semibold text-grey-300 mb-4">Reusable Component Library</h3>
                        <div className="grid grid-cols-4 gap-4">
                            {[
                                { name: 'Header Nav', category: 'Navigation', type: 'Layout', uses: 142, version: 'v2.1' },
                                { name: 'Data Card', category: 'Display', type: 'Data', uses: 284, version: 'v3.0' },
                                { name: 'Approval Button', category: 'Action', type: 'Workflow', uses: 96, version: 'v1.4' },
                                { name: 'User Picker', category: 'Input', type: 'Identity', uses: 184, version: 'v2.0' },
                                { name: 'File Viewer', category: 'Display', type: 'Document', uses: 64, version: 'v1.8' },
                                { name: 'Chart Card', category: 'Display', type: 'Data Viz', uses: 124, version: 'v2.2' },
                                { name: 'Search Bar', category: 'Input', type: 'Search', uses: 210, version: 'v1.6' },
                                { name: 'Status Badge', category: 'Display', type: 'UI', uses: 342, version: 'v1.2' },
                                { name: 'Comment Thread', category: 'Social', type: 'Collaboration', uses: 48, version: 'v1.0' },
                                { name: 'Map View', category: 'Display', type: 'Geo', uses: 24, version: 'v1.1' },
                                { name: 'Signature Pad', category: 'Input', type: 'Capture', uses: 42, version: 'v1.0' },
                                { name: 'Barcode Scanner', category: 'Input', type: 'Device', uses: 18, version: 'v1.3' },
                            ].map(c => (
                                <div key={c.name} className="bg-grey-800/50 rounded-lg p-3 border border-grey-700/50 hover:border-brand-500/50 cursor-pointer transition-colors">
                                    <div className="flex items-center justify-between mb-2">
                                        <span className="text-sm font-semibold text-grey-200">{c.name}</span>
                                        <span className="text-[10px] text-grey-500">{c.version}</span>
                                    </div>
                                    <div className="flex items-center justify-between text-xs">
                                        <Badge variant="default">{c.category}</Badge>
                                        <span className="text-grey-500">{c.uses} uses</span>
                                    </div>
                                </div>
                            ))}
                        </div>
                    </div>
                </div>
            )}

            {activeTab === 'Publishing' && (
                <div className="space-y-6">
                    <DataTable
                        columns={['App', 'Version', 'Environment', 'Audience', 'Access', 'Users', 'Status']}
                        rows={[
                            ['Expense Tracker', 'v2.3', <Badge variant="success">Production</Badge>, 'All Employees', <Badge variant="info">Org-wide</Badge>, '4,821', <Badge variant="success">Live</Badge>],
                            ['IT Helpdesk', 'v3.1', <Badge variant="success">Production</Badge>, 'All Employees', <Badge variant="info">Org-wide</Badge>, '3,284', <Badge variant="success">Live</Badge>],
                            ['Sales Dashboard', 'v1.8', <Badge variant="success">Production</Badge>, 'Sales Team', <Badge variant="warning">Team</Badge>, '124', <Badge variant="success">Live</Badge>],
                            ['Asset Tracker', 'v2.0', <Badge variant="warning">Staging</Badge>, 'IT + Facilities', <Badge variant="warning">Team</Badge>, '12 (testers)', <Badge variant="warning">Testing</Badge>],
                            ['Visitor Management', 'v1.0', <Badge variant="default">Development</Badge>, 'Dev Team', <Badge variant="danger">Dev Only</Badge>, '3', <Badge variant="info">Building</Badge>],
                            ['Procurement Portal', 'v4.2', <Badge variant="success">Production</Badge>, 'Finance + Ops', <Badge variant="warning">Team</Badge>, '284', <Badge variant="success">Live</Badge>],
                        ]}
                    />

                    <div className="gs-card p-6">
                        <h3 className="text-sm font-semibold text-grey-300 mb-4">Publishing Pipeline</h3>
                        <div className="flex items-center justify-between">
                            {[
                                { stage: 'Development', desc: 'Build & unit test', status: 'complete' },
                                { stage: 'Staging', desc: 'Integration testing', status: 'complete' },
                                { stage: 'Review', desc: 'Admin approval', status: 'current' },
                                { stage: 'Production', desc: 'Live deployment', status: 'pending' },
                            ].map((s, i) => (
                                <div key={s.stage} className="flex items-center gap-4 flex-1">
                                    <div className="text-center flex-1">
                                        <div className={`w-10 h-10 rounded-full mx-auto mb-2 flex items-center justify-center ${s.status === 'complete' ? 'bg-emerald-500/20 text-emerald-400' :
                                                s.status === 'current' ? 'bg-brand-500/20 text-brand-400' :
                                                    'bg-grey-800 text-grey-500'
                                            }`}>
                                            {s.status === 'complete' ? <CheckCircle2 size={18} /> : <span className="text-xs font-bold">{i + 1}</span>}
                                        </div>
                                        <p className="text-xs font-semibold text-grey-300">{s.stage}</p>
                                        <p className="text-[10px] text-grey-500">{s.desc}</p>
                                    </div>
                                    {i < 3 && <ArrowRight size={16} className="text-grey-600 flex-shrink-0" />}
                                </div>
                            ))}
                        </div>
                    </div>
                </div>
            )}

            {activeTab === 'Lifecycle' && (
                <div className="space-y-6">
                    <div className="gs-card p-6">
                        <h3 className="text-sm font-semibold text-grey-300 mb-4">App Lifecycle Management (ALM)</h3>

                        <div className="grid grid-cols-2 gap-6">
                            <div>
                                <h4 className="text-xs font-semibold text-grey-400 uppercase mb-3">Solution Packaging</h4>
                                <div className="space-y-3">
                                    {[
                                        { feature: 'Solution Export', desc: 'Bundle app + data + flows + connectors as portable package' },
                                        { feature: 'Environment Variables', desc: 'Config values that change per environment (dev/staging/prod)' },
                                        { feature: 'Connection References', desc: 'Abstract connection details — map at import time' },
                                        { feature: 'Version Control', desc: 'Git integration for solution source control' },
                                        { feature: 'Dependency Tracking', desc: 'Auto-detect and bundle component dependencies' },
                                        { feature: 'Rollback', desc: 'Instant rollback to any previous version' },
                                    ].map(f => (
                                        <div key={f.feature} className="py-2 border-b border-grey-800/50 last:border-0">
                                            <p className="text-sm text-grey-300">{f.feature}</p>
                                            <p className="text-xs text-grey-500 mt-0.5">{f.desc}</p>
                                        </div>
                                    ))}
                                </div>
                            </div>

                            <div>
                                <h4 className="text-xs font-semibold text-grey-400 uppercase mb-3">Governance</h4>
                                <div className="space-y-3">
                                    {[
                                        { policy: 'DLP Policy', desc: 'Block connectors from mixing business + personal data', status: 'Enforced' },
                                        { policy: 'Connector Approval', desc: 'Custom connectors require admin approval', status: 'Enforced' },
                                        { policy: 'Environment Strategy', desc: 'Separate dev/test/prod with data isolation', status: 'Enforced' },
                                        { policy: 'Maker Welcome', desc: 'Onboarding for new citizen developers', status: 'Active' },
                                        { policy: 'App Audit', desc: 'Quarterly review of published apps for compliance', status: 'Scheduled' },
                                        { policy: 'Orphaned App Policy', desc: 'Auto-notify when app owner leaves org', status: 'Enforced' },
                                    ].map(p => (
                                        <div key={p.policy} className="flex items-center justify-between py-2 border-b border-grey-800/50 last:border-0">
                                            <div>
                                                <p className="text-sm text-grey-300">{p.policy}</p>
                                                <p className="text-xs text-grey-500">{p.desc}</p>
                                            </div>
                                            <Badge variant="success">{p.status}</Badge>
                                        </div>
                                    ))}
                                </div>
                            </div>
                        </div>
                    </div>
                </div>
            )}
        </div>
    );
}
