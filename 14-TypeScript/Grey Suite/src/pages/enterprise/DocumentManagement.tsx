import { useState } from 'react';
import {
    FileText, FolderTree, Clock, Lock, Globe, Tag, Search,
    RotateCcw, Eye, Download, Upload, Layers, Settings, Users,
    CheckCircle2, AlertTriangle, BookOpen, Layout, Grid3x3
} from 'lucide-react';
import { PageHeader, StatCard, DataTable, Badge, Tabs, ProgressBar, MiniSparkline } from '../../components/ui';

export function DocumentManagement() {
    const [activeTab, setActiveTab] = useState('Libraries');

    const tabs = ['Libraries', 'Version Control', 'Intranet Builder', 'Content Types', 'Taxonomy', 'Permissions'];

    return (
        <div className="p-6 space-y-6">
            <PageHeader
                title="Enterprise Document & Content Management"
                subtitle="Document libraries, version control, intranet builder, structured content, and metadata taxonomy"
                actions={
                    <div className="flex gap-2">
                        <button className="gs-btn-secondary">
                            <Upload size={14} /> Import
                        </button>
                        <button className="gs-btn-primary">
                            <FileText size={14} /> New Document
                        </button>
                    </div>
                }
            />

            <div className="grid grid-cols-4 gap-4">
                <StatCard label="Total Documents" value="1.2M" change="+8,420 this month" changeType="positive" icon={<FileText size={18} />} />
                <StatCard label="Active Libraries" value="342" change="+12" changeType="positive" icon={<FolderTree size={18} />} />
                <StatCard label="Version History" value="4.8M" change="revisions tracked" changeType="neutral" icon={<Clock size={18} />} />
                <StatCard label="Compliance Score" value="96.4%" change="+1.2%" changeType="positive" icon={<CheckCircle2 size={18} />} />
            </div>

            <Tabs tabs={tabs} active={activeTab} onChange={setActiveTab} />

            {activeTab === 'Libraries' && (
                <div className="space-y-6">
                    <DataTable
                        columns={['Library', 'Type', 'Documents', 'Storage', 'Owner', 'Permissions', 'Status']}
                        rows={[
                            [
                                <div className="flex items-center gap-2"><FolderTree size={14} className="text-brand-400" /> Corporate Policies</div>,
                                <Badge variant="info">Managed</Badge>, '2,841', '14.2 GB', 'Legal & Compliance',
                                <Badge variant="warning">Restricted</Badge>, <Badge variant="success">Active</Badge>,
                            ],
                            [
                                <div className="flex items-center gap-2"><FolderTree size={14} className="text-brand-400" /> Engineering Docs</div>,
                                <Badge variant="info">Managed</Badge>, '18,294', '82.1 GB', 'Engineering',
                                <Badge variant="info">Team</Badge>, <Badge variant="success">Active</Badge>,
                            ],
                            [
                                <div className="flex items-center gap-2"><FolderTree size={14} className="text-brand-400" /> Sales Collateral</div>,
                                <Badge variant="purple">Template</Badge>, '4,120', '28.9 GB', 'Marketing',
                                <Badge variant="info">Org-wide</Badge>, <Badge variant="success">Active</Badge>,
                            ],
                            [
                                <div className="flex items-center gap-2"><FolderTree size={14} className="text-brand-400" /> HR Documents</div>,
                                <Badge variant="info">Managed</Badge>, '8,942', '6.3 GB', 'Human Resources',
                                <Badge variant="warning">Restricted</Badge>, <Badge variant="success">Active</Badge>,
                            ],
                            [
                                <div className="flex items-center gap-2"><FolderTree size={14} className="text-brand-400" /> Board Materials</div>,
                                <Badge variant="danger">Confidential</Badge>, '892', '4.1 GB', 'Executive Office',
                                <Badge variant="danger">Executive Only</Badge>, <Badge variant="success">Active</Badge>,
                            ],
                            [
                                <div className="flex items-center gap-2"><FolderTree size={14} className="text-brand-400" /> Product Specs</div>,
                                <Badge variant="info">Managed</Badge>, '6,421', '41.2 GB', 'Product',
                                <Badge variant="info">Team</Badge>, <Badge variant="success">Active</Badge>,
                            ],
                        ]}
                    />

                    <div className="grid grid-cols-3 gap-4">
                        {[
                            { label: 'Check-in / Check-out', desc: 'Exclusive edit locking with timeout', icon: <Lock size={20} />, status: 'Enabled' },
                            { label: 'Co-Authoring', desc: 'Real-time collaborative editing via OT/CRDT', icon: <Users size={20} />, status: 'Enabled' },
                            { label: 'Content Approval', desc: 'Multi-stage approval workflows', icon: <CheckCircle2 size={20} />, status: '142 pending' },
                        ].map(f => (
                            <div key={f.label} className="gs-card p-4">
                                <div className="flex items-center gap-3 mb-3">
                                    <div className="p-2 bg-brand-500/10 rounded-lg text-brand-400">{f.icon}</div>
                                    <div>
                                        <p className="text-sm font-semibold text-grey-200">{f.label}</p>
                                        <p className="text-xs text-grey-500">{f.desc}</p>
                                    </div>
                                </div>
                                <Badge variant="success">{f.status}</Badge>
                            </div>
                        ))}
                    </div>
                </div>
            )}

            {activeTab === 'Version Control' && (
                <div className="space-y-6">
                    <div className="gs-card p-6">
                        <h3 className="text-sm font-semibold text-grey-300 mb-4">Version History & Rollback Engine</h3>
                        <div className="bg-grey-900 rounded-lg p-4 font-mono text-xs text-grey-400 mb-6">
                            <pre>{`// Document Version Strategy
interface DocumentVersion {
  id: string;
  version: {
    major: number;     // Manual publish
    minor: number;     // Auto-save increment
  };
  author: User;
  timestamp: ISO8601;
  changeSummary: string;     // AI-generated diff summary
  diffStats: {
    additions: number;
    deletions: number;
    modifications: number;
  };
  contentHash: string;       // SHA-256 integrity
  storageStrategy: 'full_snapshot' | 'delta';
  metadata: VersionMetadata;
}

// Retention: major versions = forever
// Minor versions: last 500 or 2 years
// Deleted docs: 93-day recycle bin + legal hold override`}</pre>
                        </div>
                    </div>

                    <DataTable
                        columns={['Version', 'Author', 'Date', 'Changes', 'Size Delta', 'Type', 'Actions']}
                        rows={[
                            ['v3.0', 'Sarah Chen', '2026-02-10 09:42', 'Q1 targets updated, new sections added', '+24 KB', <Badge variant="info">Major</Badge>,
                                <div className="flex gap-1"><button className="gs-btn-ghost text-xs"><Eye size={12} /> View</button><button className="gs-btn-ghost text-xs"><RotateCcw size={12} /> Restore</button></div>],
                            ['v2.14', 'James Wilson', '2026-02-09 16:30', 'Fixed formatting in appendix', '+1 KB', <Badge variant="default">Minor</Badge>,
                                <div className="flex gap-1"><button className="gs-btn-ghost text-xs"><Eye size={12} /> View</button><button className="gs-btn-ghost text-xs"><RotateCcw size={12} /> Restore</button></div>],
                            ['v2.13', 'AI Auto-Save', '2026-02-09 16:15', 'Auto-save checkpoint', '+2 KB', <Badge variant="default">Minor</Badge>,
                                <div className="flex gap-1"><button className="gs-btn-ghost text-xs"><Eye size={12} /> View</button><button className="gs-btn-ghost text-xs"><RotateCcw size={12} /> Restore</button></div>],
                            ['v2.12', 'Maria Lopez', '2026-02-08 11:22', 'Legal review comments addressed', '+8 KB', <Badge variant="default">Minor</Badge>,
                                <div className="flex gap-1"><button className="gs-btn-ghost text-xs"><Eye size={12} /> View</button><button className="gs-btn-ghost text-xs"><RotateCcw size={12} /> Restore</button></div>],
                            ['v2.0', 'Sarah Chen', '2026-02-05 14:00', 'Major restructure for board review', '+42 KB', <Badge variant="info">Major</Badge>,
                                <div className="flex gap-1"><button className="gs-btn-ghost text-xs"><Eye size={12} /> View</button><button className="gs-btn-ghost text-xs"><RotateCcw size={12} /> Restore</button></div>],
                            ['v1.0', 'James Wilson', '2026-01-20 10:00', 'Initial draft published', '+180 KB', <Badge variant="info">Major</Badge>,
                                <div className="flex gap-1"><button className="gs-btn-ghost text-xs"><Eye size={12} /> View</button><button className="gs-btn-ghost text-xs"><RotateCcw size={12} /> Restore</button></div>],
                        ]}
                    />

                    <div className="grid grid-cols-2 gap-6">
                        <div className="gs-card p-6">
                            <h3 className="text-sm font-semibold text-grey-300 mb-4">Diff Viewer</h3>
                            <div className="bg-grey-900 rounded-lg p-4 text-xs font-mono space-y-1">
                                <div className="text-grey-500">--- v2.14 (James Wilson)</div>
                                <div className="text-grey-500">+++ v3.0 (Sarah Chen)</div>
                                <div className="text-grey-600 mt-2">@@ Section 2: Revenue Targets @@</div>
                                <div className="text-rose-400">- Q1 Revenue Target: $12.4M</div>
                                <div className="text-emerald-400">+ Q1 Revenue Target: $14.8M</div>
                                <div className="text-grey-500">  Q2 Revenue Target: $15.2M</div>
                                <div className="text-grey-600 mt-2">@@ Section 5: New Markets @@</div>
                                <div className="text-emerald-400">+ 5.1 APAC Expansion Strategy</div>
                                <div className="text-emerald-400">+ 5.2 Partnership Pipeline</div>
                                <div className="text-emerald-400">+ 5.3 Regulatory Considerations</div>
                            </div>
                        </div>

                        <div className="gs-card p-6">
                            <h3 className="text-sm font-semibold text-grey-300 mb-4">Branch & Merge (Documents)</h3>
                            <div className="space-y-3">
                                {[
                                    { feature: 'Document Branching', desc: 'Create parallel versions for different audiences', status: 'Enabled' },
                                    { feature: 'Three-way Merge', desc: 'Merge branches with conflict resolution', status: 'Enabled' },
                                    { feature: 'Conflict Detection', desc: 'AI-powered semantic conflict detection', status: 'Enabled' },
                                    { feature: 'Approval Gates', desc: 'Require approval before merging to main', status: 'Configurable' },
                                    { feature: 'Audit Trail', desc: 'Complete history of branch/merge operations', status: 'Enabled' },
                                ].map(f => (
                                    <div key={f.feature} className="flex items-center justify-between py-2 border-b border-grey-800/50 last:border-0">
                                        <div>
                                            <p className="text-sm text-grey-300">{f.feature}</p>
                                            <p className="text-xs text-grey-500">{f.desc}</p>
                                        </div>
                                        <Badge variant="info">{f.status}</Badge>
                                    </div>
                                ))}
                            </div>
                        </div>
                    </div>
                </div>
            )}

            {activeTab === 'Intranet Builder' && (
                <div className="space-y-6">
                    <div className="gs-card p-6">
                        <h3 className="text-sm font-semibold text-grey-300 mb-4">Site Builder</h3>
                        <div className="grid grid-cols-3 gap-4 mb-6">
                            {[
                                { name: 'Corporate Hub', type: 'Hub Site', pages: 48, users: '12,400', status: 'Published', lastUpdated: '2h ago' },
                                { name: 'Engineering Wiki', type: 'Team Site', pages: 284, users: '892', status: 'Published', lastUpdated: '15m ago' },
                                { name: 'HR Portal', type: 'Communication Site', pages: 32, users: '4,200', status: 'Published', lastUpdated: '1d ago' },
                            ].map(s => (
                                <div key={s.name} className="bg-grey-800/50 rounded-lg p-4 border border-grey-700/50">
                                    <div className="flex items-center justify-between mb-3">
                                        <p className="text-sm font-semibold text-grey-200">{s.name}</p>
                                        <Badge variant="success">{s.status}</Badge>
                                    </div>
                                    <div className="space-y-2 text-xs text-grey-400">
                                        <div className="flex justify-between"><span>Type</span><span className="text-grey-300">{s.type}</span></div>
                                        <div className="flex justify-between"><span>Pages</span><span className="text-grey-300">{s.pages}</span></div>
                                        <div className="flex justify-between"><span>Active Users</span><span className="text-grey-300">{s.users}</span></div>
                                        <div className="flex justify-between"><span>Last Updated</span><span className="text-grey-300">{s.lastUpdated}</span></div>
                                    </div>
                                </div>
                            ))}
                        </div>
                    </div>

                    <div className="grid grid-cols-2 gap-6">
                        <div className="gs-card p-6">
                            <h3 className="text-sm font-semibold text-grey-300 mb-4">Page Components (Web Parts)</h3>
                            <div className="grid grid-cols-2 gap-2">
                                {[
                                    { name: 'Hero Banner', icon: <Layout size={14} /> },
                                    { name: 'News Feed', icon: <BookOpen size={14} /> },
                                    { name: 'Document Library', icon: <FileText size={14} /> },
                                    { name: 'People Directory', icon: <Users size={14} /> },
                                    { name: 'Events Calendar', icon: <Clock size={14} /> },
                                    { name: 'Quick Links', icon: <Globe size={14} /> },
                                    { name: 'Embed (iframe)', icon: <Grid3x3 size={14} /> },
                                    { name: 'Markdown Block', icon: <FileText size={14} /> },
                                    { name: 'Chart Widget', icon: <Layers size={14} /> },
                                    { name: 'Form Builder', icon: <Settings size={14} /> },
                                    { name: 'Image Gallery', icon: <Eye size={14} /> },
                                    { name: 'Video Player', icon: <Eye size={14} /> },
                                ].map(w => (
                                    <div key={w.name} className="flex items-center gap-2 p-2 bg-grey-800/50 rounded text-xs text-grey-300">
                                        <span className="text-brand-400">{w.icon}</span>
                                        {w.name}
                                    </div>
                                ))}
                            </div>
                        </div>

                        <div className="gs-card p-6">
                            <h3 className="text-sm font-semibold text-grey-300 mb-4">Site Templates</h3>
                            <div className="space-y-3">
                                {[
                                    { template: 'Hub Site', desc: 'Central navigation hub connecting team sites', usage: 3 },
                                    { template: 'Team Site', desc: 'Collaboration space with doc libraries + lists', usage: 48 },
                                    { template: 'Communication Site', desc: 'Broadcast site for news + announcements', usage: 12 },
                                    { template: 'Project Site', desc: 'Time-bound project workspace with milestones', usage: 84 },
                                    { template: 'Knowledge Base', desc: 'Wiki-style documentation with search', usage: 18 },
                                    { template: 'Landing Page', desc: 'Single-page campaign or event site', usage: 24 },
                                ].map(t => (
                                    <div key={t.template} className="flex items-center justify-between py-2 border-b border-grey-800/50 last:border-0">
                                        <div>
                                            <p className="text-sm text-grey-300">{t.template}</p>
                                            <p className="text-xs text-grey-500">{t.desc}</p>
                                        </div>
                                        <Badge variant="default">{t.usage} sites</Badge>
                                    </div>
                                ))}
                            </div>
                        </div>
                    </div>
                </div>
            )}

            {activeTab === 'Content Types' && (
                <div className="space-y-6">
                    <div className="gs-card p-6">
                        <h3 className="text-sm font-semibold text-grey-300 mb-4">Structured Content Type Definitions</h3>
                        <DataTable
                            columns={['Content Type', 'Parent', 'Fields', 'Libraries Using', 'Workflows', 'Retention', 'Status']}
                            rows={[
                                ['Document', '—', '12 fields', '284', '3', '7 years', <Badge variant="success">Active</Badge>],
                                ['Policy Document', 'Document', '18 fields', '42', '5 (approval chain)', '∞', <Badge variant="success">Active</Badge>],
                                ['Contract', 'Document', '22 fields', '18', '4 (legal review)', '10 years', <Badge variant="success">Active</Badge>],
                                ['Technical Spec', 'Document', '16 fields', '64', '2', '5 years', <Badge variant="success">Active</Badge>],
                                ['Meeting Minutes', 'Document', '10 fields', '124', '1 (auto-distribute)', '3 years', <Badge variant="success">Active</Badge>],
                                ['Invoice', 'Document', '24 fields', '8', '3 (AP workflow)', '7 years', <Badge variant="success">Active</Badge>],
                                ['Project Deliverable', 'Document', '20 fields', '84', '4 (stage-gate)', '5 years', <Badge variant="success">Active</Badge>],
                            ]}
                        />
                    </div>

                    <div className="gs-card p-6">
                        <h3 className="text-sm font-semibold text-grey-300 mb-4">Content Type Schema Definition</h3>
                        <div className="bg-grey-900 rounded-lg p-4 font-mono text-xs text-grey-400">
                            <pre>{`// Content Type: Contract
{
  "id": "ct_contract_v3",
  "name": "Contract",
  "parent": "ct_document",
  "inherits": ["title", "author", "created", "modified"],
  "fields": [
    { "name": "contract_type", "type": "choice",
      "options": ["MSA", "SOW", "NDA", "SLA", "Amendment"] },
    { "name": "counterparty", "type": "lookup", "source": "crm_accounts" },
    { "name": "effective_date", "type": "date", "required": true },
    { "name": "expiration_date", "type": "date", "required": true },
    { "name": "value", "type": "currency", "precision": 2 },
    { "name": "auto_renew", "type": "boolean", "default": false },
    { "name": "renewal_term", "type": "choice",
      "options": ["1 year", "2 years", "3 years"] },
    { "name": "governing_law", "type": "choice",
      "options": ["Delaware", "California", "New York", "UK", "EU"] },
    { "name": "signatories", "type": "person[]", "min": 2 },
    { "name": "sensitivity", "type": "label",
      "options": ["Public", "Internal", "Confidential", "Restricted"] },
    { "name": "attachments", "type": "file[]" }
  ],
  "workflows": ["legal_review", "cfo_approval", "execution", "archive"],
  "retention": { "policy": "regulatory", "duration": "10y" }
}`}</pre>
                        </div>
                    </div>
                </div>
            )}

            {activeTab === 'Taxonomy' && (
                <div className="space-y-6">
                    <div className="gs-card p-6">
                        <h3 className="text-sm font-semibold text-grey-300 mb-4">Enterprise Taxonomy & Metadata</h3>
                        <div className="grid grid-cols-2 gap-6">
                            <div>
                                <h4 className="text-xs font-semibold text-grey-400 uppercase mb-3">Term Store Hierarchy</h4>
                                <div className="bg-grey-900 rounded-lg p-4 text-xs text-grey-400 font-mono">
                                    <pre>{`Enterprise Taxonomy
├── Department
│   ├── Engineering (1,842 items)
│   ├── Sales (2,104 items)
│   ├── Marketing (1,256 items)
│   ├── Finance (892 items)
│   ├── Legal (421 items)
│   └── HR (1,048 items)
├── Document Type
│   ├── Policy (284 items)
│   ├── Contract (1,842 items)
│   ├── Spec (642 items)
│   ├── Report (2,841 items)
│   └── Presentation (1,204 items)
├── Geography
│   ├── North America
│   ├── EMEA
│   ├── APAC
│   └── LATAM
├── Confidentiality
│   ├── Public
│   ├── Internal
│   ├── Confidential
│   └── Restricted
└── Project
    ├── Active (184 projects)
    ├── Completed (842 projects)
    └── Archived (2,104 projects)`}</pre>
                                </div>
                            </div>

                            <div>
                                <h4 className="text-xs font-semibold text-grey-400 uppercase mb-3">Auto-Classification (AI)</h4>
                                <div className="space-y-3">
                                    {[
                                        { classifier: 'Document Type', model: 'BERT fine-tuned', accuracy: 94.2, processed: '1.2M docs' },
                                        { classifier: 'Sensitivity Label', model: 'Custom NER + rules', accuracy: 97.8, processed: '1.2M docs' },
                                        { classifier: 'Department', model: 'TF-IDF + metadata', accuracy: 91.4, processed: '1.2M docs' },
                                        { classifier: 'Topic Extraction', model: 'LDA + embeddings', accuracy: 88.6, processed: '1.2M docs' },
                                        { classifier: 'Language Detection', model: 'fastText', accuracy: 99.1, processed: '1.2M docs' },
                                        { classifier: 'PII Detection', model: 'Presidio + custom', accuracy: 96.4, processed: '1.2M docs' },
                                    ].map(c => (
                                        <div key={c.classifier} className="bg-grey-800/50 rounded-lg p-3 border border-grey-700/50">
                                            <div className="flex justify-between mb-2">
                                                <span className="text-sm text-grey-300">{c.classifier}</span>
                                                <Badge variant="success">{c.accuracy}%</Badge>
                                            </div>
                                            <div className="flex justify-between text-xs text-grey-500">
                                                <span>{c.model}</span>
                                                <span>{c.processed}</span>
                                            </div>
                                        </div>
                                    ))}
                                </div>
                            </div>
                        </div>
                    </div>
                </div>
            )}

            {activeTab === 'Permissions' && (
                <div className="space-y-6">
                    <div className="gs-card p-6">
                        <h3 className="text-sm font-semibold text-grey-300 mb-4">Permission Model</h3>
                        <div className="bg-grey-900 rounded-lg p-4 font-mono text-xs text-grey-400 mb-6">
                            <pre>{`// Document Permission Hierarchy
Organization Policy (baseline)
└── Library Permissions
    └── Folder Permissions (inheritance breakable)
        └── Document Permissions (individual override)

// Permission Levels:
// Full Control → Design → Edit → Contribute → Read → View Only
// + Custom permission sets per content type

// Sharing Model:
// Internal: direct, group, org-wide
// External: pre-approved domains, anyone with link
// Expiry: auto-revoke after configurable period
// Audit: every access/share logged immutably`}</pre>
                        </div>
                    </div>

                    <DataTable
                        columns={['Library', 'Inherited', 'Unique Permissions', 'External Shares', 'Sensitivity', 'Last Audit']}
                        rows={[
                            ['Corporate Policies', 'Yes', '4 custom groups', '0', <Badge variant="warning">Confidential</Badge>, '2026-02-08'],
                            ['Engineering Docs', 'Yes', '2 team overrides', '12 (partner NDAs)', <Badge variant="info">Internal</Badge>, '2026-02-09'],
                            ['Sales Collateral', 'No', '8 custom groups', '284 (customer links)', <Badge variant="default">Public-safe</Badge>, '2026-02-10'],
                            ['Board Materials', 'No', '1 executive group', '0', <Badge variant="danger">Restricted</Badge>, '2026-02-07'],
                            ['HR Documents', 'Yes', '6 role-based groups', '0', <Badge variant="warning">Confidential</Badge>, '2026-02-06'],
                        ]}
                    />
                </div>
            )}
        </div>
    );
}
