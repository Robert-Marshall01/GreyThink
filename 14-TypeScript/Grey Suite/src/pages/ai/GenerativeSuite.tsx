import { PageHeader, Badge } from '../../components/ui';
import { FileText, Table, Presentation, Mail, Sparkles, Plus, Wand2 } from 'lucide-react';
import { useState } from 'react';

export function GenerativeSuite() {
    const [activeApp, setActiveApp] = useState('docs');

    const apps = [
        { id: 'docs', name: 'Docs AI', icon: <FileText size={20} />, desc: 'AI-powered document creation & editing', color: 'text-brand-400 bg-brand-500/15' },
        { id: 'sheets', name: 'Sheets AI', icon: <Table size={20} />, desc: 'Smart spreadsheets with AI formulas', color: 'text-accent-emerald bg-accent-emerald/15' },
        { id: 'slides', name: 'Slides AI', icon: <Presentation size={20} />, desc: 'Auto-generate presentations from data', color: 'text-accent-amber bg-accent-amber/15' },
        { id: 'email', name: 'Email AI', icon: <Mail size={20} />, desc: 'Intelligent email drafting & replies', color: 'text-accent-violet bg-accent-violet/15' },
    ];

    return (
        <div>
            <PageHeader
                title="Generative Suite"
                subtitle="AI-powered docs, sheets, slides & email"
                actions={<button className="gs-btn-primary text-sm flex items-center gap-2"><Plus size={14} /> New Document</button>}
            />

            {/* App Selector */}
            <div className="grid grid-cols-2 lg:grid-cols-4 gap-4 mb-6">
                {apps.map(app => (
                    <button
                        key={app.id}
                        onClick={() => setActiveApp(app.id)}
                        className={`gs-card p-4 text-left transition-all ${activeApp === app.id ? 'border-brand-500/50 bg-brand-500/5' : ''}`}
                    >
                        <div className={`w-10 h-10 rounded-xl flex items-center justify-center mb-3 ${app.color}`}>{app.icon}</div>
                        <h4 className="text-sm font-semibold text-grey-200">{app.name}</h4>
                        <p className="text-xs text-grey-500 mt-1">{app.desc}</p>
                    </button>
                ))}
            </div>

            {/* AI Composer */}
            <div className="gs-card p-6 mb-6">
                <div className="flex items-center gap-2 mb-4">
                    <Sparkles size={18} className="text-brand-400" />
                    <h3 className="gs-section-title">AI Composer</h3>
                </div>
                <div className="bg-grey-800/50 rounded-xl p-4 mb-4">
                    <textarea
                        className="w-full bg-transparent text-grey-200 placeholder-grey-500 outline-none resize-none text-sm"
                        rows={3}
                        placeholder="Describe what you want to create... e.g., 'Write a quarterly business review document summarizing Q1 2026 performance with revenue charts and key metrics'"
                    />
                </div>
                <div className="flex items-center justify-between">
                    <div className="flex items-center gap-2 text-xs text-grey-500">
                        <span>Model: Grey AI v3</span>
                        <span>·</span>
                        <span>Context: Full workspace</span>
                    </div>
                    <button className="gs-btn-primary text-sm flex items-center gap-2">
                        <Wand2 size={14} /> Generate
                    </button>
                </div>
            </div>

            {/* Recent Documents */}
            <div className="gs-card p-5">
                <h3 className="gs-section-title mb-4">Recent Creations</h3>
                <div className="space-y-2">
                    {[
                        { name: 'Q1 Business Review', type: 'Document', created: '1h ago', status: 'Draft', aiAssisted: true },
                        { name: 'Revenue Forecast Model', type: 'Spreadsheet', created: '3h ago', status: 'Published', aiAssisted: true },
                        { name: 'Product Launch Deck', type: 'Presentation', created: '1d ago', status: 'Review', aiAssisted: true },
                        { name: 'Customer Outreach Template', type: 'Email', created: '1d ago', status: 'Published', aiAssisted: false },
                        { name: 'Engineering Capacity Plan', type: 'Spreadsheet', created: '2d ago', status: 'Published', aiAssisted: true },
                        { name: 'Board Meeting Prep', type: 'Document', created: '3d ago', status: 'Published', aiAssisted: false },
                    ].map(doc => (
                        <div key={doc.name} className="flex items-center justify-between p-3 rounded-lg hover:bg-grey-800/40 transition-colors cursor-pointer">
                            <div className="flex items-center gap-3">
                                <div className={`w-8 h-8 rounded-lg flex items-center justify-center text-sm ${doc.type === 'Document' ? 'bg-brand-500/15 text-brand-400' :
                                        doc.type === 'Spreadsheet' ? 'bg-accent-emerald/15 text-accent-emerald' :
                                            doc.type === 'Presentation' ? 'bg-accent-amber/15 text-accent-amber' :
                                                'bg-accent-violet/15 text-accent-violet'
                                    }`}>
                                    {doc.type === 'Document' ? <FileText size={16} /> :
                                        doc.type === 'Spreadsheet' ? <Table size={16} /> :
                                            doc.type === 'Presentation' ? <Presentation size={16} /> :
                                                <Mail size={16} />}
                                </div>
                                <div>
                                    <p className="text-sm font-medium text-grey-200 flex items-center gap-2">
                                        {doc.name}
                                        {doc.aiAssisted && <Sparkles size={12} className="text-brand-400" />}
                                    </p>
                                    <p className="text-xs text-grey-500">{doc.type} · Created {doc.created}</p>
                                </div>
                            </div>
                            <Badge variant={doc.status === 'Published' ? 'success' : doc.status === 'Draft' ? 'default' : 'warning'}>{doc.status}</Badge>
                        </div>
                    ))}
                </div>
            </div>
        </div>
    );
}
