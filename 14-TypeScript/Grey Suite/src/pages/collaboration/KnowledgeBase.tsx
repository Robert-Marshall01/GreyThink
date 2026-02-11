import { PageHeader, Badge } from '../../components/ui';
import { BookOpen, FileText, Folder, Search, Star, Clock, Plus } from 'lucide-react';
import { useState } from 'react';

export function KnowledgeBase() {
    const [search, setSearch] = useState('');

    const spaces = [
        { name: 'Engineering Wiki', docs: 248, updated: '2h ago', icon: '⚙️' },
        { name: 'Product Playbook', docs: 86, updated: '4h ago', icon: '📋' },
        { name: 'HR & Policies', docs: 124, updated: '1d ago', icon: '📑' },
        { name: 'Sales Enablement', docs: 67, updated: '6h ago', icon: '📊' },
        { name: 'Design System', docs: 42, updated: '3h ago', icon: '🎨' },
        { name: 'Onboarding', docs: 38, updated: '2d ago', icon: '🚀' },
    ];

    const recentDocs = [
        { title: 'API Authentication Guide', space: 'Engineering Wiki', author: 'Sarah Chen', updated: '2h ago', views: 142 },
        { title: 'Q1 2026 Product Roadmap', space: 'Product Playbook', author: 'Marcus Johnson', updated: '4h ago', views: 286 },
        { title: 'Brand Guidelines v4.0', space: 'Design System', author: 'Emily Rodriguez', updated: '6h ago', views: 94 },
        { title: 'Incident Response Runbook', space: 'Engineering Wiki', author: 'David Kim', updated: '1d ago', views: 68 },
        { title: 'New Hire Checklist', space: 'Onboarding', author: 'Amanda Foster', updated: '2d ago', views: 312 },
    ];

    return (
        <div>
            <PageHeader
                title="Knowledge Base"
                subtitle="Documentation, wikis & shared knowledge"
                actions={
                    <div className="flex gap-2">
                        <button className="gs-btn-secondary text-sm flex items-center gap-2"><Folder size={14} /> New Space</button>
                        <button className="gs-btn-primary text-sm flex items-center gap-2"><Plus size={14} /> New Doc</button>
                    </div>
                }
            />

            {/* Search */}
            <div className="gs-card p-4 mb-6">
                <div className="flex items-center gap-3">
                    <Search size={18} className="text-grey-500" />
                    <input
                        value={search}
                        onChange={e => setSearch(e.target.value)}
                        className="flex-1 bg-transparent text-grey-100 placeholder-grey-500 outline-none"
                        placeholder="Search all documentation..."
                    />
                    <kbd className="text-xs bg-grey-800 text-grey-500 px-2 py-0.5 rounded">/</kbd>
                </div>
            </div>

            {/* Spaces */}
            <div className="mb-6">
                <h3 className="gs-section-title mb-4">Spaces</h3>
                <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4">
                    {spaces.map(space => (
                        <div key={space.name} className="gs-card p-4 cursor-pointer hover:border-brand-500/30 transition-colors">
                            <div className="flex items-center gap-3 mb-3">
                                <span className="text-2xl">{space.icon}</span>
                                <div>
                                    <h4 className="text-sm font-semibold text-grey-200">{space.name}</h4>
                                    <p className="text-xs text-grey-500">{space.docs} documents</p>
                                </div>
                            </div>
                            <div className="flex items-center justify-between text-xs text-grey-500">
                                <span className="flex items-center gap-1"><Clock size={12} /> Updated {space.updated}</span>
                                <button className="text-grey-500 hover:text-accent-amber"><Star size={12} /></button>
                            </div>
                        </div>
                    ))}
                </div>
            </div>

            {/* Recent Documents */}
            <div className="gs-card p-5">
                <h3 className="gs-section-title mb-4">Recent Documents</h3>
                <div className="space-y-2">
                    {recentDocs.map(doc => (
                        <div key={doc.title} className="flex items-center justify-between p-3 rounded-lg hover:bg-grey-800/40 transition-colors cursor-pointer">
                            <div className="flex items-center gap-3">
                                <FileText size={18} className="text-grey-500" />
                                <div>
                                    <p className="text-sm font-medium text-grey-200">{doc.title}</p>
                                    <p className="text-xs text-grey-500">{doc.space} · by {doc.author}</p>
                                </div>
                            </div>
                            <div className="flex items-center gap-4 text-xs text-grey-500">
                                <span>{doc.views} views</span>
                                <span>{doc.updated}</span>
                            </div>
                        </div>
                    ))}
                </div>
            </div>
        </div>
    );
}
