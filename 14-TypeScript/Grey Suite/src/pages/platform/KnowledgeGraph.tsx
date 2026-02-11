import { useState } from 'react';
import { PageHeader, StatCard, Badge, DataTable, Tabs, ProgressBar, MiniSparkline } from '../../components/ui';
import {
    Share2, Search, Database, GitBranch, Eye,
    Layers, Cpu, Zap, ArrowRight, Bot,
    Users, Building2, Briefcase, DollarSign, Activity
} from 'lucide-react';

const entities = [
    { type: 'Customer', count: '1,284', source: 'CRM', connections: '18,420', lastUpdated: '2 min ago' },
    { type: 'Employee', count: '342', source: 'HCM', connections: '4,860', lastUpdated: '5 min ago' },
    { type: 'Deal', count: '2,480', source: 'CRM', connections: '12,400', lastUpdated: '4 min ago' },
    { type: 'Product', count: '84', source: 'ERP', connections: '3,360', lastUpdated: '1h ago' },
    { type: 'Invoice', count: '18,420', source: 'ERP', connections: '36,840', lastUpdated: '1 min ago' },
    { type: 'Project', count: '148', source: 'Ops', connections: '2,960', lastUpdated: '12 min ago' },
    { type: 'Document', count: '4,200', source: 'Collaboration', connections: '16,800', lastUpdated: '8 min ago' },
    { type: 'Agent', count: '6', source: 'AI', connections: '842', lastUpdated: '30 min ago' },
];

const relationships = [
    { from: 'Customer', rel: 'PURCHASED', to: 'Product', count: '42,800' },
    { from: 'Employee', rel: 'MANAGES', to: 'Employee', count: '342' },
    { from: 'Employee', rel: 'OWNS', to: 'Deal', count: '2,480' },
    { from: 'Customer', rel: 'HAS_INVOICE', to: 'Invoice', count: '18,420' },
    { from: 'Employee', rel: 'WORKS_ON', to: 'Project', count: '1,480' },
    { from: 'Deal', rel: 'INVOLVES', to: 'Product', count: '4,200' },
    { from: 'Document', rel: 'REFERENCES', to: 'Customer', count: '8,400' },
    { from: 'Agent', rel: 'PROCESSES', to: 'Deal', count: '12,842' },
    { from: 'Customer', rel: 'BELONGS_TO', to: 'Customer', count: '2,140' },
    { from: 'Employee', rel: 'AUTHORED', to: 'Document', count: '4,200' },
];

const insights = [
    { insight: 'Acme Corp has 3 open deals worth $480K but 2 overdue invoices totaling $42K', severity: 'Warning', modules: ['CRM', 'ERP'], confidence: 94 },
    { insight: 'Sarah Chen (Sales) closes deals 40% faster when paired with Technical SE team', severity: 'Insight', modules: ['CRM', 'HCM'], confidence: 88 },
    { insight: 'Product SKU-042 has highest churn correlation — 3.2x more likely to churn within 6 months', severity: 'Critical', modules: ['CRM', 'ERP', 'BI'], confidence: 91 },
    { insight: 'Engineering team velocity dropped 18% after 3 departures — recommend backfill priority', severity: 'Warning', modules: ['HCM', 'Ops'], confidence: 86 },
    { insight: 'Top 10 customers share 4 common attributes: enterprise, >500 employees, SaaS, US-based', severity: 'Insight', modules: ['CRM', 'BI'], confidence: 92 },
];

export function KnowledgeGraph() {
    const [activeTab, setActiveTab] = useState('Graph');
    const [searchQuery, setSearchQuery] = useState('');

    return (
        <div>
            <PageHeader
                title="Knowledge Graph"
                subtitle="Cross-module entity graph with semantic search, relationship inference & insights"
                actions={
                    <div className="flex gap-2">
                        <button className="gs-btn-secondary flex items-center gap-2"><Eye size={16} /> Explore</button>
                        <button className="gs-btn-primary flex items-center gap-2"><Zap size={16} /> Run Inference</button>
                    </div>
                }
            />

            <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-4 gap-4 mb-6">
                <StatCard label="Total Entities" value="26,964" change="8 entity types" changeType="neutral" icon={<Share2 size={18} />} />
                <StatCard label="Relationships" value="96,682" change="10 relation types" changeType="neutral" icon={<GitBranch size={18} />} />
                <StatCard label="Inference Accuracy" value="91.2%" change="+2.4% this month" changeType="positive" icon={<Cpu size={18} />} />
                <StatCard label="Cross-Module Insights" value="5" change="2 critical" changeType="negative" icon={<Activity size={18} />} />
            </div>

            <Tabs tabs={['Graph', 'Semantic Search', 'Relationships', 'Insights', 'Entity Linking', 'Embeddings']} active={activeTab} onChange={setActiveTab} />

            {activeTab === 'Graph' && (
                <div className="space-y-6">
                    <div className="gs-card p-5">
                        <h3 className="gs-section-title mb-4 flex items-center gap-2"><Share2 size={16} /> Entity Registry</h3>
                        <DataTable
                            columns={['Entity Type', 'Count', 'Source Module', 'Connections', 'Last Updated']}
                            rows={entities.map(e => [
                                <div className="flex items-center gap-2">
                                    {e.type === 'Customer' && <Building2 size={14} className="text-brand-400" />}
                                    {e.type === 'Employee' && <Users size={14} className="text-accent-violet" />}
                                    {e.type === 'Deal' && <Briefcase size={14} className="text-accent-emerald" />}
                                    {e.type === 'Invoice' && <DollarSign size={14} className="text-accent-amber" />}
                                    {e.type === 'Product' && <Layers size={14} className="text-accent-cyan" />}
                                    {e.type === 'Project' && <Activity size={14} className="text-accent-rose" />}
                                    {e.type === 'Document' && <Database size={14} className="text-grey-400" />}
                                    {e.type === 'Agent' && <Bot size={14} className="text-accent-violet" />}
                                    <span className="font-medium text-grey-200">{e.type}</span>
                                </div>,
                                <span className="font-mono">{e.count}</span>,
                                <Badge variant="info">{e.source}</Badge>,
                                <span className="text-grey-400">{e.connections}</span>,
                                <span className="text-xs text-grey-500">{e.lastUpdated}</span>,
                            ])}
                        />
                    </div>

                    {/* Graph Visualization Placeholder */}
                    <div className="gs-card p-5">
                        <h3 className="gs-section-title mb-4">Graph Visualization</h3>
                        <div className="relative bg-grey-900/50 rounded-lg p-8 min-h-[300px] flex items-center justify-center">
                            <div className="absolute inset-0 opacity-10" style={{
                                backgroundImage: 'radial-gradient(circle, #1a8fe0 1px, transparent 1px)',
                                backgroundSize: '24px 24px'
                            }} />
                            <div className="relative grid grid-cols-4 gap-8">
                                {['Customer', 'Employee', 'Deal', 'Product'].map((node, i) => (
                                    <div key={node} className="flex flex-col items-center gap-2">
                                        <div className={`w-16 h-16 rounded-full flex items-center justify-center border-2 
                      ${i === 0 ? 'border-brand-400 bg-brand-500/10' :
                                                i === 1 ? 'border-accent-violet bg-violet-500/10' :
                                                    i === 2 ? 'border-accent-emerald bg-emerald-500/10' :
                                                        'border-accent-cyan bg-cyan-500/10'}`}>
                                            {i === 0 && <Building2 size={24} className="text-brand-400" />}
                                            {i === 1 && <Users size={24} className="text-accent-violet" />}
                                            {i === 2 && <Briefcase size={24} className="text-accent-emerald" />}
                                            {i === 3 && <Layers size={24} className="text-accent-cyan" />}
                                        </div>
                                        <span className="text-xs font-medium text-grey-300">{node}</span>
                                        <span className="text-xs text-grey-500">{entities[i].count}</span>
                                    </div>
                                ))}
                            </div>
                        </div>
                    </div>
                </div>
            )}

            {activeTab === 'Semantic Search' && (
                <div className="space-y-6">
                    <div className="gs-card p-5">
                        <h3 className="gs-section-title mb-4 flex items-center gap-2"><Search size={16} /> Embedding-Based Semantic Search</h3>
                        <div className="flex gap-3 mb-6">
                            <input
                                type="text"
                                placeholder="Search across all entities... (e.g., 'customers at risk of churning with open deals')"
                                className="gs-input flex-1"
                                value={searchQuery}
                                onChange={e => setSearchQuery(e.target.value)}
                            />
                            <button className="gs-btn-primary">Search</button>
                        </div>
                        <div className="space-y-3">
                            {[
                                { result: 'Acme Corporation', type: 'Customer', relevance: 98, context: 'Enterprise account, 3 open deals ($480K), 2 overdue invoices, NPS declining', module: 'CRM → ERP → BI' },
                                { result: 'Globex Industries', type: 'Customer', relevance: 92, context: 'Enterprise account, renewal in 45 days, support tickets up 40%, engagement dropping', module: 'CRM → Support → BI' },
                                { result: 'Deal: Umbrella Corp Expansion', type: 'Deal', relevance: 87, context: '$240K expansion deal, stalled in negotiation for 28 days, champion went silent', module: 'CRM → Collaboration' },
                                { result: 'Sarah Chen', type: 'Employee', relevance: 82, context: 'Sales lead, owns 4 at-risk accounts, 2 deals past expected close date', module: 'HCM → CRM' },
                            ].map((r, i) => (
                                <div key={i} className="p-4 rounded-lg border border-grey-800 hover:border-brand-500/30 cursor-pointer transition-colors">
                                    <div className="flex items-center justify-between mb-2">
                                        <div className="flex items-center gap-2">
                                            <span className="text-sm font-medium text-grey-200">{r.result}</span>
                                            <Badge variant={r.type === 'Customer' ? 'info' : r.type === 'Deal' ? 'success' : 'purple'}>{r.type}</Badge>
                                        </div>
                                        <span className="text-xs text-accent-emerald font-mono">{r.relevance}% match</span>
                                    </div>
                                    <p className="text-xs text-grey-400">{r.context}</p>
                                    <p className="text-xs text-grey-600 mt-1">Source: {r.module}</p>
                                </div>
                            ))}
                        </div>
                    </div>
                </div>
            )}

            {activeTab === 'Relationships' && (
                <div className="gs-card p-5">
                    <h3 className="gs-section-title mb-4 flex items-center gap-2"><GitBranch size={16} /> Relationship Types</h3>
                    <DataTable
                        columns={['From Entity', 'Relationship', 'To Entity', 'Count']}
                        rows={relationships.map(r => [
                            <Badge variant="info">{r.from}</Badge>,
                            <span className="font-mono text-xs text-accent-amber px-2 py-0.5 bg-amber-500/10 rounded">{r.rel}</span>,
                            <Badge variant="purple">{r.to}</Badge>,
                            <span className="font-mono text-grey-300">{r.count}</span>,
                        ])}
                    />
                </div>
            )}

            {activeTab === 'Insights' && (
                <div className="gs-card p-5">
                    <h3 className="gs-section-title mb-4 flex items-center gap-2"><Zap size={16} /> Cross-Module Insights (AI-Inferred)</h3>
                    <div className="space-y-3">
                        {insights.map((ins, i) => (
                            <div key={i} className={`p-4 rounded-lg border ${ins.severity === 'Critical' ? 'border-accent-rose/30 bg-accent-rose/5' :
                                    ins.severity === 'Warning' ? 'border-accent-amber/30 bg-accent-amber/5' :
                                        'border-grey-800'
                                }`}>
                                <div className="flex items-center justify-between mb-2">
                                    <Badge variant={ins.severity === 'Critical' ? 'danger' : ins.severity === 'Warning' ? 'warning' : 'info'}>{ins.severity}</Badge>
                                    <span className="text-xs text-grey-500">Confidence: <span className="text-accent-emerald">{ins.confidence}%</span></span>
                                </div>
                                <p className="text-sm text-grey-200 mb-2">{ins.insight}</p>
                                <div className="flex gap-1">
                                    {ins.modules.map(m => (
                                        <span key={m} className="text-xs bg-grey-800/50 text-grey-400 px-2 py-0.5 rounded">{m}</span>
                                    ))}
                                </div>
                            </div>
                        ))}
                    </div>
                </div>
            )}

            {activeTab === 'Entity Linking' && (
                <div className="gs-card p-5">
                    <h3 className="gs-section-title mb-4 flex items-center gap-2"><Layers size={16} /> Entity Linking Pipeline</h3>
                    <div className="space-y-4">
                        {[
                            { stage: 'Entity Extraction', desc: 'NER + regex patterns across all modules', throughput: '84K entities/hour', accuracy: 96.4 },
                            { stage: 'Deduplication', desc: 'Fuzzy matching + embedding similarity', throughput: '42K comparisons/sec', accuracy: 98.2 },
                            { stage: 'Relationship Detection', desc: 'Pattern mining + co-occurrence analysis', throughput: '18K relations/hour', accuracy: 91.8 },
                            { stage: 'Entity Resolution', desc: 'Cross-module identity merge', throughput: '8K resolutions/hour', accuracy: 94.6 },
                            { stage: 'Inference Engine', desc: 'Graph neural network + rule-based reasoning', throughput: '2K inferences/hour', accuracy: 89.2 },
                        ].map((s, i) => (
                            <div key={i} className="p-4 rounded-lg border border-grey-800">
                                <div className="flex items-center justify-between mb-2">
                                    <div className="flex items-center gap-2">
                                        <div className="w-6 h-6 rounded-full bg-brand-500/20 flex items-center justify-center text-xs font-bold text-brand-400">{i + 1}</div>
                                        <span className="text-sm font-medium text-grey-200">{s.stage}</span>
                                    </div>
                                    <span className={`text-sm font-bold ${s.accuracy >= 95 ? 'text-accent-emerald' : s.accuracy >= 90 ? 'text-accent-amber' : 'text-grey-300'}`}>{s.accuracy}%</span>
                                </div>
                                <p className="text-xs text-grey-500 ml-8">{s.desc} · {s.throughput}</p>
                            </div>
                        ))}
                    </div>
                </div>
            )}

            {activeTab === 'Embeddings' && (
                <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
                    <div className="gs-card p-5">
                        <h3 className="gs-section-title mb-4 flex items-center gap-2"><Cpu size={16} /> Vector Embedding Store</h3>
                        <div className="space-y-3">
                            {[
                                { collection: 'customer_embeddings', vectors: '1.2M', dims: 1536, model: 'text-embedding-3-large', size: '14.2 GB' },
                                { collection: 'document_embeddings', vectors: '4.2M', dims: 1536, model: 'text-embedding-3-large', size: '48.6 GB' },
                                { collection: 'product_embeddings', vectors: '84K', dims: 768, model: 'text-embedding-3-small', size: '0.5 GB' },
                                { collection: 'deal_embeddings', vectors: '2.4M', dims: 1536, model: 'text-embedding-3-large', size: '28.4 GB' },
                            ].map(c => (
                                <div key={c.collection} className="p-3 rounded-lg bg-grey-800/30">
                                    <div className="flex items-center justify-between mb-1">
                                        <span className="font-mono text-xs text-accent-cyan">{c.collection}</span>
                                        <span className="text-xs text-grey-500">{c.size}</span>
                                    </div>
                                    <div className="flex gap-3 text-xs text-grey-400">
                                        <span>{c.vectors} vectors</span>
                                        <span>{c.dims}d</span>
                                        <span>{c.model}</span>
                                    </div>
                                </div>
                            ))}
                        </div>
                    </div>
                    <div className="gs-card p-5">
                        <h3 className="gs-section-title mb-4">Search Performance</h3>
                        <MiniSparkline data={[12, 14, 11, 15, 13, 16, 14, 18, 15, 17, 14, 16]} height={80} />
                        <div className="grid grid-cols-2 gap-3 mt-4 text-center">
                            <div className="p-3 bg-grey-800/30 rounded-lg">
                                <p className="text-lg font-bold text-grey-100">14ms</p>
                                <p className="text-xs text-grey-500">P50 Search Latency</p>
                            </div>
                            <div className="p-3 bg-grey-800/30 rounded-lg">
                                <p className="text-lg font-bold text-grey-100">28ms</p>
                                <p className="text-xs text-grey-500">P95 Search Latency</p>
                            </div>
                            <div className="p-3 bg-grey-800/30 rounded-lg">
                                <p className="text-lg font-bold text-grey-100">91.4 GB</p>
                                <p className="text-xs text-grey-500">Total Index Size</p>
                            </div>
                            <div className="p-3 bg-grey-800/30 rounded-lg">
                                <p className="text-lg font-bold text-accent-emerald">HNSW</p>
                                <p className="text-xs text-grey-500">Index Algorithm</p>
                            </div>
                        </div>
                    </div>
                </div>
            )}
        </div>
    );
}
