import { useState } from 'react';
import { PageHeader, StatCard, Badge, DataTable, Tabs, ProgressBar, MiniSparkline } from '../../components/ui';
import {
    Database, Layers, Activity, GitBranch, Shield,
    FileSearch, ArrowRight, CheckCircle, AlertTriangle,
    Clock, Eye, Cpu, RefreshCw, Lock
} from 'lucide-react';

const pipelines = [
    { name: 'ERP → Bronze', source: 'PostgreSQL (ERP)', method: 'CDC', frequency: 'Real-time', latency: '< 2s', records: '4.2M/day', status: 'Healthy' },
    { name: 'CRM → Bronze', source: 'PostgreSQL (CRM)', method: 'CDC', frequency: 'Real-time', latency: '< 3s', records: '1.8M/day', status: 'Healthy' },
    { name: 'HCM → Bronze', source: 'PostgreSQL (HCM)', method: 'CDC', frequency: 'Real-time', latency: '< 2s', records: '420K/day', status: 'Healthy' },
    { name: 'Events → Bronze', source: 'Kafka (Clickstream)', method: 'Streaming', frequency: 'Real-time', latency: '< 500ms', records: '84M/day', status: 'Healthy' },
    { name: 'Bronze → Silver', source: 'Delta Lake', method: 'Batch + Micro', frequency: '5 min', latency: '~30s', records: '90M/day', status: 'Healthy' },
    { name: 'Silver → Gold', source: 'Delta Lake', method: 'Batch', frequency: '1 hour', latency: '~5 min', records: '12M/day', status: 'Degraded' },
];

const tables = [
    { schema: 'gold', table: 'fact_revenue', rows: '248M', freshness: '12 min', quality: 99.4, pii: false, scd: 'N/A' },
    { schema: 'gold', table: 'fact_deals', rows: '42M', freshness: '18 min', quality: 98.8, pii: false, scd: 'N/A' },
    { schema: 'gold', table: 'fact_hr_events', rows: '18M', freshness: '25 min', quality: 99.1, pii: true, scd: 'N/A' },
    { schema: 'gold', table: 'dim_customers', rows: '1.2M', freshness: '1 hour', quality: 97.6, pii: true, scd: 'SCD2' },
    { schema: 'gold', table: 'dim_employees', rows: '342K', freshness: '1 hour', quality: 98.2, pii: true, scd: 'SCD2' },
    { schema: 'gold', table: 'dim_products', rows: '8.4K', freshness: '6 hours', quality: 99.8, pii: false, scd: 'SCD2' },
    { schema: 'silver', table: 'cleaned_transactions', rows: '1.4B', freshness: '5 min', quality: 96.2, pii: false, scd: 'N/A' },
    { schema: 'silver', table: 'cleaned_interactions', rows: '680M', freshness: '5 min', quality: 95.8, pii: true, scd: 'N/A' },
    { schema: 'bronze', table: 'raw_erp_events', rows: '4.8B', freshness: '2 sec', quality: 92.4, pii: true, scd: 'N/A' },
];

const lineage = [
    { source: 'erp.transactions', transforms: ['CDC Capture', 'Schema Validation', 'Dedup', 'Type Cast'], target: 'bronze.raw_erp_events' },
    { source: 'bronze.raw_erp_events', transforms: ['Null Fill', 'PII Mask', 'Join Dims', 'Aggregate'], target: 'silver.cleaned_transactions' },
    { source: 'silver.cleaned_transactions', transforms: ['SCD2 Merge', 'Business Rules', 'KPI Calc'], target: 'gold.fact_revenue' },
];

export function EnterpriseDataWarehouse() {
    const [activeTab, setActiveTab] = useState('Medallion');

    return (
        <div>
            <PageHeader
                title="Enterprise Data Warehouse"
                subtitle="Medallion architecture with CDC, streaming ETL, lineage & quality"
                actions={
                    <div className="flex gap-2">
                        <button className="gs-btn-secondary flex items-center gap-2"><FileSearch size={16} /> Data Catalog</button>
                        <button className="gs-btn-primary flex items-center gap-2"><RefreshCw size={16} /> Run Pipeline</button>
                    </div>
                }
            />

            <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-4 gap-4 mb-6">
                <StatCard label="Total Records" value="7.2B" change="90M/day ingested" changeType="neutral" icon={<Database size={18} />} />
                <StatCard label="Pipeline Health" value="5/6" change="1 degraded" changeType="negative" icon={<Activity size={18} />} />
                <StatCard label="Avg Quality Score" value="97.4%" change="+0.6% this week" changeType="positive" icon={<CheckCircle size={18} />} />
                <StatCard label="Gold Tables" value="6" change="SCD2 enabled" changeType="neutral" icon={<Layers size={18} />} />
            </div>

            <Tabs tabs={['Medallion', 'Pipelines', 'Data Catalog', 'Lineage', 'Quality', 'PII']} active={activeTab} onChange={setActiveTab} />

            {activeTab === 'Medallion' && (
                <div className="space-y-6">
                    {/* Medallion Architecture Visual */}
                    <div className="gs-card p-5">
                        <h3 className="gs-section-title mb-6">Bronze → Silver → Gold Architecture</h3>
                        <div className="grid grid-cols-3 gap-4">
                            {[
                                { tier: 'Bronze', color: 'border-amber-600/40 bg-amber-500/5', icon: <Database size={20} className="text-accent-amber" />, desc: 'Raw ingestion layer', tables: 4, records: '4.8B', freshness: 'Real-time' },
                                { tier: 'Silver', color: 'border-grey-500/40 bg-grey-500/5', icon: <Layers size={20} className="text-grey-400" />, desc: 'Cleaned & conformed', tables: 8, records: '2.1B', freshness: '5 min' },
                                { tier: 'Gold', color: 'border-accent-amber/40 bg-yellow-500/5', icon: <Activity size={20} className="text-yellow-400" />, desc: 'Business-ready facts & dims', tables: 6, records: '310M', freshness: '1 hour' },
                            ].map((tier, i) => (
                                <div key={tier.tier} className="relative">
                                    <div className={`p-5 rounded-lg border ${tier.color}`}>
                                        <div className="flex items-center gap-2 mb-3">
                                            {tier.icon}
                                            <h4 className="text-lg font-bold text-grey-200">{tier.tier}</h4>
                                        </div>
                                        <p className="text-xs text-grey-500 mb-4">{tier.desc}</p>
                                        <div className="space-y-2 text-sm">
                                            <div className="flex justify-between"><span className="text-grey-500">Tables</span><span className="text-grey-300">{tier.tables}</span></div>
                                            <div className="flex justify-between"><span className="text-grey-500">Records</span><span className="text-grey-300">{tier.records}</span></div>
                                            <div className="flex justify-between"><span className="text-grey-500">Freshness</span><span className="text-accent-emerald">{tier.freshness}</span></div>
                                        </div>
                                    </div>
                                    {i < 2 && (
                                        <div className="absolute top-1/2 -right-4 transform -translate-y-1/2 z-10">
                                            <ArrowRight size={16} className="text-grey-600" />
                                        </div>
                                    )}
                                </div>
                            ))}
                        </div>
                    </div>

                    {/* CDC + Streaming */}
                    <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
                        <div className="gs-card p-5">
                            <h3 className="gs-section-title mb-4 flex items-center gap-2"><RefreshCw size={16} /> Change Data Capture (CDC)</h3>
                            <div className="space-y-3">
                                {['ERP (PostgreSQL)', 'CRM (PostgreSQL)', 'HCM (PostgreSQL)'].map(src => (
                                    <div key={src} className="flex items-center justify-between p-3 bg-grey-800/30 rounded-lg">
                                        <div className="flex items-center gap-2">
                                            <CheckCircle size={14} className="text-accent-emerald" />
                                            <span className="text-sm text-grey-300">{src}</span>
                                        </div>
                                        <div className="flex items-center gap-2">
                                            <span className="text-xs text-grey-500">Debezium</span>
                                            <Badge variant="success">Real-time</Badge>
                                        </div>
                                    </div>
                                ))}
                            </div>
                        </div>
                        <div className="gs-card p-5">
                            <h3 className="gs-section-title mb-4 flex items-center gap-2"><Activity size={16} /> Streaming Throughput</h3>
                            <MiniSparkline data={[42, 58, 65, 72, 68, 82, 78, 88, 92, 86, 94, 90]} height={80} />
                            <div className="grid grid-cols-3 gap-3 mt-3 text-center">
                                <div><p className="text-lg font-bold text-grey-100">90M</p><p className="text-xs text-grey-500">Events/day</p></div>
                                <div><p className="text-lg font-bold text-grey-100">1.04K</p><p className="text-xs text-grey-500">Events/sec</p></div>
                                <div><p className="text-lg font-bold text-accent-emerald">&lt;500ms</p><p className="text-xs text-grey-500">E2E Latency</p></div>
                            </div>
                        </div>
                    </div>
                </div>
            )}

            {activeTab === 'Pipelines' && (
                <div className="gs-card p-5">
                    <h3 className="gs-section-title mb-4 flex items-center gap-2"><GitBranch size={16} /> ETL Pipeline Status</h3>
                    <DataTable
                        columns={['Pipeline', 'Source', 'Method', 'Frequency', 'Latency', 'Records/day', 'Status']}
                        rows={pipelines.map(p => [
                            <span className="font-medium text-grey-200">{p.name}</span>,
                            <span className="text-xs text-grey-400">{p.source}</span>,
                            <Badge variant={p.method === 'CDC' ? 'info' : p.method === 'Streaming' ? 'purple' : 'default'}>{p.method}</Badge>,
                            p.frequency,
                            <span className="font-mono text-xs">{p.latency}</span>,
                            p.records,
                            <Badge variant={p.status === 'Healthy' ? 'success' : 'warning'}>{p.status}</Badge>,
                        ])}
                    />
                </div>
            )}

            {activeTab === 'Data Catalog' && (
                <div className="gs-card p-5">
                    <h3 className="gs-section-title mb-4 flex items-center gap-2"><FileSearch size={16} /> Data Catalog & Schema Registry</h3>
                    <DataTable
                        columns={['Schema', 'Table', 'Rows', 'Freshness', 'Quality', 'PII', 'SCD Type']}
                        rows={tables.map(t => [
                            <Badge variant={t.schema === 'gold' ? 'warning' : t.schema === 'silver' ? 'default' : 'info'}>{t.schema}</Badge>,
                            <span className="font-mono text-xs text-accent-cyan">{t.table}</span>,
                            t.rows,
                            <span className="text-xs text-grey-400">{t.freshness}</span>,
                            <span className={t.quality >= 98 ? 'text-accent-emerald' : t.quality >= 95 ? 'text-accent-amber' : 'text-accent-rose'}>{t.quality}%</span>,
                            t.pii ? <Badge variant="danger">PII</Badge> : <span className="text-xs text-grey-600">—</span>,
                            t.scd !== 'N/A' ? <Badge variant="purple">{t.scd}</Badge> : <span className="text-xs text-grey-600">—</span>,
                        ])}
                    />
                </div>
            )}

            {activeTab === 'Lineage' && (
                <div className="gs-card p-5">
                    <h3 className="gs-section-title mb-4 flex items-center gap-2"><Eye size={16} /> Column-Level Data Lineage</h3>
                    <div className="space-y-6">
                        {lineage.map((l, i) => (
                            <div key={i} className="p-4 rounded-lg border border-grey-800">
                                <div className="flex items-center gap-3 mb-3">
                                    <span className="font-mono text-xs text-accent-cyan bg-grey-800/50 px-2 py-1 rounded">{l.source}</span>
                                    <div className="flex items-center gap-1">
                                        {l.transforms.map((t, j) => (
                                            <span key={j} className="flex items-center gap-1">
                                                <ArrowRight size={10} className="text-grey-600" />
                                                <span className="text-xs text-grey-400 bg-grey-800/30 px-2 py-0.5 rounded">{t}</span>
                                            </span>
                                        ))}
                                    </div>
                                    <ArrowRight size={10} className="text-grey-600" />
                                    <span className="font-mono text-xs text-accent-emerald bg-grey-800/50 px-2 py-1 rounded">{l.target}</span>
                                </div>
                            </div>
                        ))}
                    </div>
                </div>
            )}

            {activeTab === 'Quality' && (
                <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
                    <div className="gs-card p-5">
                        <h3 className="gs-section-title mb-4 flex items-center gap-2"><CheckCircle size={16} /> Data Quality Scores</h3>
                        <div className="space-y-3">
                            {tables.filter(t => t.schema === 'gold').map(t => (
                                <div key={t.table} className="p-3 rounded-lg bg-grey-800/30">
                                    <div className="flex justify-between mb-1">
                                        <span className="font-mono text-xs text-grey-300">{t.table}</span>
                                        <span className={`text-sm font-bold ${t.quality >= 99 ? 'text-accent-emerald' : t.quality >= 97 ? 'text-accent-amber' : 'text-accent-rose'}`}>{t.quality}%</span>
                                    </div>
                                    <ProgressBar value={t.quality} color={t.quality >= 99 ? 'bg-accent-emerald' : t.quality >= 97 ? 'bg-accent-amber' : 'bg-accent-rose'} size="sm" />
                                </div>
                            ))}
                        </div>
                    </div>
                    <div className="gs-card p-5">
                        <h3 className="gs-section-title mb-4">Quality Dimensions</h3>
                        <div className="space-y-3">
                            {[
                                { dim: 'Completeness', score: 98.4, desc: 'Non-null required fields' },
                                { dim: 'Accuracy', score: 97.8, desc: 'Values within expected ranges' },
                                { dim: 'Consistency', score: 96.2, desc: 'Cross-table referential integrity' },
                                { dim: 'Timeliness', score: 99.1, desc: 'Data freshness within SLA' },
                                { dim: 'Uniqueness', score: 99.6, desc: 'Duplicate detection rate' },
                            ].map(d => (
                                <div key={d.dim} className="flex items-center justify-between p-3 bg-grey-800/30 rounded-lg">
                                    <div>
                                        <p className="text-sm text-grey-300">{d.dim}</p>
                                        <p className="text-xs text-grey-500">{d.desc}</p>
                                    </div>
                                    <span className={`text-sm font-bold ${d.score >= 98 ? 'text-accent-emerald' : 'text-accent-amber'}`}>{d.score}%</span>
                                </div>
                            ))}
                        </div>
                    </div>
                </div>
            )}

            {activeTab === 'PII' && (
                <div className="gs-card p-5">
                    <h3 className="gs-section-title mb-4 flex items-center gap-2"><Lock size={16} /> PII Classification & Masking</h3>
                    <DataTable
                        columns={['Table', 'Column', 'PII Type', 'Classification', 'Masking Strategy', 'Status']}
                        rows={[
                            ['dim_customers', 'email', 'Email', 'Confidential', 'Hash (SHA-256)', 'Enforced'],
                            ['dim_customers', 'phone', 'Phone', 'Confidential', 'Partial mask (***-1234)', 'Enforced'],
                            ['dim_customers', 'ssn', 'SSN', 'Restricted', 'Full redaction', 'Enforced'],
                            ['dim_employees', 'salary', 'Financial', 'Restricted', 'Role-based access', 'Enforced'],
                            ['dim_employees', 'home_address', 'Address', 'Confidential', 'Tokenization', 'Enforced'],
                            ['fact_hr_events', 'actor_email', 'Email', 'Internal', 'Hash (SHA-256)', 'Enforced'],
                        ].map(r => [
                            <span className="font-mono text-xs text-accent-cyan">{r[0]}</span>,
                            <span className="font-mono text-xs text-grey-300">{r[1]}</span>,
                            <Badge variant="danger">{r[2]}</Badge>,
                            <Badge variant={r[3] === 'Restricted' ? 'danger' : r[3] === 'Confidential' ? 'warning' : 'info'}>{r[3]}</Badge>,
                            <span className="text-xs text-grey-400">{r[4]}</span>,
                            <Badge variant="success">{r[5]}</Badge>,
                        ])}
                    />
                </div>
            )}
        </div>
    );
}
