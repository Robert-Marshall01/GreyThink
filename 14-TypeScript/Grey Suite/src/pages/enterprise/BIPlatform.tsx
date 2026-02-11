import { useState } from 'react';
import {
    BarChart3, Database, Code2, Layers, FileText, Search, Settings,
    Download, RefreshCw, CheckCircle2, AlertTriangle, Zap, Users,
    Globe, Lock, Eye, GitBranch, Clock, Filter, Plus, Table
} from 'lucide-react';
import { PageHeader, StatCard, DataTable, Badge, Tabs, ProgressBar, MiniSparkline } from '../../components/ui';

export function BIPlatform() {
    const [activeTab, setActiveTab] = useState('Data Model');

    const tabs = ['Data Model', 'Expression Engine', 'Datasets', 'Semantic Layer', 'Report Builder'];

    return (
        <div className="p-6 space-y-6">
            <PageHeader
                title="Full BI Platform"
                subtitle="Data modeling, DAX-style expression language, dataset publishing, semantic models, and report builder"
                actions={
                    <div className="flex gap-2">
                        <button className="gs-btn-secondary">
                            <Download size={14} /> Export
                        </button>
                        <button className="gs-btn-primary">
                            <Plus size={14} /> New Report
                        </button>
                    </div>
                }
            />

            <div className="grid grid-cols-4 gap-4">
                <StatCard label="Published Reports" value="842" change="+64 this month" changeType="positive" icon={<BarChart3 size={18} />} />
                <StatCard label="Semantic Models" value="48" change="+4" changeType="positive" icon={<Layers size={18} />} />
                <StatCard label="Query Performance" value="1.2s" change="-0.3s" changeType="positive" icon={<Zap size={18} />} />
                <StatCard label="Active Viewers" value="4,821" change="+18%" changeType="positive" icon={<Users size={18} />} />
            </div>

            <Tabs tabs={tabs} active={activeTab} onChange={setActiveTab} />

            {activeTab === 'Data Model' && (
                <div className="space-y-6">
                    <div className="gs-card p-6">
                        <h3 className="text-sm font-semibold text-grey-300 mb-4">Data Model Designer</h3>

                        {/* Schema Visualization */}
                        <div className="bg-grey-900 rounded-lg p-6 border border-grey-700/50 min-h-[300px]" style={{ backgroundImage: 'radial-gradient(circle, #374151 1px, transparent 1px)', backgroundSize: '24px 24px' }}>
                            <div className="grid grid-cols-4 gap-6">
                                {[
                                    { table: 'fact_Sales', columns: ['sale_id (PK)', 'date_key (FK)', 'product_key (FK)', 'customer_key (FK)', 'quantity', 'unit_price', 'discount', 'total_amount'], type: 'Fact', rows: '48.2M' },
                                    { table: 'dim_Date', columns: ['date_key (PK)', 'date', 'year', 'quarter', 'month', 'week', 'day_of_week', 'is_holiday'], type: 'Dimension', rows: '3,650' },
                                    { table: 'dim_Product', columns: ['product_key (PK)', 'product_name', 'category', 'subcategory', 'brand', 'unit_cost', 'launch_date'], type: 'Dimension', rows: '8,420' },
                                    { table: 'dim_Customer', columns: ['customer_key (PK)', 'name', 'segment', 'region', 'country', 'account_id', 'tier', 'lifetime_value'], type: 'Dimension', rows: '284K' },
                                ].map(t => (
                                    <div key={t.table} className="bg-grey-800/80 rounded-lg border border-grey-700/50 overflow-hidden">
                                        <div className={`px-3 py-2 ${t.type === 'Fact' ? 'bg-brand-500/10 border-b border-brand-500/30' : 'bg-violet-500/10 border-b border-violet-500/30'}`}>
                                            <div className="flex items-center justify-between">
                                                <span className="text-xs font-semibold text-grey-200">{t.table}</span>
                                                <Badge variant={t.type === 'Fact' ? 'info' : 'purple'}>{t.type}</Badge>
                                            </div>
                                            <span className="text-[10px] text-grey-500">{t.rows} rows</span>
                                        </div>
                                        <div className="p-2">
                                            {t.columns.map(c => (
                                                <div key={c} className="text-[10px] font-mono text-grey-400 py-0.5 px-1 hover:bg-grey-700/50 rounded">
                                                    {c.includes('PK') ? <span className="text-amber-400">{c}</span> :
                                                        c.includes('FK') ? <span className="text-brand-400">{c}</span> :
                                                            c}
                                                </div>
                                            ))}
                                        </div>
                                    </div>
                                ))}
                            </div>
                        </div>

                        <div className="mt-4 grid grid-cols-3 gap-4">
                            {[
                                { label: 'Star Schema', desc: 'Fact table surrounded by dimension tables', status: 'Active' },
                                { label: 'Relationships', desc: '1:Many with referential integrity', status: '12 defined' },
                                { label: 'Incremental Refresh', desc: 'Only load new/changed data partitions', status: 'Enabled' },
                            ].map(f => (
                                <div key={f.label} className="bg-grey-800/50 rounded-lg p-3 border border-grey-700/50">
                                    <p className="text-sm font-semibold text-grey-200 mb-1">{f.label}</p>
                                    <p className="text-xs text-grey-500 mb-2">{f.desc}</p>
                                    <Badge variant="info">{f.status}</Badge>
                                </div>
                            ))}
                        </div>
                    </div>
                </div>
            )}

            {activeTab === 'Expression Engine' && (
                <div className="space-y-6">
                    <div className="gs-card p-6">
                        <h3 className="text-sm font-semibold text-grey-300 mb-4">GSDAX — Grey Suite Data Analysis Expressions</h3>
                        <p className="text-xs text-grey-500 mb-4">A DAX-inspired expression language for creating calculated columns, measures, and KPIs</p>

                        <div className="grid grid-cols-2 gap-6">
                            <div>
                                <h4 className="text-xs font-semibold text-grey-400 uppercase mb-3">Measure Definitions</h4>
                                <div className="bg-grey-900 rounded-lg p-4 font-mono text-xs text-grey-400 space-y-4">
                                    <div>
                                        <p className="text-brand-400">// Revenue Measures</p>
                                        <pre>{`Total Revenue := 
  SUM(fact_Sales[total_amount])

Revenue YoY Growth := 
  VAR CurrentYear = [Total Revenue]
  VAR PriorYear = 
    CALCULATE(
      [Total Revenue],
      DATEADD(dim_Date[date], -1, YEAR)
    )
  RETURN
    DIVIDE(CurrentYear - PriorYear, PriorYear)`}</pre>
                                    </div>
                                    <div>
                                        <p className="text-brand-400">// Customer Metrics</p>
                                        <pre>{`Customer LTV := 
  CALCULATE(
    SUMX(
      RELATEDTABLE(fact_Sales),
      fact_Sales[total_amount]
    ),
    ALL(dim_Date)
  )

Active Customers := 
  DISTINCTCOUNT(
    FILTER(
      fact_Sales,
      fact_Sales[date_key] >= TODAY() - 90
    )[customer_key]
  )`}</pre>
                                    </div>
                                </div>
                            </div>

                            <div>
                                <h4 className="text-xs font-semibold text-grey-400 uppercase mb-3">Function Categories</h4>
                                <div className="space-y-2">
                                    {[
                                        { category: 'Aggregation', functions: 'SUM, AVERAGE, COUNT, MIN, MAX, DISTINCTCOUNT, MEDIAN', count: 28 },
                                        { category: 'Filter', functions: 'CALCULATE, FILTER, ALL, ALLEXCEPT, REMOVEFILTERS, KEEPFILTERS', count: 18 },
                                        { category: 'Time Intelligence', functions: 'DATEADD, DATESYTD, SAMEPERIODLASTYEAR, TOTALYTD, PARALLELPERIOD', count: 24 },
                                        { category: 'Table', functions: 'SUMMARIZE, ADDCOLUMNS, SELECTCOLUMNS, CROSSJOIN, UNION, EXCEPT', count: 22 },
                                        { category: 'Text', functions: 'CONCATENATE, FORMAT, LEFT, RIGHT, SEARCH, REPLACE, UPPER', count: 16 },
                                        { category: 'Logical', functions: 'IF, SWITCH, AND, OR, COALESCE, IFERROR, ISBLANK', count: 12 },
                                        { category: 'Statistical', functions: 'STDEV, PERCENTILE, RANK, TOPN, SAMPLE, GEOMEAN', count: 14 },
                                        { category: 'Window', functions: 'WINDOW, OFFSET, INDEX, MOVINGAVERAGE, RUNNINGTOTAL', count: 8 },
                                    ].map(c => (
                                        <div key={c.category} className="bg-grey-800/50 rounded-lg p-3 border border-grey-700/50">
                                            <div className="flex items-center justify-between mb-1">
                                                <span className="text-sm font-semibold text-grey-200">{c.category}</span>
                                                <Badge variant="default">{c.count} functions</Badge>
                                            </div>
                                            <p className="text-[10px] text-grey-500 font-mono">{c.functions}</p>
                                        </div>
                                    ))}
                                </div>
                            </div>
                        </div>
                    </div>

                    <div className="gs-card p-6">
                        <h3 className="text-sm font-semibold text-grey-300 mb-4">Query Engine Performance</h3>
                        <div className="grid grid-cols-3 gap-4">
                            {[
                                { metric: 'Query Cache Hit Rate', data: [82, 84, 86, 85, 88, 89, 91], unit: '%', color: '#1a8fe0' },
                                { metric: 'Avg Query Duration', data: [2.1, 1.8, 1.6, 1.5, 1.3, 1.2, 1.2], unit: 's', color: '#10b981' },
                                { metric: 'Concurrent Queries', data: [120, 142, 168, 154, 180, 196, 184], unit: '', color: '#f59e0b' },
                            ].map(m => (
                                <div key={m.metric}>
                                    <p className="text-xs text-grey-500 mb-2">{m.metric}</p>
                                    <MiniSparkline data={m.data} color={m.color} height={40} />
                                    <p className="text-lg font-bold text-grey-200 mt-1">{m.data[m.data.length - 1]}{m.unit}</p>
                                </div>
                            ))}
                        </div>
                    </div>
                </div>
            )}

            {activeTab === 'Datasets' && (
                <div className="space-y-6">
                    <DataTable
                        columns={['Dataset', 'Model', 'Tables', 'Size', 'Refresh', 'Last Refresh', 'Consumers', 'Status']}
                        rows={[
                            ['Enterprise Sales', 'Star Schema', '8 tables', '4.2 GB', 'Hourly', '42m ago', '284 reports', <Badge variant="success">Active</Badge>],
                            ['HR Analytics', 'Snowflake Schema', '12 tables', '1.8 GB', 'Daily 6am', '4h ago', '64 reports', <Badge variant="success">Active</Badge>],
                            ['Financial Metrics', 'Star Schema', '6 tables', '2.4 GB', 'Every 15 min', '8m ago', '142 reports', <Badge variant="success">Active</Badge>],
                            ['Customer 360', 'Star Schema', '14 tables', '8.6 GB', 'Hourly', '18m ago', '96 reports', <Badge variant="success">Active</Badge>],
                            ['Supply Chain', 'Galaxy Schema', '18 tables', '12.4 GB', 'Every 30 min', '12m ago', '48 reports', <Badge variant="success">Active</Badge>],
                            ['Product Analytics', 'Star Schema', '10 tables', '6.2 GB', 'Real-time (DirectQuery)', 'Live', '84 reports', <Badge variant="info">DirectQuery</Badge>],
                        ]}
                    />

                    <div className="gs-card p-6">
                        <h3 className="text-sm font-semibold text-grey-300 mb-4">Dataset Refresh Strategy</h3>
                        <div className="bg-grey-900 rounded-lg p-4 font-mono text-xs text-grey-400">
                            <pre>{`// Refresh Modes
1. Import Mode (Scheduled)
   - Full data loaded into in-memory columnar store
   - Compression ratio: ~10:1
   - Incremental refresh: partition-based (date ranges)
   - Max dataset size: 100 GB (Premium)

2. DirectQuery Mode (Real-time)
   - Queries pass through to source database
   - No data cached — always live
   - Performance depends on source query speed
   - Best for: real-time dashboards, large datasets

3. Composite Mode (Hybrid)
   - Mix import + DirectQuery per table
   - Dimensions: imported (fast filter/slicer)
   - Facts: DirectQuery (always current)
   - Best of both worlds

4. Streaming Mode (Push)
   - Events pushed via REST API
   - 1-second refresh latency
   - Used for: IoT, live monitoring, alerts`}</pre>
                        </div>
                    </div>
                </div>
            )}

            {activeTab === 'Semantic Layer' && (
                <div className="space-y-6">
                    <div className="gs-card p-6">
                        <h3 className="text-sm font-semibold text-grey-300 mb-4">Semantic Model Registry</h3>
                        <p className="text-xs text-grey-500 mb-4">Business-friendly abstraction layer over raw data models — single source of truth for metrics</p>

                        <DataTable
                            columns={['Semantic Model', 'Domain', 'Metrics', 'Dimensions', 'Consumers', 'Owner', 'Certified']}
                            rows={[
                                ['Revenue Analytics', 'Finance', '42 measures', '18 dimensions', '284 reports + 12 apps', 'Finance Team', <Badge variant="success">✓ Certified</Badge>],
                                ['Customer Intelligence', 'CRM', '28 measures', '14 dimensions', '96 reports + 4 apps', 'CX Team', <Badge variant="success">✓ Certified</Badge>],
                                ['People Analytics', 'HR', '24 measures', '12 dimensions', '48 reports + 2 apps', 'HR Analytics', <Badge variant="success">✓ Certified</Badge>],
                                ['Operations', 'Ops', '36 measures', '22 dimensions', '64 reports + 6 apps', 'Ops Team', <Badge variant="warning">Pending Review</Badge>],
                                ['Product Engagement', 'Product', '18 measures', '10 dimensions', '42 reports + 3 apps', 'Product Analytics', <Badge variant="success">✓ Certified</Badge>],
                            ]}
                        />
                    </div>

                    <div className="grid grid-cols-2 gap-6">
                        <div className="gs-card p-6">
                            <h3 className="text-sm font-semibold text-grey-300 mb-4">Metric Definition (Revenue Analytics)</h3>
                            <div className="bg-grey-900 rounded-lg p-4 font-mono text-xs text-grey-400">
                                <pre>{`// Semantic Model: Revenue Analytics
model RevenueSemantic {
  // Canonical Metrics
  metric TotalRevenue {
    expression: SUM(fact_Sales.total_amount)
    format: "$#,##0"
    certification: "finance_approved"
  }

  metric MRR {
    expression: SUM(
      FILTER(fact_Sales, is_recurring = TRUE)
        .total_amount
    )
    granularity: monthly
    format: "$#,##0"
  }

  metric NetRetention {
    expression: 
      current_mrr / prior_period_mrr * 100
    benchmark: "> 120% is excellent"
    format: "0.0%"
  }

  // Dimension Hierarchies
  dimension Date {
    hierarchy: Year > Quarter > Month > Week > Day
    fiscal_year_start: "February"
  }

  dimension Geography {
    hierarchy: Region > Country > State > City
  }
}`}</pre>
                            </div>
                        </div>

                        <div className="gs-card p-6">
                            <h3 className="text-sm font-semibold text-grey-300 mb-4">Semantic Layer Benefits</h3>
                            <div className="space-y-3">
                                {[
                                    { benefit: 'Single Source of Truth', desc: 'One definition of "Revenue" across all reports, dashboards, and apps' },
                                    { benefit: 'Business Glossary', desc: 'Natural language descriptions for every metric and dimension' },
                                    { benefit: 'Row-Level Security', desc: 'Data access rules defined once, enforced everywhere' },
                                    { benefit: 'Self-Service', desc: 'Business users explore data without knowing SQL' },
                                    { benefit: 'AI-Ready', desc: 'LLMs query semantic layer with natural language (Text-to-SQL)' },
                                    { benefit: 'Governance', desc: 'Certified metrics vs. exploratory — trust badges on reports' },
                                    { benefit: 'Performance', desc: 'Semantic layer enables query optimization + caching' },
                                    { benefit: 'Lineage', desc: 'Trace any metric back to its source tables and transformations' },
                                ].map(b => (
                                    <div key={b.benefit} className="py-2 border-b border-grey-800/50 last:border-0">
                                        <p className="text-sm text-grey-300">{b.benefit}</p>
                                        <p className="text-xs text-grey-500 mt-0.5">{b.desc}</p>
                                    </div>
                                ))}
                            </div>
                        </div>
                    </div>
                </div>
            )}

            {activeTab === 'Report Builder' && (
                <div className="space-y-6">
                    <div className="gs-card p-6">
                        <h3 className="text-sm font-semibold text-grey-300 mb-4">Visual Report Builder</h3>

                        <div className="bg-grey-900 rounded-lg border border-grey-700/50 overflow-hidden">
                            {/* Toolbar */}
                            <div className="flex items-center gap-1 px-3 py-2 bg-grey-800/80 border-b border-grey-700/50">
                                <button className="p-1.5 rounded hover:bg-grey-700 text-grey-400 text-xs">Bar Chart</button>
                                <button className="p-1.5 rounded hover:bg-grey-700 text-grey-400 text-xs">Line Chart</button>
                                <button className="p-1.5 rounded hover:bg-grey-700 text-grey-400 text-xs">Pie Chart</button>
                                <button className="p-1.5 rounded hover:bg-grey-700 text-grey-400 text-xs">Table</button>
                                <button className="p-1.5 rounded hover:bg-grey-700 text-grey-400 text-xs">KPI Card</button>
                                <button className="p-1.5 rounded hover:bg-grey-700 text-grey-400 text-xs">Map</button>
                                <button className="p-1.5 rounded hover:bg-grey-700 text-grey-400 text-xs">Slicer</button>
                                <div className="flex-1" />
                                <button className="gs-btn-ghost text-xs"><Eye size={12} /> Reading View</button>
                            </div>

                            {/* Canvas */}
                            <div className="p-4 grid grid-cols-3 gap-3 min-h-[250px]">
                                {/* KPI Cards */}
                                <div className="col-span-3 grid grid-cols-4 gap-3">
                                    {[
                                        { label: 'Total Revenue', value: '$48.2M', trend: '+12%' },
                                        { label: 'Active Customers', value: '8,421', trend: '+6%' },
                                        { label: 'Avg Deal Size', value: '$24.8K', trend: '+3%' },
                                        { label: 'Win Rate', value: '34.2%', trend: '-1.2%' },
                                    ].map(k => (
                                        <div key={k.label} className="bg-grey-800/50 rounded p-3 border border-grey-700/50">
                                            <p className="text-[10px] text-grey-500">{k.label}</p>
                                            <p className="text-lg font-bold text-grey-200">{k.value}</p>
                                            <p className={`text-[10px] ${k.trend.startsWith('+') ? 'text-emerald-400' : 'text-rose-400'}`}>{k.trend}</p>
                                        </div>
                                    ))}
                                </div>

                                {/* Chart area placeholder */}
                                <div className="col-span-2 bg-grey-800/30 rounded-lg p-3 border border-grey-700/30 flex items-center justify-center h-32">
                                    <div className="text-center">
                                        <BarChart3 size={24} className="text-grey-600 mx-auto mb-1" />
                                        <p className="text-xs text-grey-500">Revenue by Region (Bar Chart)</p>
                                    </div>
                                </div>

                                <div className="bg-grey-800/30 rounded-lg p-3 border border-grey-700/30 flex items-center justify-center h-32">
                                    <div className="text-center">
                                        <Table size={24} className="text-grey-600 mx-auto mb-1" />
                                        <p className="text-xs text-grey-500">Top 10 Accounts (Table)</p>
                                    </div>
                                </div>
                            </div>
                        </div>
                    </div>

                    <div className="grid grid-cols-3 gap-4">
                        <div className="gs-card p-4">
                            <h4 className="text-xs font-semibold text-grey-400 uppercase mb-3">Visualization Types</h4>
                            <div className="space-y-1">
                                {[
                                    'Bar / Column Chart', 'Line / Area Chart', 'Pie / Donut Chart',
                                    'Scatter Plot', 'Treemap', 'Waterfall Chart', 'Gauge / KPI',
                                    'Matrix / Pivot Table', 'Map (Choropleth)', 'Funnel Chart',
                                    'Decomposition Tree', 'Key Influencers (AI)',
                                ].map(v => (
                                    <div key={v} className="text-xs px-2 py-1.5 bg-grey-800/50 rounded text-grey-400">{v}</div>
                                ))}
                            </div>
                        </div>

                        <div className="gs-card p-4">
                            <h4 className="text-xs font-semibold text-grey-400 uppercase mb-3">Interactivity</h4>
                            <div className="space-y-2">
                                {[
                                    { feature: 'Cross-filtering', desc: 'Click one visual to filter all others' },
                                    { feature: 'Drill-through', desc: 'Navigate to detail page with context' },
                                    { feature: 'Drill-down', desc: 'Expand hierarchy levels in visuals' },
                                    { feature: 'Bookmarks', desc: 'Save filter/slicer state as shareable view' },
                                    { feature: 'Tooltips', desc: 'Rich hover cards with additional data' },
                                    { feature: 'Q&A (NLP)', desc: 'Ask questions in natural language' },
                                ].map(f => (
                                    <div key={f.feature} className="py-1">
                                        <p className="text-xs text-grey-300">{f.feature}</p>
                                        <p className="text-[10px] text-grey-500">{f.desc}</p>
                                    </div>
                                ))}
                            </div>
                        </div>

                        <div className="gs-card p-4">
                            <h4 className="text-xs font-semibold text-grey-400 uppercase mb-3">Distribution</h4>
                            <div className="space-y-2">
                                {[
                                    { method: 'Workspace', desc: 'Share with team workspace members' },
                                    { method: 'App', desc: 'Package reports as self-service app' },
                                    { method: 'Embed', desc: 'iframe in portals and custom apps' },
                                    { method: 'Email Subscription', desc: 'Scheduled PDF/PNG delivery' },
                                    { method: 'Teams/Chat', desc: 'Inline preview in chat channels' },
                                    { method: 'Public Web', desc: 'Anonymous access (opt-in)' },
                                ].map(d => (
                                    <div key={d.method} className="py-1">
                                        <p className="text-xs text-grey-300">{d.method}</p>
                                        <p className="text-[10px] text-grey-500">{d.desc}</p>
                                    </div>
                                ))}
                            </div>
                        </div>
                    </div>
                </div>
            )}
        </div>
    );
}
