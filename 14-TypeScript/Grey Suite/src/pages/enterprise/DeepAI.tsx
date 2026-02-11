import { useState } from 'react';
import {
    Brain, MessageSquare, FileText, BarChart3, Search, Sparkles,
    Zap, Target, TrendingUp, Mail, Video, Calendar, ClipboardList,
    Shield, Eye, Layers, RefreshCw, AlertTriangle, CheckCircle2,
    Wand2, ArrowRight
} from 'lucide-react';
import { PageHeader, StatCard, DataTable, Badge, Tabs, ProgressBar, MiniSparkline } from '../../components/ui';

export function DeepAI() {
    const [activeTab, setActiveTab] = useState('Copilot');

    const tabs = ['Copilot', 'Summarization', 'AI Tasks', 'AI Insights', 'AI Search'];

    return (
        <div className="p-6 space-y-6">
            <PageHeader
                title="Deep AI Integration Across Modules"
                subtitle="Context-aware copilot, meeting/email summarization, AI task creation, dashboard insights, and unified AI search"
                actions={
                    <div className="flex gap-2">
                        <button className="gs-btn-secondary">
                            <Shield size={14} /> Trust Center
                        </button>
                        <button className="gs-btn-primary">
                            <Brain size={14} /> AI Settings
                        </button>
                    </div>
                }
            />

            <div className="grid grid-cols-4 gap-4">
                <StatCard label="AI Interactions (24h)" value="48,291" change="+22%" changeType="positive" icon={<Brain size={18} />} />
                <StatCard label="Drafts Generated" value="12,840" change="+18%" changeType="positive" icon={<Wand2 size={18} />} />
                <StatCard label="Time Saved (est.)" value="2,841h" change="This month" changeType="positive" icon={<Zap size={18} />} />
                <StatCard label="Accuracy Score" value="94.2%" change="+1.4%" changeType="positive" icon={<Target size={18} />} />
            </div>

            <Tabs tabs={tabs} active={activeTab} onChange={setActiveTab} />

            {activeTab === 'Copilot' && (
                <div className="space-y-6">
                    <div className="gs-card p-6">
                        <h3 className="text-sm font-semibold text-grey-300 mb-4">Context-Aware Drafting Engine</h3>
                        <p className="text-xs text-grey-500 mb-4">AI copilot embedded in every module — drafts, edits, and suggests with full context awareness</p>

                        <div className="grid grid-cols-2 gap-6">
                            <div>
                                <h4 className="text-xs font-semibold text-grey-400 uppercase mb-3">Module-Specific Copilot Actions</h4>
                                <div className="space-y-2">
                                    {[
                                        { module: 'Email', actions: ['Draft reply', 'Summarize thread', 'Adjust tone', 'Extract action items'], icon: <Mail size={14} /> },
                                        { module: 'Documents', actions: ['Continue writing', 'Rewrite section', 'Generate outline', 'Add citations'], icon: <FileText size={14} /> },
                                        { module: 'Chat', actions: ['Suggest response', 'Translate message', 'Create thread summary', 'Draft announcement'], icon: <MessageSquare size={14} /> },
                                        { module: 'CRM', actions: ['Draft proposal', 'Summarize account', 'Predict close date', 'Generate follow-up'], icon: <Target size={14} /> },
                                        { module: 'HCM', actions: ['Draft job description', 'Summarize candidate', 'Generate review', 'Suggest development plan'], icon: <ClipboardList size={14} /> },
                                    ].map(m => (
                                        <div key={m.module} className="bg-grey-800/50 rounded-lg p-3 border border-grey-700/50">
                                            <div className="flex items-center gap-2 mb-2">
                                                <span className="text-brand-400">{m.icon}</span>
                                                <span className="text-sm font-semibold text-grey-200">{m.module}</span>
                                            </div>
                                            <div className="flex flex-wrap gap-1">
                                                {m.actions.map(a => (
                                                    <span key={a} className="text-[10px] px-2 py-1 bg-grey-700/50 rounded text-grey-400">{a}</span>
                                                ))}
                                            </div>
                                        </div>
                                    ))}
                                </div>
                            </div>

                            <div>
                                <h4 className="text-xs font-semibold text-grey-400 uppercase mb-3">Context Pipeline</h4>
                                <div className="bg-grey-900 rounded-lg p-4 font-mono text-xs text-grey-400">
                                    <pre>{`// AI Context Assembly Pipeline
interface CopilotContext {
  // 1. Immediate Context
  current_document: string;
  cursor_position: number;
  selected_text?: string;

  // 2. Module Context
  module: 'email' | 'docs' | 'chat' | ...;
  module_state: ModuleState;  // draft, compose, etc.
  related_items: RelatedItem[];

  // 3. User Context
  role: string;
  department: string;
  writing_style: StyleProfile; // learned
  preferences: UserPreferences;

  // 4. Organizational Context
  recent_interactions: Interaction[];
  team_context: TeamContext;
  org_knowledge: RAGResult[];

  // 5. Privacy Controls
  sensitivity_level: SensitivityLabel;
  data_boundary: 'tenant' | 'region';
  pii_filter: boolean;
}

// Model Routing:
// Simple tasks → GPT-4o-mini (low latency)
// Complex tasks → GPT-4o / Claude Sonnet
// Code tasks → Codex / Claude
// All queries scoped to tenant data boundary`}</pre>
                                </div>
                            </div>
                        </div>
                    </div>
                </div>
            )}

            {activeTab === 'Summarization' && (
                <div className="space-y-6">
                    <div className="gs-card p-6">
                        <h3 className="text-sm font-semibold text-grey-300 mb-4">Meeting & Email Summarization Engine</h3>

                        <div className="grid grid-cols-2 gap-6">
                            <div>
                                <h4 className="text-xs font-semibold text-grey-400 uppercase mb-3">Meeting Intelligence</h4>
                                <div className="space-y-3">
                                    {[
                                        { meeting: 'Product Roadmap Review', duration: '45 min', participants: 8, summaryReady: true, actions: 6, decisions: 3 },
                                        { meeting: 'Sprint 14 Planning', duration: '60 min', participants: 12, summaryReady: true, actions: 14, decisions: 5 },
                                        { meeting: 'Customer Success Sync', duration: '30 min', participants: 4, summaryReady: true, actions: 4, decisions: 2 },
                                        { meeting: 'Board Prep (Confidential)', duration: '90 min', participants: 6, summaryReady: false, actions: 0, decisions: 0 },
                                    ].map(m => (
                                        <div key={m.meeting} className="bg-grey-800/50 rounded-lg p-3 border border-grey-700/50">
                                            <div className="flex items-center justify-between mb-2">
                                                <span className="text-sm font-semibold text-grey-200">{m.meeting}</span>
                                                {m.summaryReady ? <Badge variant="success">Summary Ready</Badge> : <Badge variant="warning">Processing</Badge>}
                                            </div>
                                            <div className="flex items-center gap-4 text-xs text-grey-500">
                                                <span>{m.duration}</span>
                                                <span>{m.participants} people</span>
                                                {m.summaryReady && <span className="text-brand-400">{m.actions} action items</span>}
                                                {m.summaryReady && <span className="text-emerald-400">{m.decisions} decisions</span>}
                                            </div>
                                        </div>
                                    ))}
                                </div>

                                <div className="mt-4 bg-grey-900 rounded-lg p-4 text-xs text-grey-400">
                                    <p className="text-grey-300 font-semibold mb-2">Sample Meeting Summary</p>
                                    <div className="space-y-2">
                                        <p><span className="text-brand-400 font-semibold">Key Decisions:</span></p>
                                        <p>1. Ship v2.4 by Feb 28 with reduced scope</p>
                                        <p>2. Hire 2 additional SREs for Q2</p>
                                        <p>3. Sunset legacy API by March 31</p>
                                        <p className="mt-2"><span className="text-amber-400 font-semibold">Action Items:</span></p>
                                        <p>→ @sarah: Update roadmap by EOD</p>
                                        <p>→ @james: Draft migration guide</p>
                                        <p>→ @team: Review RFC-142 before next sync</p>
                                    </div>
                                </div>
                            </div>

                            <div>
                                <h4 className="text-xs font-semibold text-grey-400 uppercase mb-3">Email Intelligence</h4>
                                <div className="space-y-3">
                                    {[
                                        { capability: 'Thread Summarization', desc: 'Collapse long email chains into key points', usage: '8,420/day' },
                                        { capability: 'Priority Detection', desc: 'Classify urgency based on content + sender', usage: '12,840/day' },
                                        { capability: 'Action Item Extraction', desc: 'Identify tasks with deadlines and owners', usage: '4,284/day' },
                                        { capability: 'Sentiment Analysis', desc: 'Flag negative or escalation-risk emails', usage: '12,840/day' },
                                        { capability: 'Reply Suggestions', desc: '3 contextual draft responses per email', usage: '6,420/day' },
                                        { capability: 'Meeting Detection', desc: 'Extract scheduling intent, suggest slots', usage: '1,842/day' },
                                    ].map(c => (
                                        <div key={c.capability} className="flex items-center justify-between py-2 border-b border-grey-800/50 last:border-0">
                                            <div>
                                                <p className="text-sm text-grey-300">{c.capability}</p>
                                                <p className="text-xs text-grey-500">{c.desc}</p>
                                            </div>
                                            <Badge variant="info">{c.usage}</Badge>
                                        </div>
                                    ))}
                                </div>
                            </div>
                        </div>
                    </div>
                </div>
            )}

            {activeTab === 'AI Tasks' && (
                <div className="space-y-6">
                    <div className="gs-card p-6">
                        <h3 className="text-sm font-semibold text-grey-300 mb-4">AI-Assisted Task Creation & Management</h3>
                        <div className="grid grid-cols-2 gap-6">
                            <div>
                                <h4 className="text-xs font-semibold text-grey-400 uppercase mb-3">Automatic Task Detection</h4>
                                <div className="space-y-2">
                                    {[
                                        { source: 'Meeting transcript', task: 'Update roadmap with Q2 priorities', assignee: '@sarah', due: 'Feb 12', confidence: 96 },
                                        { source: 'Email thread', task: 'Send partnership proposal to Acme Corp', assignee: '@james', due: 'Feb 14', confidence: 92 },
                                        { source: 'Chat message', task: 'Review PR #1842 for memory leak fix', assignee: '@dev-team', due: 'Today', confidence: 89 },
                                        { source: 'Document comment', task: 'Add compliance section to security doc', assignee: '@maria', due: 'Feb 15', confidence: 87 },
                                        { source: 'Slack channel', task: 'Schedule design review for new dashboard', assignee: '@alex', due: 'This week', confidence: 84 },
                                    ].map(t => (
                                        <div key={t.task} className="bg-grey-800/50 rounded-lg p-3 border border-grey-700/50">
                                            <div className="flex items-center justify-between mb-2">
                                                <span className="text-sm text-grey-200">{t.task}</span>
                                                <Badge variant="info">{t.confidence}%</Badge>
                                            </div>
                                            <div className="flex items-center gap-3 text-xs text-grey-500">
                                                <span>From: {t.source}</span>
                                                <span>{t.assignee}</span>
                                                <span>Due: {t.due}</span>
                                            </div>
                                            <div className="flex gap-2 mt-2">
                                                <button className="gs-btn-primary text-[10px] py-0.5 px-2">Accept</button>
                                                <button className="gs-btn-ghost text-[10px] py-0.5 px-2">Edit</button>
                                                <button className="gs-btn-ghost text-[10px] py-0.5 px-2">Dismiss</button>
                                            </div>
                                        </div>
                                    ))}
                                </div>
                            </div>

                            <div>
                                <h4 className="text-xs font-semibold text-grey-400 uppercase mb-3">AI Task Capabilities</h4>
                                <div className="space-y-3">
                                    {[
                                        { feature: 'Natural Language Input', desc: 'Create tasks from plain text — AI extracts title, assignee, due date, priority' },
                                        { feature: 'Smart Scheduling', desc: 'Suggest optimal due dates based on workload, dependencies, and calendar' },
                                        { feature: 'Auto-Prioritization', desc: 'Score tasks using urgency, impact, effort, and strategic alignment' },
                                        { feature: 'Dependency Detection', desc: 'Identify blocked/blocking relationships from task descriptions' },
                                        { feature: 'Effort Estimation', desc: 'Story point / hour estimation based on historical data and complexity' },
                                        { feature: 'Recurring Pattern', desc: 'Detect and suggest automation for repetitive task patterns' },
                                        { feature: 'Delegation Suggest', desc: 'Recommend best assignee based on skills, capacity, and past performance' },
                                    ].map(f => (
                                        <div key={f.feature} className="py-2 border-b border-grey-800/50 last:border-0">
                                            <p className="text-sm text-grey-300">{f.feature}</p>
                                            <p className="text-xs text-grey-500 mt-0.5">{f.desc}</p>
                                        </div>
                                    ))}
                                </div>
                            </div>
                        </div>
                    </div>
                </div>
            )}

            {activeTab === 'AI Insights' && (
                <div className="space-y-6">
                    <div className="gs-card p-6">
                        <h3 className="text-sm font-semibold text-grey-300 mb-4">AI-Powered Dashboard Insights</h3>

                        <div className="grid grid-cols-3 gap-4 mb-6">
                            {[
                                { insight: 'Revenue Anomaly Detected', desc: 'EMEA revenue 23% below forecast — correlated with 2 delayed enterprise deals', severity: 'High', module: 'Finance', icon: <TrendingUp size={20} /> },
                                { insight: 'Attrition Risk Signal', desc: '3 senior engineers showing disengagement patterns (low commit frequency, missed 1:1s)', severity: 'High', module: 'HCM', icon: <AlertTriangle size={20} /> },
                                { insight: 'Pipeline Velocity Drop', desc: 'Average deal cycle time increased 8 days in Jan — suggest reviewing qualification criteria', severity: 'Medium', module: 'CRM', icon: <Target size={20} /> },
                            ].map(i => (
                                <div key={i.insight} className="bg-grey-800/50 rounded-lg p-4 border border-grey-700/50">
                                    <div className="flex items-center gap-3 mb-3">
                                        <div className={`p-2 rounded-lg ${i.severity === 'High' ? 'bg-rose-500/10 text-rose-400' : 'bg-amber-500/10 text-amber-400'}`}>{i.icon}</div>
                                        <Badge variant={i.severity === 'High' ? 'danger' : 'warning'}>{i.severity}</Badge>
                                    </div>
                                    <p className="text-sm font-semibold text-grey-200 mb-1">{i.insight}</p>
                                    <p className="text-xs text-grey-500 mb-2">{i.desc}</p>
                                    <Badge variant="info">{i.module}</Badge>
                                </div>
                            ))}
                        </div>

                        <h4 className="text-xs font-semibold text-grey-400 uppercase mb-3">Insight Generation Pipeline</h4>
                        <div className="bg-grey-900 rounded-lg p-4 font-mono text-xs text-grey-400">
                            <pre>{`// AI Insight Pipeline
1. Data Ingestion (all modules → data lake)
2. Anomaly Detection (statistical + ML)
   ├── Time-series: Prophet + custom LSTMs
   ├── Categorical: isolation forest
   └── Cross-module: correlation analysis
3. Root Cause Analysis (causal inference)
4. Natural Language Generation
   ├── Insight text (GPT-4o)
   ├── Recommended actions
   └── Confidence scoring
5. Delivery
   ├── Dashboard cards (proactive)
   ├── Email digest (daily/weekly)
   ├── Slack alerts (critical only)
   └── Executive summary (weekly/monthly)`}</pre>
                        </div>
                    </div>
                </div>
            )}

            {activeTab === 'AI Search' && (
                <div className="space-y-6">
                    <div className="gs-card p-6">
                        <h3 className="text-sm font-semibold text-grey-300 mb-4">AI-Powered Unified Search</h3>

                        {/* Simulated Search Bar */}
                        <div className="bg-grey-900 rounded-lg border border-grey-700/50 p-4 mb-6">
                            <div className="flex items-center gap-3 mb-4">
                                <Search size={18} className="text-grey-500" />
                                <span className="text-sm text-grey-300">What were the key decisions from last week&apos;s product meeting?</span>
                                <div className="flex-1" />
                                <Badge variant="info">AI Search</Badge>
                            </div>

                            <div className="bg-grey-800/50 rounded-lg p-4 border border-grey-700/50">
                                <p className="text-xs text-brand-400 font-semibold mb-2">AI-Generated Answer</p>
                                <p className="text-sm text-grey-200 mb-3">Based on the Product Roadmap Review meeting on Feb 7, 2026, the key decisions were:</p>
                                <div className="text-xs text-grey-400 space-y-1 mb-3">
                                    <p>1. <span className="text-grey-200">Ship v2.4 by Feb 28</span> with reduced scope (cut features: bulk import, advanced filters)</p>
                                    <p>2. <span className="text-grey-200">Hire 2 SREs for Q2</span> — approved by VP Engineering</p>
                                    <p>3. <span className="text-grey-200">Sunset legacy API v1</span> by March 31 with 60-day deprecation notice</p>
                                </div>
                                <div className="flex items-center gap-2 text-[10px] text-grey-500">
                                    <span>Sources: Meeting recording, Transcript, 3 follow-up emails</span>
                                    <span>•</span>
                                    <span>Confidence: 96%</span>
                                </div>
                            </div>
                        </div>
                    </div>

                    <div className="grid grid-cols-2 gap-6">
                        <div className="gs-card p-6">
                            <h3 className="text-sm font-semibold text-grey-300 mb-4">Search Scope & Indexes</h3>
                            <div className="space-y-2">
                                {[
                                    { source: 'Documents & Files', indexed: '1.2M', lastSync: '2m ago', status: 'healthy' },
                                    { source: 'Emails', indexed: '8.4M', lastSync: '5m ago', status: 'healthy' },
                                    { source: 'Chat Messages', indexed: '48M', lastSync: 'Real-time', status: 'healthy' },
                                    { source: 'Meeting Transcripts', indexed: '42K', lastSync: '15m ago', status: 'healthy' },
                                    { source: 'CRM Records', indexed: '284K', lastSync: '10m ago', status: 'healthy' },
                                    { source: 'Support Tickets', indexed: '142K', lastSync: '5m ago', status: 'healthy' },
                                    { source: 'Wiki / Knowledge Base', indexed: '18K', lastSync: '1h ago', status: 'healthy' },
                                    { source: 'Code Repositories', indexed: '2.4M files', lastSync: '30m ago', status: 'healthy' },
                                ].map(s => (
                                    <div key={s.source} className="flex items-center justify-between py-2 border-b border-grey-800/50 last:border-0">
                                        <span className="text-sm text-grey-300">{s.source}</span>
                                        <div className="flex items-center gap-3">
                                            <span className="text-xs text-grey-500">{s.indexed} indexed</span>
                                            <span className="text-xs text-grey-500">{s.lastSync}</span>
                                            <Badge variant="success">{s.status}</Badge>
                                        </div>
                                    </div>
                                ))}
                            </div>
                        </div>

                        <div className="gs-card p-6">
                            <h3 className="text-sm font-semibold text-grey-300 mb-4">Search Architecture</h3>
                            <div className="bg-grey-900 rounded-lg p-4 font-mono text-xs text-grey-400">
                                <pre>{`// AI Search Pipeline
Query → Intent Classification
     → Query Expansion (synonyms, entities)
     → Parallel Search:
        ├── Vector Search (embeddings)
        │   └── text-embedding-3-large
        ├── Keyword Search (BM25)
        │   └── Elasticsearch
        └── Graph Search (relationships)
            └── Knowledge Graph
     → Fusion & Re-ranking
        ├── Reciprocal Rank Fusion
        ├── Cross-encoder reranking
        └── Permission filtering
     → Answer Generation
        ├── RAG (top-k chunks → LLM)
        ├── Citation extraction
        └── Confidence scoring
     → Response
        ├── AI-generated answer
        ├── Source documents 
        ├── Related items
        └── Suggested follow-ups

// Guardrails:
// - Tenant data isolation
// - Permission-aware results
// - PII redaction in answers
// - Audit logging of all queries`}</pre>
                            </div>
                        </div>
                    </div>
                </div>
            )}
        </div>
    );
}
