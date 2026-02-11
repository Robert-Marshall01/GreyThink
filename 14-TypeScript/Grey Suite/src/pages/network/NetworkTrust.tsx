import { useState } from 'react';
import { PageHeader, StatCard, Badge, DataTable, Tabs, ProgressBar, MiniSparkline } from '../../components/ui';
import {
    Shield, AlertTriangle, Ban, Flag, Eye, UserX,
    MessageSquare, Clock, CheckCircle, XCircle,
    Activity, Filter, FileText, Zap, Lock, Users
} from 'lucide-react';

const reports = [
    { id: 'RPT-001', reporter: 'priya@initech.com', reported: 'spam_account_42', type: 'Spam', content: 'Bulk connection requests with identical messages', status: 'Under Review', severity: 'Medium', created: '2h ago' },
    { id: 'RPT-002', reporter: 'david@wayne.com', reported: 'crypto_recruiter', type: 'Spam', content: 'Unsolicited crypto MLM messages to 200+ users', status: 'Action Taken', severity: 'High', created: '5h ago' },
    { id: 'RPT-003', reporter: 'anika@stark.com', reported: 'fake_profile_99', type: 'Fake Account', content: 'Profile impersonating a Stark Industries executive', status: 'Under Review', severity: 'Critical', created: '1d ago' },
    { id: 'RPT-004', reporter: 'marcus@globex.com', reported: 'angry_user_12', type: 'Harassment', content: 'Repeated abusive comments on professional posts', status: 'Action Taken', severity: 'High', created: '2d ago' },
    { id: 'RPT-005', reporter: 'auto-detect', reported: 'bot_farm_cluster', type: 'Bot Activity', content: 'Coordinated inauthentic behavior — 14 accounts', status: 'Investigating', severity: 'Critical', created: '3d ago' },
    { id: 'RPT-006', reporter: 'rachel@netflix.com', reported: 'competitor_scraper', type: 'Data Scraping', content: 'Automated profile scraping via API abuse', status: 'Blocked', severity: 'High', created: '1w ago' },
];

const blocks = [
    { blocked: 'spam_account_42', reason: 'Bulk spam messages', blockedBy: 'priya@initech.com', date: '2h ago', mutual: false },
    { blocked: 'crypto_recruiter', reason: 'MLM spam', blockedBy: 'system', date: '5h ago', mutual: false },
    { blocked: 'angry_user_12', reason: 'Harassment', blockedBy: 'marcus@globex.com', date: '2d ago', mutual: true },
];

const abuseSignals = [
    { signal: 'Rapid Connection Requests', desc: '> 50 requests in 1 hour', threshold: 50, current: 0, users: 2, action: 'Rate limit + review', risk: 'High' },
    { signal: 'Identical Messages', desc: 'Same message to > 20 users', threshold: 20, current: 0, users: 4, action: 'Auto-flag + throttle', risk: 'High' },
    { signal: 'Profile Scraping', desc: '> 500 profile views/hour', threshold: 500, current: 0, users: 1, action: 'Block + investigate', risk: 'Critical' },
    { signal: 'Fake Profile Indicators', desc: 'Stock photo + no activity + bulk adds', threshold: 3, current: 0, users: 8, action: 'Flag for manual review', risk: 'Medium' },
    { signal: 'Engagement Manipulation', desc: 'Coordinated likes from same IP range', threshold: 10, current: 0, users: 14, action: 'Shadow ban + investigate', risk: 'High' },
    { signal: 'Content Spam', desc: '> 10 posts/hour with external links', threshold: 10, current: 0, users: 3, action: 'Rate limit + review', risk: 'Medium' },
];

const moderationActions = [
    { time: '2h ago', actor: 'auto-mod', action: 'Rate limited', target: 'spam_account_42', reason: 'Exceeded connection request threshold (52/50)', severity: 'Warning' },
    { time: '5h ago', actor: 'trust-team', action: 'Account suspended', target: 'crypto_recruiter', reason: '200+ identical MLM messages', severity: 'Enforcement' },
    { time: '1d ago', actor: 'auto-mod', action: 'Content hidden', target: 'fake_profile_99', reason: 'Impersonation — awaiting manual review', severity: 'Enforcement' },
    { time: '2d ago', actor: 'trust-team', action: 'Formal warning', target: 'angry_user_12', reason: 'Repeated abusive comments (3rd offense)', severity: 'Warning' },
    { time: '3d ago', actor: 'auto-mod', action: 'IP range blocked', target: 'bot_farm_cluster', reason: 'Coordinated inauthentic behavior detected', severity: 'Enforcement' },
    { time: '1w ago', actor: 'auto-mod', action: 'API key revoked', target: 'competitor_scraper', reason: 'Automated scraping via API abuse', severity: 'Enforcement' },
];

const policyRules = [
    { rule: 'Connection Rate Limit', limit: '50/hour, 200/day', action: 'Throttle + captcha', enabled: true },
    { rule: 'Message Dedup', limit: 'Max 3 identical messages', action: 'Block + flag', enabled: true },
    { rule: 'Profile View Rate', limit: '500/hour per viewer', action: 'Temporary block', enabled: true },
    { rule: 'Post Frequency', limit: '10 posts/hour', action: 'Rate limit', enabled: true },
    { rule: 'New Account Restrictions', limit: '< 7 days old: limited actions', action: 'Gradual unlock', enabled: true },
    { rule: 'Content Filtering', limit: 'ML-based toxicity scoring', action: 'Auto-hide if > 0.85', enabled: true },
    { rule: 'Bot Detection', limit: 'Behavioral fingerprinting', action: 'Captcha + review', enabled: true },
];

export function NetworkTrust() {
    const [activeTab, setActiveTab] = useState('Reports');

    return (
        <div>
            <PageHeader
                title="Trust & Safety"
                subtitle="Content moderation, abuse detection, user blocks, and safety policies"
                actions={
                    <div className="flex gap-2">
                        <button className="gs-btn-secondary flex items-center gap-2"><Filter size={16} /> Filters</button>
                        <button className="gs-btn-primary flex items-center gap-2"><Flag size={16} /> New Report</button>
                    </div>
                }
            />

            <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-4 gap-4 mb-6">
                <StatCard label="Open Reports" value="3" change="2 critical" changeType="negative" icon={<Flag size={18} />} />
                <StatCard label="Accounts Actioned" value="42" change="This month" changeType="neutral" icon={<Ban size={18} />} />
                <StatCard label="Spam Blocked" value="1,284" change="Auto-detected" changeType="positive" icon={<Shield size={18} />} />
                <StatCard label="Trust Score" value="97.2%" change="Platform-wide" changeType="positive" icon={<CheckCircle size={18} />} />
            </div>

            <Tabs tabs={['Reports', 'Blocks', 'Abuse Signals', 'Moderation Log', 'Safety Policies', 'Trust API']} active={activeTab} onChange={setActiveTab} />

            {activeTab === 'Reports' && (
                <div className="gs-card p-5">
                    <h3 className="gs-section-title mb-4 flex items-center gap-2"><Flag size={16} /> User Reports</h3>
                    <DataTable
                        columns={['Report ID', 'Reporter', 'Reported User', 'Type', 'Severity', 'Status', 'Created', 'Actions']}
                        rows={reports.map(r => [
                            <span className="font-mono text-xs text-grey-400">{r.id}</span>,
                            <span className="font-mono text-xs text-grey-300">{r.reporter}</span>,
                            <span className="font-mono text-xs text-accent-rose">{r.reported}</span>,
                            <Badge variant={r.type === 'Spam' ? 'warning' : r.type === 'Harassment' ? 'danger' : r.type === 'Fake Account' ? 'danger' : 'purple'}>{r.type}</Badge>,
                            <Badge variant={r.severity === 'Critical' ? 'danger' : r.severity === 'High' ? 'warning' : 'info'}>{r.severity}</Badge>,
                            <Badge variant={r.status === 'Action Taken' || r.status === 'Blocked' ? 'success' : r.status === 'Under Review' ? 'info' : 'warning'}>{r.status}</Badge>,
                            <span className="text-xs text-grey-500">{r.created}</span>,
                            <button className="text-xs text-brand-400 hover:text-brand-300">Review</button>,
                        ])}
                    />
                </div>
            )}

            {activeTab === 'Blocks' && (
                <div className="gs-card p-5">
                    <h3 className="gs-section-title mb-4 flex items-center gap-2"><Ban size={16} /> Blocked Users</h3>
                    <div className="space-y-3">
                        {blocks.map((b, i) => (
                            <div key={i} className="flex items-center justify-between p-4 rounded-lg border border-grey-800">
                                <div className="flex items-center gap-3">
                                    <UserX size={16} className="text-accent-rose" />
                                    <div>
                                        <p className="text-sm font-medium text-grey-200">{b.blocked}</p>
                                        <p className="text-xs text-grey-500">Reason: {b.reason} · By: {b.blockedBy}</p>
                                    </div>
                                </div>
                                <div className="flex items-center gap-3">
                                    <span className="text-xs text-grey-600">{b.date}</span>
                                    {b.mutual && <Badge variant="warning">Mutual</Badge>}
                                    <button className="text-xs text-grey-500 hover:text-grey-300">Unblock</button>
                                </div>
                            </div>
                        ))}
                    </div>
                    <div className="mt-4 p-3 bg-grey-800/30 rounded-lg text-xs text-grey-500">
                        <p className="font-medium text-grey-400 mb-1">Block Effects:</p>
                        <ul className="space-y-1 pl-3 list-disc">
                            <li>Cannot view your profile or posts</li>
                            <li>Cannot send connection requests or messages</li>
                            <li>Removed from your feed and search results</li>
                            <li>Cannot see your comments or reactions</li>
                            <li>Existing connection removed silently</li>
                        </ul>
                    </div>
                </div>
            )}

            {activeTab === 'Abuse Signals' && (
                <div className="gs-card p-5">
                    <h3 className="gs-section-title mb-4 flex items-center gap-2"><AlertTriangle size={16} /> Abuse Detection Signals</h3>
                    <div className="space-y-3">
                        {abuseSignals.map((s, i) => (
                            <div key={i} className="p-4 rounded-lg border border-grey-800">
                                <div className="flex items-center justify-between mb-2">
                                    <div>
                                        <p className="text-sm font-medium text-grey-200">{s.signal}</p>
                                        <p className="text-xs text-grey-500">{s.desc}</p>
                                    </div>
                                    <Badge variant={s.risk === 'Critical' ? 'danger' : s.risk === 'High' ? 'warning' : 'info'}>{s.risk}</Badge>
                                </div>
                                <div className="grid grid-cols-3 gap-3 text-xs mt-2">
                                    <div>
                                        <span className="text-grey-500">Threshold</span>
                                        <p className="text-grey-300 font-mono">{s.threshold}</p>
                                    </div>
                                    <div>
                                        <span className="text-grey-500">Flagged Users (30d)</span>
                                        <p className="text-accent-rose font-mono">{s.users}</p>
                                    </div>
                                    <div>
                                        <span className="text-grey-500">Action</span>
                                        <p className="text-grey-400">{s.action}</p>
                                    </div>
                                </div>
                            </div>
                        ))}
                    </div>
                </div>
            )}

            {activeTab === 'Moderation Log' && (
                <div className="gs-card p-5">
                    <h3 className="gs-section-title mb-4 flex items-center gap-2"><Eye size={16} /> Moderation Action Log</h3>
                    <p className="text-xs text-grey-500 mb-4">Append-only, immutable log — routed to platform audit layer</p>
                    <div className="space-y-2">
                        {moderationActions.map((a, i) => (
                            <div key={i} className="flex items-center gap-3 p-3 rounded-lg hover:bg-grey-800/30 transition-colors">
                                <span className="text-xs text-grey-600 w-14 flex-shrink-0">{a.time}</span>
                                {a.severity === 'Enforcement' ? <Shield size={14} className="text-accent-rose flex-shrink-0" /> : <AlertTriangle size={14} className="text-accent-amber flex-shrink-0" />}
                                <span className="font-mono text-xs text-grey-500 w-20 flex-shrink-0">{a.actor}</span>
                                <Badge variant={a.severity === 'Enforcement' ? 'danger' : 'warning'}>{a.action}</Badge>
                                <span className="font-mono text-xs text-accent-rose w-32 flex-shrink-0 truncate">{a.target}</span>
                                <span className="text-xs text-grey-400 flex-1 truncate">{a.reason}</span>
                            </div>
                        ))}
                    </div>
                </div>
            )}

            {activeTab === 'Safety Policies' && (
                <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
                    <div className="gs-card p-5">
                        <h3 className="gs-section-title mb-4 flex items-center gap-2"><Lock size={16} /> Automated Safety Rules</h3>
                        <div className="space-y-3">
                            {policyRules.map((r, i) => (
                                <div key={i} className="flex items-center justify-between p-3 rounded-lg border border-grey-800">
                                    <div className="flex-1">
                                        <p className="text-sm font-medium text-grey-200">{r.rule}</p>
                                        <p className="text-xs text-grey-500">Limit: {r.limit} · Action: {r.action}</p>
                                    </div>
                                    <div className={`w-10 h-5 rounded-full flex items-center px-0.5 ${r.enabled ? 'bg-accent-emerald/30 justify-end' : 'bg-grey-800 justify-start'}`}>
                                        <div className={`w-4 h-4 rounded-full ${r.enabled ? 'bg-accent-emerald' : 'bg-grey-600'}`} />
                                    </div>
                                </div>
                            ))}
                        </div>
                    </div>
                    <div className="gs-card p-5">
                        <h3 className="gs-section-title mb-4 flex items-center gap-2"><Activity size={16} /> Trust Metrics (30d)</h3>
                        <MiniSparkline data={[92, 94, 93, 95, 96, 94, 97, 96, 97, 98, 97, 97]} height={60} />
                        <div className="grid grid-cols-2 gap-3 mt-4">
                            <div className="p-3 bg-grey-800/30 rounded-lg text-center">
                                <p className="text-lg font-bold text-accent-emerald">97.2%</p>
                                <p className="text-xs text-grey-500">Platform Trust Score</p>
                            </div>
                            <div className="p-3 bg-grey-800/30 rounded-lg text-center">
                                <p className="text-lg font-bold text-grey-100">1,284</p>
                                <p className="text-xs text-grey-500">Spam Blocked</p>
                            </div>
                            <div className="p-3 bg-grey-800/30 rounded-lg text-center">
                                <p className="text-lg font-bold text-grey-100">42</p>
                                <p className="text-xs text-grey-500">Accounts Actioned</p>
                            </div>
                            <div className="p-3 bg-grey-800/30 rounded-lg text-center">
                                <p className="text-lg font-bold text-accent-amber">{'< 2h'}</p>
                                <p className="text-xs text-grey-500">Avg Response Time</p>
                            </div>
                        </div>
                        <div className="mt-4 space-y-2">
                            <h4 className="text-xs font-semibold text-grey-500 uppercase">Escalation Tiers</h4>
                            {[
                                { tier: 'T1 — Auto-mod', handler: 'ML classifier + rules engine', sla: '< 1 min' },
                                { tier: 'T2 — Trust Team', handler: 'Human review queue', sla: '< 2 hours' },
                                { tier: 'T3 — Legal/Compliance', handler: 'Legal team + law enforcement', sla: '< 24 hours' },
                            ].map(t => (
                                <div key={t.tier} className="flex items-center justify-between p-2 bg-grey-800/30 rounded text-xs">
                                    <span className="text-grey-300 font-medium">{t.tier}</span>
                                    <span className="text-grey-500">{t.handler}</span>
                                    <Badge variant="info">SLA: {t.sla}</Badge>
                                </div>
                            ))}
                        </div>
                    </div>
                </div>
            )}

            {activeTab === 'Trust API' && (
                <div className="gs-card p-5">
                    <h3 className="gs-section-title mb-4 flex items-center gap-2"><FileText size={16} /> Trust Service API</h3>
                    <div className="space-y-2">
                        {[
                            { method: 'POST', path: '/api/v1/reports', desc: 'Submit user/content report' },
                            { method: 'GET', path: '/api/v1/reports', desc: 'List reports (moderator)' },
                            { method: 'PUT', path: '/api/v1/reports/{id}/resolve', desc: 'Resolve report with action' },
                            { method: 'POST', path: '/api/v1/blocks', desc: 'Block a user' },
                            { method: 'DELETE', path: '/api/v1/blocks/{id}', desc: 'Unblock a user' },
                            { method: 'GET', path: '/api/v1/blocks', desc: 'List blocked users' },
                            { method: 'POST', path: '/api/v1/mutes', desc: 'Mute a user' },
                            { method: 'GET', path: '/api/v1/trust/signals', desc: 'Abuse signals for a user' },
                            { method: 'POST', path: '/api/v1/trust/actions', desc: 'Take moderation action' },
                            { method: 'GET', path: '/api/v1/trust/audit-log', desc: 'Immutable moderation audit log' },
                        ].map((e, i) => (
                            <div key={i} className="flex items-center gap-3 p-2.5 rounded-lg bg-grey-800/30">
                                <Badge variant={e.method === 'GET' ? 'info' : e.method === 'POST' ? 'success' : e.method === 'PUT' ? 'warning' : 'danger'}>{e.method}</Badge>
                                <span className="font-mono text-xs text-accent-cyan flex-1">{e.path}</span>
                                <span className="text-xs text-grey-500">{e.desc}</span>
                            </div>
                        ))}
                    </div>
                    <div className="mt-4 p-3 bg-grey-800/30 rounded-lg text-xs text-grey-500">
                        <p className="font-medium text-grey-400 mb-1">Audit Integration:</p>
                        <p>All moderation events flow to the platform Security & Compliance audit log via event bus (Kafka topic: <span className="font-mono text-accent-cyan">trust.moderation.events</span>). Events are immutable, cryptographically hashed (SHA-256 chain), and retained for 7 years per SOC2 CC-05.</p>
                    </div>
                </div>
            )}
        </div>
    );
}
