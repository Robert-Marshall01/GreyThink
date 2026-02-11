import { useState } from 'react';
import {
    Radio, Wifi, Users, MessageSquare, Video, Phone, Clock, Shield,
    ArrowUpRight, ArrowDownRight, Circle, Hash, Lock, Bell, Paperclip,
    HardDrive, RefreshCw, AlertTriangle, CheckCircle2, Zap, Globe
} from 'lucide-react';
import { PageHeader, StatCard, DataTable, Badge, Tabs, ProgressBar, MiniSparkline } from '../../components/ui';

export function RealTimeComms() {
    const [activeTab, setActiveTab] = useState('WebSockets');

    const tabs = ['WebSockets', 'Presence', 'Channels', 'Conferencing', 'Retention', 'File Sharing'];

    return (
        <div className="p-6 space-y-6">
            <PageHeader
                title="Real-Time Communication Infrastructure"
                subtitle="WebSocket backbone, presence engine, channel architecture, and compliance-grade messaging"
                actions={
                    <div className="flex gap-2">
                        <button className="gs-btn-secondary">
                            <AlertTriangle size={14} /> Health Check
                        </button>
                        <button className="gs-btn-primary">
                            <Radio size={14} /> Live Monitor
                        </button>
                    </div>
                }
            />

            <div className="grid grid-cols-4 gap-4">
                <StatCard label="Active Connections" value="48,291" change="+12.4%" changeType="positive" icon={<Wifi size={18} />} />
                <StatCard label="Messages / sec" value="14,820" change="+8.1%" changeType="positive" icon={<Zap size={18} />} />
                <StatCard label="Avg Latency" value="23ms" change="-4ms" changeType="positive" icon={<Clock size={18} />} />
                <StatCard label="Uptime (30d)" value="99.998%" change="0 incidents" changeType="positive" icon={<CheckCircle2 size={18} />} />
            </div>

            <Tabs tabs={tabs} active={activeTab} onChange={setActiveTab} />

            {activeTab === 'WebSockets' && (
                <div className="space-y-6">
                    <div className="gs-card p-6">
                        <h3 className="text-sm font-semibold text-grey-300 mb-4">WebSocket Connection Architecture</h3>
                        <div className="grid grid-cols-3 gap-4 mb-6">
                            {[
                                { layer: 'Load Balancer', tech: 'HAProxy + Sticky Sessions', status: 'healthy', connections: '48,291', icon: <Globe size={20} /> },
                                { layer: 'WS Gateway Cluster', tech: 'Node.js + uWebSockets.js', status: 'healthy', connections: '8 nodes', icon: <Radio size={20} /> },
                                { layer: 'Message Broker', tech: 'Redis Pub/Sub + Streams', status: 'healthy', connections: '3 replicas', icon: <RefreshCw size={20} /> },
                            ].map(l => (
                                <div key={l.layer} className="bg-grey-800/50 rounded-lg p-4 border border-grey-700/50">
                                    <div className="flex items-center gap-3 mb-3">
                                        <div className="p-2 bg-brand-500/10 rounded-lg text-brand-400">{l.icon}</div>
                                        <div>
                                            <p className="text-sm font-semibold text-grey-200">{l.layer}</p>
                                            <p className="text-xs text-grey-500">{l.tech}</p>
                                        </div>
                                    </div>
                                    <div className="flex items-center justify-between">
                                        <span className="text-xs text-grey-400">{l.connections}</span>
                                        <Badge variant="success">{l.status}</Badge>
                                    </div>
                                </div>
                            ))}
                        </div>
                    </div>

                    <div className="grid grid-cols-2 gap-6">
                        <div className="gs-card p-6">
                            <h3 className="text-sm font-semibold text-grey-300 mb-4">Connection Pool Distribution</h3>
                            {[
                                { node: 'ws-gateway-01', connections: 6284, cpu: 42, memory: 61 },
                                { node: 'ws-gateway-02', connections: 6102, cpu: 39, memory: 58 },
                                { node: 'ws-gateway-03', connections: 5891, cpu: 41, memory: 60 },
                                { node: 'ws-gateway-04', connections: 6440, cpu: 45, memory: 63 },
                                { node: 'ws-gateway-05', connections: 5720, cpu: 37, memory: 55 },
                                { node: 'ws-gateway-06', connections: 6180, cpu: 40, memory: 59 },
                                { node: 'ws-gateway-07', connections: 5988, cpu: 38, memory: 57 },
                                { node: 'ws-gateway-08', connections: 5686, cpu: 36, memory: 54 },
                            ].map(n => (
                                <div key={n.node} className="flex items-center gap-3 py-2 border-b border-grey-800/50 last:border-0">
                                    <Circle size={6} className="text-accent-emerald fill-accent-emerald" />
                                    <span className="text-xs font-mono text-grey-400 w-32">{n.node}</span>
                                    <span className="text-xs text-grey-300 w-16 text-right">{n.connections.toLocaleString()}</span>
                                    <div className="flex-1">
                                        <ProgressBar value={n.cpu} color="bg-brand-500" />
                                    </div>
                                    <span className="text-xs text-grey-500 w-16">CPU {n.cpu}%</span>
                                </div>
                            ))}
                        </div>

                        <div className="gs-card p-6">
                            <h3 className="text-sm font-semibold text-grey-300 mb-4">Message Protocol Specification</h3>
                            <div className="bg-grey-900 rounded-lg p-4 font-mono text-xs text-grey-400 overflow-auto">
                                <pre>{`// WebSocket Frame Protocol
interface WSFrame {
  v: 2;                    // Protocol version
  id: string;              // Message UUID
  type: 'msg' | 'presence' | 'typing'
       | 'ack' | 'ping' | 'system';
  channel: string;         // Channel ID
  tenant: string;          // Tenant context
  payload: {
    content?: string;
    attachments?: Attachment[];
    mentions?: string[];
    thread_id?: string;
    metadata?: Record<string, unknown>;
  };
  ts: number;              // Unix timestamp (ms)
  seq: number;             // Sequence number
  sig: string;             // HMAC-SHA256 signature
}

// Delivery Guarantees
// - At-least-once via ACK mechanism
// - Sequence-based ordering
// - Client-side dedup via msg ID
// - Auto-reconnect with exponential backoff
// - Offline queue (max 1000 messages)`}</pre>
                            </div>
                        </div>
                    </div>

                    <div className="gs-card p-6">
                        <h3 className="text-sm font-semibold text-grey-300 mb-4">Real-Time Throughput (Last 60 Minutes)</h3>
                        <div className="grid grid-cols-3 gap-6">
                            {[
                                { label: 'Messages Sent', data: [12400, 13200, 14100, 13800, 14500, 15200, 14820], color: '#1a8fe0' },
                                { label: 'Connections Opened', data: [320, 280, 410, 390, 350, 420, 380], color: '#10b981' },
                                { label: 'Reconnects', data: [12, 8, 15, 9, 11, 7, 10], color: '#f59e0b' },
                            ].map(m => (
                                <div key={m.label}>
                                    <p className="text-xs text-grey-500 mb-2">{m.label}</p>
                                    <MiniSparkline data={m.data} color={m.color} height={40} />
                                    <p className="text-lg font-bold text-grey-200 mt-1">{m.data[m.data.length - 1].toLocaleString()}</p>
                                </div>
                            ))}
                        </div>
                    </div>
                </div>
            )}

            {activeTab === 'Presence' && (
                <div className="space-y-6">
                    <div className="gs-card p-6">
                        <h3 className="text-sm font-semibold text-grey-300 mb-4">Presence Engine</h3>
                        <div className="grid grid-cols-4 gap-4 mb-6">
                            {[
                                { status: 'Online', count: 8421, color: 'bg-emerald-500' },
                                { status: 'Away', count: 3218, color: 'bg-amber-500' },
                                { status: 'DND', count: 1847, color: 'bg-rose-500' },
                                { status: 'Offline', count: 34805, color: 'bg-grey-600' },
                            ].map(s => (
                                <div key={s.status} className="bg-grey-800/50 rounded-lg p-4 border border-grey-700/50 text-center">
                                    <div className={`w-3 h-3 rounded-full ${s.color} mx-auto mb-2`} />
                                    <p className="text-xl font-bold text-grey-200">{s.count.toLocaleString()}</p>
                                    <p className="text-xs text-grey-500">{s.status}</p>
                                </div>
                            ))}
                        </div>
                    </div>

                    <div className="grid grid-cols-2 gap-6">
                        <div className="gs-card p-6">
                            <h3 className="text-sm font-semibold text-grey-300 mb-4">Typing Indicators Architecture</h3>
                            <div className="bg-grey-900 rounded-lg p-4 font-mono text-xs text-grey-400">
                                <pre>{`// Typing Indicator Protocol
// - Debounce: 300ms client-side
// - Broadcast to channel members only
// - Auto-expire after 5s without update
// - Throttled to max 1 event/sec/user

interface TypingEvent {
  type: 'typing_start' | 'typing_stop';
  user_id: string;
  channel_id: string;
  thread_id?: string;
  ttl: 5000;  // Auto-expire ms
}

// Server-side aggregation:
// Redis SET per channel with PEXPIRE
// Fan-out via Redis Pub/Sub
// Client renders: "Alice, Bob are typing..."`}</pre>
                            </div>
                        </div>

                        <div className="gs-card p-6">
                            <h3 className="text-sm font-semibold text-grey-300 mb-4">Presence Sync Configuration</h3>
                            <div className="space-y-3">
                                {[
                                    { setting: 'Heartbeat Interval', value: '30s', desc: 'Client ping frequency' },
                                    { setting: 'Stale Timeout', value: '90s', desc: 'Mark away after silence' },
                                    { setting: 'Disconnect Timeout', value: '180s', desc: 'Mark offline after silence' },
                                    { setting: 'Presence Fanout', value: 'Lazy', desc: 'Broadcast to visible contacts only' },
                                    { setting: 'Calendar Integration', value: 'Enabled', desc: 'Auto-DND during meetings' },
                                    { setting: 'Cross-Device Sync', value: 'Highest Priority', desc: 'Desktop > Mobile > Web' },
                                    { setting: 'Custom Status TTL', value: '24h', desc: 'Auto-clear custom status' },
                                ].map(s => (
                                    <div key={s.setting} className="flex items-center justify-between py-2 border-b border-grey-800/50 last:border-0">
                                        <div>
                                            <p className="text-sm text-grey-300">{s.setting}</p>
                                            <p className="text-xs text-grey-500">{s.desc}</p>
                                        </div>
                                        <Badge variant="info">{s.value}</Badge>
                                    </div>
                                ))}
                            </div>
                        </div>
                    </div>
                </div>
            )}

            {activeTab === 'Channels' && (
                <div className="space-y-6">
                    <div className="gs-card p-6">
                        <h3 className="text-sm font-semibold text-grey-300 mb-4">Channel Architecture</h3>
                        <div className="grid grid-cols-3 gap-4 mb-6">
                            {[
                                { type: 'Public Channels', count: 342, icon: <Hash size={20} />, desc: 'Discoverable, open membership' },
                                { type: 'Private Channels', count: 1284, icon: <Lock size={20} />, desc: 'Invite-only, encrypted at rest' },
                                { type: 'Direct Messages', count: 18420, icon: <MessageSquare size={20} />, desc: '1:1 and group DMs, E2E encrypted' },
                            ].map(c => (
                                <div key={c.type} className="bg-grey-800/50 rounded-lg p-4 border border-grey-700/50">
                                    <div className="flex items-center gap-3 mb-3">
                                        <div className="p-2 bg-brand-500/10 rounded-lg text-brand-400">{c.icon}</div>
                                        <div>
                                            <p className="text-sm font-semibold text-grey-200">{c.type}</p>
                                            <p className="text-xs text-grey-500">{c.desc}</p>
                                        </div>
                                    </div>
                                    <p className="text-2xl font-bold text-grey-100">{c.count.toLocaleString()}</p>
                                </div>
                            ))}
                        </div>
                    </div>

                    <DataTable
                        columns={['Channel', 'Type', 'Members', 'Messages/day', 'Retention', 'Last Active', 'Status']}
                        rows={[
                            ['#general', <Badge variant="info">Public</Badge>, '4,821', '2,340', '∞', '12s ago', <Badge variant="success">Active</Badge>],
                            ['#engineering', <Badge variant="info">Public</Badge>, '892', '1,842', '∞', '5s ago', <Badge variant="success">Active</Badge>],
                            ['#incidents', <Badge variant="warning">Private</Badge>, '124', '89', '7 years', '2m ago', <Badge variant="success">Active</Badge>],
                            ['#exec-team', <Badge variant="warning">Private</Badge>, '18', '234', '1 year', '15m ago', <Badge variant="success">Active</Badge>],
                            ['#deal-room-acme', <Badge variant="warning">Private</Badge>, '8', '156', '90 days', '1h ago', <Badge variant="success">Active</Badge>],
                            ['#random', <Badge variant="info">Public</Badge>, '3,910', '4,120', '90 days', '3s ago', <Badge variant="success">Active</Badge>],
                            ['#security-alerts', <Badge variant="warning">Private</Badge>, '42', '67', '∞', '8m ago', <Badge variant="success">Active</Badge>],
                        ]}
                    />

                    <div className="gs-card p-6">
                        <h3 className="text-sm font-semibold text-grey-300 mb-4">Thread & Conversation Model</h3>
                        <div className="bg-grey-900 rounded-lg p-4 font-mono text-xs text-grey-400">
                            <pre>{`// Conversation Hierarchy
Channel
├── Message (top-level)
│   ├── Thread Reply 1
│   │   └── Reaction[] + Attachment[]
│   ├── Thread Reply 2
│   └── Thread Reply N
├── Pinned Messages (ordered list)
├── Bookmarks (per-user)
└── Channel Metadata
    ├── topic: string
    ├── purpose: string
    ├── retention_policy: RetentionPolicy
    ├── permissions: ChannelPermissions
    └── integrations: Integration[]

// Search Index: Elasticsearch
// - Full-text across messages, files, threads
// - Faceted by channel, user, date, type
// - Tenant-isolated indices`}</pre>
                        </div>
                    </div>
                </div>
            )}

            {activeTab === 'Conferencing' && (
                <div className="space-y-6">
                    <div className="grid grid-cols-4 gap-4">
                        <StatCard label="Active Meetings" value="284" change="+32" changeType="positive" icon={<Video size={18} />} />
                        <StatCard label="Participants Now" value="1,842" icon={<Users size={18} />} />
                        <StatCard label="Avg Duration" value="42m" change="-3m" changeType="positive" icon={<Clock size={18} />} />
                        <StatCard label="SFU Capacity" value="78%" icon={<HardDrive size={18} />} />
                    </div>

                    <div className="gs-card p-6">
                        <h3 className="text-sm font-semibold text-grey-300 mb-4">Media Server Architecture (SFU)</h3>
                        <div className="grid grid-cols-4 gap-4 mb-6">
                            {[
                                { component: 'TURN/STUN Server', tech: 'coturn', status: 'healthy', detail: 'NAT traversal, relay' },
                                { component: 'SFU Cluster', tech: 'mediasoup', status: 'healthy', detail: 'Selective forwarding, simulcast' },
                                { component: 'Recording Engine', tech: 'FFmpeg pipeline', status: 'healthy', detail: 'Composite + gallery layout' },
                                { component: 'Transcription', tech: 'Whisper v3', status: 'healthy', detail: 'Real-time STT, 40 languages' },
                            ].map(c => (
                                <div key={c.component} className="bg-grey-800/50 rounded-lg p-4 border border-grey-700/50">
                                    <p className="text-sm font-semibold text-grey-200 mb-1">{c.component}</p>
                                    <p className="text-xs text-brand-400 mb-2">{c.tech}</p>
                                    <p className="text-xs text-grey-500 mb-3">{c.detail}</p>
                                    <Badge variant="success">{c.status}</Badge>
                                </div>
                            ))}
                        </div>
                    </div>

                    <div className="grid grid-cols-2 gap-6">
                        <div className="gs-card p-6">
                            <h3 className="text-sm font-semibold text-grey-300 mb-4">WebRTC Capabilities</h3>
                            <div className="space-y-3">
                                {[
                                    { feature: 'Simulcast', desc: 'Multi-quality streams (720p/360p/180p)', enabled: true },
                                    { feature: 'SVC (Scalable Video)', desc: 'VP9 SVC with temporal layers', enabled: true },
                                    { feature: 'DTX (Discontinuous TX)', desc: 'Bandwidth savings during silence', enabled: true },
                                    { feature: 'FEC (Forward Error Correction)', desc: 'Packet loss recovery without retransmit', enabled: true },
                                    { feature: 'Screen Sharing', desc: 'Application/window/tab with audio', enabled: true },
                                    { feature: 'E2E Encryption', desc: 'Insertable Streams API (SFrame)', enabled: true },
                                    { feature: 'Noise Suppression', desc: 'AI-based background noise cancellation', enabled: true },
                                    { feature: 'Virtual Background', desc: 'ML segmentation + GPU compositing', enabled: true },
                                ].map(f => (
                                    <div key={f.feature} className="flex items-center justify-between py-2 border-b border-grey-800/50 last:border-0">
                                        <div>
                                            <p className="text-sm text-grey-300">{f.feature}</p>
                                            <p className="text-xs text-grey-500">{f.desc}</p>
                                        </div>
                                        <Badge variant={f.enabled ? 'success' : 'default'}>{f.enabled ? 'Enabled' : 'Disabled'}</Badge>
                                    </div>
                                ))}
                            </div>
                        </div>

                        <div className="gs-card p-6">
                            <h3 className="text-sm font-semibold text-grey-300 mb-4">Meeting Quality Metrics</h3>
                            <div className="space-y-4">
                                {[
                                    { metric: 'Video Quality Score (MOS)', value: 4.2, max: 5, pct: 84 },
                                    { metric: 'Audio Quality Score (MOS)', value: 4.5, max: 5, pct: 90 },
                                    { metric: 'Packet Loss Rate', value: 0.3, max: 5, pct: 6 },
                                    { metric: 'Jitter (ms)', value: 12, max: 100, pct: 12 },
                                    { metric: 'Round-Trip Time (ms)', value: 45, max: 300, pct: 15 },
                                ].map(m => (
                                    <div key={m.metric}>
                                        <div className="flex justify-between mb-1">
                                            <span className="text-sm text-grey-300">{m.metric}</span>
                                            <span className="text-sm font-mono text-grey-200">{m.value}</span>
                                        </div>
                                        <ProgressBar value={m.pct} color={m.pct < 50 ? 'bg-accent-emerald' : m.pct < 80 ? 'bg-brand-500' : 'bg-accent-rose'} />
                                    </div>
                                ))}
                            </div>
                        </div>
                    </div>
                </div>
            )}

            {activeTab === 'Retention' && (
                <div className="space-y-6">
                    <div className="gs-card p-6">
                        <h3 className="text-sm font-semibold text-grey-300 mb-4">Message Retention & Compliance Policies</h3>
                        <DataTable
                            columns={['Policy Name', 'Scope', 'Retention', 'Legal Hold', 'eDiscovery', 'DLP', 'Status']}
                            rows={[
                                ['Default Corporate', 'All channels', '7 years', <Badge variant="success">Enabled</Badge>, <Badge variant="success">Indexed</Badge>, <Badge variant="success">Active</Badge>, <Badge variant="success">Enforced</Badge>],
                                ['Financial Services', '#finance-*', '10 years', <Badge variant="success">Enabled</Badge>, <Badge variant="success">Indexed</Badge>, <Badge variant="success">Active</Badge>, <Badge variant="success">Enforced</Badge>],
                                ['Healthcare (HIPAA)', '#health-*', '6 years', <Badge variant="success">Enabled</Badge>, <Badge variant="success">Indexed</Badge>, <Badge variant="success">Active</Badge>, <Badge variant="success">Enforced</Badge>],
                                ['Legal Hold - Case #2847', 'Targeted users', '∞', <Badge variant="warning">Active Hold</Badge>, <Badge variant="success">Indexed</Badge>, <Badge variant="success">Active</Badge>, <Badge variant="warning">Hold</Badge>],
                                ['Ephemeral Channels', '#temp-*', '30 days', <Badge variant="default">Disabled</Badge>, <Badge variant="default">Excluded</Badge>, <Badge variant="success">Active</Badge>, <Badge variant="success">Enforced</Badge>],
                                ['Executive Comms', '#exec-*', '3 years', <Badge variant="success">Enabled</Badge>, <Badge variant="success">Indexed</Badge>, <Badge variant="success">Active</Badge>, <Badge variant="success">Enforced</Badge>],
                            ]}
                        />
                    </div>

                    <div className="grid grid-cols-2 gap-6">
                        <div className="gs-card p-6">
                            <h3 className="text-sm font-semibold text-grey-300 mb-4">Data Loss Prevention (DLP) Rules</h3>
                            <div className="space-y-3">
                                {[
                                    { rule: 'Credit Card Detection', pattern: 'Regex + Luhn validation', action: 'Block + Alert', triggers: 142 },
                                    { rule: 'SSN Pattern Match', pattern: 'NER + Regex', action: 'Block + Alert', triggers: 23 },
                                    { rule: 'Source Code Leak', pattern: 'TF-IDF classifier', action: 'Warn + Log', triggers: 891 },
                                    { rule: 'Confidential Labels', pattern: 'Sensitivity label match', action: 'Block external', triggers: 67 },
                                    { rule: 'PHI Detection', pattern: 'Medical NER model', action: 'Block + Alert', triggers: 12 },
                                    { rule: 'API Key / Secret', pattern: 'Entropy + pattern match', action: 'Redact + Alert', triggers: 456 },
                                ].map(r => (
                                    <div key={r.rule} className="bg-grey-800/50 rounded-lg p-3 border border-grey-700/50">
                                        <div className="flex items-center justify-between mb-2">
                                            <span className="text-sm font-semibold text-grey-200">{r.rule}</span>
                                            <Badge variant="danger">{r.triggers} triggers</Badge>
                                        </div>
                                        <div className="flex items-center justify-between text-xs text-grey-500">
                                            <span>{r.pattern}</span>
                                            <span className="text-amber-400">{r.action}</span>
                                        </div>
                                    </div>
                                ))}
                            </div>
                        </div>

                        <div className="gs-card p-6">
                            <h3 className="text-sm font-semibold text-grey-300 mb-4">eDiscovery & Export</h3>
                            <div className="space-y-3">
                                {[
                                    { type: 'Content Search', desc: 'Full-text across all retained messages', status: 'Available' },
                                    { type: 'Custodian Hold', desc: 'Preserve all content for specific users', status: '3 active' },
                                    { type: 'PST Export', desc: 'Export to PST/EML/JSON/MBOX formats', status: 'Available' },
                                    { type: 'Audit Trail', desc: 'Immutable log of all search/export ops', status: 'Enabled' },
                                    { type: 'Chain of Custody', desc: 'Cryptographic evidence integrity', status: 'SHA-256' },
                                    { type: 'Review Set', desc: 'AI-powered relevance scoring for review', status: '2 active' },
                                ].map(e => (
                                    <div key={e.type} className="flex items-center justify-between py-2 border-b border-grey-800/50 last:border-0">
                                        <div>
                                            <p className="text-sm text-grey-300">{e.type}</p>
                                            <p className="text-xs text-grey-500">{e.desc}</p>
                                        </div>
                                        <Badge variant="info">{e.status}</Badge>
                                    </div>
                                ))}
                            </div>
                        </div>
                    </div>
                </div>
            )}

            {activeTab === 'File Sharing' && (
                <div className="space-y-6">
                    <div className="grid grid-cols-4 gap-4">
                        <StatCard label="Files Shared (30d)" value="184,291" change="+14.2%" changeType="positive" icon={<Paperclip size={18} />} />
                        <StatCard label="Storage Used" value="2.4 TB" change="+180 GB" changeType="neutral" icon={<HardDrive size={18} />} />
                        <StatCard label="Active Versions" value="482K" icon={<RefreshCw size={18} />} />
                        <StatCard label="Malware Blocked" value="23" change="This month" changeType="negative" icon={<Shield size={18} />} />
                    </div>

                    <DataTable
                        columns={['Feature', 'Implementation', 'Max Size', 'Versioning', 'Scanning', 'Status']}
                        rows={[
                            ['Inline Upload', 'Resumable chunked upload (tus protocol)', '5 GB', 'Auto-version on overwrite', 'ClamAV + ML', <Badge variant="success">Active</Badge>],
                            ['Drag & Drop', 'HTML5 File API + progress tracking', '5 GB', 'Same', 'Same', <Badge variant="success">Active</Badge>],
                            ['Clipboard Paste', 'Clipboard API with image optimization', '25 MB', 'N/A', 'Same', <Badge variant="success">Active</Badge>],
                            ['External Share', 'Pre-signed URLs with expiry + password', '5 GB', 'Snapshot at share time', 'Same + DLP', <Badge variant="success">Active</Badge>],
                            ['API Upload', 'REST + GraphQL with multipart/form-data', '10 GB', 'Full history', 'Same', <Badge variant="success">Active</Badge>],
                        ]}
                    />

                    <div className="gs-card p-6">
                        <h3 className="text-sm font-semibold text-grey-300 mb-4">Permission & Access Model</h3>
                        <div className="bg-grey-900 rounded-lg p-4 font-mono text-xs text-grey-400">
                            <pre>{`// File Permission Inheritance
Channel File
├── Inherits channel membership
├── Override: restrict to specific users
├── External share: requires admin approval
│   ├── Password protection (optional)
│   ├── Expiry date (required for external)
│   ├── Download limit (optional)
│   └── Watermark (optional for sensitive)
└── Version Access
    ├── Current version: all members
    ├── Previous versions: configurable
    └── Deleted files: 30-day soft delete

// Storage Backend: S3-compatible (MinIO)
// CDN: CloudFront with signed URLs
// Thumbnails: Lambda@Edge generation
// Preview: LibreOffice + PDF.js + video transcode`}</pre>
                        </div>
                    </div>
                </div>
            )}
        </div>
    );
}
