import { useState } from 'react';
import { PageHeader, StatCard, Badge, DataTable, Tabs, MiniSparkline } from '../../components/ui';
import {
    MessageSquare, Heart, Share, Repeat, Image, Send,
    MoreHorizontal, ThumbsUp, Globe, Users, TrendingUp,
    Clock, Eye, Award, Bookmark, Bell, Filter
} from 'lucide-react';

const posts = [
    {
        id: 1, author: 'Sarah Chen', avatar: 'SC', headline: 'Staff Engineer @ Acme Corp', time: '2h ago',
        content: 'Just open-sourced our graph permission engine! 🚀 After 6 months of building it internally at Acme, we\'re releasing it to the community. It handles 6M+ authorization checks/day using graph traversal for RBAC/ABAC.\n\nBuilt with Rust + Neo4j + gRPC. Benchmarks: 0.8ms p50, 2.4ms p99.\n\nCheck it out: github.com/sarahchen/graph-perm-engine',
        reactions: { like: 248, celebrate: 42, insightful: 38, love: 12 }, comments: 34, reposts: 86, views: 4200,
        visibility: 'Public', tags: ['#OpenSource', '#Rust', '#Authorization'],
    },
    {
        id: 2, author: 'James O\'Brien', avatar: 'JO', headline: 'Principal Architect @ Umbrella', time: '5h ago',
        content: 'Hot take: The biggest mistake in microservices is starting with microservices.\n\nStart with a well-structured monolith. Extract services when you have:\n1. Clear bounded contexts\n2. Independent deployment needs\n3. Different scaling requirements\n4. Team autonomy pressure\n\nConvert when the pain is real, not theoretical.',
        reactions: { like: 486, celebrate: 8, insightful: 124, love: 6 }, comments: 92, reposts: 142, views: 12800,
        visibility: 'Public', tags: ['#SystemDesign', '#Architecture'],
    },
    {
        id: 3, author: 'Anika Gupta', avatar: 'AG', headline: 'Director of Data Science @ Stark', time: '8h ago',
        content: 'We just shipped a feature store that reduced our ML model training pipeline from 4 hours to 22 minutes. Key insight: precompute and version features at ingestion time, not at training time.\n\nFull write-up on our engineering blog.',
        reactions: { like: 186, celebrate: 62, insightful: 48, love: 18 }, comments: 28, reposts: 42, views: 6400,
        visibility: 'Public', tags: ['#MachineLearning', '#DataEngineering'],
    },
    {
        id: 4, author: 'Acme Corporation', avatar: 'AC', headline: 'Enterprise Software Company', time: '1d ago',
        content: '📢 We\'re hiring! 42 open positions across engineering, product, and design.\n\nHighlights:\n- Staff Platform Engineer (Remote)\n- Senior ML Engineer (SF/NY)\n- Engineering Manager, Data Platform (SF)\n\nAll roles: careers.acme.corp',
        reactions: { like: 124, celebrate: 28, insightful: 4, love: 8 }, comments: 18, reposts: 64, views: 8200,
        visibility: 'Public', tags: ['#Hiring', '#EngineeringJobs'],
    },
];

const comments = [
    { author: 'Marcus Johnson', avatar: 'MJ', time: '1h ago', content: 'This is incredible work, Sarah! The graph traversal approach is exactly what we\'ve been exploring at Globex. Mind if I DM you about integration?', likes: 12, postId: 1 },
    { author: 'David Kim', avatar: 'DK', time: '45m ago', content: 'Benchmarks look solid. How does it handle multi-tenant isolation in the permission graph?', likes: 8, postId: 1 },
    { author: 'Emily Rodriguez', avatar: 'ER', time: '30m ago', content: 'We\'re using a similar pattern at Datadog. One thing to watch: cache invalidation when permission hierarchies change mid-session.', likes: 14, postId: 1 },
];

const feedAlgorithm = [
    { signal: 'Connection Distance', weight: 0.30, desc: '1st degree > 2nd degree > followed company' },
    { signal: 'Content Recency', weight: 0.25, desc: 'Time decay with 24h half-life' },
    { signal: 'Engagement Score', weight: 0.20, desc: 'Reactions + comments + reposts (weighted)' },
    { signal: 'Content Relevance', weight: 0.15, desc: 'Skill/topic match with viewer profile' },
    { signal: 'Author Authority', weight: 0.10, desc: 'Network size + endorsements + post history' },
];

const notifications = [
    { type: 'reaction', message: 'James O\'Brien and 42 others reacted to your post', time: '2h ago', read: false },
    { type: 'comment', message: 'Marcus Johnson commented on your post', time: '3h ago', read: false },
    { type: 'connection', message: 'Emily Rodriguez accepted your connection request', time: '5h ago', read: true },
    { type: 'mention', message: 'David Kim mentioned you in a comment', time: '8h ago', read: true },
    { type: 'job', message: 'New job that matches your profile: Staff Engineer at Stripe', time: '1d ago', read: true },
    { type: 'milestone', message: 'Your post reached 1,000 views!', time: '1d ago', read: true },
];

export function NetworkFeed() {
    const [activeTab, setActiveTab] = useState('Home Feed');

    return (
        <div>
            <PageHeader
                title="Activity Feed"
                subtitle="Posts, comments, reactions, and shares across your professional network"
                actions={
                    <button className="gs-btn-primary flex items-center gap-2"><Send size={16} /> New Post</button>
                }
            />

            <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-4 gap-4 mb-6">
                <StatCard label="Posts (30d)" value="12" change="4.2K avg reach" changeType="neutral" icon={<MessageSquare size={18} />} />
                <StatCard label="Engagement Rate" value="8.4%" change="+1.2% MoM" changeType="positive" icon={<TrendingUp size={18} />} />
                <StatCard label="Total Impressions" value="52K" change="+28% MoM" changeType="positive" icon={<Eye size={18} />} />
                <StatCard label="Notifications" value="24" change="6 unread" changeType="neutral" icon={<Bell size={18} />} />
            </div>

            <Tabs tabs={['Home Feed', 'Profile Feed', 'Company Feed', 'Notifications', 'Feed Algorithm', 'Feed API']} active={activeTab} onChange={setActiveTab} />

            {activeTab === 'Home Feed' && (
                <div className="space-y-4">
                    {/* Post Composer */}
                    <div className="gs-card p-4">
                        <div className="flex items-start gap-3">
                            <div className="w-10 h-10 rounded-full bg-brand-500 flex items-center justify-center text-sm font-bold text-white flex-shrink-0">SC</div>
                            <div className="flex-1">
                                <div className="bg-grey-800/50 rounded-lg p-3 text-sm text-grey-500 cursor-pointer hover:bg-grey-800/70 transition-colors">
                                    Share an insight, project, or update...
                                </div>
                                <div className="flex items-center gap-4 mt-2">
                                    <button className="flex items-center gap-1.5 text-xs text-grey-500 hover:text-grey-300"><Image size={14} /> Media</button>
                                    <button className="flex items-center gap-1.5 text-xs text-grey-500 hover:text-grey-300"><Award size={14} /> Milestone</button>
                                    <button className="flex items-center gap-1.5 text-xs text-grey-500 hover:text-grey-300"><Globe size={14} /> Public</button>
                                </div>
                            </div>
                        </div>
                    </div>

                    {/* Posts */}
                    {posts.map(post => (
                        <div key={post.id} className="gs-card p-5">
                            <div className="flex items-start justify-between mb-3">
                                <div className="flex items-center gap-3">
                                    <div className="w-12 h-12 rounded-full bg-grey-800 flex items-center justify-center text-sm font-bold text-grey-400">{post.avatar}</div>
                                    <div>
                                        <p className="text-sm font-medium text-grey-200">{post.author}</p>
                                        <p className="text-xs text-grey-500">{post.headline}</p>
                                        <p className="text-xs text-grey-600 flex items-center gap-1"><Clock size={10} />{post.time} · <Globe size={10} />{post.visibility}</p>
                                    </div>
                                </div>
                                <button className="text-grey-600 hover:text-grey-400"><MoreHorizontal size={16} /></button>
                            </div>

                            <div className="text-sm text-grey-300 leading-relaxed whitespace-pre-line mb-3">{post.content}</div>

                            <div className="flex gap-1.5 mb-3">
                                {post.tags.map(tag => (
                                    <span key={tag} className="text-xs text-brand-400 hover:underline cursor-pointer">{tag}</span>
                                ))}
                            </div>

                            <div className="flex items-center justify-between py-2 border-t border-b border-grey-800 mb-3">
                                <div className="flex items-center gap-1">
                                    <div className="flex -space-x-1">
                                        <span className="w-5 h-5 rounded-full bg-brand-500 border border-grey-950 flex items-center justify-center"><ThumbsUp size={10} className="text-white" /></span>
                                        <span className="w-5 h-5 rounded-full bg-accent-emerald border border-grey-950 flex items-center justify-center"><Heart size={10} className="text-white" /></span>
                                    </div>
                                    <span className="text-xs text-grey-500 ml-1">{Object.values(post.reactions).reduce((a, b) => a + b, 0)}</span>
                                </div>
                                <div className="flex items-center gap-3 text-xs text-grey-500">
                                    <span>{post.comments} comments</span>
                                    <span>{post.reposts} reposts</span>
                                    <span>{post.views.toLocaleString()} views</span>
                                </div>
                            </div>

                            <div className="flex items-center gap-1">
                                <button className="flex-1 flex items-center justify-center gap-1.5 py-2 rounded-lg text-xs text-grey-400 hover:bg-grey-800/50 transition-colors"><ThumbsUp size={14} /> Like</button>
                                <button className="flex-1 flex items-center justify-center gap-1.5 py-2 rounded-lg text-xs text-grey-400 hover:bg-grey-800/50 transition-colors"><MessageSquare size={14} /> Comment</button>
                                <button className="flex-1 flex items-center justify-center gap-1.5 py-2 rounded-lg text-xs text-grey-400 hover:bg-grey-800/50 transition-colors"><Repeat size={14} /> Repost</button>
                                <button className="flex-1 flex items-center justify-center gap-1.5 py-2 rounded-lg text-xs text-grey-400 hover:bg-grey-800/50 transition-colors"><Share size={14} /> Share</button>
                            </div>
                        </div>
                    ))}
                </div>
            )}

            {activeTab === 'Profile Feed' && (
                <div className="space-y-6">
                    <div className="gs-card p-5">
                        <h3 className="gs-section-title mb-4 flex items-center gap-2"><Users size={16} /> Sarah Chen&apos;s Posts</h3>
                        <div className="space-y-4">
                            {posts.filter(p => p.author === 'Sarah Chen').map(post => (
                                <div key={post.id} className="p-4 rounded-lg border border-grey-800">
                                    <p className="text-xs text-grey-500 mb-2 flex items-center gap-1"><Clock size={10} />{post.time}</p>
                                    <p className="text-sm text-grey-300 line-clamp-3">{post.content.split('\n')[0]}</p>
                                    <div className="flex items-center gap-4 mt-2 text-xs text-grey-500">
                                        <span className="flex items-center gap-1"><ThumbsUp size={10} />{Object.values(post.reactions).reduce((a, b) => a + b, 0)}</span>
                                        <span className="flex items-center gap-1"><MessageSquare size={10} />{post.comments}</span>
                                        <span className="flex items-center gap-1"><Eye size={10} />{post.views.toLocaleString()}</span>
                                    </div>
                                </div>
                            ))}
                        </div>
                    </div>

                    <div className="gs-card p-5">
                        <h3 className="gs-section-title mb-4">Post Performance (30d)</h3>
                        <MiniSparkline data={[120, 340, 280, 560, 420, 680, 520, 840, 720, 1100, 860, 1247]} height={80} />
                        <div className="grid grid-cols-4 gap-3 mt-3 text-center">
                            <div><p className="text-lg font-bold text-grey-100">12</p><p className="text-xs text-grey-500">Posts</p></div>
                            <div><p className="text-lg font-bold text-grey-100">1,842</p><p className="text-xs text-grey-500">Reactions</p></div>
                            <div><p className="text-lg font-bold text-grey-100">248</p><p className="text-xs text-grey-500">Comments</p></div>
                            <div><p className="text-lg font-bold text-grey-100">52K</p><p className="text-xs text-grey-500">Impressions</p></div>
                        </div>
                    </div>
                </div>
            )}

            {activeTab === 'Company Feed' && (
                <div className="gs-card p-5">
                    <h3 className="gs-section-title mb-4 flex items-center gap-2"><Globe size={16} /> Company Posts (Followed Companies)</h3>
                    <div className="space-y-4">
                        {posts.filter(p => p.author === 'Acme Corporation').map(post => (
                            <div key={post.id} className="p-4 rounded-lg border border-grey-800">
                                <div className="flex items-center gap-3 mb-3">
                                    <div className="w-10 h-10 rounded-lg bg-grey-800 flex items-center justify-center text-xs font-bold text-grey-400">{post.avatar}</div>
                                    <div>
                                        <p className="text-sm font-medium text-grey-200">{post.author}</p>
                                        <p className="text-xs text-grey-500">{post.headline} · {post.time}</p>
                                    </div>
                                </div>
                                <p className="text-sm text-grey-300 whitespace-pre-line">{post.content}</p>
                                <div className="flex items-center gap-4 mt-3 text-xs text-grey-500">
                                    <span className="flex items-center gap-1"><ThumbsUp size={10} />{Object.values(post.reactions).reduce((a, b) => a + b, 0)}</span>
                                    <span className="flex items-center gap-1"><MessageSquare size={10} />{post.comments}</span>
                                    <span className="flex items-center gap-1"><Share size={10} />{post.reposts}</span>
                                </div>
                            </div>
                        ))}
                    </div>
                </div>
            )}

            {activeTab === 'Notifications' && (
                <div className="gs-card p-5">
                    <h3 className="gs-section-title mb-4 flex items-center gap-2"><Bell size={16} /> Activity Notifications</h3>
                    <div className="space-y-1">
                        {notifications.map((n, i) => (
                            <div key={i} className={`flex items-center gap-3 p-3 rounded-lg transition-colors ${n.read ? 'hover:bg-grey-800/30' : 'bg-brand-500/5 hover:bg-brand-500/10 border-l-2 border-brand-500'}`}>
                                <div className={`w-8 h-8 rounded-full flex items-center justify-center flex-shrink-0 ${n.read ? 'bg-grey-800' : 'bg-brand-500/20'}`}>
                                    {n.type === 'reaction' && <ThumbsUp size={14} className={n.read ? 'text-grey-500' : 'text-brand-400'} />}
                                    {n.type === 'comment' && <MessageSquare size={14} className={n.read ? 'text-grey-500' : 'text-brand-400'} />}
                                    {n.type === 'connection' && <Users size={14} className={n.read ? 'text-grey-500' : 'text-accent-emerald'} />}
                                    {n.type === 'mention' && <Award size={14} className={n.read ? 'text-grey-500' : 'text-accent-amber'} />}
                                    {n.type === 'job' && <Bookmark size={14} className={n.read ? 'text-grey-500' : 'text-accent-violet'} />}
                                    {n.type === 'milestone' && <TrendingUp size={14} className={n.read ? 'text-grey-500' : 'text-accent-cyan'} />}
                                </div>
                                <div className="flex-1">
                                    <p className={`text-sm ${n.read ? 'text-grey-400' : 'text-grey-200'}`}>{n.message}</p>
                                    <p className="text-xs text-grey-600">{n.time}</p>
                                </div>
                                {!n.read && <div className="w-2 h-2 rounded-full bg-brand-400 flex-shrink-0" />}
                            </div>
                        ))}
                    </div>
                </div>
            )}

            {activeTab === 'Feed Algorithm' && (
                <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
                    <div className="gs-card p-5">
                        <h3 className="gs-section-title mb-4 flex items-center gap-2"><Filter size={16} /> Feed Ranking Signals</h3>
                        <div className="space-y-3">
                            {feedAlgorithm.map((s, i) => (
                                <div key={i} className="p-3 rounded-lg border border-grey-800">
                                    <div className="flex items-center justify-between mb-2">
                                        <span className="text-sm font-medium text-grey-200">{s.signal}</span>
                                        <span className="text-sm font-bold text-accent-cyan">{(s.weight * 100).toFixed(0)}%</span>
                                    </div>
                                    <p className="text-xs text-grey-500 mb-2">{s.desc}</p>
                                    <div className="w-full bg-grey-800 rounded-full h-1.5">
                                        <div className="bg-brand-500 h-1.5 rounded-full" style={{ width: `${s.weight * 100}%` }} />
                                    </div>
                                </div>
                            ))}
                        </div>
                    </div>
                    <div className="gs-card p-5">
                        <h3 className="gs-section-title mb-4">Ranking Pipeline</h3>
                        <div className="bg-grey-900/50 rounded-lg p-4 font-mono text-xs space-y-2">
                            <div className="text-grey-500">// Feed ranking pipeline (v1: rule-based)</div>
                            <div className="text-accent-cyan">function rankFeedItems(viewer, candidates) {'{'}</div>
                            <div className="pl-3 text-grey-300">return candidates</div>
                            <div className="pl-6 text-grey-400">.map(post =&gt; {'({'}</div>
                            <div className="pl-9 text-grey-400">...post,</div>
                            <div className="pl-9 text-grey-400">score:</div>
                            <div className="pl-12 text-accent-emerald">connectionDistance(viewer, post.author) * 0.30 +</div>
                            <div className="pl-12 text-accent-emerald">recencyDecay(post.created_at) * 0.25 +</div>
                            <div className="pl-12 text-accent-emerald">engagementScore(post) * 0.20 +</div>
                            <div className="pl-12 text-accent-emerald">contentRelevance(viewer, post) * 0.15 +</div>
                            <div className="pl-12 text-accent-emerald">authorAuthority(post.author) * 0.10</div>
                            <div className="pl-6 text-grey-400">{'})'}</div>
                            <div className="pl-6 text-grey-400">.sort((a, b) =&gt; b.score - a.score)</div>
                            <div className="pl-6 text-grey-400">.slice(0, 50);</div>
                            <div className="text-accent-cyan">{'}'}</div>
                            <div className="mt-3 text-grey-600">// v2: Replace with embedding-based ranking</div>
                            <div className="text-grey-600">// using viewer_embedding · post_embedding cosine similarity</div>
                        </div>
                    </div>
                </div>
            )}

            {activeTab === 'Feed API' && (
                <div className="gs-card p-5">
                    <h3 className="gs-section-title mb-4 flex items-center gap-2"><Globe size={16} /> Feed Service API</h3>
                    <div className="space-y-2">
                        {[
                            { method: 'POST', path: '/api/v1/posts', desc: 'Create a new post (text, attachments, visibility)' },
                            { method: 'GET', path: '/api/v1/feed/home', desc: 'Ranked home feed for current user' },
                            { method: 'GET', path: '/api/v1/feed/profile/{id}', desc: 'Posts by a specific person' },
                            { method: 'GET', path: '/api/v1/feed/company/{id}', desc: 'Posts by/about a specific company' },
                            { method: 'POST', path: '/api/v1/posts/{id}/react', desc: 'Add reaction (like, celebrate, insightful, love)' },
                            { method: 'POST', path: '/api/v1/posts/{id}/comment', desc: 'Add comment to a post' },
                            { method: 'POST', path: '/api/v1/posts/{id}/repost', desc: 'Share/repost to your network' },
                            { method: 'DELETE', path: '/api/v1/posts/{id}', desc: 'Delete own post' },
                            { method: 'GET', path: '/api/v1/notifications', desc: 'Activity notifications (paginated)' },
                            { method: 'PUT', path: '/api/v1/notifications/read', desc: 'Mark notifications as read' },
                        ].map((e, i) => (
                            <div key={i} className="flex items-center gap-3 p-2.5 rounded-lg bg-grey-800/30">
                                <Badge variant={e.method === 'GET' ? 'info' : e.method === 'POST' ? 'success' : e.method === 'PUT' ? 'warning' : 'danger'}>{e.method}</Badge>
                                <span className="font-mono text-xs text-accent-cyan flex-1">{e.path}</span>
                                <span className="text-xs text-grey-500">{e.desc}</span>
                            </div>
                        ))}
                    </div>
                </div>
            )}
        </div>
    );
}
