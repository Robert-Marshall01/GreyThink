import { PageHeader, StatCard, Badge } from '../../components/ui';
import { Globe, Users, TrendingUp, MessageSquare, ThumbsUp, Plus } from 'lucide-react';

export function Networking() {
    return (
        <div>
            <PageHeader
                title="Professional Networking"
                subtitle="Profiles, posts, connections & professional identity"
                actions={<button className="gs-btn-primary text-sm flex items-center gap-2"><Plus size={14} /> New Post</button>}
            />

            <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-4 gap-4 mb-6">
                <StatCard label="Connections" value="2,847" change="+124 this month" changeType="positive" icon={<Users size={18} />} />
                <StatCard label="Profile Views" value="1,842" change="+32% MoM" changeType="positive" icon={<Globe size={18} />} />
                <StatCard label="Post Impressions" value="48.2K" change="+18% vs avg" changeType="positive" icon={<TrendingUp size={18} />} />
                <StatCard label="Engagement Rate" value="4.8%" change="+0.6%" changeType="positive" icon={<ThumbsUp size={18} />} />
            </div>

            {/* Profile Card */}
            <div className="gs-card p-6 mb-6">
                <div className="flex items-start gap-6">
                    <div className="w-20 h-20 rounded-2xl bg-gradient-to-br from-brand-400 to-accent-violet flex items-center justify-center text-2xl font-bold text-white flex-shrink-0">AU</div>
                    <div className="flex-1">
                        <h3 className="text-xl font-bold text-grey-100">Admin User</h3>
                        <p className="text-sm text-grey-400 mt-1">Chief Technology Officer at Grey Suite</p>
                        <p className="text-sm text-grey-500 mt-0.5">San Francisco Bay Area · 500+ connections</p>
                        <div className="flex gap-2 mt-3">
                            <Badge variant="info">Technology</Badge>
                            <Badge variant="info">Leadership</Badge>
                            <Badge variant="info">AI/ML</Badge>
                            <Badge variant="info">Enterprise Software</Badge>
                        </div>
                    </div>
                    <button className="gs-btn-secondary text-sm">Edit Profile</button>
                </div>
            </div>

            <div className="grid grid-cols-1 lg:grid-cols-3 gap-6">
                {/* Feed */}
                <div className="lg:col-span-2 space-y-4">
                    <h3 className="gs-section-title">Recent Feed</h3>
                    {[
                        { author: 'Sarah Chen', role: 'VP Engineering', time: '2h', content: 'Excited to announce that our engineering team shipped 3 major features this sprint. The new AI-powered analytics dashboard is a game changer for our enterprise clients. 🚀', likes: 142, comments: 28 },
                        { author: 'Marcus Johnson', role: 'Head of Sales', time: '4h', content: 'Just closed our biggest enterprise deal of Q1! The Grey Suite platform resonated strongly with the client\'s need for a unified corporate operating system. Team effort all the way.', likes: 98, comments: 16 },
                        { author: 'Emily Rodriguez', role: 'Design Lead', time: '8h', content: 'Published our new design system documentation. 200+ components, all accessible and theme-aware. Open to feedback from the community! Link in comments.', likes: 214, comments: 42 },
                    ].map(post => (
                        <div key={post.author} className="gs-card p-5">
                            <div className="flex items-center gap-3 mb-3">
                                <div className="w-10 h-10 rounded-full bg-grey-700 flex items-center justify-center text-sm font-bold text-grey-300">
                                    {post.author.split(' ').map(n => n[0]).join('')}
                                </div>
                                <div>
                                    <p className="text-sm font-semibold text-grey-200">{post.author}</p>
                                    <p className="text-xs text-grey-500">{post.role} · {post.time} ago</p>
                                </div>
                            </div>
                            <p className="text-sm text-grey-300 mb-3">{post.content}</p>
                            <div className="flex items-center gap-4 pt-3 border-t border-grey-800 text-xs text-grey-500">
                                <button className="flex items-center gap-1 hover:text-brand-400 transition-colors"><ThumbsUp size={14} /> {post.likes}</button>
                                <button className="flex items-center gap-1 hover:text-brand-400 transition-colors"><MessageSquare size={14} /> {post.comments}</button>
                            </div>
                        </div>
                    ))}
                </div>

                {/* Suggestions */}
                <div>
                    <h3 className="gs-section-title mb-4">Suggested Connections</h3>
                    <div className="space-y-3">
                        {[
                            { name: 'Alex Morgan', role: 'CTO at DataPrime', mutual: 12 },
                            { name: 'Priya Sharma', role: 'VP Product at CloudScale', mutual: 8 },
                            { name: 'Tom Baker', role: 'CEO at NexGen', mutual: 15 },
                            { name: 'Lisa Wang', role: 'Head of AI at Vertex', mutual: 6 },
                            { name: 'James Park', role: 'Director Eng at Apex', mutual: 9 },
                        ].map(s => (
                            <div key={s.name} className="gs-card p-3 flex items-center justify-between">
                                <div className="flex items-center gap-3">
                                    <div className="w-8 h-8 rounded-full bg-grey-700 flex items-center justify-center text-xs font-bold text-grey-300">
                                        {s.name.split(' ').map(n => n[0]).join('')}
                                    </div>
                                    <div>
                                        <p className="text-sm font-medium text-grey-200">{s.name}</p>
                                        <p className="text-xs text-grey-500">{s.role} · {s.mutual} mutual</p>
                                    </div>
                                </div>
                                <button className="gs-btn-secondary text-xs px-2 py-1">Connect</button>
                            </div>
                        ))}
                    </div>
                </div>
            </div>
        </div>
    );
}
