import { PageHeader, StatCard, Badge, MiniSparkline } from '../../components/ui';
import { Star, TrendingUp, MessageSquare, ThumbsUp } from 'lucide-react';

export function Reputation() {
    return (
        <div>
            <PageHeader
                title="Reputation"
                subtitle="Company reviews, discussions & workplace insights"
            />

            <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-4 gap-4 mb-6">
                <StatCard label="Overall Rating" value="4.6/5" change="+0.2 YoY" changeType="positive" icon={<Star size={18} />} />
                <StatCard label="Reviews" value={284} change="+18 this month" changeType="positive" icon={<MessageSquare size={18} />} />
                <StatCard label="Recommend Rate" value="92%" change="+4%" changeType="positive" icon={<ThumbsUp size={18} />} />
                <StatCard label="CEO Approval" value="96%" change="+2%" changeType="positive" icon={<TrendingUp size={18} />} />
            </div>

            <div className="grid grid-cols-1 lg:grid-cols-2 gap-6 mb-6">
                <div className="gs-card p-5">
                    <h3 className="gs-section-title mb-4">Rating Trends</h3>
                    <MiniSparkline data={[4.2, 4.3, 4.3, 4.4, 4.4, 4.5, 4.5, 4.6, 4.6, 4.6, 4.7, 4.6]} height={100} color="#f59e0b" />
                    <div className="grid grid-cols-2 gap-4 mt-4">
                        {[
                            { category: 'Culture', rating: 4.8 },
                            { category: 'Compensation', rating: 4.4 },
                            { category: 'Work-Life Balance', rating: 4.5 },
                            { category: 'Career Growth', rating: 4.6 },
                            { category: 'Management', rating: 4.3 },
                            { category: 'Diversity', rating: 4.7 },
                        ].map(c => (
                            <div key={c.category} className="flex items-center justify-between p-2">
                                <span className="text-sm text-grey-300">{c.category}</span>
                                <span className="text-sm font-semibold text-accent-amber">★ {c.rating}</span>
                            </div>
                        ))}
                    </div>
                </div>

                <div className="gs-card p-5">
                    <h3 className="gs-section-title mb-4">Recent Reviews</h3>
                    <div className="space-y-4">
                        {[
                            { role: 'Software Engineer', tenure: '2 years', rating: 5, pros: 'Amazing culture, cutting-edge tech stack, great work-life balance', cons: 'Growth can be fast-paced', date: 'Feb 8' },
                            { role: 'Product Manager', tenure: '1 year', rating: 4, pros: 'Collaborative environment, strong leadership, good compensation', cons: 'Sometimes too many meetings', date: 'Feb 5' },
                            { role: 'Data Scientist', tenure: '3 years', rating: 5, pros: 'Best data infrastructure I\'ve worked with, supportive team', cons: 'None significant', date: 'Feb 2' },
                        ].map((r, i) => (
                            <div key={i} className="p-3 bg-grey-800/30 rounded-lg">
                                <div className="flex items-center justify-between mb-2">
                                    <div>
                                        <p className="text-sm font-medium text-grey-200">{r.role}</p>
                                        <p className="text-xs text-grey-500">{r.tenure} · {r.date}</p>
                                    </div>
                                    <span className="text-sm font-semibold text-accent-amber">{'★'.repeat(r.rating)}</span>
                                </div>
                                <p className="text-xs text-grey-400"><span className="text-accent-emerald">Pros:</span> {r.pros}</p>
                                <p className="text-xs text-grey-400 mt-1"><span className="text-accent-amber">Cons:</span> {r.cons}</p>
                            </div>
                        ))}
                    </div>
                </div>
            </div>

            <div className="gs-card p-5">
                <h3 className="gs-section-title mb-4">Workplace Discussions</h3>
                <div className="space-y-3">
                    {[
                        { topic: 'How is the engineering culture at Grey Suite?', replies: 24, likes: 86, category: 'Culture' },
                        { topic: 'Remote work policy — what\'s the latest?', replies: 18, likes: 64, category: 'Policy' },
                        { topic: 'Interview process tips for Grey Suite', replies: 32, likes: 142, category: 'Interviews' },
                        { topic: 'Best teams to join as a new grad?', replies: 14, likes: 48, category: 'Career' },
                    ].map(d => (
                        <div key={d.topic} className="flex items-center justify-between p-3 rounded-lg hover:bg-grey-800/40 transition-colors cursor-pointer">
                            <div>
                                <p className="text-sm text-grey-200">{d.topic}</p>
                                <p className="text-xs text-grey-500">{d.replies} replies · {d.likes} likes</p>
                            </div>
                            <Badge>{d.category}</Badge>
                        </div>
                    ))}
                </div>
            </div>
        </div>
    );
}
