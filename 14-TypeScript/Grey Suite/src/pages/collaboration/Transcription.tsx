import { PageHeader, Badge, DataTable } from '../../components/ui';
import { FileAudio, Clock, Download, Search, Sparkles } from 'lucide-react';

export function Transcription() {
    return (
        <div>
            <PageHeader
                title="Transcription & Meeting Intelligence"
                subtitle="AI-powered transcripts, summaries & action items"
                actions={<button className="gs-btn-primary text-sm flex items-center gap-2"><FileAudio size={14} /> Upload Recording</button>}
            />

            {/* Latest Transcription */}
            <div className="gs-card p-5 mb-6">
                <div className="flex items-center justify-between mb-4">
                    <div>
                        <h3 className="gs-section-title">Sprint Planning — Feb 10, 2026</h3>
                        <p className="text-xs text-grey-500 mt-1">Duration: 52:14 · 8 participants · Transcribed with 98.2% accuracy</p>
                    </div>
                    <div className="flex gap-2">
                        <button className="gs-btn-secondary text-sm flex items-center gap-1"><Download size={14} /> Export</button>
                        <button className="gs-btn-ghost text-sm flex items-center gap-1"><Sparkles size={14} /> AI Summary</button>
                    </div>
                </div>

                <div className="grid grid-cols-1 lg:grid-cols-3 gap-6">
                    {/* Transcript */}
                    <div className="lg:col-span-2">
                        <h4 className="text-sm font-semibold text-grey-400 mb-3">Transcript</h4>
                        <div className="space-y-3 max-h-64 overflow-y-auto">
                            {[
                                { speaker: 'Sarah Chen', time: '0:00', text: "Alright team, let's kick off sprint planning. We have 14 story points of carryover from last sprint." },
                                { speaker: 'Marcus Johnson', time: '0:42', text: "The auth refactor is almost done — I'd estimate 3 more points to finish the token refresh flow." },
                                { speaker: 'Emily Rodriguez', time: '1:18', text: "On the design side, I've completed the mockups for the new dashboard. Ready for engineering review." },
                                { speaker: 'David Kim', time: '2:05', text: "I can pick up the dashboard implementation. The API endpoints are already in staging." },
                                { speaker: 'Sarah Chen', time: '2:38', text: "Great. Let's also prioritize the performance fixes — we're seeing latency spikes in production." },
                            ].map((t, i) => (
                                <div key={i} className="flex gap-3">
                                    <div className="flex-shrink-0 w-12 text-xs text-grey-600 pt-0.5">{t.time}</div>
                                    <div>
                                        <span className="text-xs font-semibold text-brand-400">{t.speaker}</span>
                                        <p className="text-sm text-grey-300 mt-0.5">{t.text}</p>
                                    </div>
                                </div>
                            ))}
                        </div>
                    </div>

                    {/* AI Insights */}
                    <div>
                        <h4 className="text-sm font-semibold text-grey-400 mb-3 flex items-center gap-1"><Sparkles size={14} /> AI Summary</h4>
                        <div className="space-y-3">
                            <div className="p-3 bg-brand-500/10 rounded-lg border border-brand-500/20">
                                <p className="text-xs font-semibold text-brand-400 mb-1">Key Decisions</p>
                                <ul className="text-xs text-grey-300 space-y-1">
                                    <li>• Prioritize auth refactor completion</li>
                                    <li>• Dashboard implementation starts this sprint</li>
                                    <li>• Performance fixes elevated to P1</li>
                                </ul>
                            </div>
                            <div className="p-3 bg-accent-emerald/10 rounded-lg border border-accent-emerald/20">
                                <p className="text-xs font-semibold text-accent-emerald mb-1">Action Items</p>
                                <ul className="text-xs text-grey-300 space-y-1">
                                    <li>• Marcus: Complete token refresh (by Wed)</li>
                                    <li>• David: Start dashboard build (today)</li>
                                    <li>• Sarah: Triage latency issues (by Thu)</li>
                                </ul>
                            </div>
                            <div className="p-3 bg-accent-amber/10 rounded-lg border border-accent-amber/20">
                                <p className="text-xs font-semibold text-accent-amber mb-1">Sentiment</p>
                                <p className="text-xs text-grey-300">Team morale is high. General alignment on priorities. Minor concern about production latency.</p>
                            </div>
                        </div>
                    </div>
                </div>
            </div>

            {/* All Transcriptions */}
            <div className="gs-card p-5">
                <h3 className="gs-section-title mb-4">All Transcriptions</h3>
                <DataTable
                    columns={['Meeting', 'Date', 'Duration', 'Speakers', 'Action Items', 'Status']}
                    rows={[
                        ['Sprint Planning', 'Feb 10', '52:14', '8', '6', <Badge variant="success">Complete</Badge>],
                        ['Design Review', 'Feb 9', '45:30', '6', '4', <Badge variant="success">Complete</Badge>],
                        ['Client Call — Acme', 'Feb 8', '38:42', '4', '8', <Badge variant="success">Complete</Badge>],
                        ['Architecture Review', 'Feb 7', '1:12:08', '10', '12', <Badge variant="success">Complete</Badge>],
                        ['1:1 with Manager', 'Feb 6', '28:15', '2', '3', <Badge variant="info">Processing</Badge>],
                    ]}
                />
            </div>
        </div>
    );
}
