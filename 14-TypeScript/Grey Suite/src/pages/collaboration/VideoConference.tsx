import { PageHeader, Badge } from '../../components/ui';
import { Video, Mic, MicOff, Monitor, Phone, Users, Calendar, Plus } from 'lucide-react';
import { useState } from 'react';

export function VideoConference() {
    const [inCall] = useState(false);

    const scheduled = [
        { title: 'Sprint Planning', time: '10:00 AM', duration: '1h', attendees: 12, status: 'Starting Soon' },
        { title: 'Design Review', time: '11:30 AM', duration: '45m', attendees: 6, status: 'Upcoming' },
        { title: '1:1 with Sarah', time: '2:00 PM', duration: '30m', attendees: 2, status: 'Upcoming' },
        { title: 'All Hands Meeting', time: '3:30 PM', duration: '1h', attendees: 342, status: 'Upcoming' },
    ];

    const recentRecordings = [
        { title: 'Product Roadmap Review', date: 'Feb 9', duration: '52:14', views: 24 },
        { title: 'Q4 Retrospective', date: 'Feb 7', duration: '1:12:08', views: 86 },
        { title: 'Client Demo — Acme Corp', date: 'Feb 5', duration: '38:42', views: 12 },
        { title: 'Architecture Decision Record', date: 'Feb 3', duration: '45:20', views: 34 },
    ];

    return (
        <div>
            <PageHeader
                title="Video Conferencing"
                subtitle="Meetings, calls & screen sharing"
                actions={
                    <div className="flex gap-2">
                        <button className="gs-btn-secondary text-sm flex items-center gap-2"><Calendar size={14} /> Schedule</button>
                        <button className="gs-btn-primary text-sm flex items-center gap-2"><Video size={14} /> New Meeting</button>
                    </div>
                }
            />

            {/* Quick Join */}
            <div className="gs-card p-6 mb-6 flex items-center justify-between">
                <div>
                    <h3 className="text-lg font-semibold text-grey-100">Start or Join a Meeting</h3>
                    <p className="text-sm text-grey-500 mt-1">Enter a meeting ID or create a new instant meeting</p>
                </div>
                <div className="flex items-center gap-3">
                    <input className="gs-input text-sm w-48" placeholder="Meeting ID or link..." />
                    <button className="gs-btn-primary text-sm">Join</button>
                </div>
            </div>

            <div className="grid grid-cols-1 lg:grid-cols-2 gap-6 mb-6">
                {/* Scheduled Meetings */}
                <div className="gs-card p-5">
                    <div className="flex items-center justify-between mb-4">
                        <h3 className="gs-section-title">Today's Meetings</h3>
                        <span className="text-xs text-grey-500">Feb 10, 2026</span>
                    </div>
                    <div className="space-y-3">
                        {scheduled.map(m => (
                            <div key={m.title} className="flex items-center justify-between p-3 rounded-lg border border-grey-800 hover:border-grey-700 transition-colors">
                                <div className="flex items-center gap-3">
                                    <div className={`w-10 h-10 rounded-lg flex items-center justify-center ${m.status === 'Starting Soon' ? 'bg-accent-emerald/15 text-accent-emerald' : 'bg-grey-800 text-grey-400'}`}>
                                        <Video size={18} />
                                    </div>
                                    <div>
                                        <p className="text-sm font-medium text-grey-200">{m.title}</p>
                                        <p className="text-xs text-grey-500">{m.time} · {m.duration} · {m.attendees} attendees</p>
                                    </div>
                                </div>
                                <div className="flex items-center gap-2">
                                    {m.status === 'Starting Soon' ? (
                                        <button className="gs-btn-primary text-xs px-3 py-1">Join Now</button>
                                    ) : (
                                        <Badge>{m.status}</Badge>
                                    )}
                                </div>
                            </div>
                        ))}
                    </div>
                </div>

                {/* Recent Recordings */}
                <div className="gs-card p-5">
                    <h3 className="gs-section-title mb-4">Recent Recordings</h3>
                    <div className="space-y-3">
                        {recentRecordings.map(r => (
                            <div key={r.title} className="flex items-center justify-between p-3 rounded-lg hover:bg-grey-800/40 transition-colors cursor-pointer">
                                <div className="flex items-center gap-3">
                                    <div className="w-10 h-10 rounded-lg bg-grey-800 flex items-center justify-center text-grey-400">
                                        <Monitor size={18} />
                                    </div>
                                    <div>
                                        <p className="text-sm font-medium text-grey-200">{r.title}</p>
                                        <p className="text-xs text-grey-500">{r.date} · {r.duration}</p>
                                    </div>
                                </div>
                                <span className="text-xs text-grey-500">{r.views} views</span>
                            </div>
                        ))}
                    </div>
                </div>
            </div>

            {/* Meeting Rooms */}
            <div className="gs-card p-5">
                <h3 className="gs-section-title mb-4">Virtual Meeting Rooms</h3>
                <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-3">
                    {[
                        { name: 'Engineering Standup', active: true, participants: 8 },
                        { name: 'Conference Room A', active: false, participants: 0 },
                        { name: 'Brainstorm Space', active: true, participants: 3 },
                        { name: 'Open Office', active: false, participants: 0 },
                    ].map(room => (
                        <div key={room.name} className={`p-4 rounded-lg border ${room.active ? 'border-accent-emerald/30 bg-accent-emerald/5' : 'border-grey-800 bg-grey-800/30'}`}>
                            <div className="flex items-center justify-between mb-2">
                                <span className="text-sm font-medium text-grey-200">{room.name}</span>
                                {room.active && <span className="w-2 h-2 rounded-full bg-accent-emerald dot-pulse" />}
                            </div>
                            <p className="text-xs text-grey-500">{room.active ? `${room.participants} participants` : 'Empty'}</p>
                            <button className={`mt-3 w-full text-xs py-1.5 rounded-lg ${room.active ? 'gs-btn-primary' : 'gs-btn-secondary'}`}>
                                {room.active ? 'Join Room' : 'Open Room'}
                            </button>
                        </div>
                    ))}
                </div>
            </div>
        </div>
    );
}
