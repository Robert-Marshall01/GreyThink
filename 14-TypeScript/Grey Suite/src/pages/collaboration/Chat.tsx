import { useState } from 'react';
import { Hash, Send, Smile, Paperclip, Phone, Video, Search, Plus, Circle } from 'lucide-react';

const channels = [
    { name: 'general', unread: 3, type: 'channel' },
    { name: 'engineering', unread: 12, type: 'channel' },
    { name: 'marketing', unread: 0, type: 'channel' },
    { name: 'product', unread: 5, type: 'channel' },
    { name: 'random', unread: 1, type: 'channel' },
    { name: 'announcements', unread: 0, type: 'channel' },
];

const dms = [
    { name: 'Sarah Chen', status: 'online', lastMsg: 'Sounds good, let me check the PR' },
    { name: 'Marcus Johnson', status: 'online', lastMsg: 'Can we sync on the demo?' },
    { name: 'Emily Rodriguez', status: 'away', lastMsg: 'Updated the design specs' },
    { name: 'David Kim', status: 'offline', lastMsg: 'Thanks for the feedback!' },
];

const messages = [
    { user: 'Sarah Chen', time: '9:42 AM', text: 'Hey team! The new API endpoints are deployed to staging. Can someone run the integration tests?', avatar: 'SC' },
    { user: 'Marcus Johnson', time: '9:45 AM', text: 'On it — running the full test suite now. Should take about 15 minutes.', avatar: 'MJ' },
    { user: 'Emily Rodriguez', time: '9:48 AM', text: 'I noticed we need to update the docs for the new auth flow. Created a ticket for it: ENG-4521', avatar: 'ER' },
    { user: 'Sarah Chen', time: '9:51 AM', text: 'Perfect. @David can you review the auth changes before we merge to main?', avatar: 'SC' },
    { user: 'David Kim', time: '9:54 AM', text: 'Sure thing. Looking at it now. The token refresh logic looks solid. One minor suggestion on error handling — I\'ll leave a comment on the PR.', avatar: 'DK' },
    { user: 'Marcus Johnson', time: '10:02 AM', text: '✅ All 847 tests passing. No regressions detected. Good to merge!', avatar: 'MJ' },
    { user: 'Bot — Grey CI', time: '10:03 AM', text: '🟢 Build #4521 passed — staging deployment successful. Coverage: 94.2%', avatar: '🤖' },
];

export function Chat() {
    const [activeChannel, setActiveChannel] = useState('engineering');
    const [message, setMessage] = useState('');

    return (
        <div className="flex h-[calc(100vh-8rem)] -m-6">
            {/* Sidebar */}
            <div className="w-64 bg-grey-950 border-r border-grey-800 flex flex-col flex-shrink-0">
                <div className="p-3 border-b border-grey-800">
                    <div className="flex items-center gap-2 bg-grey-900 rounded-lg px-3 py-1.5">
                        <Search size={14} className="text-grey-500" />
                        <input className="bg-transparent text-sm text-grey-200 placeholder-grey-500 outline-none flex-1" placeholder="Search messages..." />
                    </div>
                </div>
                <div className="flex-1 overflow-y-auto p-2">
                    <div className="flex items-center justify-between px-2 py-1 mb-1">
                        <span className="text-xs font-semibold text-grey-500 uppercase tracking-wider">Channels</span>
                        <button className="text-grey-500 hover:text-grey-300"><Plus size={14} /></button>
                    </div>
                    {channels.map(ch => (
                        <button
                            key={ch.name}
                            onClick={() => setActiveChannel(ch.name)}
                            className={`w-full flex items-center gap-2 px-2 py-1.5 rounded-lg text-sm transition-colors ${activeChannel === ch.name ? 'bg-brand-500/10 text-brand-400' : 'text-grey-400 hover:bg-grey-800/60 hover:text-grey-200'
                                }`}
                        >
                            <Hash size={14} />
                            <span className="flex-1 text-left">{ch.name}</span>
                            {ch.unread > 0 && (
                                <span className="bg-brand-500 text-white text-xs px-1.5 py-0.5 rounded-full min-w-[20px] text-center">{ch.unread}</span>
                            )}
                        </button>
                    ))}

                    <div className="flex items-center justify-between px-2 py-1 mb-1 mt-4">
                        <span className="text-xs font-semibold text-grey-500 uppercase tracking-wider">Direct Messages</span>
                        <button className="text-grey-500 hover:text-grey-300"><Plus size={14} /></button>
                    </div>
                    {dms.map(dm => (
                        <button key={dm.name} className="w-full flex items-center gap-2 px-2 py-1.5 rounded-lg text-sm text-grey-400 hover:bg-grey-800/60 hover:text-grey-200 transition-colors">
                            <div className="relative">
                                <div className="w-6 h-6 rounded-full bg-grey-700 flex items-center justify-center text-[10px] font-bold text-grey-300">
                                    {dm.name.split(' ').map(n => n[0]).join('')}
                                </div>
                                <Circle
                                    size={8}
                                    className={`absolute -bottom-0.5 -right-0.5 ${dm.status === 'online' ? 'fill-accent-emerald text-accent-emerald' :
                                            dm.status === 'away' ? 'fill-accent-amber text-accent-amber' : 'fill-grey-600 text-grey-600'
                                        }`}
                                />
                            </div>
                            <span className="flex-1 text-left truncate">{dm.name}</span>
                        </button>
                    ))}
                </div>
            </div>

            {/* Main Chat Area */}
            <div className="flex-1 flex flex-col min-w-0">
                {/* Channel Header */}
                <div className="flex items-center justify-between px-6 py-3 border-b border-grey-800 bg-grey-950/80 backdrop-blur-md">
                    <div className="flex items-center gap-2">
                        <Hash size={18} className="text-grey-500" />
                        <span className="font-semibold text-grey-200">{activeChannel}</span>
                        <span className="text-xs text-grey-500 ml-2">24 members</span>
                    </div>
                    <div className="flex items-center gap-2">
                        <button className="gs-btn-ghost p-2"><Phone size={16} /></button>
                        <button className="gs-btn-ghost p-2"><Video size={16} /></button>
                        <button className="gs-btn-ghost p-2"><Search size={16} /></button>
                    </div>
                </div>

                {/* Messages */}
                <div className="flex-1 overflow-y-auto px-6 py-4 space-y-4">
                    {messages.map((msg, i) => (
                        <div key={i} className="flex gap-3 group hover:bg-grey-800/20 -mx-3 px-3 py-1 rounded-lg transition-colors">
                            <div className="w-9 h-9 rounded-full bg-grey-700 flex items-center justify-center text-xs font-bold text-grey-300 flex-shrink-0 mt-0.5">
                                {msg.avatar}
                            </div>
                            <div className="min-w-0">
                                <div className="flex items-baseline gap-2">
                                    <span className="text-sm font-semibold text-grey-200">{msg.user}</span>
                                    <span className="text-xs text-grey-600">{msg.time}</span>
                                </div>
                                <p className="text-sm text-grey-300 mt-0.5 break-words">{msg.text}</p>
                            </div>
                        </div>
                    ))}
                </div>

                {/* Message Input */}
                <div className="px-6 py-3 border-t border-grey-800">
                    <div className="flex items-center gap-2 bg-grey-900 border border-grey-700 rounded-xl px-4 py-2.5 focus-within:border-brand-500 transition-colors">
                        <button className="text-grey-500 hover:text-grey-300"><Paperclip size={18} /></button>
                        <input
                            value={message}
                            onChange={e => setMessage(e.target.value)}
                            className="flex-1 bg-transparent text-sm text-grey-100 placeholder-grey-500 outline-none"
                            placeholder={`Message #${activeChannel}...`}
                        />
                        <button className="text-grey-500 hover:text-grey-300"><Smile size={18} /></button>
                        <button className="gs-btn-primary p-1.5 rounded-lg"><Send size={16} /></button>
                    </div>
                </div>
            </div>
        </div>
    );
}
