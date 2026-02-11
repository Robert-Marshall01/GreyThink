import { Outlet } from 'react-router-dom';
import { useState } from 'react';
import { Sidebar } from './Sidebar';
import { Topbar } from './Topbar';

export function Layout() {
    const [sidebarCollapsed, setSidebarCollapsed] = useState(false);
    const [searchOpen, setSearchOpen] = useState(false);

    return (
        <div className="flex h-screen overflow-hidden bg-grey-950">
            <Sidebar collapsed={sidebarCollapsed} onToggle={() => setSidebarCollapsed(!sidebarCollapsed)} />
            <div className="flex flex-col flex-1 min-w-0">
                <Topbar onSearchOpen={() => setSearchOpen(!searchOpen)} />
                <main className="flex-1 overflow-auto p-6">
                    <div className="max-w-[1600px] mx-auto animate-fade-in">
                        <Outlet />
                    </div>
                </main>
            </div>
            {/* Command Palette / Search Overlay */}
            {searchOpen && (
                <div className="fixed inset-0 z-50 flex items-start justify-center pt-[20vh]" onClick={() => setSearchOpen(false)}>
                    <div className="fixed inset-0 bg-black/60 backdrop-blur-sm" />
                    <div className="relative z-10 w-full max-w-2xl mx-4" onClick={e => e.stopPropagation()}>
                        <div className="bg-grey-900 border border-grey-700 rounded-xl shadow-panel overflow-hidden">
                            <div className="flex items-center gap-3 px-4 py-3 border-b border-grey-800">
                                <svg className="w-5 h-5 text-grey-500" fill="none" viewBox="0 0 24 24" stroke="currentColor"><path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M21 21l-6-6m2-5a7 7 0 11-14 0 7 7 0 0114 0z" /></svg>
                                <input
                                    autoFocus
                                    className="flex-1 bg-transparent text-grey-100 placeholder-grey-500 outline-none text-base"
                                    placeholder="Search modules, commands, people..."
                                />
                                <kbd className="text-xs text-grey-500 bg-grey-800 px-2 py-0.5 rounded">ESC</kbd>
                            </div>
                            <div className="p-3 text-sm text-grey-500">
                                <p className="px-2 pb-2 text-xs font-semibold uppercase tracking-wider text-grey-600">Quick Actions</p>
                                {['Open Dashboard', 'Go to CRM', 'New Chat', 'Create Task', 'View Reports'].map(action => (
                                    <div key={action} className="px-3 py-2 rounded-lg hover:bg-grey-800 cursor-pointer text-grey-300 hover:text-grey-100 transition-colors">
                                        {action}
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
