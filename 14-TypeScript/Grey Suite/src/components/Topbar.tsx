import { Bell, Search, Settings, User, Command } from 'lucide-react';

interface TopbarProps {
    onSearchOpen: () => void;
}

export function Topbar({ onSearchOpen }: TopbarProps) {
    return (
        <header className="h-14 border-b border-grey-800 bg-grey-950/80 backdrop-blur-md flex items-center justify-between px-6 flex-shrink-0">
            <div className="flex items-center gap-3">
                <button
                    onClick={onSearchOpen}
                    className="flex items-center gap-2 bg-grey-900 border border-grey-800 rounded-lg px-3 py-1.5 text-sm text-grey-500 hover:border-grey-600 hover:text-grey-300 transition-colors min-w-[280px]"
                >
                    <Search size={14} />
                    <span className="flex-1 text-left">Search or jump to...</span>
                    <div className="flex items-center gap-0.5">
                        <kbd className="text-xs bg-grey-800 text-grey-400 px-1.5 py-0.5 rounded">
                            <Command size={10} className="inline" />
                        </kbd>
                        <kbd className="text-xs bg-grey-800 text-grey-400 px-1.5 py-0.5 rounded">K</kbd>
                    </div>
                </button>
            </div>

            <div className="flex items-center gap-2">
                {/* Notifications */}
                <button className="gs-btn-ghost relative p-2">
                    <Bell size={18} />
                    <span className="absolute top-1.5 right-1.5 w-2 h-2 bg-accent-rose rounded-full dot-pulse" />
                </button>

                {/* Settings */}
                <button className="gs-btn-ghost p-2">
                    <Settings size={18} />
                </button>

                {/* User Avatar */}
                <button className="flex items-center gap-2 ml-2 pl-2 border-l border-grey-800">
                    <div className="w-8 h-8 rounded-full bg-gradient-to-br from-brand-400 to-brand-600 flex items-center justify-center">
                        <User size={16} className="text-white" />
                    </div>
                    <div className="text-left hidden lg:block">
                        <p className="text-sm font-medium text-grey-200">Admin User</p>
                        <p className="text-xs text-grey-500">Enterprise</p>
                    </div>
                </button>
            </div>
        </header>
    );
}
