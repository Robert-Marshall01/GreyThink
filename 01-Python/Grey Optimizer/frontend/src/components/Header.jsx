/**
 * Header Component
 * 
 * Displays the Grey Optimizer logo, status indicators,
 * and global controls like the rollback button.
 */
function Header({ status, onRollback }) {
  return (
    <header className="bg-grey-800 border-b border-grey-700 sticky top-0 z-50">
      <div className="container mx-auto px-4 py-3">
        <div className="flex items-center justify-between">
          {/* Logo and title */}
          <div className="flex items-center space-x-3">
            <div className="w-10 h-10 rounded-lg bg-gradient-to-br from-grey-500 to-grey-700 flex items-center justify-center">
              <svg className="w-6 h-6 text-green-400" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M5 13l4 4L19 7" />
              </svg>
            </div>
            <div>
              <h1 className="text-xl font-bold text-white">Grey Optimizer</h1>
              <p className="text-xs text-grey-400">Resource Optimization Dashboard</p>
            </div>
          </div>

          {/* Status indicators */}
          <div className="flex items-center space-x-6">
            {/* Connection status */}
            <div className="flex items-center space-x-2">
              <span className={`w-2 h-2 rounded-full status-dot ${
                status.connected ? 'bg-green-400' : 'bg-red-400'
              }`} />
              <span className="text-sm text-grey-300">
                {status.connected ? 'Connected' : 'Disconnected'}
              </span>
            </div>

            {/* Mode indicator */}
            <div className={`px-3 py-1 rounded-full text-sm font-medium ${
              status.simulation_mode 
                ? 'bg-yellow-900/50 text-yellow-300 border border-yellow-700'
                : 'bg-green-900/50 text-green-300 border border-green-700'
            }`}>
              {status.simulation_mode ? '🧪 Simulation' : '⚡ Live'}
            </div>

            {/* Rollback button */}
            <button
              onClick={onRollback}
              className="px-4 py-2 bg-red-600 hover:bg-red-700 text-white rounded-lg text-sm font-medium transition-colors flex items-center space-x-2"
            >
              <svg className="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M3 10h10a8 8 0 018 8v2M3 10l6 6m-6-6l6-6" />
              </svg>
              <span>Rollback</span>
            </button>
          </div>
        </div>
      </div>
    </header>
  )
}

export default Header
