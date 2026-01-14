/**
 * RecentActions Component
 * 
 * Displays a list of recent enforcement actions taken by the optimizer.
 */
function RecentActions({ actions }) {
  if (!actions || actions.length === 0) {
    return (
      <div className="bg-grey-800 rounded-xl border border-grey-700 p-6">
        <h2 className="text-lg font-semibold text-white mb-4 flex items-center">
          <svg className="w-5 h-5 mr-2 text-grey-400" fill="none" stroke="currentColor" viewBox="0 0 24 24">
            <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M12 8v4l3 3m6-3a9 9 0 11-18 0 9 9 0 0118 0z" />
          </svg>
          Recent Actions
        </h2>
        <p className="text-grey-500 text-sm text-center py-4">No recent actions</p>
      </div>
    )
  }

  const getActionIcon = (type) => {
    switch (type) {
      case 'cpu':
        return <span className="text-blue-400">⚡</span>
      case 'ram':
        return <span className="text-green-400">🧠</span>
      case 'disk':
        return <span className="text-purple-400">💾</span>
      case 'rollback':
        return <span className="text-red-400">↩️</span>
      default:
        return <span className="text-grey-400">🔧</span>
    }
  }

  const getStatusBadge = (status) => {
    switch (status) {
      case 'success':
        return <span className="px-2 py-0.5 rounded text-xs bg-green-900/50 text-green-300">Success</span>
      case 'failed':
        return <span className="px-2 py-0.5 rounded text-xs bg-red-900/50 text-red-300">Failed</span>
      case 'simulated':
        return <span className="px-2 py-0.5 rounded text-xs bg-yellow-900/50 text-yellow-300">Simulated</span>
      default:
        return <span className="px-2 py-0.5 rounded text-xs bg-grey-700 text-grey-300">{status}</span>
    }
  }

  return (
    <div className="bg-grey-800 rounded-xl border border-grey-700 p-6">
      <h2 className="text-lg font-semibold text-white mb-4 flex items-center">
        <svg className="w-5 h-5 mr-2 text-grey-400" fill="none" stroke="currentColor" viewBox="0 0 24 24">
          <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M12 8v4l3 3m6-3a9 9 0 11-18 0 9 9 0 0118 0z" />
        </svg>
        Recent Actions
      </h2>

      <div className="space-y-3 max-h-80 overflow-y-auto">
        {actions.slice(0, 10).map((action, index) => (
          <div 
            key={action.id || index}
            className="flex items-start space-x-3 p-3 rounded-lg bg-grey-900/50"
          >
            <div className="text-lg mt-0.5">
              {getActionIcon(action.type)}
            </div>
            <div className="flex-grow min-w-0">
              <div className="flex items-center justify-between mb-1">
                <p className="text-sm font-medium text-white truncate">
                  {action.description || action.action}
                </p>
                {getStatusBadge(action.status)}
              </div>
              <p className="text-xs text-grey-500">
                {action.timestamp ? new Date(action.timestamp).toLocaleTimeString() : 'Just now'}
                {action.target && <span className="ml-2 text-grey-600">• {action.target}</span>}
              </p>
            </div>
          </div>
        ))}
      </div>

      {actions.length > 10 && (
        <p className="text-xs text-grey-500 text-center mt-4">
          Showing 10 of {actions.length} actions
        </p>
      )}
    </div>
  )
}

export default RecentActions
