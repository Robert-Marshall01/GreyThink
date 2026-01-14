/**
 * AlertBanner Component
 * 
 * Displays a list of alerts with severity indicators.
 * Supports info, warning, and error alert levels.
 */
function AlertBanner({ alerts }) {
  if (!alerts || alerts.length === 0) {
    return null
  }

  const getSeverityStyles = (severity) => {
    switch (severity) {
      case 'error':
        return 'bg-red-900/50 border-red-700 text-red-200'
      case 'warning':
        return 'bg-yellow-900/50 border-yellow-700 text-yellow-200'
      case 'info':
      default:
        return 'bg-blue-900/50 border-blue-700 text-blue-200'
    }
  }

  const getSeverityIcon = (severity) => {
    switch (severity) {
      case 'error':
        return (
          <svg className="w-5 h-5 text-red-400" fill="none" stroke="currentColor" viewBox="0 0 24 24">
            <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M12 8v4m0 4h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z" />
          </svg>
        )
      case 'warning':
        return (
          <svg className="w-5 h-5 text-yellow-400" fill="none" stroke="currentColor" viewBox="0 0 24 24">
            <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M12 9v2m0 4h.01m-6.938 4h13.856c1.54 0 2.502-1.667 1.732-3L13.732 4c-.77-1.333-2.694-1.333-3.464 0L3.34 16c-.77 1.333.192 3 1.732 3z" />
          </svg>
        )
      case 'info':
      default:
        return (
          <svg className="w-5 h-5 text-blue-400" fill="none" stroke="currentColor" viewBox="0 0 24 24">
            <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M13 16h-1v-4h-1m1-4h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z" />
          </svg>
        )
    }
  }

  return (
    <div className="space-y-2 mb-6">
      {alerts.slice(0, 5).map((alert, index) => (
        <div
          key={alert.id || index}
          className={`flex items-center p-3 rounded-lg border ${getSeverityStyles(alert.severity)}`}
        >
          <div className="flex-shrink-0 mr-3">
            {getSeverityIcon(alert.severity)}
          </div>
          <div className="flex-grow">
            <p className="text-sm font-medium">{alert.message}</p>
            {alert.timestamp && (
              <p className="text-xs opacity-75 mt-1">
                {new Date(alert.timestamp).toLocaleTimeString()}
              </p>
            )}
          </div>
          {alert.subsystem && (
            <span className="ml-4 px-2 py-0.5 rounded text-xs font-mono bg-black/20">
              {alert.subsystem}
            </span>
          )}
        </div>
      ))}
      {alerts.length > 5 && (
        <p className="text-sm text-grey-400 text-center">
          +{alerts.length - 5} more alerts
        </p>
      )}
    </div>
  )
}

export default AlertBanner
