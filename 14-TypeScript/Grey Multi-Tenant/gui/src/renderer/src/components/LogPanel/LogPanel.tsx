/**
 * Log Panel Component
 */
import React from 'react'
import { useLogsStore } from '../../stores'
import './LogPanel.css'

export function LogPanel() {
  const { logs, fetchLogs, clearLogs } = useLogsStore()
  const panelRef = React.useRef<HTMLDivElement>(null)

  React.useEffect(() => {
    fetchLogs()
  }, [fetchLogs])

  React.useEffect(() => {
    if (panelRef.current) {
      panelRef.current.scrollTop = panelRef.current.scrollHeight
    }
  }, [logs])

  const getLevelClass = (level: string) => {
    switch (level) {
      case 'error': return 'log-entry--error'
      case 'warn': return 'log-entry--warn'
      case 'info': return 'log-entry--info'
      default: return 'log-entry--debug'
    }
  }

  const formatTime = (date: Date) => {
    return new Date(date).toLocaleTimeString()
  }

  return (
    <div className="log-panel">
      <div className="log-panel-header">
        <span>Logs</span>
        <button className="log-clear-btn" onClick={clearLogs}>Clear</button>
      </div>
      <div className="log-panel-content" ref={panelRef}>
        {logs.length === 0 ? (
          <div className="log-empty">No logs to display</div>
        ) : (
          logs.map((log, index) => (
            <div key={index} className={`log-entry ${getLevelClass(log.level)}`}>
              <span className="log-time">{formatTime(log.timestamp)}</span>
              <span className="log-level">[{log.level.toUpperCase()}]</span>
              <span className="log-message">{log.message}</span>
            </div>
          ))
        )}
      </div>
    </div>
  )
}
