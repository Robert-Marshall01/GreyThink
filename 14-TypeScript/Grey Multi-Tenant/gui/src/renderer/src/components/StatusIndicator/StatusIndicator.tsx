/**
 * Status Indicator Component
 */
import React from 'react'
import './StatusIndicator.css'

interface StatusIndicatorProps {
  status: 'running' | 'stopped' | 'error' | 'unknown'
  label?: string
  showLabel?: boolean
}

export function StatusIndicator({ status, label, showLabel = true }: StatusIndicatorProps) {
  const statusLabels = {
    running: 'Running',
    stopped: 'Stopped',
    error: 'Error',
    unknown: 'Unknown'
  }

  return (
    <div className="status-indicator">
      <span className={`status-dot status-dot--${status}`} />
      {showLabel && (
        <span className="status-label">{label || statusLabels[status]}</span>
      )}
    </div>
  )
}
