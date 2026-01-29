/**
 * Card Component
 */
import React from 'react'
import './Card.css'

interface CardProps {
  children: React.ReactNode
  title?: string
  actions?: React.ReactNode
  padding?: 'none' | 'sm' | 'md' | 'lg'
  className?: string
}

export function Card({
  children,
  title,
  actions,
  padding = 'md',
  className = ''
}: CardProps) {
  return (
    <div className={`card card--padding-${padding} ${className}`}>
      {(title || actions) && (
        <div className="card-header">
          {title && <h3 className="card-title">{title}</h3>}
          {actions && <div className="card-actions">{actions}</div>}
        </div>
      )}
      <div className="card-content">
        {children}
      </div>
    </div>
  )
}
