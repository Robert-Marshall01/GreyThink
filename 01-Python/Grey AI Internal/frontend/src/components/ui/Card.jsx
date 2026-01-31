/**
 * Card Component
 * ==============
 * Reusable card container with consistent styling.
 */

import PropTypes from 'prop-types'

function Card({ children, className = '', padding = true }) {
  return (
    <div 
      className={`
        bg-white rounded-xl shadow-sm border border-gray-100
        ${padding ? 'p-6' : ''}
        ${className}
      `}
    >
      {children}
    </div>
  )
}

Card.propTypes = {
  children: PropTypes.node.isRequired,
  className: PropTypes.string,
  padding: PropTypes.bool,
}

export default Card
