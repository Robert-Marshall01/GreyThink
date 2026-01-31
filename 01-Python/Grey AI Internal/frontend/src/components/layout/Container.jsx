/**
 * Container Component
 * ===================
 * Responsive container that constrains content width and adds consistent padding.
 * Used throughout the app for layout consistency.
 */

import PropTypes from 'prop-types'

function Container({ children, className = '' }) {
  return (
    <div className={`max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 ${className}`}>
      {children}
    </div>
  )
}

Container.propTypes = {
  children: PropTypes.node.isRequired,
  className: PropTypes.string,
}

export default Container
