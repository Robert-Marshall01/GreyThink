/**
 * Navbar Component
 * ================
 * Top navigation bar with project branding and optional actions.
 * Fixed at the top of the viewport for consistent navigation.
 */

import PropTypes from 'prop-types'

function Navbar({ onReset }) {
  return (
    <header className="bg-white border-b border-gray-200 sticky top-0 z-50">
      <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8">
        <div className="flex items-center justify-between h-16">
          
          {/* Logo & Title */}
          <div className="flex items-center space-x-3">
            {/* Logo Icon */}
            <div className="w-10 h-10 bg-gradient-to-br from-blue-600 to-indigo-700 rounded-lg flex items-center justify-center shadow-sm">
              <svg 
                className="w-6 h-6 text-white" 
                fill="none" 
                stroke="currentColor" 
                viewBox="0 0 24 24"
              >
                <path 
                  strokeLinecap="round" 
                  strokeLinejoin="round" 
                  strokeWidth={2} 
                  d="M9 19v-6a2 2 0 00-2-2H5a2 2 0 00-2 2v6a2 2 0 002 2h2a2 2 0 002-2zm0 0V9a2 2 0 012-2h2a2 2 0 012 2v10m-6 0a2 2 0 002 2h2a2 2 0 002-2m0 0V5a2 2 0 012-2h2a2 2 0 012 2v14a2 2 0 01-2 2h-2a2 2 0 01-2-2z" 
                />
              </svg>
            </div>
            
            {/* Title */}
            <div>
              <h1 className="text-xl font-bold text-gray-900">
                Grey AI Internal
              </h1>
              <p className="text-xs text-gray-500 hidden sm:block">
                AI-Powered Data Insights
              </p>
            </div>
          </div>

          {/* Right Side Actions */}
          <div className="flex items-center space-x-4">
            {/* Reset Button */}
            {onReset && (
              <button
                onClick={onReset}
                className="text-sm text-gray-600 hover:text-gray-900 flex items-center space-x-1 transition-colors"
              >
                <svg className="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                  <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M4 4v5h.582m15.356 2A8.001 8.001 0 004.582 9m0 0H9m11 11v-5h-.581m0 0a8.003 8.003 0 01-15.357-2m15.357 2H15" />
                </svg>
                <span className="hidden sm:inline">New Analysis</span>
              </button>
            )}
            
            {/* Status Indicator */}
            <div className="flex items-center space-x-2 text-sm text-gray-500">
              <span className="w-2 h-2 bg-green-500 rounded-full animate-pulse"></span>
              <span className="hidden md:inline">Local LLM</span>
            </div>
          </div>
        </div>
      </div>
    </header>
  )
}

Navbar.propTypes = {
  onReset: PropTypes.func,
}

export default Navbar
