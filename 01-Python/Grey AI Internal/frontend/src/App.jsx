/**
 * Grey AI Internal - Main App Component
 * ======================================
 * Multi-tab AI demonstration platform showcasing broad AI mastery.
 * Each tab demonstrates a distinct AI capability with consistent design.
 */

import { useState, useCallback } from 'react'

// Layout Components
import Navbar from './components/layout/Navbar'
import Container from './components/layout/Container'
import TabNavigation from './components/layout/TabNavigation'

// Tab Components
import {
  HomeTab,
  DataAnalysisTab,
  AIWritingTab,
  AICodeTab,
  AIArtTab,
  AIAudioTab,
  GovernanceTab,
  BITab,
} from './components/tabs'

/**
 * Tab Component Map
 * Maps tab IDs to their respective components
 */
const TAB_COMPONENTS = {
  home: HomeTab,
  data: DataAnalysisTab,
  writing: AIWritingTab,
  code: AICodeTab,
  art: AIArtTab,
  audio: AIAudioTab,
  governance: GovernanceTab,
  bi: BITab,
}

/**
 * Main Application Component
 * Manages multi-tab navigation and renders active tab content
 */
function App() {
  // ---------------------------------------------------------------------------
  // State Management
  // ---------------------------------------------------------------------------
  
  // Active tab state
  const [activeTab, setActiveTab] = useState('home')

  // ---------------------------------------------------------------------------
  // Event Handlers
  // ---------------------------------------------------------------------------

  /**
   * Handle tab change
   */
  const handleTabChange = useCallback((tabId) => {
    setActiveTab(tabId)
    // Scroll to top when changing tabs
    window.scrollTo({ top: 0, behavior: 'smooth' })
  }, [])

  /**
   * Reset to home tab
   */
  const handleReset = useCallback(() => {
    setActiveTab('home')
  }, [])

  // ---------------------------------------------------------------------------
  // Render
  // ---------------------------------------------------------------------------

  // Get active tab component
  const ActiveTabComponent = TAB_COMPONENTS[activeTab] || HomeTab

  return (
    <div className="min-h-screen bg-gray-50 flex flex-col">
      {/* Navigation Bar */}
      <Navbar onReset={handleReset} />

      {/* Tab Navigation */}
      <TabNavigation 
        activeTab={activeTab} 
        onTabChange={handleTabChange} 
      />

      {/* Main Content */}
      <main className="flex-1 py-8">
        <Container>
          <ActiveTabComponent onTabChange={handleTabChange} />
        </Container>
      </main>

      {/* Footer */}
      <footer className="py-6 border-t border-gray-100 bg-white mt-auto">
        <Container>
          <div className="flex flex-col sm:flex-row items-center justify-between gap-4">
            <p className="text-sm text-gray-500">
              Grey AI Internal &copy; {new Date().getFullYear()} — Multi-Domain AI Platform
            </p>
            <div className="flex items-center gap-4 text-sm text-gray-400">
              <span className="flex items-center gap-1">
                <span className="w-2 h-2 bg-green-500 rounded-full animate-pulse"></span>
                Powered by Local LLM
              </span>
              <span>•</span>
              <span>8 AI Domains</span>
            </div>
          </div>
        </Container>
      </footer>
    </div>
  )
}

export default App
