/**
 * Grey Reference App - Entry Point
 * 
 * Wraps the application in GreyProvider, React Router, and context providers.
 */

import React from 'react';
import ReactDOM from 'react-dom/client';
import { BrowserRouter } from 'react-router-dom';
import { GreyProvider } from '@grey/react';
import { App } from './App';
import { OrganizationProvider } from './context/OrganizationContext';
import './styles/global.css';

/**
 * API base URL - reads from Vite environment variable
 * Set VITE_GREY_API_URL in .env file or defaults to local backend
 */
const API_BASE_URL = import.meta.env.VITE_GREY_API_URL || 'http://localhost:8080/api/v1';

/**
 * Root component that initializes all providers.
 * 
 * Provider hierarchy:
 * 1. GreyProvider - Authentication, user, and project state from grey-react
 * 2. OrganizationProvider - Active organization context for multi-tenancy
 * 3. BrowserRouter - React Router for navigation
 */
function Root() {
  return (
    <React.StrictMode>
      {/* GreyProvider wraps the entire app to provide auth, user, and projects context */}
      <GreyProvider baseUrl={API_BASE_URL}>
        {/* OrganizationProvider manages the active organization selection */}
        <OrganizationProvider>
          {/* BrowserRouter enables client-side routing */}
          <BrowserRouter>
            <App />
          </BrowserRouter>
        </OrganizationProvider>
      </GreyProvider>
    </React.StrictMode>
  );
}

// Mount the React app to the DOM
const rootElement = document.getElementById('root');
if (!rootElement) {
  throw new Error('Failed to find root element. Make sure index.html has a #root div.');
}

ReactDOM.createRoot(rootElement).render(<Root />);
