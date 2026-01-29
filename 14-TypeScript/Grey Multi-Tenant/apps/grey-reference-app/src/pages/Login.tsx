/**
 * Login Page
 * 
 * Provides email/password login form.
 * Uses useAuth() hook from grey-react for authentication.
 */

import { useState, type FormEvent } from 'react';
import { useNavigate, useLocation } from 'react-router-dom';
import { useAuth } from '@grey/react';

/**
 * Login Component
 * 
 * Simple login form that:
 * - Accepts email and password
 * - Calls useAuth().login() on submit
 * - Shows loading state during authentication
 * - Displays error messages on failure
 * - Redirects to intended destination or dashboard on success
 * 
 * TODO: Add form validation
 * TODO: Add "forgot password" link
 * TODO: Add social login buttons
 * TODO: Replace with proper UI components
 */
export function Login() {
  const navigate = useNavigate();
  const location = useLocation();
  const { login, loading, error } = useAuth();

  // Form state
  const [email, setEmail] = useState('');
  const [password, setPassword] = useState('');
  const [localError, setLocalError] = useState<string | null>(null);

  // Get the intended destination from location state (set by ProtectedRoute)
  const from = (location.state as { from?: { pathname: string } })?.from?.pathname || '/dashboard';

  const handleSubmit = async (e: FormEvent) => {
    e.preventDefault();
    setLocalError(null);

    // Basic validation
    if (!email.trim()) {
      setLocalError('Email is required');
      return;
    }
    if (!password) {
      setLocalError('Password is required');
      return;
    }

    try {
      // Call the login action from useAuth hook
      await login(email, password);
      
      // Success - redirect to intended destination
      navigate(from, { replace: true });
    } catch (err) {
      // Error is already captured in auth state, but we can also handle it here
      console.error('Login failed:', err);
    }
  };

  const displayError = localError || error?.message;

  return (
    <div style={{
      minHeight: '100vh',
      display: 'flex',
      alignItems: 'center',
      justifyContent: 'center',
      background: 'linear-gradient(135deg, #1a1a2e 0%, #16213e 100%)',
    }}>
      <div style={{
        background: 'white',
        padding: '2rem',
        borderRadius: '8px',
        boxShadow: '0 4px 6px rgba(0,0,0,0.1)',
        width: '100%',
        maxWidth: '400px',
      }}>
        <h1 style={{ 
          margin: '0 0 1.5rem', 
          fontSize: '1.5rem', 
          textAlign: 'center',
          color: '#1a1a2e',
        }}>
          Grey Multi-Tenant
        </h1>

        <form onSubmit={handleSubmit}>
          {/* Email field */}
          <div style={{ marginBottom: '1rem' }}>
            <label 
              htmlFor="email" 
              style={{ display: 'block', marginBottom: '0.5rem', fontWeight: 500 }}
            >
              Email
            </label>
            <input
              id="email"
              type="email"
              value={email}
              onChange={(e) => setEmail(e.target.value)}
              placeholder="user@example.com"
              disabled={loading}
              style={{
                width: '100%',
                padding: '0.75rem',
                border: '1px solid #ddd',
                borderRadius: '4px',
                fontSize: '1rem',
              }}
            />
          </div>

          {/* Password field */}
          <div style={{ marginBottom: '1.5rem' }}>
            <label 
              htmlFor="password" 
              style={{ display: 'block', marginBottom: '0.5rem', fontWeight: 500 }}
            >
              Password
            </label>
            <input
              id="password"
              type="password"
              value={password}
              onChange={(e) => setPassword(e.target.value)}
              placeholder="••••••••"
              disabled={loading}
              style={{
                width: '100%',
                padding: '0.75rem',
                border: '1px solid #ddd',
                borderRadius: '4px',
                fontSize: '1rem',
              }}
            />
          </div>

          {/* Error message */}
          {displayError && (
            <div style={{
              marginBottom: '1rem',
              padding: '0.75rem',
              background: '#fee2e2',
              border: '1px solid #fecaca',
              borderRadius: '4px',
              color: '#dc2626',
              fontSize: '0.875rem',
            }}>
              {displayError}
            </div>
          )}

          {/* Submit button */}
          <button
            type="submit"
            disabled={loading}
            style={{
              width: '100%',
              padding: '0.75rem',
              background: loading ? '#9ca3af' : '#1a1a2e',
              color: 'white',
              border: 'none',
              borderRadius: '4px',
              fontSize: '1rem',
              fontWeight: 500,
              cursor: loading ? 'not-allowed' : 'pointer',
            }}
          >
            {loading ? 'Signing in...' : 'Sign In'}
          </button>
        </form>

        {/* Demo credentials hint */}
        <p style={{
          marginTop: '1.5rem',
          fontSize: '0.75rem',
          color: '#666',
          textAlign: 'center',
        }}>
          {/* TODO: Remove this in production */}
          Demo: Use any valid credentials from your backend
        </p>
      </div>
    </div>
  );
}
