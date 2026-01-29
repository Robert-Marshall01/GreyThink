/**
 * Login Screen
 */
import React, { useState } from 'react'
import { useNavigate } from 'react-router-dom'
import { Button, Input, Card, StatusIndicator } from '../../components'
import { useAuthStore, useServicesStore } from '../../stores'
import './LoginScreen.css'

export function LoginScreen() {
  const navigate = useNavigate()
  const { login, signUp, error } = useAuthStore()
  const { statuses, checkStatus } = useServicesStore()
  
  const [isSignUp, setIsSignUp] = useState(false)
  const [name, setName] = useState('')
  const [email, setEmail] = useState('')
  const [password, setPassword] = useState('')
  const [confirmPassword, setConfirmPassword] = useState('')
  const [isSubmitting, setIsSubmitting] = useState(false)
  const [localError, setLocalError] = useState('')

  React.useEffect(() => {
    checkStatus()
  }, [checkStatus])

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault()
    setLocalError('')
    console.log('[LoginScreen] handleSubmit called, isSignUp=', isSignUp)
    
    if (isSignUp) {
      if (password !== confirmPassword) {
        setLocalError('Passwords do not match')
        return
      }
      if (password.length < 6) {
        setLocalError('Password must be at least 6 characters')
        return
      }
      setIsSubmitting(true)
      console.log('[LoginScreen] Calling signUp...')
      const success = await signUp(email, password, name)
      console.log('[LoginScreen] signUp returned:', success)
      setIsSubmitting(false)
      if (success) {
        console.log('[LoginScreen] signUp success, navigating to /dashboard')
        navigate('/dashboard')
      } else {
        console.log('[LoginScreen] signUp failed')
      }
    } else {
      setIsSubmitting(true)
      console.log('[LoginScreen] Calling login with email:', email)
      const success = await login(email, password)
      console.log('[LoginScreen] login returned:', success)
      setIsSubmitting(false)
      if (success) {
        console.log('[LoginScreen] login success, navigating to /dashboard')
        navigate('/dashboard')
      } else {
        console.log('[LoginScreen] login failed, error state:', error)
      }
    }
  }

  const toggleMode = () => {
    setIsSignUp(!isSignUp)
    setLocalError('')
    setName('')
    setPassword('')
    setConfirmPassword('')
  }

  const allServicesRunning = statuses.length > 0 && statuses.every(s => s.status === 'running')
  const displayError = localError || error

  return (
    <div className="login-screen">
      <div className="login-container">
        <div className="login-header">
          <div className="login-logo">
            <span className="logo-icon">◇</span>
            <h1>Grey Multi-Tenant</h1>
          </div>
          <p className="login-subtitle">
            {isSignUp ? 'Create a new account' : 'Sign in to your account'}
          </p>
        </div>

        <Card className="login-card">
          <form onSubmit={handleSubmit} className="login-form">
            {isSignUp && (
              <Input
                label="Name"
                type="text"
                value={name}
                onChange={(e) => setName(e.target.value)}
                placeholder="Enter your name"
                required
                autoFocus
              />
            )}
            
            <Input
              label="Email"
              type="email"
              value={email}
              onChange={(e) => setEmail(e.target.value)}
              placeholder="Enter your email"
              required
              autoFocus={!isSignUp}
            />
            
            <Input
              label="Password"
              type="password"
              value={password}
              onChange={(e) => setPassword(e.target.value)}
              placeholder="Enter your password"
              required
            />

            {isSignUp && (
              <Input
                label="Confirm Password"
                type="password"
                value={confirmPassword}
                onChange={(e) => setConfirmPassword(e.target.value)}
                placeholder="Confirm your password"
                required
              />
            )}

            {displayError && (
              <div className="login-error">
                {displayError}
              </div>
            )}

            <Button
              type="submit"
              variant="primary"
              fullWidth
              loading={isSubmitting}
              disabled={!email || !password || isSubmitting || (isSignUp && (!name || !confirmPassword))}
            >
              {isSignUp ? 'Sign Up' : 'Sign In'}
            </Button>

            <div className="login-toggle">
              <span>{isSignUp ? 'Already have an account?' : "Don't have an account?"}</span>
              <button type="button" className="toggle-link" onClick={toggleMode}>
                {isSignUp ? 'Sign In' : 'Sign Up'}
              </button>
            </div>
          </form>
        </Card>

        <div className="login-services">
          <h3>Service Status</h3>
          <div className="services-grid">
            {statuses.map((service) => (
              <div key={service.name} className="service-status-item">
                <StatusIndicator status={service.status} />
                <span>{service.name}</span>
              </div>
            ))}
          </div>
          {!allServicesRunning && (
            <p className="services-warning">
              ⚠️ Some services are not running. Login may fail.
            </p>
          )}
        </div>
      </div>
    </div>
  )
}
