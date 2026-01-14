import { useState, useEffect, useRef, useCallback } from 'react'

/**
 * Custom hook for WebSocket connection with auto-reconnect
 * 
 * @param {string} url - WebSocket URL to connect to
 * @param {object} options - Configuration options
 * @returns {object} - WebSocket state and methods
 */
export function useWebSocket(url, options = {}) {
  const {
    reconnectInterval = 3000,
    maxReconnectAttempts = 10,
    onOpen,
    onClose,
    onError,
    onMessage
  } = options

  const [connectionStatus, setConnectionStatus] = useState('disconnected')
  const [lastMessage, setLastMessage] = useState(null)
  const [reconnectAttempts, setReconnectAttempts] = useState(0)

  const wsRef = useRef(null)
  const reconnectTimeoutRef = useRef(null)

  // Connect to WebSocket
  const connect = useCallback(() => {
    if (wsRef.current?.readyState === WebSocket.OPEN) {
      return
    }

    try {
      setConnectionStatus('connecting')
      wsRef.current = new WebSocket(url)

      wsRef.current.onopen = (event) => {
        setConnectionStatus('connected')
        setReconnectAttempts(0)
        onOpen?.(event)
      }

      wsRef.current.onclose = (event) => {
        setConnectionStatus('disconnected')
        onClose?.(event)

        // Auto-reconnect if not intentionally closed
        if (!event.wasClean && reconnectAttempts < maxReconnectAttempts) {
          reconnectTimeoutRef.current = setTimeout(() => {
            setReconnectAttempts(prev => prev + 1)
            connect()
          }, reconnectInterval)
        }
      }

      wsRef.current.onerror = (event) => {
        setConnectionStatus('error')
        onError?.(event)
      }

      wsRef.current.onmessage = (event) => {
        setLastMessage(event.data)
        onMessage?.(event)
      }
    } catch (error) {
      console.error('WebSocket connection error:', error)
      setConnectionStatus('error')
    }
  }, [url, reconnectAttempts, maxReconnectAttempts, reconnectInterval, onOpen, onClose, onError, onMessage])

  // Disconnect from WebSocket
  const disconnect = useCallback(() => {
    if (reconnectTimeoutRef.current) {
      clearTimeout(reconnectTimeoutRef.current)
    }

    if (wsRef.current) {
      wsRef.current.close(1000, 'Client disconnect')
      wsRef.current = null
    }

    setConnectionStatus('disconnected')
  }, [])

  // Send message through WebSocket
  const sendMessage = useCallback((message) => {
    if (wsRef.current?.readyState === WebSocket.OPEN) {
      wsRef.current.send(typeof message === 'string' ? message : JSON.stringify(message))
      return true
    }
    return false
  }, [])

  // Connect on mount, disconnect on unmount
  useEffect(() => {
    connect()

    return () => {
      disconnect()
    }
  }, []) // eslint-disable-line react-hooks/exhaustive-deps

  return {
    connectionStatus,
    lastMessage,
    reconnectAttempts,
    sendMessage,
    connect,
    disconnect
  }
}
