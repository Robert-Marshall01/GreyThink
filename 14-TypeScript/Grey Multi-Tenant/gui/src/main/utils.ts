/**
 * Electron Toolkit Utils
 * Helpers for Electron main process
 */
import { app, BrowserWindow, shell } from 'electron'
import { platform } from 'os'

/**
 * Check if running in development mode
 */
export const is = {
  dev: !app.isPackaged,
  windows: platform() === 'win32',
  macos: platform() === 'darwin',
  linux: platform() === 'linux'
}

/**
 * Electron App utilities
 */
export const electronApp = {
  /**
   * Set the app user model ID for Windows
   */
  setAppUserModelId(id: string): void {
    if (is.windows) {
      app.setAppUserModelId(id)
    }
  }
}

/**
 * Window optimizer utilities
 */
export const optimizer = {
  /**
   * Watch window shortcuts for DevTools toggle
   */
  watchWindowShortcuts(window: BrowserWindow): void {
    window.webContents.on('before-input-event', (event, input) => {
      // Toggle DevTools with F12
      if (input.key === 'F12') {
        window.webContents.toggleDevTools()
        event.preventDefault()
      }
      // Reload with Ctrl+R / Cmd+R
      if ((input.control || input.meta) && input.key === 'r') {
        window.reload()
        event.preventDefault()
      }
    })

    // Handle external links
    window.webContents.setWindowOpenHandler(({ url }) => {
      shell.openExternal(url)
      return { action: 'deny' }
    })
  }
}

/**
 * Externalize dependencies plugin for Vite
 */
export function externalizeDepsPlugin() {
  return {
    name: 'externalize-deps',
    config() {
      return {
        build: {
          rollupOptions: {
            external: [
              'electron',
              ...Object.keys(require('../../package.json').dependencies || {})
            ]
          }
        }
      }
    }
  }
}
