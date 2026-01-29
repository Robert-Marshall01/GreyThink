/**
 * Grey Multi-Tenant GUI - Main Process
 * Entry point for the Electron application.
 */
import { app, BrowserWindow, Menu, Tray, nativeImage } from 'electron'
import { join } from 'path'
import { electronApp, optimizer, is } from './utils'
import { setupIpcHandlers } from './ipcHandlers'
import { ServiceManager } from './serviceManager'
import Store from 'electron-store'

// Global error handlers
process.on('uncaughtException', (error) => {
  console.error('Uncaught Exception:', error)
})

process.on('unhandledRejection', (reason: unknown) => {
  console.error('Unhandled Rejection:', reason)
})

// Initialize persistent store
const store = new Store({
  name: 'grey-multitenant-config',
  defaults: {
    httpEndpoint: 'http://localhost:8080',
    grpcEndpoint: 'localhost:50051',
    autoStartServices: true,
    theme: 'system',
    windowBounds: { width: 1200, height: 800 }
  }
})

let mainWindow: BrowserWindow | null = null
let tray: Tray | null = null
const serviceManager = new ServiceManager()

function createWindow(): void {
  const bounds = store.get('windowBounds') as { width: number; height: number }
  
  mainWindow = new BrowserWindow({
    width: bounds.width,
    height: bounds.height,
    minWidth: 800,
    minHeight: 600,
    show: false,
    autoHideMenuBar: false,
    title: 'Grey Multi-Tenant',
    icon: join(__dirname, '../../resources/icon.png'),
    webPreferences: {
      preload: join(__dirname, '../preload/index.js'),
      sandbox: false,
      contextIsolation: true,
      nodeIntegration: false
    }
  })

  mainWindow.on('ready-to-show', () => {
    mainWindow?.show()
  })

  mainWindow.on('resize', () => {
    if (mainWindow) {
      const [width, height] = mainWindow.getSize()
      store.set('windowBounds', { width, height })
    }
  })

  mainWindow.on('closed', () => {
    mainWindow = null
  })

  // Monitor renderer crashes
  mainWindow.webContents.on('crashed', (event, killed) => {
    console.error(`Renderer crashed! killed=${killed}`)
  })

  mainWindow.webContents.on('did-fail-load', (event, errorCode, errorDescription, validatedURL) => {
    console.error(`Failed to load: ${errorCode} ${errorDescription} url=${validatedURL}`)
  })

  // Load the renderer
  if (is.dev && process.env['ELECTRON_RENDERER_URL']) {
    mainWindow.loadURL(process.env['ELECTRON_RENDERER_URL'])
  } else {
    mainWindow.loadFile(join(__dirname, '../renderer/index.html'))
  }
}

function createTray(): void {
  const iconPath = join(__dirname, '../../resources/icon.png')
  const icon = nativeImage.createFromPath(iconPath)
  tray = new Tray(icon.resize({ width: 16, height: 16 }))
  
  const contextMenu = Menu.buildFromTemplate([
    { label: 'Show App', click: () => mainWindow?.show() },
    { label: 'Check Services', click: () => mainWindow?.webContents.send('check-services') },
    { type: 'separator' },
    { label: 'Quit', click: () => app.quit() }
  ])
  
  tray.setToolTip('Grey Multi-Tenant')
  tray.setContextMenu(contextMenu)
  
  tray.on('click', () => {
    mainWindow?.show()
  })
}

function createMenu(): void {
  const template: Electron.MenuItemConstructorOptions[] = [
    {
      label: 'File',
      submenu: [
        { label: 'Settings', accelerator: 'CmdOrCtrl+,', click: () => mainWindow?.webContents.send('navigate', '/settings') },
        { type: 'separator' },
        { role: 'quit' }
      ]
    },
    {
      label: 'View',
      submenu: [
        { role: 'reload' },
        { role: 'forceReload' },
        { role: 'toggleDevTools' },
        { type: 'separator' },
        { role: 'resetZoom' },
        { role: 'zoomIn' },
        { role: 'zoomOut' },
        { type: 'separator' },
        { role: 'togglefullscreen' }
      ]
    },
    {
      label: 'Services',
      submenu: [
        { label: 'Check Status', accelerator: 'CmdOrCtrl+Shift+S', click: () => mainWindow?.webContents.send('check-services') },
        { label: 'Start All Services', click: () => serviceManager.startAll() },
        { label: 'Stop All Services', click: () => serviceManager.stopAll() }
      ]
    },
    {
      label: 'Help',
      submenu: [
        { label: 'Documentation', click: () => require('electron').shell.openExternal('https://grey.dev/docs') },
        { label: 'About', click: () => mainWindow?.webContents.send('navigate', '/about') }
      ]
    }
  ]
  
  const menu = Menu.buildFromTemplate(template)
  Menu.setApplicationMenu(menu)
}

app.whenReady().then(async () => {
  // Set app user model id for windows
  electronApp.setAppUserModelId('com.grey.multitenant')

  // Default open or close DevTools by F12 in development
  app.on('browser-window-created', (_, window) => {
    optimizer.watchWindowShortcuts(window)
  })

  // Setup IPC handlers
  setupIpcHandlers(store, serviceManager)

  // Auto-start PostgreSQL on app launch
  if (store.get('autoStartServices')) {
    console.log('[Main] Auto-starting services...')
    try {
      await serviceManager.ensurePostgres()
    } catch (error) {
      console.error('[Main] Failed to auto-start services:', error)
    }
  }

  createWindow()
  createTray()
  createMenu()

  app.on('activate', () => {
    if (BrowserWindow.getAllWindows().length === 0) {
      createWindow()
    }
  })
})

app.on('window-all-closed', () => {
  if (process.platform !== 'darwin') {
    app.quit()
  }
})

app.on('before-quit', () => {
  // Don't stop services on quit - let them run in background
  serviceManager.stopHealthChecks()
})
