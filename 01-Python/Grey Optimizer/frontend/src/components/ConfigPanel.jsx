/**
 * ConfigPanel Component
 * 
 * Displays and allows toggling of optimizer configuration settings.
 */
function ConfigPanel({ config, onToggle }) {
  const configItems = [
    { 
      key: 'cpu_enabled', 
      label: 'CPU Optimization', 
      description: 'Control CPU quotas via cgroup v2',
      color: 'text-blue-400'
    },
    { 
      key: 'ram_enabled', 
      label: 'RAM Optimization', 
      description: 'Manage memory limits and KSM',
      color: 'text-green-400'
    },
    { 
      key: 'disk_enabled', 
      label: 'Disk Optimization', 
      description: 'Control I/O priority and cache',
      color: 'text-purple-400'
    },
    { 
      key: 'simulation_mode', 
      label: 'Simulation Mode', 
      description: 'Test without applying changes',
      color: 'text-yellow-400'
    },
  ]

  return (
    <div className="bg-grey-800 rounded-xl border border-grey-700 p-6">
      <h2 className="text-lg font-semibold text-white mb-4 flex items-center">
        <svg className="w-5 h-5 mr-2 text-grey-400" fill="none" stroke="currentColor" viewBox="0 0 24 24">
          <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M10.325 4.317c.426-1.756 2.924-1.756 3.35 0a1.724 1.724 0 002.573 1.066c1.543-.94 3.31.826 2.37 2.37a1.724 1.724 0 001.065 2.572c1.756.426 1.756 2.924 0 3.35a1.724 1.724 0 00-1.066 2.573c.94 1.543-.826 3.31-2.37 2.37a1.724 1.724 0 00-2.572 1.065c-.426 1.756-2.924 1.756-3.35 0a1.724 1.724 0 00-2.573-1.066c-1.543.94-3.31-.826-2.37-2.37a1.724 1.724 0 00-1.065-2.572c-1.756-.426-1.756-2.924 0-3.35a1.724 1.724 0 001.066-2.573c-.94-1.543.826-3.31 2.37-2.37.996.608 2.296.07 2.572-1.065z" />
          <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M15 12a3 3 0 11-6 0 3 3 0 016 0z" />
        </svg>
        Configuration
      </h2>

      <div className="space-y-4">
        {configItems.map(item => (
          <div 
            key={item.key}
            className="flex items-center justify-between p-3 rounded-lg bg-grey-900/50 hover:bg-grey-900 transition-colors"
          >
            <div>
              <p className={`font-medium ${item.color}`}>{item.label}</p>
              <p className="text-xs text-grey-500 mt-0.5">{item.description}</p>
            </div>
            <button
              onClick={() => onToggle(item.key)}
              className={`relative w-12 h-6 rounded-full transition-colors ${
                config[item.key] ? 'bg-green-600' : 'bg-grey-600'
              }`}
            >
              <span 
                className={`absolute top-1 w-4 h-4 rounded-full bg-white shadow transition-transform ${
                  config[item.key] ? 'translate-x-7' : 'translate-x-1'
                }`}
              />
            </button>
          </div>
        ))}
      </div>

      {/* Safety info */}
      <div className="mt-6 p-4 rounded-lg bg-yellow-900/20 border border-yellow-900/50">
        <div className="flex items-start space-x-2">
          <svg className="w-5 h-5 text-yellow-500 flex-shrink-0 mt-0.5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
            <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M12 9v2m0 4h.01m-6.938 4h13.856c1.54 0 2.502-1.667 1.732-3L13.732 4c-.77-1.333-2.694-1.333-3.464 0L3.34 16c-.77 1.333.192 3 1.732 3z" />
          </svg>
          <div>
            <p className="text-sm font-medium text-yellow-300">Safety Limits Active</p>
            <p className="text-xs text-yellow-500/80 mt-1">
              Minimum thresholds: 64 MB RAM, 1% CPU, protected processes excluded
            </p>
          </div>
        </div>
      </div>
    </div>
  )
}

export default ConfigPanel
