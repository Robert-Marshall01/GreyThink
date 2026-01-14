/**
 * Gauge Component
 * 
 * A circular gauge visualization for displaying resource usage
 * as a percentage with animated transitions.
 */
function Gauge({ value, maxValue = 100, label, color = 'rgb(59, 130, 246)', size = 120 }) {
  const percentage = Math.min((value / maxValue) * 100, 100)
  const strokeWidth = 8
  const radius = (size - strokeWidth) / 2
  const circumference = 2 * Math.PI * radius
  const offset = circumference - (percentage / 100) * circumference

  return (
    <div className="flex flex-col items-center">
      <div className="relative" style={{ width: size, height: size }}>
        {/* Background circle */}
        <svg className="transform -rotate-90" width={size} height={size}>
          <circle
            cx={size / 2}
            cy={size / 2}
            r={radius}
            stroke="rgba(255, 255, 255, 0.1)"
            strokeWidth={strokeWidth}
            fill="transparent"
          />
          {/* Progress circle */}
          <circle
            cx={size / 2}
            cy={size / 2}
            r={radius}
            stroke={color}
            strokeWidth={strokeWidth}
            fill="transparent"
            strokeLinecap="round"
            strokeDasharray={circumference}
            strokeDashoffset={offset}
            className="transition-all duration-500 ease-out"
          />
        </svg>
        {/* Center text */}
        <div className="absolute inset-0 flex flex-col items-center justify-center">
          <span className="text-2xl font-bold text-white">
            {percentage.toFixed(1)}%
          </span>
        </div>
      </div>
      {label && (
        <span className="mt-2 text-sm text-grey-400 font-medium">{label}</span>
      )}
    </div>
  )
}

export default Gauge
