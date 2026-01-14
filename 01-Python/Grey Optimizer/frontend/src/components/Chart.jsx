/**
 * Chart Component
 * 
 * A time-series line chart for displaying historical metrics
 * using Recharts library.
 */
import { 
  LineChart, 
  Line, 
  XAxis, 
  YAxis, 
  CartesianGrid, 
  Tooltip, 
  ResponsiveContainer,
  Legend
} from 'recharts'

function Chart({ 
  data, 
  dataKeys, 
  colors, 
  height = 200,
  showLegend = true,
  yAxisFormatter,
  tooltipFormatter
}) {
  if (!data || data.length === 0) {
    return (
      <div 
        className="flex items-center justify-center text-grey-500" 
        style={{ height }}
      >
        No data available
      </div>
    )
  }

  const formatXAxis = (timestamp) => {
    if (!timestamp) return ''
    const date = new Date(timestamp)
    return date.toLocaleTimeString([], { hour: '2-digit', minute: '2-digit' })
  }

  const CustomTooltip = ({ active, payload, label }) => {
    if (active && payload && payload.length) {
      return (
        <div className="bg-grey-800 border border-grey-600 rounded-lg p-3 shadow-lg">
          <p className="text-xs text-grey-400 mb-2">
            {new Date(label).toLocaleString()}
          </p>
          {payload.map((entry, index) => (
            <p key={index} className="text-sm" style={{ color: entry.color }}>
              {entry.name}: {tooltipFormatter ? tooltipFormatter(entry.value) : entry.value.toFixed(2)}
            </p>
          ))}
        </div>
      )
    }
    return null
  }

  return (
    <ResponsiveContainer width="100%" height={height}>
      <LineChart data={data} margin={{ top: 5, right: 5, left: 0, bottom: 5 }}>
        <CartesianGrid strokeDasharray="3 3" stroke="rgba(255,255,255,0.1)" />
        <XAxis 
          dataKey="timestamp" 
          tickFormatter={formatXAxis}
          stroke="#6B7280"
          tick={{ fill: '#9CA3AF', fontSize: 10 }}
        />
        <YAxis 
          stroke="#6B7280"
          tick={{ fill: '#9CA3AF', fontSize: 10 }}
          tickFormatter={yAxisFormatter}
          width={40}
        />
        <Tooltip content={<CustomTooltip />} />
        {showLegend && (
          <Legend 
            wrapperStyle={{ paddingTop: 10 }}
            formatter={(value) => (
              <span className="text-grey-300 text-xs">{value}</span>
            )}
          />
        )}
        {dataKeys.map((key, index) => (
          <Line
            key={key}
            type="monotone"
            dataKey={key}
            stroke={colors[index] || '#3B82F6'}
            strokeWidth={2}
            dot={false}
            activeDot={{ r: 4, fill: colors[index] || '#3B82F6' }}
          />
        ))}
      </LineChart>
    </ResponsiveContainer>
  )
}

export default Chart
