// Package consensus - Metrics for monitoring Raft performance.
//
// Observability is critical for distributed systems. These metrics enable:
// 1. Detecting performance degradation before it causes outages
// 2. Understanding cluster behavior during incidents
// 3. Capacity planning based on historical data
// 4. Alerting on abnormal conditions
package consensus

import (
	"sync/atomic"
)

// Metrics collects performance and health metrics for the Raft node.
//
// All metrics are implemented using atomic operations for thread-safety
// without lock contention. This is important because metrics are updated
// on every operation, and we don't want them to become a bottleneck.
type Metrics struct {
	// Election metrics
	ElectionsStarted atomic.Uint64 // Number of elections this node started
	ElectionsWon     atomic.Uint64 // Number of elections this node won
	LeaderElections  atomic.Uint64 // Number of times this node became leader
	
	// Replication metrics
	EntriesAppended      atomic.Uint64 // Log entries appended
	EntriesReplicated    atomic.Uint64 // Entries sent to followers
	ReplicationErrors    atomic.Uint64 // Failed replication attempts
	SnapshotsSent        atomic.Uint64 // Snapshots sent to followers
	SnapshotsReceived    atomic.Uint64 // Snapshots received from leader
	
	// Latency tracking (stored as nanoseconds)
	LastCommitLatency    atomic.Int64  // Time from propose to commit
	LastApplyLatency     atomic.Int64  // Time from commit to apply
	LastReplicationRTT   atomic.Int64  // Round-trip time for replication
	
	// Log metrics
	LogSize              atomic.Uint64 // Current log size in entries
	LogBytes             atomic.Uint64 // Current log size in bytes
	CompactedEntries     atomic.Uint64 // Entries removed by compaction
	
	// State metrics
	CurrentTerm          atomic.Uint64 // Current Raft term
	CommitIndex          atomic.Uint64 // Current commit index
	LastAppliedIndex     atomic.Uint64 // Last applied index
	
	// Error metrics
	VoteRequestsRejected atomic.Uint64 // Vote requests rejected
	EntriesRejected      atomic.Uint64 // AppendEntries rejected
	TermMismatches       atomic.Uint64 // Operations rejected due to term
}

// NewMetrics creates a new Metrics instance.
func NewMetrics() *Metrics {
	return &Metrics{}
}

// Snapshot returns a point-in-time copy of all metrics.
// Useful for exporting to monitoring systems.
type MetricsSnapshot struct {
	ElectionsStarted     uint64
	ElectionsWon         uint64
	LeaderElections      uint64
	EntriesAppended      uint64
	EntriesReplicated    uint64
	ReplicationErrors    uint64
	SnapshotsSent        uint64
	SnapshotsReceived    uint64
	LastCommitLatencyNs  int64
	LastApplyLatencyNs   int64
	LastReplicationRTTNs int64
	LogSize              uint64
	LogBytes             uint64
	CompactedEntries     uint64
	CurrentTerm          uint64
	CommitIndex          uint64
	LastAppliedIndex     uint64
	VoteRequestsRejected uint64
	EntriesRejected      uint64
	TermMismatches       uint64
}

// Snapshot captures current metrics values.
func (m *Metrics) Snapshot() MetricsSnapshot {
	return MetricsSnapshot{
		ElectionsStarted:     m.ElectionsStarted.Load(),
		ElectionsWon:         m.ElectionsWon.Load(),
		LeaderElections:      m.LeaderElections.Load(),
		EntriesAppended:      m.EntriesAppended.Load(),
		EntriesReplicated:    m.EntriesReplicated.Load(),
		ReplicationErrors:    m.ReplicationErrors.Load(),
		SnapshotsSent:        m.SnapshotsSent.Load(),
		SnapshotsReceived:    m.SnapshotsReceived.Load(),
		LastCommitLatencyNs:  m.LastCommitLatency.Load(),
		LastApplyLatencyNs:   m.LastApplyLatency.Load(),
		LastReplicationRTTNs: m.LastReplicationRTT.Load(),
		LogSize:              m.LogSize.Load(),
		LogBytes:             m.LogBytes.Load(),
		CompactedEntries:     m.CompactedEntries.Load(),
		CurrentTerm:          m.CurrentTerm.Load(),
		CommitIndex:          m.CommitIndex.Load(),
		LastAppliedIndex:     m.LastAppliedIndex.Load(),
		VoteRequestsRejected: m.VoteRequestsRejected.Load(),
		EntriesRejected:      m.EntriesRejected.Load(),
		TermMismatches:       m.TermMismatches.Load(),
	}
}

// Reset resets all metrics to zero.
// Useful for testing or after a cluster restart.
func (m *Metrics) Reset() {
	m.ElectionsStarted.Store(0)
	m.ElectionsWon.Store(0)
	m.LeaderElections.Store(0)
	m.EntriesAppended.Store(0)
	m.EntriesReplicated.Store(0)
	m.ReplicationErrors.Store(0)
	m.SnapshotsSent.Store(0)
	m.SnapshotsReceived.Store(0)
	m.LastCommitLatency.Store(0)
	m.LastApplyLatency.Store(0)
	m.LastReplicationRTT.Store(0)
	m.LogSize.Store(0)
	m.LogBytes.Store(0)
	m.CompactedEntries.Store(0)
	m.CurrentTerm.Store(0)
	m.CommitIndex.Store(0)
	m.LastAppliedIndex.Store(0)
	m.VoteRequestsRejected.Store(0)
	m.EntriesRejected.Store(0)
	m.TermMismatches.Store(0)
}
