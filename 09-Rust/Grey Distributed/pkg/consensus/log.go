// Package consensus - Log implementation for Raft.
//
// The log is the core data structure of Raft. It stores all commands that
// will be applied to the state machine, in the order they were proposed.
//
// # Design Decisions
//
// 1. In-Memory + WAL: The log is kept in memory for fast access, with a
//    Write-Ahead Log on disk for durability. Snapshots allow log compaction.
//
// 2. Immutable Entries: Once written, log entries are never modified. The
//    only mutation is truncation (removing suffix during conflict resolution).
//
// 3. Zero-Copy Reads: EntryAt returns a pointer to avoid copying large payloads.
//    Callers must not modify the returned entry.
package consensus

import (
	"sync"

	"github.com/grey-systems/grey-distributed/pkg/types"
)

// Log is an in-memory log with efficient index-based access.
// All operations are thread-safe.
type Log struct {
	mu sync.RWMutex
	
	// entries is the in-memory log. Index 0 is reserved (entries[0] is a dummy).
	// This simplifies indexing: log entry at index i is entries[i].
	entries []*types.LogEntry
	
	// lastIncludedIndex/Term record the snapshot boundary.
	// All entries up to and including lastIncludedIndex have been snapshotted.
	lastIncludedIndex types.LogIndex
	lastIncludedTerm  types.Term
}

// NewLog creates a new empty log.
func NewLog() *Log {
	return &Log{
		// Initialize with a dummy entry at index 0
		// This avoids special-casing index 0 throughout the code
		entries: []*types.LogEntry{
			{Index: 0, Term: 0},
		},
	}
}

// Append adds a new entry to the log and returns its index.
//
// The entry's Index field is set automatically to ensure monotonicity.
// The caller should set Term, Type, and Data.
func (l *Log) Append(entry *types.LogEntry) types.LogIndex {
	l.mu.Lock()
	defer l.mu.Unlock()
	
	index := l.lastIncludedIndex + types.LogIndex(len(l.entries))
	entry.Index = index
	l.entries = append(l.entries, entry)
	
	return index
}

// AppendBatch appends multiple entries atomically.
// Returns the index of the last appended entry.
func (l *Log) AppendBatch(entries []*types.LogEntry) types.LogIndex {
	l.mu.Lock()
	defer l.mu.Unlock()
	
	startIndex := l.lastIncludedIndex + types.LogIndex(len(l.entries))
	
	for i, entry := range entries {
		entry.Index = startIndex + types.LogIndex(i)
		l.entries = append(l.entries, entry)
	}
	
	return startIndex + types.LogIndex(len(entries)) - 1
}

// EntryAt returns the entry at the given index, or nil if not found.
//
// Warning: Returns a pointer to the internal entry. Do not modify.
//
// Returns nil if:
// - index == 0 (no entry exists at index 0)
// - index <= lastIncludedIndex (entry has been compacted)
// - index > lastIndex (entry doesn't exist yet)
func (l *Log) EntryAt(index types.LogIndex) *types.LogEntry {
	l.mu.RLock()
	defer l.mu.RUnlock()
	
	return l.entryAtUnlocked(index)
}

// entryAtUnlocked is the lock-free version for internal use.
func (l *Log) entryAtUnlocked(index types.LogIndex) *types.LogEntry {
	if index <= l.lastIncludedIndex {
		return nil // Compacted
	}
	
	arrayIndex := int(index - l.lastIncludedIndex)
	if arrayIndex < 0 || arrayIndex >= len(l.entries) {
		return nil
	}
	
	return l.entries[arrayIndex]
}

// TermAt returns the term of the entry at the given index.
// Returns 0 if the entry doesn't exist.
//
// Special case: If index == lastIncludedIndex, returns lastIncludedTerm.
// This is necessary for log consistency checks after a snapshot.
func (l *Log) TermAt(index types.LogIndex) types.Term {
	l.mu.RLock()
	defer l.mu.RUnlock()
	
	if index == 0 {
		return 0
	}
	
	if index == l.lastIncludedIndex {
		return l.lastIncludedTerm
	}
	
	entry := l.entryAtUnlocked(index)
	if entry == nil {
		return 0
	}
	
	return entry.Term
}

// LastIndex returns the index of the last entry in the log.
// Returns 0 if the log is empty.
func (l *Log) LastIndex() types.LogIndex {
	l.mu.RLock()
	defer l.mu.RUnlock()
	
	return l.lastIncludedIndex + types.LogIndex(len(l.entries)) - 1
}

// LastIndexAndTerm returns the index and term of the last entry.
// Used for leader election to determine log up-to-dateness.
func (l *Log) LastIndexAndTerm() (types.LogIndex, types.Term) {
	l.mu.RLock()
	defer l.mu.RUnlock()
	
	lastIndex := l.lastIncludedIndex + types.LogIndex(len(l.entries)) - 1
	
	if len(l.entries) <= 1 {
		// Only the dummy entry exists
		return l.lastIncludedIndex, l.lastIncludedTerm
	}
	
	lastEntry := l.entries[len(l.entries)-1]
	return lastIndex, lastEntry.Term
}

// EntriesFrom returns entries starting at the given index.
// Returns at most maxEntries entries.
//
// Used by the leader to prepare AppendEntries RPCs.
func (l *Log) EntriesFrom(startIndex types.LogIndex, maxEntries int) []*types.LogEntry {
	l.mu.RLock()
	defer l.mu.RUnlock()
	
	if startIndex <= l.lastIncludedIndex {
		// Requested entries have been compacted
		return nil
	}
	
	arrayStart := int(startIndex - l.lastIncludedIndex)
	if arrayStart >= len(l.entries) {
		return nil
	}
	
	arrayEnd := arrayStart + maxEntries
	if arrayEnd > len(l.entries) {
		arrayEnd = len(l.entries)
	}
	
	// Return a copy to prevent mutations
	result := make([]*types.LogEntry, arrayEnd-arrayStart)
	copy(result, l.entries[arrayStart:arrayEnd])
	
	return result
}

// TruncateFrom removes all entries from the given index onward (inclusive).
//
// This is called when we receive entries that conflict with our log.
// Raft guarantees that if two logs have an entry with the same index and term,
// all preceding entries are identical. So we only need to consider conflicts
// at specific indices.
func (l *Log) TruncateFrom(index types.LogIndex) {
	l.mu.Lock()
	defer l.mu.Unlock()
	
	if index <= l.lastIncludedIndex {
		// Cannot truncate into the snapshot
		return
	}
	
	arrayIndex := int(index - l.lastIncludedIndex)
	if arrayIndex >= len(l.entries) {
		return
	}
	
	l.entries = l.entries[:arrayIndex]
}

// FirstIndexOfTerm returns the first log index of the given term.
// Returns 0 if no entry with that term exists.
//
// Used for the conflict optimization in AppendEntries responses.
// When a follower has a conflicting entry, it can tell the leader
// where the conflicting term starts, allowing the leader to skip
// multiple entries instead of decrementing nextIndex one at a time.
func (l *Log) FirstIndexOfTerm(term types.Term) types.LogIndex {
	l.mu.RLock()
	defer l.mu.RUnlock()
	
	for i := 1; i < len(l.entries); i++ {
		if l.entries[i].Term == term {
			return l.lastIncludedIndex + types.LogIndex(i)
		}
	}
	
	return 0
}

// Compact removes all entries up to and including the given index.
// The state machine should be snapshotted at this index before calling.
//
// After compaction:
// - lastIncludedIndex = index
// - lastIncludedTerm = term of entry at index
// - Entries up to index are no longer accessible
func (l *Log) Compact(index types.LogIndex, term types.Term) {
	l.mu.Lock()
	defer l.mu.Unlock()
	
	if index <= l.lastIncludedIndex {
		return // Already compacted past this point
	}
	
	arrayIndex := int(index - l.lastIncludedIndex)
	if arrayIndex >= len(l.entries) {
		// Compacting beyond our log - create a new dummy entry
		l.entries = []*types.LogEntry{{Index: index, Term: term}}
	} else {
		// Keep entries after the compaction point
		remaining := l.entries[arrayIndex:]
		l.entries = make([]*types.LogEntry, len(remaining))
		copy(l.entries, remaining)
	}
	
	l.lastIncludedIndex = index
	l.lastIncludedTerm = term
}

// InstallSnapshot replaces the log with a snapshot.
// Called when receiving an InstallSnapshot RPC from the leader.
//
// All existing entries are discarded; the log starts fresh from the
// snapshot boundary. The state machine should be restored from the
// snapshot data before this is called.
func (l *Log) InstallSnapshot(lastIndex types.LogIndex, lastTerm types.Term) {
	l.mu.Lock()
	defer l.mu.Unlock()
	
	l.lastIncludedIndex = lastIndex
	l.lastIncludedTerm = lastTerm
	l.entries = []*types.LogEntry{{Index: lastIndex, Term: lastTerm}}
}

// SnapshotInfo returns the current snapshot boundary.
func (l *Log) SnapshotInfo() (index types.LogIndex, term types.Term) {
	l.mu.RLock()
	defer l.mu.RUnlock()
	
	return l.lastIncludedIndex, l.lastIncludedTerm
}

// Length returns the number of entries in the log (excluding compacted entries).
func (l *Log) Length() int {
	l.mu.RLock()
	defer l.mu.RUnlock()
	
	// Subtract 1 for the dummy entry
	return len(l.entries) - 1
}
