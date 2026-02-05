// Package verification provides tests for invariant checking.
//
//go:build invariants
// +build invariants

package verification

import (
	"testing"
	"time"

	"github.com/grey-systems/grey-distributed/pkg/types"
)

// TestAgreementInvariant verifies the single-leader-per-term property.
func TestAgreementInvariant(t *testing.T) {
	checker := NewInvariantChecker(100)

	// Test case 1: Valid state - single leader
	t.Run("SingleLeader", func(t *testing.T) {
		state := &SystemState{
			Timestamp: time.Now(),
			Consensus: ConsensusState{
				NodeID:      types.NodeID{1},
				CurrentTerm: 5,
				Role:        "Leader",
				KnownLeaders: map[uint64]types.NodeID{
					5: {1}, // Same node is leader
				},
			},
		}

		violations := checker.Check(state)
		if len(violations) > 0 {
			for _, v := range violations {
				if v.Invariant.Name == "Agreement" {
					t.Errorf("Unexpected Agreement violation: %v", v.Error)
				}
			}
		}
	})

	// Test case 2: Invalid state - two leaders same term
	t.Run("TwoLeaders", func(t *testing.T) {
		state := &SystemState{
			Timestamp: time.Now(),
			Consensus: ConsensusState{
				NodeID:      types.NodeID{1},
				CurrentTerm: 5,
				Role:        "Leader",
				KnownLeaders: map[uint64]types.NodeID{
					5: {2}, // Different node claims to be leader
				},
			},
		}

		violations := checker.Check(state)
		foundAgreementViolation := false
		for _, v := range violations {
			if v.Invariant.Name == "Agreement" {
				foundAgreementViolation = true
				break
			}
		}
		if !foundAgreementViolation {
			t.Error("Expected Agreement violation but got none")
		}
	})
}

// TestNoQuotaViolationInvariant verifies tenant quota enforcement.
func TestNoQuotaViolationInvariant(t *testing.T) {
	checker := NewInvariantChecker(100)

	// Test case 1: Valid state - within quota
	t.Run("WithinQuota", func(t *testing.T) {
		state := &SystemState{
			Timestamp: time.Now(),
			Scheduler: SchedulerState{
				TenantUsage: map[types.TenantID]ResourceUsage{
					{1}: {CPUUsed: 50, CPUQuota: 100, MemoryUsed: 500, MemoryQuota: 1000},
					{2}: {CPUUsed: 75, CPUQuota: 100, MemoryUsed: 800, MemoryQuota: 1000},
				},
			},
		}

		violations := checker.Check(state)
		for _, v := range violations {
			if v.Invariant.Name == "NoQuotaViolation" {
				t.Errorf("Unexpected quota violation: %v", v.Error)
			}
		}
	})

	// Test case 2: Invalid state - CPU quota exceeded
	t.Run("CPUQuotaExceeded", func(t *testing.T) {
		state := &SystemState{
			Timestamp: time.Now(),
			Scheduler: SchedulerState{
				TenantUsage: map[types.TenantID]ResourceUsage{
					{1}: {CPUUsed: 150, CPUQuota: 100, MemoryUsed: 500, MemoryQuota: 1000},
				},
			},
		}

		violations := checker.Check(state)
		foundQuotaViolation := false
		for _, v := range violations {
			if v.Invariant.Name == "NoQuotaViolation" {
				foundQuotaViolation = true
				break
			}
		}
		if !foundQuotaViolation {
			t.Error("Expected NoQuotaViolation but got none")
		}
	})

	// Test case 3: Invalid state - Memory quota exceeded
	t.Run("MemoryQuotaExceeded", func(t *testing.T) {
		state := &SystemState{
			Timestamp: time.Now(),
			Scheduler: SchedulerState{
				TenantUsage: map[types.TenantID]ResourceUsage{
					{1}: {CPUUsed: 50, CPUQuota: 100, MemoryUsed: 1500, MemoryQuota: 1000},
				},
			},
		}

		violations := checker.Check(state)
		foundQuotaViolation := false
		for _, v := range violations {
			if v.Invariant.Name == "NoQuotaViolation" {
				foundQuotaViolation = true
				break
			}
		}
		if !foundQuotaViolation {
			t.Error("Expected NoQuotaViolation but got none")
		}
	})
}

// TestCapacityLimitsInvariant verifies node capacity bounds.
func TestCapacityLimitsInvariant(t *testing.T) {
	checker := NewInvariantChecker(100)

	// Test case 1: Valid state - within capacity
	t.Run("WithinCapacity", func(t *testing.T) {
		state := &SystemState{
			Timestamp: time.Now(),
			Scheduler: SchedulerState{
				NodeCapacity: 1000,
				NodeUsed:     500,
			},
		}

		violations := checker.Check(state)
		for _, v := range violations {
			if v.Invariant.Name == "CapacityLimits" {
				t.Errorf("Unexpected capacity violation: %v", v.Error)
			}
		}
	})

	// Test case 2: Invalid state - over capacity
	t.Run("OverCapacity", func(t *testing.T) {
		state := &SystemState{
			Timestamp: time.Now(),
			Scheduler: SchedulerState{
				NodeCapacity: 1000,
				NodeUsed:     1500,
			},
		}

		violations := checker.Check(state)
		foundCapacityViolation := false
		for _, v := range violations {
			if v.Invariant.Name == "CapacityLimits" {
				foundCapacityViolation = true
				break
			}
		}
		if !foundCapacityViolation {
			t.Error("Expected CapacityLimits violation but got none")
		}
	})
}

// TestQuorumIntersectionInvariant verifies R + W > N.
func TestQuorumIntersectionInvariant(t *testing.T) {
	checker := NewInvariantChecker(100)

	// Test case 1: Valid quorum (3, 2, 2)
	t.Run("ValidQuorum", func(t *testing.T) {
		state := &SystemState{
			Timestamp: time.Now(),
			Storage: StorageState{
				ReplicationFactor: 3,
				ReadQuorum:        2,
				WriteQuorum:       2,
			},
		}

		violations := checker.Check(state)
		for _, v := range violations {
			if v.Invariant.Name == "QuorumIntersection" {
				t.Errorf("Unexpected quorum violation: %v", v.Error)
			}
		}
	})

	// Test case 2: Invalid quorum (3, 1, 1) - R + W = 2 <= N
	t.Run("InvalidQuorum", func(t *testing.T) {
		state := &SystemState{
			Timestamp: time.Now(),
			Storage: StorageState{
				ReplicationFactor: 3,
				ReadQuorum:        1,
				WriteQuorum:       1,
			},
		}

		violations := checker.Check(state)
		foundQuorumViolation := false
		for _, v := range violations {
			if v.Invariant.Name == "QuorumIntersection" {
				foundQuorumViolation = true
				break
			}
		}
		if !foundQuorumViolation {
			t.Error("Expected QuorumIntersection violation but got none")
		}
	})
}

// TestLogBoundedByCommitInvariant verifies commit index <= log length.
func TestLogBoundedByCommitInvariant(t *testing.T) {
	checker := NewInvariantChecker(100)

	// Test case 1: Valid state - commit <= log length
	t.Run("ValidCommit", func(t *testing.T) {
		state := &SystemState{
			Timestamp: time.Now(),
			Consensus: ConsensusState{
				CommitIndex: 50,
				LogLength:   100,
			},
		}

		violations := checker.Check(state)
		for _, v := range violations {
			if v.Invariant.Name == "LogBoundedByCommit" {
				t.Errorf("Unexpected log bound violation: %v", v.Error)
			}
		}
	})

	// Test case 2: Invalid state - commit > log length
	t.Run("InvalidCommit", func(t *testing.T) {
		state := &SystemState{
			Timestamp: time.Now(),
			Consensus: ConsensusState{
				CommitIndex: 150,
				LogLength:   100,
			},
		}

		violations := checker.Check(state)
		foundLogViolation := false
		for _, v := range violations {
			if v.Invariant.Name == "LogBoundedByCommit" {
				foundLogViolation = true
				break
			}
		}
		if !foundLogViolation {
			t.Error("Expected LogBoundedByCommit violation but got none")
		}
	})
}

// TestSecurityInvariants verifies security-related invariants.
func TestSecurityInvariants(t *testing.T) {
	checker := NewInvariantChecker(100)

	// Test case: Expired certificates
	t.Run("ExpiredCertificates", func(t *testing.T) {
		state := &SystemState{
			Timestamp: time.Now(),
			Security: SecurityState{
				ValidCerts:   10,
				ExpiringSoon: 2,
				Expired:      1, // Safety violation!
			},
		}

		violations := checker.Check(state)
		foundSecurityViolation := false
		for _, v := range violations {
			if v.Invariant.Name == "NoExpiredCertificates" {
				foundSecurityViolation = true
				break
			}
		}
		if !foundSecurityViolation {
			t.Error("Expected NoExpiredCertificates violation but got none")
		}
	})

	// Test case: High auth failure rate
	t.Run("HighAuthFailureRate", func(t *testing.T) {
		state := &SystemState{
			Timestamp: time.Now(),
			Security: SecurityState{
				ValidCerts:       10,
				FailedAuthLast5m: 200, // Above threshold
			},
		}

		violations := checker.Check(state)
		foundRateViolation := false
		for _, v := range violations {
			if v.Invariant.Name == "AuthFailureRateLimit" {
				foundRateViolation = true
				break
			}
		}
		if !foundRateViolation {
			t.Error("Expected AuthFailureRateLimit violation but got none")
		}
	})
}

// TestContinuousChecker verifies the background checker works.
func TestContinuousChecker(t *testing.T) {
	checker := NewInvariantChecker(100)

	var checkCount int
	stateFunc := func() *SystemState {
		checkCount++
		return &SystemState{
			Timestamp: time.Now(),
			Consensus: ConsensusState{
				CurrentTerm: uint64(checkCount),
				Role:        "Follower",
			},
		}
	}

	cc := NewContinuousChecker(checker, stateFunc, 10*time.Millisecond)

	ctx, cancel := context.WithCancel(context.Background())
	cc.Start(ctx)

	// Let it run for a bit
	time.Sleep(50 * time.Millisecond)

	cancel()
	cc.Stop()

	if checkCount < 3 {
		t.Errorf("Expected at least 3 checks, got %d", checkCount)
	}
}

// TestViolationHandlers verifies handlers are called correctly.
func TestViolationHandlers(t *testing.T) {
	checker := NewInvariantChecker(100)

	var safetyHandlerCalled bool
	var generalHandlerCalled bool

	checker.SetSafetyViolationHandler(func(v *Violation) {
		safetyHandlerCalled = true
	})

	checker.SetViolationHandler(func(v *Violation) {
		generalHandlerCalled = true
	})

	// Trigger a safety violation
	state := &SystemState{
		Timestamp: time.Now(),
		Scheduler: SchedulerState{
			NodeCapacity: 1000,
			NodeUsed:     2000, // Over capacity
		},
	}

	checker.Check(state)

	// Wait for async handlers
	time.Sleep(10 * time.Millisecond)

	if !safetyHandlerCalled {
		t.Error("Safety handler was not called")
	}
	if !generalHandlerCalled {
		t.Error("General handler was not called")
	}
}

// TestMetrics verifies metrics are tracked correctly.
func TestMetrics(t *testing.T) {
	checker := NewInvariantChecker(100)

	// Valid state
	validState := &SystemState{
		Timestamp: time.Now(),
		Consensus: ConsensusState{
			Role:        "Follower",
			CommitIndex: 50,
			LogLength:   100,
		},
		Scheduler: SchedulerState{
			NodeCapacity: 1000,
			NodeUsed:     500,
			TenantUsage:  map[types.TenantID]ResourceUsage{},
		},
		Storage: StorageState{
			ReplicationFactor: 3,
			ReadQuorum:        2,
			WriteQuorum:       2,
		},
		Security: SecurityState{
			ValidCerts:       10,
			FailedAuthLast5m: 5,
		},
	}

	// Run multiple checks
	for i := 0; i < 10; i++ {
		checker.Check(validState)
	}

	metrics := checker.Metrics()
	if metrics.ChecksPerformed.Load() < 10 {
		t.Errorf("Expected at least 10 checks, got %d", metrics.ChecksPerformed.Load())
	}
}

// BenchmarkInvariantCheck measures invariant checking overhead.
func BenchmarkInvariantCheck(b *testing.B) {
	checker := NewInvariantChecker(100)

	state := &SystemState{
		Timestamp: time.Now(),
		Consensus: ConsensusState{
			Role:        "Follower",
			CommitIndex: 50,
			LogLength:   100,
		},
		Scheduler: SchedulerState{
			NodeCapacity: 1000,
			NodeUsed:     500,
			TenantUsage: map[types.TenantID]ResourceUsage{
				{1}: {CPUUsed: 50, CPUQuota: 100},
				{2}: {CPUUsed: 75, CPUQuota: 100},
			},
		},
		Storage: StorageState{
			ReplicationFactor: 3,
			ReadQuorum:        2,
			WriteQuorum:       2,
		},
		Security: SecurityState{
			ValidCerts: 10,
		},
	}

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		checker.Check(state)
	}
}

// context import helper (would be in a real implementation)
import "context"
