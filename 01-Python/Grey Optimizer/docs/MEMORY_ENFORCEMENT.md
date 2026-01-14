# Memory Enforcement

Grey Optimizer provides comprehensive RAM reduction capabilities using multiple kernel mechanisms. This document explains how memory enforcement works and how to troubleshoot RAM reduction issues.

## Table of Contents

- [Why RAM Reduction is Difficult](#why-ram-reduction-is-difficult)
- [Enforcement Mechanisms](#enforcement-mechanisms)
- [Diagnostics](#diagnostics)
- [CLI Commands](#cli-commands)
- [Troubleshooting](#troubleshooting)
- [Architecture](#architecture)

---

## Why RAM Reduction is Difficult

Unlike CPU or disk I/O, RAM reduction is inherently challenging:

1. **Memory is demand-paged**: The kernel allocates physical pages only when they're accessed, not at `malloc()` time
2. **Caches are beneficial**: Much "used" memory is page cache that improves performance
3. **No remote control**: You can't directly reclaim memory from a running process without its cooperation
4. **Process cooperation required**: Real reduction often requires the application to release memory internally
5. **cgroup limits prevent growth, don't reclaim**: Setting `memory.max` stops a process from using *more* memory, but doesn't reduce current usage

### What Actually Works

| Mechanism | Effect | Permanence | Notes |
|-----------|--------|------------|-------|
| `memory.max` | Limits growth | Until removed | Triggers kernel pressure when approached |
| `memory.reclaim` | Forces reclaim | Temporary | Linux 5.17+, kernel chooses what to reclaim |
| `drop_caches` | Frees page cache | Temporary | Refills quickly on I/O |
| KSM | Merges duplicate pages | Ongoing | Slow, effective for VMs/containers |
| `madvise(MADV_DONTNEED)` | Releases pages | Until accessed | Requires process cooperation |
| Application signals | App releases caches | Varies | Requires app support |

---

## Enforcement Mechanisms

### 1. cgroup Memory Limits

Grey Optimizer uses cgroup v2 memory controller:

```bash
# cgroup location
/sys/fs/cgroup/grey-optimizer/<cgroup-name>/

# Key files
memory.max       # Hard limit (bytes or "max")
memory.high      # Soft limit (causes throttling)
memory.current   # Current usage
memory.stat      # Detailed breakdown
memory.events    # OOM/limit event counters
memory.reclaim   # Force reclamation (5.17+)
```

**Safety**: Minimum limit of 64MB is always enforced to prevent OOM.

### 2. Memory Reclamation (Linux 5.17+)

The `memory.reclaim` interface allows forcing the kernel to reclaim pages:

```bash
# Force reclaim of 100MB from cgroup
echo 104857600 > /sys/fs/cgroup/grey-optimizer/mem-limited/memory.reclaim
```

The kernel uses its normal LRU reclaim logic:
- File-backed pages are preferred (written back if dirty)
- Anonymous pages may be swapped if swap available
- Kernel decides what's safe to reclaim

**Why this actually works**: Unlike just setting limits, reclaim actively frees pages.

### 3. Kernel Same-page Merging (KSM)

KSM scans memory for identical pages and merges them:

```bash
# KSM files
/sys/kernel/mm/ksm/run            # 0=off, 1=on
/sys/kernel/mm/ksm/pages_to_scan  # Pages per scan
/sys/kernel/mm/ksm/sleep_millisecs # Delay between scans
/sys/kernel/mm/ksm/pages_shared   # Pages being shared
/sys/kernel/mm/ksm/pages_sharing  # Total sharing pages
```

**Best for**: 
- Multiple VMs running similar guests
- Containers from same base image
- Multiple instances of same application

**Not effective for**:
- Unique data (databases, caches)
- Encrypted memory
- Rapidly changing data

### 4. Cache Pruning

Drop kernel page cache:

```bash
# Level 1: Page cache
echo 1 > /proc/sys/vm/drop_caches

# Level 2: Slab objects
echo 2 > /proc/sys/vm/drop_caches

# Level 3: Both
echo 3 > /proc/sys/vm/drop_caches
```

**Effect**: Immediately frees cached file data. Effect is temporary as caches refill on I/O.

**Safety**: Always sync before dropping to prevent data loss.

### 5. madvise Hints (C Helper)

The `libgrey_memory.so` C helper provides `madvise(MADV_DONTNEED)`:

```c
// Tells kernel process doesn't need these pages
madvise(addr, len, MADV_DONTNEED);
```

**For anonymous memory**: Pages are immediately freed. Next access returns zero-filled pages.

**For file-backed memory**: Pages are marked for reclaim but data is preserved.

---

## Diagnostics

### Memory Snapshot

Grey Optimizer takes detailed snapshots before and after enforcement:

```python
from grey_optimizer.telemetry.memory_diagnostics import MemoryDiagnostics

diagnostics = MemoryDiagnostics()

# Take snapshot
snapshot = await diagnostics.take_snapshot(
    target_pids=[1234, 5678],  # Specific PIDs to track
    include_all_processes=True,  # Or track all
)

print(f"Total: {snapshot.total_mb} MB")
print(f"Used: {snapshot.used_mb} MB ({snapshot.used_percent}%)")
print(f"Available: {snapshot.available_mb} MB")
```

### Process Memory Analysis

Each process is tracked with:

- **RSS**: Resident Set Size (physical memory in use)
- **PSS**: Proportional Set Size (shared pages divided among sharers)
- **USS**: Unique Set Size (memory unique to this process)
- **Swap**: Swapped-out memory

### Leak Detection

Grey Optimizer tracks memory growth over time:

```python
leaks = await diagnostics.get_leak_candidates(
    threshold_mb=10,           # Minimum growth to flag
    threshold_growth_rate=0.1, # 10% growth rate
)

for leak in leaks:
    print(f"{leak.name} (PID {leak.pid}): +{leak.growth//1024//1024} MB")
```

### Before/After Comparison

```python
comparison = await diagnostics.compare_snapshots(before, after)

print(f"System used delta: {comparison.system_used_delta_mb:+.1f} MB")
print(f"RSS delta: {comparison.rss_delta_mb:+.1f} MB")
print(f"Reduction: {comparison.reduction_percent:.1f}%")
```

---

## CLI Commands

### memory-diagnose

Run comprehensive memory diagnostics:

```bash
# Basic diagnostics
greyctl memory-diagnose

# With leak detection
greyctl memory-diagnose --detect-leaks

# JSON output
greyctl memory-diagnose --json

# Verbose (show all processes)
greyctl memory-diagnose --verbose
```

Output includes:
- System memory totals
- Memory breakdown (cached, buffers, swap)
- Top memory consumers
- Potential memory leaks
- cgroup memory stats
- KSM status
- Memory pressure (PSI)

### memory-enforce

Apply memory enforcement:

```bash
# Simulation mode (safe)
greyctl memory-enforce --simulation

# Drop kernel caches
sudo greyctl memory-enforce --drop-caches

# Enable KSM
sudo greyctl memory-enforce --enable-ksm

# Aggressive mode (faster KSM scanning)
sudo greyctl memory-enforce --enable-ksm --aggressive
```

### memory-watch

Real-time memory monitoring:

```bash
# Basic watch
greyctl memory-watch

# With top processes
greyctl memory-watch --show-processes

# Custom interval
greyctl memory-watch --interval 5

# Without screen clearing
greyctl memory-watch --no-clear
```

---

## Troubleshooting

### "RAM not reducing"

**Symptom**: After enforcement, memory usage hasn't decreased.

**Possible causes and solutions**:

1. **Only limits were set, not reclamation**
   - Setting `memory.max` prevents growth but doesn't reclaim
   - Solution: Use `memory.reclaim` (Linux 5.17+) or KSM

2. **Memory is file cache (beneficial)**
   - Page cache reduces I/O and improves performance
   - Check: Run `greyctl memory-diagnose` and look at "Page Cache"
   - Solution: Only drop caches if really needed; they'll refill anyway

3. **Anonymous memory can't be reclaimed without swap**
   - Without swap, the kernel can't reclaim anonymous pages
   - Check: `cat /proc/meminfo | grep Swap`
   - Solution: Enable swap or use cgroup memory.reclaim

4. **Processes are actively using memory**
   - Memory in active use can't be safely reclaimed
   - Check: Use `greyctl memory-diagnose --verbose` to see per-process RSS
   - Solution: Target idle processes or use application-level hints

5. **KSM needs time to merge pages**
   - KSM scans slowly to avoid CPU overhead
   - Check: `cat /sys/kernel/mm/ksm/pages_sharing`
   - Solution: Wait longer or use aggressive mode

### "OOM kills after enforcement"

**Symptom**: Processes are being killed by the OOM killer.

**Causes and solutions**:

1. **Memory limit too low**
   - Solution: Increase `memory.max` limit
   
2. **Too aggressive reclamation**
   - Solution: Reduce reclaim target, add safety margin

3. **Watchdog not triggered**
   - Check: Verify watchdog is running
   - Solution: The watchdog should auto-rollback on OOM

**Recovery**:
```bash
# Emergency rollback
greyctl rollback --force

# Manual: restore processes to root cgroup
echo $PID > /sys/fs/cgroup/cgroup.procs
```

### "memory.reclaim not available"

**Symptom**: Reclaim operations fail with "not available".

**Cause**: Requires Linux 5.17 or later.

**Check kernel version**:
```bash
uname -r
# Must be 5.17 or higher
```

**Alternatives for older kernels**:
- Use KSM for gradual reduction
- Drop caches for immediate (temporary) reduction
- Set memory.high for throttling-based pressure

### "KSM not merging pages"

**Symptom**: KSM is enabled but not showing savings.

**Possible causes**:

1. **No duplicate content**
   - KSM only works when processes have identical pages
   - Works best with VMs, containers, or multiple instances

2. **Pages are encrypted**
   - Encrypted pages won't have identical content

3. **Not enough time**
   - KSM scans gradually
   - Solution: Check `pages_sharing` over time

4. **Scan rate too low**
   - Increase `pages_to_scan` for faster (but more CPU) scanning

**Check KSM effectiveness**:
```bash
# Pages being merged
cat /sys/kernel/mm/ksm/pages_sharing

# Savings in MB (4KB pages)
echo $(( $(cat /sys/kernel/mm/ksm/pages_sharing) * 4 / 1024 )) MB saved
```

### "Permission denied" errors

**Symptom**: Operations fail with permission errors.

**Solution**: Most memory enforcement requires root:

```bash
# Use sudo
sudo greyctl memory-enforce --drop-caches

# Or run daemon as root
sudo systemctl restart grey-optimizer
```

---

## Architecture

### Module Structure

```
grey_optimizer/
├── telemetry/
│   └── memory_diagnostics.py   # Snapshots, comparisons, leak detection
├── enforcement/
│   ├── manager.py              # Enforcement coordinator
│   ├── memory_enforcer.py      # High-level memory enforcement
│   ├── memory_ops.py           # Python wrapper for C helper
│   └── watchdog.py             # Stability monitoring
└── ...

enforcement/
├── src/
│   └── memory_ops.c            # madvise, cold page detection
└── lib/
    └── libgrey_memory.so       # Compiled C helper
```

### Data Flow

```
                ┌─────────────────┐
                │  greyctl CLI    │
                └────────┬────────┘
                         │
                         ▼
              ┌──────────────────────┐
              │  MemoryDiagnostics   │
              │  - take_snapshot()   │
              │  - compare_snapshots()│
              └──────────┬───────────┘
                         │
           ┌─────────────┴─────────────┐
           ▼                           ▼
┌─────────────────────┐    ┌─────────────────────┐
│  EnforcementManager │    │    MemoryWatchdog   │
│  - apply_ram_*()    │    │  - monitor OOM      │
│  - rollback_all()   │    │  - check services   │
└──────────┬──────────┘    │  - auto rollback    │
           │               └─────────────────────┘
           ▼
┌─────────────────────────────────────────────────┐
│                 Kernel Interfaces                │
├──────────────┬──────────────┬───────────────────┤
│ cgroup v2    │ /proc/meminfo│ /sys/kernel/mm/ksm│
│ memory.*     │ /proc/*/smaps│                   │
└──────────────┴──────────────┴───────────────────┘
```

### Safety Watchdog

The watchdog continuously monitors during enforcement:

1. **OOM Events**: Watches `memory.events` for `oom_kill` counter increases
2. **Critical Services**: Verifies systemd, sshd, dbus are running
3. **Memory Pressure**: Monitors PSI `avg10` for high pressure
4. **Available Memory**: Alerts if below 128MB threshold

On any critical issue, the watchdog triggers automatic rollback.

---

## Best Practices

### For Simulation Mode

1. Run `greyctl memory-diagnose` first to understand baseline
2. Identify top memory consumers
3. Test enforcement actions in simulation
4. Review what would happen before going live

### For Live Mode

1. Start with conservative limits (50% of current usage)
2. Enable watchdog monitoring
3. Set memory.high first (soft limit) before memory.max
4. Monitor for a stability period before declaring success
5. Keep rollback path clear

### For Containers/VMs

1. Enable KSM for page sharing across similar instances
2. Use cgroup memory limits per container
3. Consider memory.reclaim for gradual pressure
4. Monitor container OOM events separately

---

## Kernel Version Requirements

| Feature | Minimum Kernel | Notes |
|---------|---------------|-------|
| cgroup v2 | 4.5 | Recommended 5.0+ |
| memory.max | 4.5 | Basic limit |
| memory.high | 4.5 | Soft limit |
| memory.reclaim | 5.17 | Active reclamation |
| PSI | 4.20 | Pressure metrics |
| KSM | 2.6.32 | Page merging |

Check your kernel:
```bash
uname -r
```

Check cgroup v2:
```bash
mount | grep cgroup2
# Should show /sys/fs/cgroup as cgroup2
```
