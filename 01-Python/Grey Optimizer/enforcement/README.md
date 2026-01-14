# Grey Optimizer - C Enforcement Modules

This directory contains the C modules that implement low-level Linux system operations for resource enforcement.

## Modules

### cgroup_controller.c
Cgroup v2 manipulation for CPU and memory management:
- Create and configure cgroups under `/sys/fs/cgroup/grey-optimizer/`
- Set CPU quotas (`cpu.max`) and weights (`cpu.weight`)
- Set memory limits (`memory.max`)
- Move processes into cgroups

### scheduler.c
Linux scheduler manipulation:
- `sched_setscheduler()` wrappers for SCHED_IDLE, SCHED_BATCH
- Nice value management with `setpriority()`
- Rollback support to restore original scheduler settings

### ksm_controller.c
Kernel Same-page Merging (KSM) control:
- Enable/disable KSM
- Configure scan intervals and pages per pass
- Get KSM statistics (pages shared, memory saved)
- Tuning presets (aggressive, balanced, conservative)

### cache_ops.c
Cache management operations:
- Drop pagecache, dentries, and inodes
- Safe threshold checking before cache drops
- Memory compaction triggers

### disk_ops.c
Disk optimization operations:
- Sparse file detection and creation
- File hole punching
- I/O priority (ionice) control
- File hashing for deduplication detection

## Building

```bash
# Standard build
make

# Debug build with logging
make debug

# Clean build artifacts
make clean

# Run basic tests
make test
```

## Requirements

- GCC or Clang
- Linux kernel 4.5+ (for full cgroup v2 support)
- Python 3.x (for tests)

## Usage from Python

```python
import ctypes

# Load the combined library
lib = ctypes.CDLL('./lib/libgrey_optimizer.so')

# Example: Check if KSM is available
lib.grey_ksm_available.restype = ctypes.c_int
if lib.grey_ksm_available():
    print("KSM is available")

# Example: Set SCHED_IDLE for a process
lib.grey_sched_set_idle.argtypes = [ctypes.c_int]
lib.grey_sched_set_idle.restype = ctypes.c_int
result = lib.grey_sched_set_idle(12345)  # PID
```

## Safety Notes

- All functions validate inputs before operations
- Protected processes (init, systemd, sshd, kernel threads) are never modified
- Original state is stored for rollback
- Safety floors prevent dangerous configurations:
  - CPU quota minimum: 1% of a core
  - Memory limit minimum: 64 MB

## Error Codes

| Code | Name | Description |
|------|------|-------------|
| 0 | GREY_OK | Success |
| -1 | GREY_ERR_INVALID | Invalid argument |
| -2 | GREY_ERR_PERM | Permission denied |
| -3 | GREY_ERR_NOTFOUND | Resource not found |
| -4 | GREY_ERR_IO | I/O error |
| -5 | GREY_ERR_NOMEM | Out of memory |
| -6 | GREY_ERR_BUSY | Resource busy |

## License

MIT License - See project root LICENSE file.
