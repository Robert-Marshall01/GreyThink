# Grey Distributed — CLI Reference

Complete reference for the `greyctl` command-line tool.

---

## Table of Contents

1. [Installation](#installation)
2. [Configuration](#configuration)
3. [Global Options](#global-options)
4. [Commands](#commands)
   - [task](#task-commands)
   - [cluster](#cluster-commands)
   - [replay](#replay-command)
   - [quarantine](#quarantine-commands)
   - [attest](#attestation-commands)
   - [config](#config-commands)
5. [Environment Variables](#environment-variables)
6. [Examples](#examples)
7. [Shell Completion](#shell-completion)

---

## Installation

### From Release

```bash
# macOS (Homebrew)
brew install grey-io/tap/greyctl

# Linux (apt)
curl -fsSL https://grey.io/install.sh | sudo bash

# Linux (manual)
curl -LO https://github.com/grey-io/grey/releases/latest/download/greyctl-linux-amd64
chmod +x greyctl-linux-amd64
sudo mv greyctl-linux-amd64 /usr/local/bin/greyctl

# Windows (scoop)
scoop bucket add grey https://github.com/grey-io/scoop-bucket
scoop install greyctl
```

### From Source

```bash
# Requires Go 1.21+
go install github.com/grey-io/grey/cmd/greyctl@latest
```

### Docker

```bash
docker run --rm -it grey-io/greyctl:latest --help
```

---

## Configuration

### Config File Location

```
~/.grey/config.yaml
```

### Config File Format

```yaml
# Grey cluster endpoint
endpoint: https://api.grey.io

# API authentication key
api_key: grey_api_key_tenant123_abc...

# Default tenant ID (usually embedded in API key)
tenant_id: tenant-123

# Default output format: table, json, yaml
output: table

# Request timeout
timeout: 30s

# Default labels to apply to tasks
labels:
  env: production
  team: data-engineering
```

### Initialize Configuration

```bash
# Interactive setup
greyctl config init

# Set individual values
greyctl config set endpoint https://api.grey.io
greyctl config set api_key your-api-key
greyctl config set output json
```

---

## Global Options

These options apply to all commands:

| Option | Short | Description |
|--------|-------|-------------|
| `--config` | | Path to config file (default: `~/.grey/config.yaml`) |
| `--endpoint` | `-e` | Grey API endpoint URL |
| `--api-key` | `-k` | API authentication key |
| `--output` | `-o` | Output format: `table`, `json`, `yaml` |
| `--timeout` | | Request timeout (e.g., `30s`, `5m`) |
| `--verbose` | `-v` | Enable verbose output |
| `--help` | `-h` | Show help for command |

**Example:**

```bash
greyctl --endpoint https://staging.grey.io --output json task list
```

---

## Commands

### Task Commands

Commands for submitting and managing tasks.

#### greyctl task submit

Submit a new task for execution.

```bash
greyctl task submit [flags]
```

**Flags:**

| Flag | Description | Default |
|------|-------------|---------|
| `--type` | Task type: `stateless`, `stateful`, `pipeline`, `batch` | `stateless` |
| `--payload` | Task payload as JSON string | |
| `--file`, `-f` | Read payload from file (JSON/YAML) | |
| `--batch` | Directory containing batch task files | |
| `--priority` | Priority: `low`, `normal`, `high`, `critical` | `normal` |
| `--task-timeout` | Task execution timeout | `5m` |
| `--label` | Labels (repeatable): `key=value` | |
| `--tee` | Require TEE execution | `false` |
| `--idempotency-key` | Idempotency key for deduplication | |
| `--wait` | Wait for task completion | `false` |

**Examples:**

```bash
# Submit simple task
greyctl task submit --type stateless --payload '{"numbers": [1,2,3]}'

# Submit from file
greyctl task submit --file task.yaml

# Submit with priority and labels
greyctl task submit --payload '{}' --priority high --label env=prod --label team=ml

# Submit batch from directory
greyctl task submit --batch ./tasks/

# Submit and wait for result
greyctl task submit --payload '{"data": "test"}' --wait

# Submit with TEE requirement
greyctl task submit --payload '{}' --tee --idempotency-key unique-123
```

**Output (table):**

```
TASK ID                                  STATUS   CREATED
task_550e8400-e29b-41d4-a716-446655440000  pending  2024-01-15T10:30:00Z
```

**Output (json):**

```json
{
  "task_id": "task_550e8400-e29b-41d4-a716-446655440000",
  "status": "pending",
  "created_at": "2024-01-15T10:30:00Z"
}
```

---

#### greyctl task status

Get status of one or more tasks.

```bash
greyctl task status <task-id> [task-id...] [flags]
```

**Flags:**

| Flag | Description | Default |
|------|-------------|---------|
| `--watch`, `-w` | Watch for status changes | `false` |
| `--interval` | Polling interval for watch | `2s` |

**Examples:**

```bash
# Get single task status
greyctl task status task_abc123

# Get multiple task statuses
greyctl task status task_abc123 task_def456

# Watch until completion
greyctl task status task_abc123 --watch

# Watch with custom interval
greyctl task status task_abc123 --watch --interval 5s
```

**Output:**

```
TASK ID        STATUS     PROGRESS  DURATION   NODE
task_abc123    running    75%       12s        grey-worker-7
```

---

#### greyctl task list

List tasks with optional filtering.

```bash
greyctl task list [flags]
```

**Flags:**

| Flag | Description | Default |
|------|-------------|---------|
| `--status` | Filter by status (comma-separated) | |
| `--label` | Filter by label (repeatable) | |
| `--since` | Show tasks created since duration | |
| `--limit` | Maximum results | `100` |

**Examples:**

```bash
# List all tasks
greyctl task list

# Filter by status
greyctl task list --status running,pending

# Filter by labels
greyctl task list --label env=production --label team=data

# Recent tasks
greyctl task list --since 1h

# Combine filters
greyctl task list --status running --label env=prod --limit 50
```

**Output:**

```
TASK ID        TYPE       STATUS     CREATED              LABELS
task_abc123    stateless  completed  2024-01-15 10:30:00  env=prod
task_def456    stateful   running    2024-01-15 10:31:00  env=prod
task_ghi789    stateless  pending    2024-01-15 10:32:00  env=staging
```

---

#### greyctl task cancel

Cancel one or more tasks.

```bash
greyctl task cancel <task-id> [task-id...] [flags]
```

**Flags:**

| Flag | Description |
|------|-------------|
| `--reason` | Cancellation reason |

**Examples:**

```bash
# Cancel single task
greyctl task cancel task_abc123

# Cancel with reason
greyctl task cancel task_abc123 --reason "No longer needed"

# Cancel multiple tasks
greyctl task cancel task_abc123 task_def456 task_ghi789
```

---

#### greyctl task proof

Retrieve TEE proof artifact for a task.

```bash
greyctl task proof <task-id> [flags]
```

**Flags:**

| Flag | Description |
|------|-------------|
| `--output` | Save proof to file |
| `--verify` | Verify proof locally |

**Examples:**

```bash
# Get proof as JSON
greyctl task proof task_abc123

# Save proof to file
greyctl task proof task_abc123 --output proof.bin

# Verify proof
greyctl task proof task_abc123 --verify
```

**Output:**

```json
{
  "task_id": "task_abc123",
  "platform": "sgx",
  "mrenclave": "a1b2c3d4e5f6...",
  "verified": true
}
```

---

### Cluster Commands

Commands for cluster health and management.

#### greyctl cluster health

Display cluster health status.

```bash
greyctl cluster health [flags]
```

**Flags:**

| Flag | Description | Default |
|------|-------------|---------|
| `--watch`, `-w` | Watch health status | `false` |
| `--interval` | Polling interval | `5s` |

**Examples:**

```bash
# Basic health check
greyctl cluster health

# Watch mode
greyctl cluster health --watch

# JSON output
greyctl cluster health -o json
```

**Output:**

```
Grey Cluster Health
───────────────────────────────────────
Status:     HEALTHY
Version:    1.5.0

Nodes:
  Total:        15
  Active:       14
  Draining:     1
  Quarantined:  0

Consensus:
  Leader:   grey-coordinator-0
  Term:     42
  Quorum:   OK (3/3)

Resources:
  CPU:     72%
  Memory:  65%

Queue:
  Depth:      1,234 tasks
  Throughput: 5,000 tasks/sec
```

---

#### greyctl cluster nodes

List cluster nodes.

```bash
greyctl cluster nodes [flags]
```

**Flags:**

| Flag | Description |
|------|-------------|
| `--status` | Filter by status: `active`, `draining`, `quarantined` |
| `--role` | Filter by role: `coordinator`, `worker`, `gateway` |

**Examples:**

```bash
# List all nodes
greyctl cluster nodes

# Filter by role
greyctl cluster nodes --role worker

# Filter by status
greyctl cluster nodes --status active
```

**Output:**

```
NODE ID              ROLE         STATUS    TASKS   CPU    MEMORY   TEE
grey-coordinator-0   coordinator  active    0       15%    22%      no
grey-coordinator-1   coordinator  active    0       12%    20%      no
grey-worker-1        worker       active    42      78%    65%      yes
grey-worker-2        worker       draining  12      45%    55%      yes
grey-worker-3        worker       active    38      72%    62%      yes
```

---

#### greyctl cluster usage

Show tenant resource usage and quotas.

```bash
greyctl cluster usage
```

**Output:**

```
Resource Usage for tenant-abc
─────────────────────────────────────────────────

Resource               Used      Limit     Usage
───────────────────────────────────────────────────
CPU Cores              72        100       72%  ████████████████░░░░
Memory (GB)            168       256       66%  █████████████░░░░░░░
Concurrent Tasks       3,402     10,000    34%  ███████░░░░░░░░░░░░░
Queue Depth            1,234     50,000    2%   ░░░░░░░░░░░░░░░░░░░░

Rate Limits:
  Submit Rate:  450/1000 per minute
  API Rate:     8500/10000 per minute

Status: Not throttled
```

---

### Replay Command

Replay a previously executed task.

```bash
greyctl replay <task-id> [flags]
```

**Flags:**

| Flag | Description |
|------|-------------|
| `--payload-override` | Override original payload (JSON) |
| `--at` | Point-in-time for state reconstruction (RFC3339) |
| `--watch`, `-w` | Watch replay execution |

**Examples:**

```bash
# Replay task as-is
greyctl replay task_abc123

# Replay with modified payload
greyctl replay task_abc123 --payload-override '{"debug": true}'

# Replay at specific point in time
greyctl replay task_abc123 --at "2024-01-15T10:30:00Z"

# Replay and watch
greyctl replay task_abc123 --watch
```

**Output:**

```
Replaying task_abc123...

New Task ID: task_xyz789
Status:      pending
Original:    task_abc123

Use 'greyctl task status task_xyz789 --watch' to monitor.
```

---

### Quarantine Commands

Commands for node quarantine management.

#### greyctl quarantine node

Quarantine a node.

```bash
greyctl quarantine node <node-id> [flags]
```

**Flags:**

| Flag | Description |
|------|-------------|
| `--reason` | Quarantine reason (required) |
| `--immediate` | Kill running tasks immediately |

**Examples:**

```bash
# Quarantine with reason
greyctl quarantine node grey-worker-7 --reason "High error rate"

# Immediate quarantine
greyctl quarantine node grey-worker-7 --reason "Security incident" --immediate
```

---

#### greyctl quarantine list

List quarantined nodes.

```bash
greyctl quarantine list
```

**Output:**

```
NODE ID          QUARANTINED AT        REASON
grey-worker-5    2024-01-15 10:30:00   High error rate
grey-worker-9    2024-01-15 09:15:00   Memory leak suspected
```

---

#### greyctl quarantine lift

Remove quarantine from a node.

```bash
greyctl quarantine lift <node-id>
```

**Examples:**

```bash
greyctl quarantine lift grey-worker-5
```

---

### Attestation Commands

Commands for TEE attestation verification.

#### greyctl attest verify

Verify node attestation.

```bash
greyctl attest verify [node-id] [flags]
```

**Flags:**

| Flag | Description |
|------|-------------|
| `--all` | Verify all nodes |
| `--mrenclave` | Expected mrenclave value |
| `--mrsigner` | Expected mrsigner value |

**Examples:**

```bash
# Verify specific node
greyctl attest verify grey-worker-7

# Verify with expected measurements
greyctl attest verify grey-worker-7 --mrenclave a1b2c3...

# Verify all nodes
greyctl attest verify --all
```

**Output:**

```
Attestation for grey-worker-7
────────────────────────────────
Platform:    SGX
Status:      VALID ✓
MRENCLAVE:   a1b2c3d4e5f6789...
MRSIGNER:    f6e5d4c3b2a1987...
Verified At: 2024-01-15 10:30:00
Expires At:  2024-01-15 11:30:00
```

---

#### greyctl attest request

Request fresh attestation.

```bash
greyctl attest request [flags]
```

**Flags:**

| Flag | Description |
|------|-------------|
| `--node` | Specific node to request from |

**Examples:**

```bash
# Request from any node
greyctl attest request

# Request from specific node
greyctl attest request --node grey-worker-7
```

---

### Config Commands

Commands for managing greyctl configuration.

#### greyctl config view

Display current configuration.

```bash
greyctl config view
```

**Output:**

```yaml
endpoint: https://api.grey.io
tenant_id: tenant-123
api_key: ***abc123
output: table
timeout: 30s
```

---

#### greyctl config set

Set a configuration value.

```bash
greyctl config set <key> <value>
```

**Available Keys:**

| Key | Description |
|-----|-------------|
| `endpoint` | Grey API endpoint URL |
| `api_key` | API authentication key |
| `tenant_id` | Default tenant ID |
| `output` | Default output format |
| `timeout` | Request timeout |

**Examples:**

```bash
greyctl config set endpoint https://api.grey.io
greyctl config set api_key grey_api_key_tenant123_abc...
greyctl config set output json
greyctl config set timeout 60s
```

---

#### greyctl config init

Initialize configuration file.

```bash
greyctl config init
```

Creates `~/.grey/config.yaml` with defaults.

---

### Version Command

```bash
greyctl version
```

**Output:**

```
greyctl version 1.5.0
  Build: 2024-01-15
  Go: 1.21
  Commit: abc123
```

---

## Environment Variables

| Variable | Description | Overrides |
|----------|-------------|-----------|
| `GREY_API_KEY` | API authentication key | `--api-key`, config `api_key` |
| `GREY_API_ENDPOINT` | Grey API endpoint | `--endpoint`, config `endpoint` |
| `GREY_TENANT_ID` | Tenant ID | config `tenant_id` |
| `GREY_TIMEOUT` | Request timeout | `--timeout`, config `timeout` |
| `GREY_OUTPUT` | Output format | `--output`, config `output` |
| `NO_COLOR` | Disable colored output | |

**Precedence Order (highest to lowest):**

1. Command-line flags
2. Environment variables
3. Config file
4. Defaults

---

## Examples

### Workflow: Submit and Monitor Task

```bash
# Submit task and capture task ID
TASK_ID=$(greyctl task submit --payload '{"data": "test"}' -o json | jq -r '.task_id')
echo "Submitted: $TASK_ID"

# Monitor until completion
greyctl task status $TASK_ID --watch

# Get result
greyctl task status $TASK_ID -o json | jq '.result'
```

### Workflow: Batch Processing

```bash
# Create task files
for i in {1..100}; do
  echo "{\"item\": $i}" > /tmp/tasks/task-$i.json
done

# Submit batch
greyctl task submit --batch /tmp/tasks/

# Monitor batch
greyctl task list --since 1m --status running,pending
```

### Workflow: Debugging Failed Task

```bash
# Find failed tasks
greyctl task list --status failed --since 24h

# Inspect failed task
greyctl task status task_abc123 -o json | jq '.error'

# Replay with debugging
greyctl replay task_abc123 --payload-override '{"debug": true}' --watch
```

### Workflow: Incident Response

```bash
# Check cluster health
greyctl cluster health

# Find problematic node
greyctl cluster nodes --role worker | sort -k5 -rn | head

# Quarantine node
greyctl quarantine node grey-worker-7 --reason "High error rate detected"

# Verify remaining cluster
greyctl cluster health

# Replay affected tasks
greyctl task list --status failed --since 30m -o json | \
  jq -r '.tasks[].task_id' | \
  xargs -I{} greyctl replay {}
```

### Scripting with JSON Output

```bash
# Get cluster utilization as percentage
CPU_UTIL=$(greyctl cluster health -o json | jq '.utilization.cpu_percent')
if [ "$CPU_UTIL" -gt 80 ]; then
  echo "Warning: CPU utilization is ${CPU_UTIL}%"
fi

# Count pending tasks
PENDING=$(greyctl task list --status pending -o json | jq '.total')
echo "Pending tasks: $PENDING"
```

---

## Shell Completion

### Bash

```bash
# Add to ~/.bashrc
source <(greyctl completion bash)
```

### Zsh

```bash
# Add to ~/.zshrc
source <(greyctl completion zsh)
```

### Fish

```fish
# Add to ~/.config/fish/config.fish
greyctl completion fish | source
```

### PowerShell

```powershell
greyctl completion powershell | Out-String | Invoke-Expression
```

---

## Exit Codes

| Code | Meaning |
|------|---------|
| 0 | Success |
| 1 | General error |
| 2 | Invalid usage |
| 3 | Authentication error |
| 4 | Not found |
| 5 | Rate limited |
| 6 | Timeout |

---

*CLI Version: 1.5.0*  
*Last Updated: 2024*
