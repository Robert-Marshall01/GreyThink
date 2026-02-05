# Grey Distributed — Contribution Guide

Welcome to Grey Distributed! We're excited that you're interested in contributing. This guide will help you get started, whether you're fixing a typo or implementing a major feature.

---

## Table of Contents

1. [Getting Started](#getting-started)
2. [Development Environment](#development-environment)
3. [Making Your First Contribution](#making-your-first-contribution)
4. [Contribution Types](#contribution-types)
5. [Code Standards](#code-standards)
6. [Testing Requirements](#testing-requirements)
7. [Documentation](#documentation)
8. [Pull Request Process](#pull-request-process)
9. [Review Process](#review-process)
10. [Recognition & Advancement](#recognition--advancement)
11. [Getting Help](#getting-help)

---

## Getting Started

### Prerequisites

Before contributing, please:

1. **Read the Manifesto**: Understand our [community values](community_manifesto.md)
2. **Sign the CLA**: Complete the [Contributor License Agreement](https://cla.grey-distributed.io)
3. **Join the Community**: Introduce yourself in the [community forum](https://discuss.grey-distributed.io/c/introductions)

### Contribution Types We Accept

| Type | Examples | Difficulty |
|------|----------|------------|
| Documentation | Typos, clarifications, tutorials | Beginner |
| Bug fixes | Issue reproduction, fixes | Beginner-Intermediate |
| Tests | Unit tests, integration tests | Intermediate |
| Features | New functionality | Intermediate-Advanced |
| Performance | Optimizations, benchmarks | Advanced |
| Architecture | RFCs, design changes | Advanced |

### Choosing What to Work On

- **Good First Issues**: [github.com/grey-distributed/grey/labels/good-first-issue](https://github.com/grey-distributed/grey/labels/good-first-issue)
- **Help Wanted**: [github.com/grey-distributed/grey/labels/help-wanted](https://github.com/grey-distributed/grey/labels/help-wanted)
- **SIG Backlogs**: Each Special Interest Group maintains a backlog
- **Your Own Ideas**: Open an issue to discuss before implementing

---

## Development Environment

### System Requirements

```yaml
minimum:
  os: Linux (Ubuntu 20.04+), macOS 12+, Windows 11 (WSL2)
  cpu: 4 cores
  memory: 16 GB RAM
  storage: 50 GB free space
  
recommended:
  os: Linux (Ubuntu 22.04)
  cpu: 8+ cores
  memory: 32 GB RAM
  storage: 100 GB SSD
```

### Required Tools

```bash
# Rust toolchain (1.75+)
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
rustup default stable
rustup component add rustfmt clippy

# Go toolchain (1.21+)
# Follow instructions at https://go.dev/dl/

# Protocol Buffers
# Ubuntu/Debian
sudo apt install protobuf-compiler

# macOS
brew install protobuf

# Additional tools
cargo install cargo-watch cargo-nextest cargo-audit
```

### Repository Setup

```bash
# Clone the repository
git clone https://github.com/grey-distributed/grey.git
cd grey

# Configure git hooks
./scripts/setup-hooks.sh

# Build the project
make build

# Run tests
make test

# Start a local development cluster
make dev-cluster
```

### IDE Configuration

#### VS Code (Recommended)

```json
// .vscode/settings.json
{
  "rust-analyzer.cargo.features": "all",
  "rust-analyzer.checkOnSave.command": "clippy",
  "editor.formatOnSave": true,
  "[rust]": {
    "editor.defaultFormatter": "rust-lang.rust-analyzer"
  }
}
```

Recommended extensions:
- rust-analyzer
- Even Better TOML
- crates
- Error Lens

#### JetBrains (CLion/RustRover)

- Install Rust plugin
- Enable "Run rustfmt on save"
- Configure Clippy as external linter

---

## Making Your First Contribution

### Step 1: Find an Issue

1. Browse [good first issues](https://github.com/grey-distributed/grey/labels/good-first-issue)
2. Check that no one else is assigned
3. Comment expressing interest: "I'd like to work on this"
4. Wait for maintainer confirmation before starting

### Step 2: Fork and Branch

```bash
# Fork the repo on GitHub, then:
git clone https://github.com/YOUR-USERNAME/grey.git
cd grey
git remote add upstream https://github.com/grey-distributed/grey.git

# Create a branch
git checkout -b fix/issue-123-describe-fix
```

Branch naming conventions:
- `fix/issue-NNN-short-description` for bug fixes
- `feat/issue-NNN-short-description` for features
- `docs/short-description` for documentation
- `test/short-description` for test additions
- `refactor/short-description` for refactoring

### Step 3: Make Changes

1. Write code following our [code standards](#code-standards)
2. Add tests for new functionality
3. Update documentation as needed
4. Ensure all tests pass locally

### Step 4: Commit

```bash
# Stage changes
git add -A

# Commit with conventional commit message
git commit -m "fix(scheduler): prevent panic on empty task queue (#123)

The scheduler would panic when processing an empty task queue due to
an unchecked unwrap() call. This change adds proper handling for the
empty case.

Fixes #123"
```

Commit message format:
```
<type>(<scope>): <subject> (#issue)

<body>

<footer>
```

Types: `feat`, `fix`, `docs`, `style`, `refactor`, `test`, `chore`

### Step 5: Push and Create PR

```bash
# Push your branch
git push origin fix/issue-123-describe-fix

# Create PR on GitHub
# Use the PR template provided
```

---

## Contribution Types

### Bug Fixes

```rust
// Before: Bug report indicates panic
fn process_task(&mut self) {
    let task = self.queue.pop().unwrap(); // Panics if empty!
    // ...
}

// After: Handle the empty case
fn process_task(&mut self) -> Option<TaskResult> {
    let task = self.queue.pop()?;
    // ...
    Some(result)
}
```

Checklist for bug fixes:
- [ ] Issue exists describing the bug
- [ ] Test case reproduces the bug
- [ ] Fix addresses root cause, not symptoms
- [ ] No regression in existing tests
- [ ] Changelog entry added

### New Features

Features require an RFC if they:
- Add new public API
- Change existing behavior
- Affect multiple subsystems
- Have security implications

RFC Process:
1. Copy `rfc/0000-template.md` to `rfc/0000-my-feature.md`
2. Fill in the template sections
3. Open PR titled "RFC: My Feature"
4. Participate in discussion
5. Address feedback
6. TSC reviews and votes

### Documentation

We value documentation contributions highly. Types include:

| Type | Location | Format |
|------|----------|--------|
| API docs | Inline in code | Rustdoc |
| Tutorials | `docs/tutorials/` | Markdown |
| Guides | `docs/guides/` | Markdown |
| Architecture | `docs/architecture/` | Markdown + diagrams |
| Reference | `docs/reference/` | Markdown |

Documentation guidelines:
- Use plain language (no jargon without explanation)
- Include code examples that compile and run
- Use present tense
- Link to related documentation

### Performance Improvements

Performance contributions should include:

1. **Benchmark showing current performance**
   ```bash
   cargo bench --bench my_benchmark > before.txt
   ```

2. **Clear explanation of the optimization**

3. **Benchmark showing improvement**
   ```bash
   cargo bench --bench my_benchmark > after.txt
   ```

4. **Analysis of tradeoffs** (memory vs. speed, etc.)

---

## Code Standards

### Rust Style

We follow the [Rust API Guidelines](https://rust-lang.github.io/api-guidelines/) with these additions:

```rust
// ✅ Good: Use descriptive names
fn calculate_resource_allocation(
    tenant: &Tenant,
    request: ResourceRequest,
) -> Result<Allocation, AllocationError>

// ❌ Bad: Abbreviated names
fn calc_res_alloc(t: &Tenant, r: ResourceRequest) -> Result<Allocation, AllocationError>

// ✅ Good: Handle errors explicitly
let config = load_config(&path).map_err(|e| {
    ConfigError::LoadFailed {
        path: path.clone(),
        source: e,
    }
})?;

// ❌ Bad: Unwrap in library code
let config = load_config(&path).unwrap();

// ✅ Good: Document public items
/// Schedules a task for execution on an appropriate node.
///
/// # Arguments
///
/// * `task` - The task to schedule
/// * `constraints` - Placement constraints
///
/// # Returns
///
/// The node ID where the task was scheduled, or an error if no
/// suitable node was found.
///
/// # Example
///
/// ```
/// let node = scheduler.schedule(task, constraints)?;
/// ```
pub fn schedule(&self, task: Task, constraints: Constraints) -> Result<NodeId, ScheduleError>
```

### Error Handling

```rust
// Use thiserror for library errors
#[derive(Debug, thiserror::Error)]
pub enum SchedulerError {
    #[error("no nodes available matching constraints")]
    NoNodesAvailable,
    
    #[error("node {node_id} has insufficient resources: need {needed}, have {available}")]
    InsufficientResources {
        node_id: NodeId,
        needed: Resources,
        available: Resources,
    },
    
    #[error("task {task_id} not found")]
    TaskNotFound { task_id: TaskId },
    
    #[error("internal error")]
    Internal(#[from] anyhow::Error),
}

// Use anyhow for application code
fn main() -> anyhow::Result<()> {
    let config = load_config("grey.toml")
        .context("failed to load configuration")?;
    // ...
}
```

### Async Code

```rust
// ✅ Good: Use async where appropriate
async fn fetch_node_status(&self, node_id: NodeId) -> Result<NodeStatus, Error> {
    let response = self.client
        .get(&format!("/nodes/{}/status", node_id))
        .send()
        .await?;
    Ok(response.json().await?)
}

// ✅ Good: Avoid holding locks across await points
async fn update_state(&self, update: Update) -> Result<(), Error> {
    let current = {
        let state = self.state.read().await;
        state.clone()
    }; // Lock released here
    
    let new_state = compute_new_state(current, update).await?;
    
    let mut state = self.state.write().await;
    *state = new_state;
    Ok(())
}
```

### Safety

```rust
// ❌ Unsafe code requires justification and review
// Only use unsafe when absolutely necessary

// If you must use unsafe, document why it's sound:
/// # Safety
///
/// The caller must ensure that:
/// - `ptr` is valid and properly aligned
/// - `ptr` points to initialized memory
/// - No other references to this memory exist
unsafe fn read_raw(ptr: *const u8, len: usize) -> Vec<u8> {
    // ...
}
```

### Formatting and Linting

```bash
# Before committing, run:
cargo fmt --all
cargo clippy --all-targets --all-features -- -D warnings

# These checks run in CI and must pass
```

---

## Testing Requirements

### Test Coverage

| Change Type | Required Tests |
|-------------|----------------|
| Bug fix | Regression test that fails before fix |
| New feature | Unit tests + integration tests |
| Performance | Benchmarks |
| Refactoring | Existing tests must pass |

### Running Tests

```bash
# Run all tests
make test

# Run specific test
cargo test test_scheduler_allocation

# Run tests with output
cargo test -- --nocapture

# Run integration tests
make test-integration

# Run with coverage
make coverage
```

### Writing Good Tests

```rust
#[cfg(test)]
mod tests {
    use super::*;
    
    // ✅ Good: Descriptive test name
    #[test]
    fn schedule_returns_error_when_no_nodes_have_sufficient_memory() {
        // Arrange
        let scheduler = Scheduler::new();
        scheduler.add_node(Node::with_memory(1024)); // 1GB
        let task = Task::requiring_memory(2048); // 2GB
        
        // Act
        let result = scheduler.schedule(task);
        
        // Assert
        assert!(matches!(result, Err(SchedulerError::InsufficientResources { .. })));
    }
    
    // ✅ Good: Property-based testing for complex logic
    #[test]
    fn allocation_never_exceeds_node_capacity() {
        proptest!(|(tasks in vec(task_strategy(), 1..100))| {
            let scheduler = Scheduler::new();
            scheduler.add_node(Node::default());
            
            for task in tasks {
                let _ = scheduler.schedule(task);
            }
            
            for node in scheduler.nodes() {
                prop_assert!(node.used_resources() <= node.capacity());
            }
        });
    }
}
```

### Integration Tests

Integration tests live in `tests/` and test full system behavior:

```rust
// tests/federation.rs

#[tokio::test]
async fn federated_clusters_can_share_resources() {
    let cluster_a = TestCluster::spawn().await;
    let cluster_b = TestCluster::spawn().await;
    
    // Establish federation
    cluster_a.federate_with(&cluster_b).await.unwrap();
    
    // Submit task that requires federation
    let task = Task::requiring_resources(cluster_a.capacity() + 1);
    let result = cluster_a.submit(task).await.unwrap();
    
    // Verify task ran on cluster_b
    assert_eq!(result.executed_on, cluster_b.id());
}
```

---

## Documentation

### Code Documentation

All public items must have documentation:

```rust
/// A distributed task scheduler that assigns tasks to nodes.
///
/// The scheduler uses a fairness-aware algorithm to ensure equitable
/// resource distribution across tenants while optimizing for locality
/// and resource utilization.
///
/// # Thread Safety
///
/// `Scheduler` is `Send + Sync` and can be shared across threads.
/// Internal state is protected by fine-grained locks.
///
/// # Example
///
/// ```
/// use grey::scheduler::{Scheduler, Task};
///
/// let scheduler = Scheduler::builder()
///     .fairness_weight(0.5)
///     .locality_weight(0.3)
///     .utilization_weight(0.2)
///     .build();
///
/// scheduler.add_node(node);
/// scheduler.schedule(task).await?;
/// ```
pub struct Scheduler { /* ... */ }
```

### Changelog

Update `CHANGELOG.md` for user-facing changes:

```markdown
## [Unreleased]

### Added
- New `--dry-run` flag for `greyctl apply` (#456)

### Changed
- Improved scheduler performance by 25% (#789)

### Deprecated
- `Scheduler::schedule_sync` in favor of `schedule().await` (#234)

### Fixed
- Panic when processing empty task queue (#123)

### Security
- Updated `openssl` dependency to address CVE-2024-XXXX (#567)
```

---

## Pull Request Process

### PR Template

When you open a PR, fill in the template:

```markdown
## Description
Brief description of what this PR does.

## Related Issues
Fixes #123
Related to #456

## Changes
- Added proper error handling for empty queue
- Added regression test
- Updated documentation

## Testing
- [ ] Unit tests pass
- [ ] Integration tests pass
- [ ] Manual testing performed

## Checklist
- [ ] Code follows style guidelines
- [ ] Self-review completed
- [ ] Documentation updated
- [ ] Changelog entry added
- [ ] No new warnings
```

### PR Size Guidelines

| Size | Lines Changed | Review Time |
|------|---------------|-------------|
| Small | < 100 | Same day |
| Medium | 100-500 | 1-2 days |
| Large | 500-1000 | 3-5 days |
| X-Large | > 1000 | Consider splitting |

Large PRs should be broken into smaller, logical commits.

---

## Review Process

### What Reviewers Look For

1. **Correctness**: Does the code do what it claims?
2. **Tests**: Are edge cases covered?
3. **Style**: Does it follow our conventions?
4. **Performance**: Any obvious inefficiencies?
5. **Security**: Any vulnerabilities introduced?
6. **Documentation**: Are changes documented?

### Responding to Reviews

```markdown
# Responding to feedback

## If you agree:
"Good catch! Fixed in abc1234."

## If you disagree:
"I considered that approach, but chose this because [reason].
Would you be open to keeping it as-is, or should we discuss further?"

## If you're unsure:
"I'm not sure I understand the concern. Could you elaborate?"
```

### Approval Requirements

| Change Type | Required Approvals | Required Reviewers |
|-------------|-------------------|-------------------|
| Documentation | 1 | Any maintainer |
| Bug fix | 1 | Subsystem maintainer |
| Feature | 2 | 1 core + 1 subsystem |
| Security | 2 | Security team member |
| Breaking change | 3 | Core team majority |

---

## Recognition & Advancement

### Contribution Recognition

| Achievement | Recognition |
|-------------|-------------|
| First merged PR | Welcome message + listing in contributors |
| 10 merged PRs | "Regular Contributor" badge |
| 50 merged PRs | "Active Contributor" badge |
| Significant feature | Mention in release notes |
| Security fix | Security acknowledgments page |

### Path to Maintainer

1. **Consistent Contribution**: Regular, quality contributions over 6+ months
2. **Community Engagement**: Helping others, reviewing PRs
3. **Domain Expertise**: Deep knowledge of a subsystem
4. **Nomination**: By existing maintainer
5. **Approval**: TSC vote

Maintainer responsibilities:
- Review and merge PRs in your area
- Triage issues
- Participate in maintainer meetings
- Mentor new contributors

---

## Getting Help

### Communication Channels

| Channel | Use For | Response Time |
|---------|---------|---------------|
| [GitHub Issues](https://github.com/grey-distributed/grey/issues) | Bug reports, feature requests | 1-3 days |
| [Forum](https://discuss.grey-distributed.io) | Questions, discussions | 1-2 days |
| [Chat](https://chat.grey-distributed.io) | Quick questions, synchronous discussion | Minutes-hours |
| [Office Hours](https://grey-distributed.io/office-hours) | Live help from maintainers | Weekly |

### Asking Good Questions

```markdown
# Good question format:

## What I'm trying to do
I want to schedule a task that requires exactly 4 GPUs.

## What I've tried
```
let task = Task::builder()
    .gpu_count(4)
    .gpu_scheduling(GpuScheduling::Exact)
    .build();
scheduler.schedule(task).await?;
```

## What happened
Error: "GpuScheduling::Exact not supported"

## What I expected
The task to be scheduled on a node with exactly 4 GPUs.

## Environment
- Grey version: 0.5.2
- OS: Ubuntu 22.04
- Rust: 1.75.0
```

---

## Thank You!

Every contribution, no matter how small, makes Grey better. Whether you're fixing a typo or implementing a major feature, you're helping build infrastructure that benefits everyone.

Welcome to the Grey community!

---

*Last updated: 2026-02-04*
*Questions? Ask in the [community forum](https://discuss.grey-distributed.io).*
