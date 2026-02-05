# Grey Distributed — Database Integration

Integration patterns for PostgreSQL, Cassandra, and MongoDB.

---

## Overview

Grey Distributed uses external databases for:

- **Task state persistence** — Durable storage of task metadata
- **Cluster state** — Raft log persistence and snapshots
- **Proof artifacts** — Immutable TEE attestation records
- **Tenant metadata** — Quotas, policies, and configuration

---

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    Grey Storage Layer                        │
│  ┌─────────────────────────────────────────────────────────┐ │
│  │                   Quorum Adapter                         │ │
│  │  read_quorum(nodes) → consistent reads                   │ │
│  │  write_quorum(nodes) → durable writes                    │ │
│  └─────────────────────────────────────────────────────────┘ │
│           │              │              │                    │
│           ▼              ▼              ▼                    │
│   ┌─────────────┐ ┌─────────────┐ ┌─────────────┐           │
│   │  PostgreSQL │ │  Cassandra  │ │   MongoDB   │           │
│   │   Adapter   │ │   Adapter   │ │   Adapter   │           │
│   └─────────────┘ └─────────────┘ └─────────────┘           │
└─────────────────────────────────────────────────────────────┘
```

---

## PostgreSQL Adapter

**File:** [integrations/db/postgres_adapter.rs](../../integrations/db/postgres_adapter.rs)

### Use Case

Best for:
- ACID transactions
- Complex queries and joins
- Smaller deployments (< 1TB)
- Strong consistency requirements

### Configuration

```yaml
postgres:
  primary:
    url: postgres://user:pass@primary:5432/grey
    pool_size: 20
  replicas:
    - postgres://user:pass@replica1:5432/grey
    - postgres://user:pass@replica2:5432/grey
  
  sharding:
    enabled: true
    key: tenant_id
    shards: 16
  
  quorum:
    read: majority    # Read from N/2+1 replicas
    write: majority   # Write to N/2+1 replicas
```

### Sharding Strategy

Grey uses **consistent hashing** on `tenant_id`:

```rust
// Shard selection
let shard = consistent_hash(tenant_id) % num_shards;
let pool = self.shard_pools[shard];
```

**Tradeoffs:**
- ✅ Even distribution across shards
- ✅ Minimal resharding on scale-out
- ⚠️ Cross-tenant queries require scatter-gather
- ⚠️ Hot tenants may create hot shards

### Read/Write Quorum

```rust
// Quorum configuration
pub enum QuorumLevel {
    One,        // Fastest, least consistent
    Majority,   // N/2+1 nodes, balanced
    All,        // Slowest, strongest consistency
}

// Quorum read
pub async fn quorum_read(&self, query: &str) -> Result<Row> {
    let futures = self.replicas.iter()
        .map(|r| r.query_one(query));
    
    let results = join_all(futures).await;
    let quorum_size = (self.replicas.len() / 2) + 1;
    
    // Wait for quorum agreement
    wait_for_quorum(results, quorum_size)
}
```

### Schema Design

```sql
-- Tasks table (per-shard)
CREATE TABLE tasks (
    id UUID PRIMARY KEY,
    tenant_id TEXT NOT NULL,
    status TEXT NOT NULL,
    payload JSONB,
    result JSONB,
    proof JSONB,
    created_at TIMESTAMPTZ DEFAULT NOW(),
    updated_at TIMESTAMPTZ DEFAULT NOW(),
    CONSTRAINT tenant_isolation CHECK (tenant_id IS NOT NULL)
);

-- Indexes
CREATE INDEX idx_tasks_tenant ON tasks(tenant_id);
CREATE INDEX idx_tasks_status ON tasks(tenant_id, status);
CREATE INDEX idx_tasks_created ON tasks(tenant_id, created_at DESC);

-- Row-level security
ALTER TABLE tasks ENABLE ROW LEVEL SECURITY;
CREATE POLICY tenant_policy ON tasks
    USING (tenant_id = current_setting('grey.tenant_id'));
```

---

## Cassandra Adapter

**File:** [integrations/db/cassandra_adapter.rs](../../integrations/db/cassandra_adapter.rs)

### Use Case

Best for:
- Time-series data (task history)
- Multi-region deployments
- High write throughput
- Predictable latency at scale

### Configuration

```yaml
cassandra:
  hosts:
    - cass1.grey.io:9042
    - cass2.grey.io:9042
    - cass3.grey.io:9042
  
  keyspace: grey
  replication:
    strategy: NetworkTopologyStrategy
    datacenters:
      us-east-1: 3
      eu-west-1: 3
  
  consistency:
    read: LOCAL_QUORUM
    write: LOCAL_QUORUM
  
  driver:
    connection_pool_size: 4
    request_timeout_ms: 12000
    speculative_retry_delay_ms: 50
```

### Consistency Levels

| Level | Description | Use Case |
|-------|-------------|----------|
| `ONE` | Single replica | Analytics reads |
| `LOCAL_QUORUM` | Majority in local DC | Default for Grey |
| `QUORUM` | Majority across all DCs | Cross-region consistency |
| `ALL` | All replicas | Rare, for auditing |

**Tradeoffs:**
- `LOCAL_QUORUM`: Fast within region, eventually consistent across regions
- `QUORUM`: Slower, but consistent globally
- Grey uses `LOCAL_QUORUM` with async replication for cross-region

### Schema Design

```cql
-- Keyspace
CREATE KEYSPACE grey WITH replication = {
    'class': 'NetworkTopologyStrategy',
    'us-east-1': 3,
    'eu-west-1': 3
};

-- Tasks by tenant (partition by tenant, cluster by time)
CREATE TABLE tasks_by_tenant (
    tenant_id TEXT,
    task_id UUID,
    status TEXT,
    payload BLOB,
    result BLOB,
    proof BLOB,
    created_at TIMESTAMP,
    updated_at TIMESTAMP,
    PRIMARY KEY ((tenant_id), created_at, task_id)
) WITH CLUSTERING ORDER BY (created_at DESC, task_id ASC)
  AND default_time_to_live = 2592000  -- 30 days
  AND compaction = {'class': 'TimeWindowCompactionStrategy'};

-- Task by ID (for direct lookups)
CREATE TABLE tasks_by_id (
    task_id UUID,
    tenant_id TEXT,
    status TEXT,
    payload BLOB,
    result BLOB,
    proof BLOB,
    created_at TIMESTAMP,
    updated_at TIMESTAMP,
    PRIMARY KEY (task_id)
);

-- Materialized view for status queries
CREATE MATERIALIZED VIEW tasks_by_status AS
    SELECT * FROM tasks_by_tenant
    WHERE tenant_id IS NOT NULL AND status IS NOT NULL
    PRIMARY KEY ((tenant_id, status), created_at, task_id);
```

### Tombstone Management

Cassandra creates tombstones on deletes. Grey mitigates with:

1. **TTL-based expiration** — Auto-expire old tasks
2. **Time-window compaction** — Efficient cleanup of time-series data
3. **Soft deletes** — Mark as deleted, batch physical cleanup

---

## MongoDB Adapter

**File:** [integrations/db/mongo_adapter.rs](../../integrations/db/mongo_adapter.rs)

### Use Case

Best for:
- Flexible schema (evolving task payloads)
- Document-oriented data
- Aggregation pipelines
- Operational simplicity

### Configuration

```yaml
mongodb:
  uri: mongodb://user:pass@host1:27017,host2:27017,host3:27017/grey?replicaSet=rs0
  
  database: grey
  
  collections:
    tasks: tasks
    cluster: cluster_state
    tenants: tenants
  
  read_preference: primaryPreferred
  read_concern: majority
  write_concern:
    w: majority
    j: true
    wtimeout: 5000
  
  sharding:
    enabled: true
    key: tenant_id
    strategy: hashed  # or ranged
```

### Read Preference vs Read Concern

| Read Preference | Read Concern | Use Case |
|-----------------|--------------|----------|
| `primary` | `local` | Fastest, may read uncommitted |
| `primary` | `majority` | Consistent, reads committed data |
| `primaryPreferred` | `majority` | Default for Grey |
| `secondary` | `majority` | Analytics, reduce primary load |

**Grey defaults:**
- `primaryPreferred` + `majority` for task reads
- `primary` for cluster state (Raft log)

### Schema Design

```javascript
// Tasks collection
{
  "_id": "task_550e8400-e29b-41d4-a716-446655440000",
  "tenant_id": "tenant-123",
  "status": "completed",
  "task_type": "stateless",
  "payload": {
    "data": "..."
  },
  "result": {
    "output": "..."
  },
  "proof": {
    "platform": "SGX",
    "mrenclave": "abc...",
    "signature": "def..."
  },
  "created_at": ISODate("2024-01-15T10:30:00Z"),
  "updated_at": ISODate("2024-01-15T10:30:05Z"),
  "execution": {
    "node_id": "grey-worker-7",
    "duration_ms": 1234,
    "retries": 0
  }
}

// Indexes
db.tasks.createIndex({ "tenant_id": 1, "created_at": -1 })
db.tasks.createIndex({ "tenant_id": 1, "status": 1 })
db.tasks.createIndex({ "_id": "hashed" })  // For sharding
```

### Sharding Strategy

```javascript
// Enable sharding
sh.enableSharding("grey")

// Hashed sharding on tenant_id (even distribution)
sh.shardCollection("grey.tasks", { "tenant_id": "hashed" })

// Or ranged sharding (colocate tenant data)
sh.shardCollection("grey.tasks", { "tenant_id": 1, "created_at": -1 })
```

**Tradeoffs:**
- **Hashed**: Even distribution, no hot spots, scatter-gather for tenant queries
- **Ranged**: Tenant data colocated, targeted queries, possible hot spots

Grey recommends **ranged sharding** for tenant isolation.

---

## Quorum-Aware Operations

All database adapters implement the `QuorumStorage` trait:

```rust
#[async_trait]
pub trait QuorumStorage {
    /// Read with quorum consistency
    async fn quorum_read(
        &self,
        tenant_id: &str,
        key: &str,
        quorum: QuorumLevel,
    ) -> Result<Option<Value>>;
    
    /// Write with quorum durability
    async fn quorum_write(
        &self,
        tenant_id: &str,
        key: &str,
        value: Value,
        quorum: QuorumLevel,
    ) -> Result<()>;
    
    /// Compare-and-swap for optimistic concurrency
    async fn cas(
        &self,
        tenant_id: &str,
        key: &str,
        expected: Option<Value>,
        new_value: Value,
    ) -> Result<bool>;
}
```

### Implementation by Database

| Database | Quorum Read | Quorum Write | CAS |
|----------|-------------|--------------|-----|
| PostgreSQL | Read from N/2+1 replicas | Sync replication to N/2+1 | `UPDATE ... WHERE version = ?` |
| Cassandra | `LOCAL_QUORUM` | `LOCAL_QUORUM` | Lightweight transactions |
| MongoDB | `readConcern: majority` | `writeConcern: majority` | `findOneAndUpdate` with version |

---

## Performance Tuning

### Connection Pooling

```yaml
# All databases
pool:
  min_connections: 5
  max_connections: 20
  acquire_timeout_ms: 5000
  idle_timeout_ms: 300000
```

### Batching

```rust
// Batch writes for throughput
let batch = adapter.begin_batch();
for task in tasks {
    batch.insert(tenant_id, task);
}
batch.execute().await?;
```

### Caching

Grey uses a write-through cache:

```rust
// Read: cache-first
if let Some(value) = cache.get(key) {
    return Ok(value);
}
let value = db.read(key).await?;
cache.set(key, value.clone());
Ok(value)

// Write: write-through
db.write(key, value.clone()).await?;
cache.set(key, value);
```

---

## Monitoring

### Metrics Exported

```prometheus
# PostgreSQL
grey_pg_connections_active{shard="0"}
grey_pg_query_duration_seconds{operation="read"}
grey_pg_replication_lag_seconds{replica="replica1"}

# Cassandra
grey_cass_connections_active{host="cass1"}
grey_cass_query_duration_seconds{consistency="LOCAL_QUORUM"}
grey_cass_speculative_retries_total

# MongoDB
grey_mongo_connections_active
grey_mongo_operation_duration_seconds{operation="find"}
grey_mongo_replica_lag_seconds
```

### Alerts

```yaml
groups:
  - name: grey-database
    rules:
      - alert: DatabaseReplicationLag
        expr: grey_pg_replication_lag_seconds > 5
        for: 1m
        labels:
          severity: warning
        annotations:
          summary: "Replication lag exceeds 5s"
          
      - alert: DatabaseConnectionExhaustion
        expr: grey_pg_connections_active / grey_pg_connections_max > 0.9
        for: 5m
        labels:
          severity: critical
```

---

## Migration Guide

### From Single Database to Sharded

1. **Audit existing data distribution**
2. **Choose sharding key** (`tenant_id` recommended)
3. **Create shards** with identical schema
4. **Migrate data** using consistent hash
5. **Update adapter config** to use shards
6. **Verify** with shadow reads

### Between Database Types

Grey supports live migration between databases:

```bash
# Export from PostgreSQL
greyctl migrate export --source postgres --dest ./export/

# Import to MongoDB
greyctl migrate import --dest mongodb --source ./export/

# Verify
greyctl migrate verify --source postgres --dest mongodb
```

---

*Database adapters are in [integrations/db/](../../integrations/db/)*
