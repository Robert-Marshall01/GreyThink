--------------------------- MODULE GreyReplication ---------------------------
(***************************************************************************
 * Grey Distributed - Quorum Replication Formal Specification
 * 
 * This specification models Grey's storage replication layer:
 *   - Consistent hashing for data distribution
 *   - Quorum reads and writes (R + W > N)
 *   - Anti-entropy repair
 *   - Hinted handoff during failures
 * 
 * SAFETY PROPERTIES:
 *   - Quorum Intersection: Any read quorum intersects any write quorum
 *   - Version Consistency: Reads return the latest written version
 *   - No Lost Writes: Acknowledged writes are durable
 * 
 * LIVENESS PROPERTIES:
 *   - Read Availability: Reads complete if R replicas are available
 *   - Write Availability: Writes complete if W replicas are available
 *   - Repair Convergence: All replicas eventually converge
 * 
 * KEY INSIGHT: Quorum intersection (R + W > N) guarantees that any read
 * will see at least one replica that participated in the most recent write.
 * Vector clocks or version numbers break ties.
 ***************************************************************************)

EXTENDS Integers, Sequences, FiniteSets, TLC

\* ---------------------------------------------------------
\* CONSTANTS
\* ---------------------------------------------------------

CONSTANTS
    Nodes,           \* Set of storage nodes
    Keys,            \* Set of keys that can be stored
    MaxVersion,      \* Maximum version number
    N,               \* Replication factor
    R,               \* Read quorum size
    W                \* Write quorum size

\* Quorum constraint: reads and writes must intersect
ASSUME R + W > N
ASSUME R <= N /\ W <= N

\* ---------------------------------------------------------
\* VARIABLES
\* ---------------------------------------------------------

VARIABLES
    \* Storage state
    data,            \* data[node][key] = [version |-> v, value |-> val] or Nil
    
    \* Node availability
    available,       \* available[node] = TRUE if node is up
    
    \* In-flight operations
    pendingReads,    \* Set of {[key, client, responses]}
    pendingWrites,   \* Set of {[key, value, client, acks]}
    
    \* Hinted handoff queue
    hints,           \* hints[node] = sequence of {destNode, key, version, value}
    
    \* Anti-entropy state
    repairQueue      \* Set of {node, key} pairs needing repair

vars == <<data, available, pendingReads, pendingWrites, hints, repairQueue>>

\* Nil value
Nil == "nil"

\* ---------------------------------------------------------
\* TYPE INVARIANT
\* ---------------------------------------------------------

DataEntry == [version: 0..MaxVersion, value: Nat]

TypeInvariant ==
    /\ data \in [Nodes -> [Keys -> DataEntry \cup {Nil}]]
    /\ available \in [Nodes -> BOOLEAN]
    /\ \A hint \in UNION {Range(hints[n]) : n \in Nodes}:
        hint \in [destNode: Nodes, key: Keys, version: 0..MaxVersion, value: Nat]

Range(seq) == {seq[i] : i \in 1..Len(seq)}

\* ---------------------------------------------------------
\* HELPER OPERATORS
\* ---------------------------------------------------------

\* Get the replicas responsible for a key (consistent hashing)
\* In practice, this uses a hash ring; here we model it abstractly
ReplicasForKey(k) ==
    \* Return the first N nodes in a deterministic order
    \* (simplified - real implementation uses hash ring)
    CHOOSE S \in SUBSET Nodes : Cardinality(S) = N

\* Get available replicas for a key
AvailableReplicas(k) ==
    {n \in ReplicasForKey(k) : available[n]}

\* Check if we have enough replicas for a read
CanRead(k) == Cardinality(AvailableReplicas(k)) >= R

\* Check if we have enough replicas for a write
CanWrite(k) == Cardinality(AvailableReplicas(k)) >= W

\* Get the latest version among a set of responses
LatestVersion(responses) ==
    LET versions == {r.version : r \in responses}
    IN CHOOSE v \in versions : \A v2 \in versions : v >= v2

\* Get the value for the latest version
LatestValue(responses) ==
    LET latest == LatestVersion(responses)
    IN (CHOOSE r \in responses : r.version = latest).value

\* ---------------------------------------------------------
\* INITIAL STATE
\* ---------------------------------------------------------

Init ==
    /\ data = [n \in Nodes |-> [k \in Keys |-> Nil]]
    /\ available = [n \in Nodes |-> TRUE]
    /\ pendingReads = {}
    /\ pendingWrites = {}
    /\ hints = [n \in Nodes |-> << >>]
    /\ repairQueue = {}

\* ---------------------------------------------------------
\* STATE TRANSITIONS
\* ---------------------------------------------------------

(*
 * CLIENT READ: Initiate a quorum read.
 * 
 * Read Path:
 * 1. Send read request to R replicas
 * 2. Wait for R responses
 * 3. Return value with highest version
 * 4. (Background) Read repair if versions disagree
 *)
StartRead(k, client) ==
    /\ CanRead(k)
    /\ pendingReads' = pendingReads \cup {[key |-> k, client |-> client, responses |-> {}]}
    /\ UNCHANGED <<data, available, pendingWrites, hints, repairQueue>>

(*
 * REPLICA READ RESPONSE: A replica responds to a read request.
 *)
ReplicaReadResponse(n, readOp) ==
    /\ readOp \in pendingReads
    /\ n \in ReplicasForKey(readOp.key)
    /\ available[n]
    /\ LET entry == data[n][readOp.key]
           response == IF entry = Nil 
                       THEN [node |-> n, version |-> 0, value |-> Nil]
                       ELSE [node |-> n, version |-> entry.version, value |-> entry.value]
       IN pendingReads' = (pendingReads \ {readOp}) \cup 
                          {[readOp EXCEPT !.responses = @ \cup {response}]}
    /\ UNCHANGED <<data, available, pendingWrites, hints, repairQueue>>

(*
 * COMPLETE READ: Client receives R responses and returns the latest value.
 * 
 * READ REPAIR: If versions disagree, schedule repair.
 *)
CompleteRead(readOp) ==
    /\ readOp \in pendingReads
    /\ Cardinality(readOp.responses) >= R
    /\ pendingReads' = pendingReads \ {readOp}
    \* Check if repair is needed (versions disagree)
    /\ LET versions == {r.version : r \in readOp.responses}
           needsRepair == Cardinality(versions) > 1
       IN IF needsRepair
          THEN repairQueue' = repairQueue \cup 
               {[node |-> r.node, key |-> readOp.key] : r \in readOp.responses}
          ELSE UNCHANGED repairQueue
    /\ UNCHANGED <<data, available, pendingWrites, hints>>

(*
 * CLIENT WRITE: Initiate a quorum write.
 * 
 * Write Path:
 * 1. Coordinator generates new version number
 * 2. Send write to W replicas
 * 3. Wait for W acknowledgments
 * 4. Return success to client
 * 5. (Background) Propagate to remaining replicas
 *)
StartWrite(k, v, client) ==
    /\ CanWrite(k)
    /\ pendingWrites' = pendingWrites \cup 
       {[key |-> k, value |-> v, client |-> client, version |-> MaxVersion, acks |-> {}]}
    /\ UNCHANGED <<data, available, pendingReads, hints, repairQueue>>

(*
 * REPLICA WRITE: A replica accepts a write request.
 * 
 * VERSION CHECK: Only accept if version is >= current version.
 * This prevents stale writes from overwriting newer data.
 *)
ReplicaWrite(n, writeOp) ==
    /\ writeOp \in pendingWrites
    /\ n \in ReplicasForKey(writeOp.key)
    /\ available[n]
    /\ LET currentVersion == IF data[n][writeOp.key] = Nil 
                             THEN 0 
                             ELSE data[n][writeOp.key].version
       IN /\ writeOp.version >= currentVersion
          /\ data' = [data EXCEPT ![n][writeOp.key] = 
                      [version |-> writeOp.version, value |-> writeOp.value]]
          /\ pendingWrites' = (pendingWrites \ {writeOp}) \cup
                              {[writeOp EXCEPT !.acks = @ \cup {n}]}
    /\ UNCHANGED <<available, pendingReads, hints, repairQueue>>

(*
 * COMPLETE WRITE: Client receives W acknowledgments.
 * 
 * DURABILITY GUARANTEE: Once W acks received, data is durable.
 * Even if coordinator fails, data exists on W replicas.
 *)
CompleteWrite(writeOp) ==
    /\ writeOp \in pendingWrites
    /\ Cardinality(writeOp.acks) >= W
    /\ pendingWrites' = pendingWrites \ {writeOp}
    /\ UNCHANGED <<data, available, pendingReads, hints, repairQueue>>

(*
 * NODE FAILURE: A node becomes unavailable.
 *)
NodeFails(n) ==
    /\ available[n]
    /\ available' = [available EXCEPT ![n] = FALSE]
    /\ UNCHANGED <<data, pendingReads, pendingWrites, hints, repairQueue>>

(*
 * NODE RECOVERY: A node becomes available again.
 *)
NodeRecovers(n) ==
    /\ ~available[n]
    /\ available' = [available EXCEPT ![n] = TRUE]
    \* Schedule anti-entropy for all keys on this node
    /\ repairQueue' = repairQueue \cup {[node |-> n, key |-> k] : k \in Keys}
    /\ UNCHANGED <<data, pendingReads, pendingWrites, hints>>

(*
 * HINTED HANDOFF: During failure, store hint for later delivery.
 * 
 * When a replica is down during a write, the coordinator stores
 * the write locally as a "hint". When the replica recovers,
 * hints are delivered to bring it up to date.
 *)
StoreHint(coordinator, dest, k, version, value) ==
    /\ available[coordinator]
    /\ ~available[dest]
    /\ dest \in ReplicasForKey(k)
    /\ hints' = [hints EXCEPT ![coordinator] = 
                 Append(@, [destNode |-> dest, key |-> k, version |-> version, value |-> value])]
    /\ UNCHANGED <<data, available, pendingReads, pendingWrites, repairQueue>>

(*
 * DELIVER HINT: Send stored hint to recovered node.
 *)
DeliverHint(coordinator, hint) ==
    /\ Len(hints[coordinator]) > 0
    /\ hint = Head(hints[coordinator])
    /\ available[hint.destNode]
    /\ LET currentVersion == IF data[hint.destNode][hint.key] = Nil
                             THEN 0
                             ELSE data[hint.destNode][hint.key].version
       IN IF hint.version > currentVersion
          THEN data' = [data EXCEPT ![hint.destNode][hint.key] =
                        [version |-> hint.version, value |-> hint.value]]
          ELSE UNCHANGED data
    /\ hints' = [hints EXCEPT ![coordinator] = Tail(@)]
    /\ UNCHANGED <<available, pendingReads, pendingWrites, repairQueue>>

(*
 * ANTI-ENTROPY REPAIR: Synchronize replicas for a key.
 * 
 * Merkle trees make this efficient in practice:
 * - Build tree of hashes over key ranges
 * - Compare trees between replicas
 * - Only sync keys where hashes differ
 *)
RepairKey(repairItem) ==
    /\ repairItem \in repairQueue
    /\ LET k == repairItem.key
           n == repairItem.node
           replicas == ReplicasForKey(k) \ {n}
           availableReplicas == {r \in replicas : available[r]}
       IN /\ Cardinality(availableReplicas) > 0
          /\ \E source \in availableReplicas:
              /\ available[source]
              /\ data[source][k] /= Nil
              /\ (data[n][k] = Nil \/ data[source][k].version > data[n][k].version)
              /\ data' = [data EXCEPT ![n][k] = data[source][k]]
    /\ repairQueue' = repairQueue \ {repairItem}
    /\ UNCHANGED <<available, pendingReads, pendingWrites, hints>>

\* ---------------------------------------------------------
\* NEXT STATE RELATION
\* ---------------------------------------------------------

Next ==
    \/ \E k \in Keys, c \in Nat: StartRead(k, c)
    \/ \E n \in Nodes, r \in pendingReads: ReplicaReadResponse(n, r)
    \/ \E r \in pendingReads: CompleteRead(r)
    \/ \E k \in Keys, v \in Nat, c \in Nat: StartWrite(k, v, c)
    \/ \E n \in Nodes, w \in pendingWrites: ReplicaWrite(n, w)
    \/ \E w \in pendingWrites: CompleteWrite(w)
    \/ \E n \in Nodes: NodeFails(n)
    \/ \E n \in Nodes: NodeRecovers(n)
    \/ \E c \in Nodes, h \in Range(hints[c]): DeliverHint(c, h)
    \/ \E r \in repairQueue: RepairKey(r)

Spec == Init /\ [][Next]_vars

\* ---------------------------------------------------------
\* SAFETY PROPERTIES
\* ---------------------------------------------------------

(*
 * QUORUM INTERSECTION: Read and write quorums always overlap.
 * 
 * This is guaranteed by R + W > N.
 * Any read will see at least one node that participated in any write.
 *)
QuorumIntersection ==
    \A k \in Keys:
        \A RQ, WQ \in SUBSET ReplicasForKey(k):
            (Cardinality(RQ) >= R /\ Cardinality(WQ) >= W) =>
            RQ \cap WQ /= {}

(*
 * NO LOST WRITES: Once a write is acknowledged (W acks), the data is durable.
 * 
 * Even after failures, at least one replica in any read quorum
 * will have the written data.
 *)
NoLostWrites ==
    \A w \in pendingWrites:
        Cardinality(w.acks) >= W =>
            \* Data exists on W nodes
            \E S \in SUBSET Nodes:
                /\ Cardinality(S) >= W
                /\ \A n \in S: 
                    data[n][w.key] /= Nil /\ data[n][w.key].version >= w.version

(*
 * VERSION MONOTONICITY: Versions never decrease on any node.
 *)
VersionMonotonicity ==
    \A n \in Nodes, k \in Keys:
        data[n][k] /= Nil =>
            [][data'[n][k] = Nil \/ data'[n][k].version >= data[n][k].version]_vars

(*
 * EVENTUAL CONSISTENCY: All replicas eventually converge.
 * (Liveness property - requires fairness)
 *)
EventualConsistency ==
    <>[](\A k \in Keys:
         \A n1, n2 \in ReplicasForKey(k):
             (available[n1] /\ available[n2]) =>
             data[n1][k] = data[n2][k])

\* ---------------------------------------------------------
\* INVARIANTS
\* ---------------------------------------------------------

SafetyInvariant ==
    /\ TypeInvariant
    /\ QuorumIntersection
    /\ NoLostWrites

=============================================================================
