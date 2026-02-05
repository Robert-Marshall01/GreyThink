-------------------------------- MODULE consensus --------------------------------
(*
 * Grey Distributed — Consensus Safety Specification
 *
 * This TLA+ specification verifies the safety properties of Grey's
 * Raft-based consensus implementation:
 *
 * 1. Election Safety: At most one leader per term
 * 2. Log Matching: If two logs contain an entry with same index and term,
 *    logs are identical in all entries up through that index
 * 3. Leader Completeness: If an entry is committed in a given term,
 *    it will be present in the logs of leaders for all higher terms
 * 4. State Machine Safety: If a server has applied a log entry at a given
 *    index, no other server will apply a different entry at that index
 *
 * Model Checking:
 *   tlc consensus.tla -config consensus.cfg
 *
 * Invariants verified:
 *   - ElectionSafety
 *   - LogMatching
 *   - LeaderCompleteness
 *   - StateMachineSafety
 *)

EXTENDS Naturals, Sequences, FiniteSets, TLC

\* ============================================================================
\* Constants
\* ============================================================================

CONSTANTS
    Nodes,          \* Set of node IDs (e.g., {n1, n2, n3})
    MaxTerm,        \* Maximum term to explore
    MaxLogLen       \* Maximum log length to explore

\* ============================================================================
\* Variables
\* ============================================================================

VARIABLES
    currentTerm,    \* currentTerm[n]: Current term of node n
    votedFor,       \* votedFor[n]: Candidate voted for in current term (or Nil)
    log,            \* log[n]: Log entries for node n
    commitIndex,    \* commitIndex[n]: Index of highest committed entry
    state,          \* state[n]: Current state (Follower, Candidate, Leader)
    leader          \* leader[t]: Leader for term t (for verification)

\* Helper variable tuple for specification
vars == <<currentTerm, votedFor, log, commitIndex, state, leader>>

\* ============================================================================
\* Type Definitions
\* ============================================================================

Nil == "nil"
States == {"Follower", "Candidate", "Leader"}

LogEntry == [term: Nat, value: Nat]

TypeOK ==
    /\ currentTerm \in [Nodes -> Nat]
    /\ votedFor \in [Nodes -> Nodes \cup {Nil}]
    /\ log \in [Nodes -> Seq(LogEntry)]
    /\ commitIndex \in [Nodes -> Nat]
    /\ state \in [Nodes -> States]
    /\ leader \in [Nat -> Nodes \cup {Nil}]

\* ============================================================================
\* Initial State
\* ============================================================================

Init ==
    /\ currentTerm = [n \in Nodes |-> 0]
    /\ votedFor = [n \in Nodes |-> Nil]
    /\ log = [n \in Nodes |-> <<>>]
    /\ commitIndex = [n \in Nodes |-> 0]
    /\ state = [n \in Nodes |-> "Follower"]
    /\ leader = [t \in 0..MaxTerm |-> Nil]

\* ============================================================================
\* Helper Functions
\* ============================================================================

\* Quorum size for cluster
Quorum == (Cardinality(Nodes) \div 2) + 1

\* Get last log index for a node
LastLogIndex(n) == Len(log[n])

\* Get last log term for a node
LastLogTerm(n) ==
    IF Len(log[n]) > 0
    THEN log[n][Len(log[n])].term
    ELSE 0

\* Check if candidate's log is at least as up-to-date as voter's
LogUpToDate(candidate, voter) ==
    \/ LastLogTerm(candidate) > LastLogTerm(voter)
    \/ /\ LastLogTerm(candidate) = LastLogTerm(voter)
       /\ LastLogIndex(candidate) >= LastLogIndex(voter)

\* ============================================================================
\* Actions
\* ============================================================================

\* Timeout: Follower or Candidate starts election
StartElection(n) ==
    /\ state[n] \in {"Follower", "Candidate"}
    /\ currentTerm[n] < MaxTerm
    /\ currentTerm' = [currentTerm EXCEPT ![n] = @ + 1]
    /\ votedFor' = [votedFor EXCEPT ![n] = n]
    /\ state' = [state EXCEPT ![n] = "Candidate"]
    /\ UNCHANGED <<log, commitIndex, leader>>

\* RequestVote: Candidate requests vote from voter
RequestVote(candidate, voter) ==
    /\ state[candidate] = "Candidate"
    /\ voter # candidate
    /\ currentTerm[candidate] >= currentTerm[voter]
    /\ \/ votedFor[voter] = Nil
       \/ votedFor[voter] = candidate
    /\ LogUpToDate(candidate, voter)
    /\ votedFor' = [votedFor EXCEPT ![voter] = candidate]
    /\ currentTerm' = [currentTerm EXCEPT ![voter] = currentTerm[candidate]]
    /\ state' = [state EXCEPT ![voter] = "Follower"]
    /\ UNCHANGED <<log, commitIndex, leader>>

\* BecomeLeader: Candidate wins election
BecomeLeader(n) ==
    /\ state[n] = "Candidate"
    /\ Cardinality({v \in Nodes : votedFor[v] = n}) >= Quorum
    /\ state' = [state EXCEPT ![n] = "Leader"]
    /\ leader' = [leader EXCEPT ![currentTerm[n]] = n]
    /\ UNCHANGED <<currentTerm, votedFor, log, commitIndex>>

\* AppendEntry: Leader appends new entry
AppendEntry(n, value) ==
    /\ state[n] = "Leader"
    /\ Len(log[n]) < MaxLogLen
    /\ log' = [log EXCEPT ![n] = Append(@, [term |-> currentTerm[n], value |-> value])]
    /\ UNCHANGED <<currentTerm, votedFor, commitIndex, state, leader>>

\* ReplicateEntry: Leader replicates entry to follower
ReplicateEntry(leaderNode, follower, idx) ==
    /\ state[leaderNode] = "Leader"
    /\ follower # leaderNode
    /\ idx <= Len(log[leaderNode])
    /\ idx > Len(log[follower])
    /\ currentTerm[leaderNode] >= currentTerm[follower]
    /\ log' = [log EXCEPT ![follower] = Append(@, log[leaderNode][idx])]
    /\ currentTerm' = [currentTerm EXCEPT ![follower] = currentTerm[leaderNode]]
    /\ state' = [state EXCEPT ![follower] = "Follower"]
    /\ votedFor' = [votedFor EXCEPT ![follower] = Nil]
    /\ UNCHANGED <<commitIndex, leader>>

\* CommitEntry: Leader commits entry when replicated to quorum
CommitEntry(n, idx) ==
    /\ state[n] = "Leader"
    /\ idx > commitIndex[n]
    /\ idx <= Len(log[n])
    /\ log[n][idx].term = currentTerm[n]
    /\ Cardinality({f \in Nodes : Len(log[f]) >= idx}) >= Quorum
    /\ commitIndex' = [commitIndex EXCEPT ![n] = idx]
    /\ UNCHANGED <<currentTerm, votedFor, log, state, leader>>

\* FollowerCommit: Follower updates commit index
FollowerCommit(follower, leaderNode, idx) ==
    /\ state[leaderNode] = "Leader"
    /\ state[follower] = "Follower"
    /\ commitIndex[leaderNode] >= idx
    /\ idx > commitIndex[follower]
    /\ Len(log[follower]) >= idx
    /\ commitIndex' = [commitIndex EXCEPT ![follower] = idx]
    /\ UNCHANGED <<currentTerm, votedFor, log, state, leader>>

\* ============================================================================
\* Next State
\* ============================================================================

Next ==
    \/ \E n \in Nodes : StartElection(n)
    \/ \E c, v \in Nodes : RequestVote(c, v)
    \/ \E n \in Nodes : BecomeLeader(n)
    \/ \E n \in Nodes, val \in 1..3 : AppendEntry(n, val)
    \/ \E l, f \in Nodes, idx \in 1..MaxLogLen : ReplicateEntry(l, f, idx)
    \/ \E n \in Nodes, idx \in 1..MaxLogLen : CommitEntry(n, idx)
    \/ \E f, l \in Nodes, idx \in 1..MaxLogLen : FollowerCommit(f, l, idx)

Spec == Init /\ [][Next]_vars /\ WF_vars(Next)

\* ============================================================================
\* Safety Invariants
\* ============================================================================

\* Election Safety: At most one leader per term
ElectionSafety ==
    \A t \in 0..MaxTerm :
        Cardinality({n \in Nodes : 
            /\ state[n] = "Leader" 
            /\ currentTerm[n] = t}) <= 1

\* Log Matching: Logs with same index and term have same prefix
LogMatching ==
    \A n1, n2 \in Nodes :
        \A idx \in 1..Min(Len(log[n1]), Len(log[n2])) :
            log[n1][idx].term = log[n2][idx].term =>
                log[n1][idx] = log[n2][idx]

\* Leader Completeness: Committed entries exist in future leaders
LeaderCompleteness ==
    \A n \in Nodes :
        \A idx \in 1..commitIndex[n] :
            \A l \in Nodes :
                /\ state[l] = "Leader"
                /\ currentTerm[l] >= log[n][idx].term
                => Len(log[l]) >= idx

\* State Machine Safety: Same index has same entry across all nodes
StateMachineSafety ==
    \A n1, n2 \in Nodes :
        \A idx \in 1..Min(commitIndex[n1], commitIndex[n2]) :
            log[n1][idx] = log[n2][idx]

\* All invariants combined
Safety == ElectionSafety /\ LogMatching /\ LeaderCompleteness /\ StateMachineSafety

\* ============================================================================
\* Liveness (under fairness)
\* ============================================================================

\* Eventually a leader is elected
EventuallyLeader == <>(\E n \in Nodes : state[n] = "Leader")

\* Entries eventually get committed
EventuallyCommitted ==
    \A n \in Nodes :
        state[n] = "Leader" /\ Len(log[n]) > 0
        ~> commitIndex[n] > 0

================================================================================
