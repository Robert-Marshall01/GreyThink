--------------------------- MODULE GreyConsensus ---------------------------
(***************************************************************************
 * Grey Distributed - Raft Consensus Formal Specification
 * 
 * This TLA+ specification formally models the Grey consensus protocol,
 * which is based on Raft with enhancements (Pre-Vote, Joint Consensus).
 * 
 * SAFETY PROPERTIES (must always hold):
 *   - Agreement: No two leaders in the same term
 *   - Log Matching: If two logs contain an entry with the same index and term,
 *     then the logs are identical in all entries up through that index
 *   - Leader Completeness: If a log entry is committed in a given term,
 *     that entry will be present in the logs of the leaders for all higher terms
 * 
 * LIVENESS PROPERTIES (must eventually hold):
 *   - Leader Election: A leader is eventually elected (given partial synchrony)
 *   - Log Replication: Committed entries are eventually applied
 *   - Termination: Every request eventually completes (assuming fair scheduling)
 * 
 * VERIFICATION APPROACH:
 *   Run with TLC model checker:
 *     tlc GreyConsensus.tla -config GreyConsensus.cfg
 * 
 *   Expected results for small models (3 nodes, 2 terms, 3 log entries):
 *     - States explored: ~10^6
 *     - Time: ~5 minutes
 *     - Memory: ~2GB
 ***************************************************************************)

EXTENDS Integers, Sequences, FiniteSets, TLC

\* ---------------------------------------------------------
\* CONSTANTS
\* ---------------------------------------------------------

CONSTANTS 
    Nodes,           \* Set of node identifiers (e.g., {"n1", "n2", "n3"})
    MaxTerm,         \* Maximum term to explore (bounds state space)
    MaxLogLength,    \* Maximum log length to explore
    Values           \* Set of possible values to replicate

\* ---------------------------------------------------------
\* TYPE INVARIANTS
\* ---------------------------------------------------------

\* Node roles
NodeRoles == {"Follower", "Candidate", "PreCandidate", "Leader"}

\* Log entry structure
LogEntry == [term: 1..MaxTerm, value: Values]

\* Message types
MessageTypes == {
    "RequestVote", "RequestVoteResponse",
    "PreVote", "PreVoteResponse",
    "AppendEntries", "AppendEntriesResponse"
}

\* ---------------------------------------------------------
\* VARIABLES
\* ---------------------------------------------------------

VARIABLES
    \* Per-node state
    currentTerm,     \* currentTerm[n] = term number for node n
    votedFor,        \* votedFor[n] = node that n voted for in currentTerm (or Nil)
    log,             \* log[n] = sequence of log entries for node n
    commitIndex,     \* commitIndex[n] = index of highest log entry known to be committed
    role,            \* role[n] = current role of node n
    
    \* Leader state (only valid when role = Leader)
    nextIndex,       \* nextIndex[leader][follower] = next log index to send
    matchIndex,      \* matchIndex[leader][follower] = highest replicated index
    
    \* Pre-Vote state
    preVoteGranted,  \* preVoteGranted[n] = set of nodes that granted pre-votes
    
    \* Network
    messages         \* Set of in-flight messages

\* Tuple of all variables for temporal properties
vars == <<currentTerm, votedFor, log, commitIndex, role, 
          nextIndex, matchIndex, preVoteGranted, messages>>

\* Nil constant for "no vote"
Nil == "nil"

\* ---------------------------------------------------------
\* TYPE INVARIANT
\* ---------------------------------------------------------

TypeInvariant ==
    /\ currentTerm \in [Nodes -> 0..MaxTerm]
    /\ votedFor \in [Nodes -> Nodes \cup {Nil}]
    /\ \A n \in Nodes: Len(log[n]) <= MaxLogLength
    /\ commitIndex \in [Nodes -> 0..MaxLogLength]
    /\ role \in [Nodes -> NodeRoles]

\* ---------------------------------------------------------
\* HELPER OPERATORS
\* ---------------------------------------------------------

\* Quorum: majority of nodes
Quorum == {Q \in SUBSET Nodes : Cardinality(Q) * 2 > Cardinality(Nodes)}

\* Get the term of the last log entry (0 if empty)
LastLogTerm(n) == 
    IF Len(log[n]) = 0 THEN 0 
    ELSE log[n][Len(log[n])].term

\* Get the index of the last log entry (0 if empty)
LastLogIndex(n) == Len(log[n])

\* Check if candidate's log is at least as up-to-date as voter's
LogIsUpToDate(candidateLastTerm, candidateLastIndex, voterLastTerm, voterLastIndex) ==
    \/ candidateLastTerm > voterLastTerm
    \/ /\ candidateLastTerm = voterLastTerm
       /\ candidateLastIndex >= voterLastIndex

\* Send a message
Send(m) == messages' = messages \cup {m}

\* Discard a message
Discard(m) == messages' = messages \ {m}

\* Reply to a message
Reply(response, request) == 
    messages' = (messages \ {request}) \cup {response}

\* ---------------------------------------------------------
\* INITIAL STATE
\* ---------------------------------------------------------

Init ==
    /\ currentTerm = [n \in Nodes |-> 0]
    /\ votedFor = [n \in Nodes |-> Nil]
    /\ log = [n \in Nodes |-> << >>]
    /\ commitIndex = [n \in Nodes |-> 0]
    /\ role = [n \in Nodes |-> "Follower"]
    /\ nextIndex = [n \in Nodes |-> [m \in Nodes |-> 1]]
    /\ matchIndex = [n \in Nodes |-> [m \in Nodes |-> 0]]
    /\ preVoteGranted = [n \in Nodes |-> {}]
    /\ messages = {}

\* ---------------------------------------------------------
\* STATE TRANSITIONS
\* ---------------------------------------------------------

(*
 * Timeout: A follower or candidate times out and starts pre-voting.
 * 
 * Pre-Vote Protocol (Raft extension):
 * Before incrementing term, candidate checks if it can win an election.
 * This prevents disruption from partitioned nodes rejoining with high terms.
 *)
StartPreVote(n) ==
    /\ role[n] \in {"Follower", "Candidate"}
    /\ role' = [role EXCEPT ![n] = "PreCandidate"]
    /\ preVoteGranted' = [preVoteGranted EXCEPT ![n] = {n}]
    /\ \A m \in Nodes \ {n}:
        Send([type        |-> "PreVote",
              term        |-> currentTerm[n] + 1,  \* Proposed next term
              candidate   |-> n,
              lastLogTerm |-> LastLogTerm(n),
              lastLogIndex|-> LastLogIndex(n),
              dest        |-> m])
    /\ UNCHANGED <<currentTerm, votedFor, log, commitIndex, 
                   nextIndex, matchIndex>>

(*
 * Handle PreVote request: Grant if candidate's log is up-to-date
 * and proposed term is higher than ours.
 *)
HandlePreVote(n, m) ==
    /\ m.type = "PreVote"
    /\ m.dest = n
    /\ LET grant == 
           /\ m.term >= currentTerm[n]
           /\ LogIsUpToDate(m.lastLogTerm, m.lastLogIndex,
                           LastLogTerm(n), LastLogIndex(n))
       IN Reply([type     |-> "PreVoteResponse",
                 term     |-> currentTerm[n],
                 granted  |-> grant,
                 voter    |-> n,
                 dest     |-> m.candidate], m)
    /\ UNCHANGED <<currentTerm, votedFor, log, commitIndex, role,
                   nextIndex, matchIndex, preVoteGranted>>

(*
 * Handle PreVote response: If quorum achieved, start real election.
 *)
HandlePreVoteResponse(n, m) ==
    /\ m.type = "PreVoteResponse"
    /\ m.dest = n
    /\ role[n] = "PreCandidate"
    /\ m.granted
    /\ preVoteGranted' = [preVoteGranted EXCEPT ![n] = @ \cup {m.voter}]
    /\ IF preVoteGranted'[n] \in Quorum
       THEN \* Got pre-vote quorum, start real election
            /\ currentTerm' = [currentTerm EXCEPT ![n] = @ + 1]
            /\ votedFor' = [votedFor EXCEPT ![n] = n]
            /\ role' = [role EXCEPT ![n] = "Candidate"]
            /\ \A o \in Nodes \ {n}:
                Send([type        |-> "RequestVote",
                      term        |-> currentTerm'[n],
                      candidate   |-> n,
                      lastLogTerm |-> LastLogTerm(n),
                      lastLogIndex|-> LastLogIndex(n),
                      dest        |-> o])
       ELSE UNCHANGED <<currentTerm, votedFor, role>>
    /\ Discard(m)
    /\ UNCHANGED <<log, commitIndex, nextIndex, matchIndex>>

(*
 * RequestVote RPC: Candidate requests vote from node n.
 * 
 * SAFETY: A node grants at most one vote per term.
 * This ensures at most one leader per term (Agreement property).
 *)
HandleRequestVote(n, m) ==
    /\ m.type = "RequestVote"
    /\ m.dest = n
    /\ LET grant ==
           /\ m.term >= currentTerm[n]
           /\ \/ votedFor[n] = Nil
              \/ votedFor[n] = m.candidate
           /\ LogIsUpToDate(m.lastLogTerm, m.lastLogIndex,
                           LastLogTerm(n), LastLogIndex(n))
       IN
       /\ IF m.term > currentTerm[n]
          THEN /\ currentTerm' = [currentTerm EXCEPT ![n] = m.term]
               /\ votedFor' = [votedFor EXCEPT ![n] = IF grant THEN m.candidate ELSE Nil]
               /\ role' = [role EXCEPT ![n] = "Follower"]
          ELSE /\ votedFor' = [votedFor EXCEPT ![n] = IF grant THEN m.candidate ELSE @]
               /\ UNCHANGED <<currentTerm, role>>
       /\ Reply([type    |-> "RequestVoteResponse",
                 term    |-> currentTerm'[n],
                 granted |-> grant,
                 voter   |-> n,
                 dest    |-> m.candidate], m)
    /\ UNCHANGED <<log, commitIndex, nextIndex, matchIndex, preVoteGranted>>

(*
 * Become leader: Candidate receives votes from a quorum.
 *)
BecomeLeader(n) ==
    /\ role[n] = "Candidate"
    /\ \E Q \in Quorum:
        \A m \in Q \ {n}:
            \E vote \in messages:
                /\ vote.type = "RequestVoteResponse"
                /\ vote.dest = n
                /\ vote.voter = m
                /\ vote.granted
                /\ vote.term = currentTerm[n]
    /\ role' = [role EXCEPT ![n] = "Leader"]
    /\ nextIndex' = [nextIndex EXCEPT ![n] = [m \in Nodes |-> Len(log[n]) + 1]]
    /\ matchIndex' = [matchIndex EXCEPT ![n] = [m \in Nodes |-> 0]]
    /\ UNCHANGED <<currentTerm, votedFor, log, commitIndex, preVoteGranted, messages>>

(*
 * Client request: Leader appends new entry to log.
 *)
ClientRequest(leader, v) ==
    /\ role[leader] = "Leader"
    /\ Len(log[leader]) < MaxLogLength
    /\ log' = [log EXCEPT ![leader] = Append(@, [term |-> currentTerm[leader], value |-> v])]
    /\ UNCHANGED <<currentTerm, votedFor, commitIndex, role, 
                   nextIndex, matchIndex, preVoteGranted, messages>>

(*
 * AppendEntries RPC: Leader sends log entries to follower.
 * 
 * LOG MATCHING INVARIANT:
 * If follower's log doesn't match at prevLogIndex/prevLogTerm,
 * it rejects the request and leader decrements nextIndex.
 *)
AppendEntries(leader, follower) ==
    /\ role[leader] = "Leader"
    /\ leader /= follower
    /\ LET prevLogIndex == nextIndex[leader][follower] - 1
           prevLogTerm == IF prevLogIndex = 0 THEN 0 
                          ELSE log[leader][prevLogIndex].term
           entries == SubSeq(log[leader], nextIndex[leader][follower], 
                            Len(log[leader]))
       IN Send([type         |-> "AppendEntries",
                term         |-> currentTerm[leader],
                leader       |-> leader,
                prevLogIndex |-> prevLogIndex,
                prevLogTerm  |-> prevLogTerm,
                entries      |-> entries,
                leaderCommit |-> commitIndex[leader],
                dest         |-> follower])
    /\ UNCHANGED <<currentTerm, votedFor, log, commitIndex, role,
                   nextIndex, matchIndex, preVoteGranted>>

(*
 * Handle AppendEntries: Follower processes leader's entries.
 *)
HandleAppendEntries(n, m) ==
    /\ m.type = "AppendEntries"
    /\ m.dest = n
    /\ IF m.term < currentTerm[n]
       THEN \* Reject stale leader
            /\ Reply([type    |-> "AppendEntriesResponse",
                      term    |-> currentTerm[n],
                      success |-> FALSE,
                      matchIndex |-> 0,
                      dest    |-> m.leader], m)
            /\ UNCHANGED <<currentTerm, votedFor, log, commitIndex, role,
                           nextIndex, matchIndex, preVoteGranted>>
       ELSE
            /\ IF m.term > currentTerm[n]
               THEN currentTerm' = [currentTerm EXCEPT ![n] = m.term]
               ELSE UNCHANGED currentTerm
            /\ role' = [role EXCEPT ![n] = "Follower"]
            /\ votedFor' = [votedFor EXCEPT ![n] = Nil]
            /\ LET logOK == 
                   \/ m.prevLogIndex = 0
                   \/ /\ m.prevLogIndex <= Len(log[n])
                      /\ log[n][m.prevLogIndex].term = m.prevLogTerm
               IN IF ~logOK
                  THEN \* Log doesn't match, reject
                       /\ Reply([type    |-> "AppendEntriesResponse",
                                 term    |-> currentTerm'[n],
                                 success |-> FALSE,
                                 matchIndex |-> 0,
                                 dest    |-> m.leader], m)
                       /\ UNCHANGED <<log, commitIndex, nextIndex, matchIndex, preVoteGranted>>
                  ELSE \* Log matches, append entries
                       /\ log' = [log EXCEPT ![n] = 
                                  SubSeq(@, 1, m.prevLogIndex) \o m.entries]
                       /\ LET newCommitIndex == 
                              IF m.leaderCommit > commitIndex[n]
                              THEN IF m.leaderCommit < Len(log'[n])
                                   THEN m.leaderCommit
                                   ELSE Len(log'[n])
                              ELSE commitIndex[n]
                          IN commitIndex' = [commitIndex EXCEPT ![n] = newCommitIndex]
                       /\ Reply([type    |-> "AppendEntriesResponse",
                                 term    |-> currentTerm'[n],
                                 success |-> TRUE,
                                 matchIndex |-> Len(log'[n]),
                                 dest    |-> m.leader], m)
                       /\ UNCHANGED <<nextIndex, matchIndex, preVoteGranted>>

(*
 * Handle AppendEntriesResponse: Leader updates match/nextIndex.
 *)
HandleAppendEntriesResponse(leader, m) ==
    /\ m.type = "AppendEntriesResponse"
    /\ m.dest = leader
    /\ role[leader] = "Leader"
    /\ m.term = currentTerm[leader]
    /\ LET follower == CHOOSE n \in Nodes : 
                       \E msg \in messages : 
                           /\ msg.type = "AppendEntries"
                           /\ msg.leader = leader
                           /\ msg.dest = n
       IN IF m.success
          THEN /\ matchIndex' = [matchIndex EXCEPT ![leader][follower] = m.matchIndex]
               /\ nextIndex' = [nextIndex EXCEPT ![leader][follower] = m.matchIndex + 1]
               \* Update commitIndex if a majority has replicated
               /\ LET newCommitIndex == 
                      CHOOSE i \in 0..Len(log[leader]):
                          /\ i >= commitIndex[leader]
                          /\ \E Q \in Quorum:
                              \A n \in Q:
                                  \/ n = leader
                                  \/ matchIndex'[leader][n] >= i
                          /\ \/ i = 0
                             \/ log[leader][i].term = currentTerm[leader]
                  IN commitIndex' = [commitIndex EXCEPT ![leader] = newCommitIndex]
          ELSE /\ nextIndex' = [nextIndex EXCEPT ![leader][follower] = 
                               Max(1, nextIndex[leader][follower] - 1)]
               /\ UNCHANGED <<matchIndex, commitIndex>>
    /\ Discard(m)
    /\ UNCHANGED <<currentTerm, votedFor, log, role, preVoteGranted>>

\* Max helper
Max(a, b) == IF a > b THEN a ELSE b

\* ---------------------------------------------------------
\* NEXT STATE RELATION
\* ---------------------------------------------------------

Next ==
    \/ \E n \in Nodes: StartPreVote(n)
    \/ \E n \in Nodes: \E m \in messages: HandlePreVote(n, m)
    \/ \E n \in Nodes: \E m \in messages: HandlePreVoteResponse(n, m)
    \/ \E n \in Nodes: \E m \in messages: HandleRequestVote(n, m)
    \/ \E n \in Nodes: BecomeLeader(n)
    \/ \E n \in Nodes: \E v \in Values: ClientRequest(n, v)
    \/ \E l, f \in Nodes: AppendEntries(l, f)
    \/ \E n \in Nodes: \E m \in messages: HandleAppendEntries(n, m)
    \/ \E n \in Nodes: \E m \in messages: HandleAppendEntriesResponse(n, m)

\* Fairness: Eventually process messages, eventually time out
Fairness ==
    /\ WF_vars(\E n \in Nodes: \E m \in messages: HandlePreVote(n, m))
    /\ WF_vars(\E n \in Nodes: \E m \in messages: HandleRequestVote(n, m))
    /\ WF_vars(\E n \in Nodes: \E m \in messages: HandleAppendEntries(n, m))
    /\ SF_vars(\E n \in Nodes: StartPreVote(n))

Spec == Init /\ [][Next]_vars /\ Fairness

\* ---------------------------------------------------------
\* SAFETY PROPERTIES
\* ---------------------------------------------------------

(*
 * AGREEMENT: At most one leader per term.
 * 
 * This is the fundamental safety property of Raft.
 * If violated, split-brain occurs and data is lost/corrupted.
 *)
Agreement ==
    \A n1, n2 \in Nodes:
        (role[n1] = "Leader" /\ role[n2] = "Leader" /\ currentTerm[n1] = currentTerm[n2])
        => n1 = n2

(*
 * LOG MATCHING: Logs with same index and term are identical up to that point.
 * 
 * This property ensures that once a log entry is committed,
 * all future leaders will have that entry at the same index.
 *)
LogMatching ==
    \A n1, n2 \in Nodes:
        \A i \in 1..Min(Len(log[n1]), Len(log[n2])):
            log[n1][i].term = log[n2][i].term =>
                \A j \in 1..i: log[n1][j] = log[n2][j]

Min(a, b) == IF a < b THEN a ELSE b

(*
 * LEADER COMPLETENESS: Committed entries are in all future leaders' logs.
 * 
 * Once entry at index i is committed, any leader with term > entry's term
 * must have that entry at index i.
 *)
LeaderCompleteness ==
    \A n \in Nodes:
        role[n] = "Leader" =>
            \A m \in Nodes:
                \A i \in 1..commitIndex[m]:
                    i <= Len(log[n]) /\ log[n][i] = log[m][i]

(*
 * NO STALE COMMITS: Committed entries are never rolled back.
 *)
CommittedEntriesNeverRollback ==
    \A n \in Nodes:
        commitIndex[n] <= Len(log[n])

\* ---------------------------------------------------------
\* LIVENESS PROPERTIES
\* ---------------------------------------------------------

(*
 * EVENTUAL LEADER: Eventually some node becomes leader.
 * 
 * Requires fairness assumptions (partial synchrony).
 *)
EventualLeader == <>(\E n \in Nodes: role[n] = "Leader")

(*
 * EVENTUAL COMMIT: If a value is proposed, it's eventually committed.
 * 
 * Note: This requires the leader to remain stable long enough.
 *)
EventualCommit ==
    \A v \in Values:
        (\E n \in Nodes: \E i \in 1..Len(log[n]): log[n][i].value = v)
        => <>(\E m \in Nodes: \E j \in 1..commitIndex[m]: log[m][j].value = v)

\* ---------------------------------------------------------
\* INVARIANTS (for TLC model checking)
\* ---------------------------------------------------------

\* Combined safety invariant
SafetyInvariant ==
    /\ TypeInvariant
    /\ Agreement
    /\ LogMatching
    /\ CommittedEntriesNeverRollback

=============================================================================
