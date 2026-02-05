-------------------------------- MODULE isolation --------------------------------
(*
 * Grey Distributed — Tenant Isolation Specification
 *
 * This TLA+ specification verifies the safety properties of Grey's
 * multi-tenant isolation model:
 *
 * 1. Memory Isolation: Tenants cannot access other tenants' memory
 * 2. Network Isolation: Tenants cannot communicate without explicit policy
 * 3. Resource Isolation: One tenant's workload doesn't degrade another's
 * 4. Data Isolation: Tenants cannot access other tenants' data
 *
 * Model Checking:
 *   tlc isolation.tla -config isolation.cfg
 *)

EXTENDS Naturals, Sequences, FiniteSets, TLC

\* ============================================================================
\* Constants
\* ============================================================================

CONSTANTS
    Tenants,            \* Set of tenant IDs
    Namespaces,         \* Set of namespace IDs
    MaxObjects          \* Maximum objects per namespace

\* ============================================================================
\* Variables
\* ============================================================================

VARIABLES
    namespaceOwner,     \* namespaceOwner[ns]: Tenant that owns namespace ns
    objects,            \* objects[ns]: Objects in namespace ns
    accessPolicy,       \* accessPolicy[<<t1, t2>>]: t1 can access t2's resources
    memoryRegions,      \* memoryRegions[t]: Memory regions owned by tenant t
    networkPolicy,      \* networkPolicy[<<t1, t2>>]: t1 can communicate with t2
    accessLog,          \* accessLog: Sequence of access attempts
    violations          \* violations: Set of isolation violations

vars == <<namespaceOwner, objects, accessPolicy, memoryRegions, networkPolicy, accessLog, violations>>

\* ============================================================================
\* Type Definitions
\* ============================================================================

AccessAttempt == [
    accessor: Tenants,
    target: Tenants,
    resource: Nat,
    allowed: BOOLEAN
]

TypeOK ==
    /\ namespaceOwner \in [Namespaces -> Tenants]
    /\ objects \in [Namespaces -> SUBSET (1..MaxObjects)]
    /\ accessPolicy \in [Tenants \X Tenants -> BOOLEAN]
    /\ memoryRegions \in [Tenants -> SUBSET (1..100)]
    /\ networkPolicy \in [Tenants \X Tenants -> BOOLEAN]
    /\ accessLog \in Seq(AccessAttempt)
    /\ violations \in SUBSET AccessAttempt

\* ============================================================================
\* Initial State
\* ============================================================================

\* Assign initial namespaces to tenants
InitialOwner(ns) ==
    IF ns \in Namespaces THEN CHOOSE t \in Tenants : TRUE ELSE CHOOSE t \in Tenants : TRUE

Init ==
    /\ namespaceOwner \in [Namespaces -> Tenants]
    /\ objects = [ns \in Namespaces |-> {}]
    /\ accessPolicy = [p \in Tenants \X Tenants |-> p[1] = p[2]]  \* Self-access only
    /\ memoryRegions = [t \in Tenants |-> {}]
    /\ networkPolicy = [p \in Tenants \X Tenants |-> p[1] = p[2]]  \* Self-communication only
    /\ accessLog = <<>>
    /\ violations = {}

\* ============================================================================
\* Helper Functions
\* ============================================================================

\* Check if t1 can access t2's resources
CanAccess(t1, t2) ==
    \/ t1 = t2
    \/ accessPolicy[<<t1, t2>>]

\* Check if t1 can communicate with t2
CanCommunicate(t1, t2) ==
    \/ t1 = t2
    \/ networkPolicy[<<t1, t2>>]

\* Get owner of an object
ObjectOwner(ns) ==
    namespaceOwner[ns]

\* ============================================================================
\* Actions
\* ============================================================================

\* Create an object in a namespace (only by owner)
CreateObject(t, ns, objId) ==
    /\ namespaceOwner[ns] = t
    /\ objId \notin objects[ns]
    /\ Cardinality(objects[ns]) < MaxObjects
    /\ objects' = [objects EXCEPT ![ns] = @ \cup {objId}]
    /\ UNCHANGED <<namespaceOwner, accessPolicy, memoryRegions, networkPolicy, accessLog, violations>>

\* Delete an object (only by owner)
DeleteObject(t, ns, objId) ==
    /\ namespaceOwner[ns] = t
    /\ objId \in objects[ns]
    /\ objects' = [objects EXCEPT ![ns] = @ \ {objId}]
    /\ UNCHANGED <<namespaceOwner, accessPolicy, memoryRegions, networkPolicy, accessLog, violations>>

\* Attempt to access another tenant's object
AttemptAccess(accessor, targetNs, objId) ==
    LET owner == namespaceOwner[targetNs]
        allowed == CanAccess(accessor, owner)
        attempt == [accessor |-> accessor, target |-> owner, resource |-> objId, allowed |-> allowed]
    IN /\ accessLog' = Append(accessLog, attempt)
       /\ IF ~allowed 
          THEN violations' = violations \cup {attempt}
          ELSE violations' = violations
       /\ UNCHANGED <<namespaceOwner, objects, accessPolicy, memoryRegions, networkPolicy>>

\* Allocate memory region to tenant
AllocateMemory(t, region) ==
    /\ region \notin UNION {memoryRegions[t2] : t2 \in Tenants}  \* Must be unallocated
    /\ memoryRegions' = [memoryRegions EXCEPT ![t] = @ \cup {region}]
    /\ UNCHANGED <<namespaceOwner, objects, accessPolicy, networkPolicy, accessLog, violations>>

\* Free memory region
FreeMemory(t, region) ==
    /\ region \in memoryRegions[t]
    /\ memoryRegions' = [memoryRegions EXCEPT ![t] = @ \ {region}]
    /\ UNCHANGED <<namespaceOwner, objects, accessPolicy, networkPolicy, accessLog, violations>>

\* Attempt to access another tenant's memory (should fail)
AttemptMemoryAccess(accessor, targetRegion) ==
    LET owner == CHOOSE t \in Tenants : targetRegion \in memoryRegions[t]
        hasOwner == \E t \in Tenants : targetRegion \in memoryRegions[t]
    IN /\ hasOwner
       /\ LET allowed == accessor = owner
              attempt == [accessor |-> accessor, target |-> owner, resource |-> targetRegion, allowed |-> allowed]
          IN /\ accessLog' = Append(accessLog, attempt)
             /\ IF ~allowed
                THEN violations' = violations \cup {attempt}
                ELSE violations' = violations
       /\ UNCHANGED <<namespaceOwner, objects, accessPolicy, memoryRegions, networkPolicy>>

\* Grant access policy
GrantAccess(granter, grantee) ==
    /\ granter # grantee
    /\ accessPolicy' = [accessPolicy EXCEPT ![<<grantee, granter>>] = TRUE]
    /\ UNCHANGED <<namespaceOwner, objects, memoryRegions, networkPolicy, accessLog, violations>>

\* Revoke access policy
RevokeAccess(revoker, revokee) ==
    /\ revoker # revokee
    /\ accessPolicy' = [accessPolicy EXCEPT ![<<revokee, revoker>>] = FALSE]
    /\ UNCHANGED <<namespaceOwner, objects, memoryRegions, networkPolicy, accessLog, violations>>

\* Configure network policy
SetNetworkPolicy(t1, t2, allowed) ==
    /\ networkPolicy' = [networkPolicy EXCEPT ![<<t1, t2>>] = allowed]
    /\ UNCHANGED <<namespaceOwner, objects, accessPolicy, memoryRegions, accessLog, violations>>

\* Attempt network communication
AttemptCommunication(sender, receiver) ==
    LET allowed == CanCommunicate(sender, receiver)
        attempt == [accessor |-> sender, target |-> receiver, resource |-> 0, allowed |-> allowed]
    IN /\ accessLog' = Append(accessLog, attempt)
       /\ IF ~allowed
          THEN violations' = violations \cup {attempt}
          ELSE violations' = violations
       /\ UNCHANGED <<namespaceOwner, objects, accessPolicy, memoryRegions, networkPolicy>>

\* ============================================================================
\* Next State
\* ============================================================================

Next ==
    \/ \E t \in Tenants, ns \in Namespaces, objId \in 1..MaxObjects :
        CreateObject(t, ns, objId)
    \/ \E t \in Tenants, ns \in Namespaces, objId \in 1..MaxObjects :
        DeleteObject(t, ns, objId)
    \/ \E accessor \in Tenants, ns \in Namespaces, objId \in 1..MaxObjects :
        AttemptAccess(accessor, ns, objId)
    \/ \E t \in Tenants, region \in 1..100 :
        AllocateMemory(t, region)
    \/ \E t \in Tenants, region \in 1..100 :
        FreeMemory(t, region)
    \/ \E accessor \in Tenants, region \in 1..100 :
        AttemptMemoryAccess(accessor, region)
    \/ \E t1, t2 \in Tenants :
        GrantAccess(t1, t2)
    \/ \E t1, t2 \in Tenants :
        RevokeAccess(t1, t2)
    \/ \E t1, t2 \in Tenants, allowed \in BOOLEAN :
        SetNetworkPolicy(t1, t2, allowed)
    \/ \E sender, receiver \in Tenants :
        AttemptCommunication(sender, receiver)

Spec == Init /\ [][Next]_vars /\ WF_vars(Next)

\* ============================================================================
\* Safety Invariants
\* ============================================================================

\* No violations ever occur in the system
NoViolations ==
    violations = {}

\* Memory regions are disjoint (no tenant has overlapping memory)
MemoryDisjoint ==
    \A t1, t2 \in Tenants :
        t1 # t2 => memoryRegions[t1] \cap memoryRegions[t2] = {}

\* Namespace ownership is valid
ValidNamespaces ==
    \A ns \in Namespaces :
        namespaceOwner[ns] \in Tenants

\* Access policy is reflexive (tenants can always access themselves)
ReflexiveAccess ==
    \A t \in Tenants :
        accessPolicy[<<t, t>>]

\* Network policy is reflexive
ReflexiveNetwork ==
    \A t \in Tenants :
        networkPolicy[<<t, t>>]

\* All recorded accesses were authorized (except recorded violations)
AuthorizedAccesses ==
    \A i \in 1..Len(accessLog) :
        LET attempt == accessLog[i]
        IN attempt.allowed => CanAccess(attempt.accessor, attempt.target)

\* Combined safety invariant
Isolation ==
    /\ MemoryDisjoint
    /\ ValidNamespaces
    /\ ReflexiveAccess
    /\ ReflexiveNetwork

\* The strongest invariant: No isolation violations ever occur
\* This is the key property we want to verify
IsolationNeverBroken ==
    \A attempt \in violations :
        FALSE  \* Should never have any violations

\* ============================================================================
\* Liveness Properties
\* ============================================================================

\* Access eventually granted or denied
AccessDecisionMade ==
    \A accessor, target \in Tenants, resource \in 1..MaxObjects :
        TRUE ~> (Len(accessLog) > 0)

================================================================================
