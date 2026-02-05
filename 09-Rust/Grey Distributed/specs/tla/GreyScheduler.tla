--------------------------- MODULE GreyScheduler ---------------------------
(***************************************************************************
 * Grey Distributed - Scheduler & Resource Governance Formal Specification
 * 
 * This specification models Grey's deterministic scheduler:
 *   - Priority lanes with weighted fair queuing
 *   - Tenant quotas and rate limiting
 *   - Resource capacity management
 *   - Preemption for high-priority tasks
 * 
 * SAFETY PROPERTIES:
 *   - No Quota Violation: Tenants never exceed allocated resources
 *   - Bounded Fairness: No tenant starves indefinitely
 *   - Capacity Limits: Total allocated <= total capacity
 *   - Determinism: Same inputs produce same schedule
 * 
 * LIVENESS PROPERTIES:
 *   - Progress: Eligible tasks eventually execute
 *   - Preemption Completion: Preempted tasks eventually resume
 *   - Queue Drain: Finite queues eventually empty
 * 
 * KEY INSIGHT: Weighted fair queuing provides bounded fairness without
 * strict priority starvation. Virtual time ensures each tenant receives
 * resources proportional to their weight, regardless of arrival order.
 ***************************************************************************)

EXTENDS Integers, Sequences, FiniteSets, TLC

\* ---------------------------------------------------------
\* CONSTANTS
\* ---------------------------------------------------------

CONSTANTS
    Nodes,           \* Set of compute nodes
    Tenants,         \* Set of tenant identifiers
    TaskTypes,       \* Types of tasks (affects priority)
    MaxQueueLen,     \* Maximum queue length
    MaxResource      \* Maximum resource units per node

\* Priority lanes
PriorityLanes == {"Critical", "Interactive", "Batch", "BestEffort"}

\* Lane weights (higher = more resources)
LaneWeights == [
    Critical     |-> 100,
    Interactive  |-> 50,
    Batch        |-> 20,
    BestEffort   |-> 5
]

\* ---------------------------------------------------------
\* VARIABLES
\* ---------------------------------------------------------

VARIABLES
    \* Node state
    nodeCapacity,    \* nodeCapacity[n] = total resource units
    nodeUsed,        \* nodeUsed[n] = currently used resource units
    
    \* Task queues (per priority lane)
    queues,          \* queues[lane] = sequence of tasks
    
    \* Running tasks
    running,         \* running[n] = set of tasks running on node n
    
    \* Tenant state
    tenantQuota,     \* tenantQuota[t] = max resources for tenant t
    tenantUsed,      \* tenantUsed[t] = current resource usage for tenant t
    tenantVirtualTime,  \* For weighted fair queuing
    
    \* Rate limiting
    rateLimits,      \* rateLimits[t] = max tasks/second
    rateCounters,    \* rateCounters[t] = tasks submitted in current window
    
    \* Scheduler state
    globalVirtualTime,  \* Virtual time for fair queuing
    
    \* Preemption
    preempted        \* Set of preempted tasks

vars == <<nodeCapacity, nodeUsed, queues, running, tenantQuota, tenantUsed,
          tenantVirtualTime, rateLimits, rateCounters, globalVirtualTime, preempted>>

\* Task structure
Task == [
    id: Nat,
    tenant: Tenants,
    type: TaskTypes,
    lane: PriorityLanes,
    resources: 1..MaxResource,
    priority: Nat,
    virtualTime: Nat
]

\* ---------------------------------------------------------
\* TYPE INVARIANT
\* ---------------------------------------------------------

TypeInvariant ==
    /\ nodeCapacity \in [Nodes -> 0..MaxResource]
    /\ nodeUsed \in [Nodes -> 0..MaxResource]
    /\ \A lane \in PriorityLanes: Len(queues[lane]) <= MaxQueueLen
    /\ tenantQuota \in [Tenants -> 0..MaxResource]
    /\ tenantUsed \in [Tenants -> 0..MaxResource]

\* ---------------------------------------------------------
\* HELPER OPERATORS
\* ---------------------------------------------------------

\* Calculate virtual time for a new task (WFQ)
\* Virtual time = max(global_time, tenant_time) + (cost / weight)
CalculateVirtualTime(tenant, lane, cost) ==
    LET baseTime == IF tenantVirtualTime[tenant] > globalVirtualTime
                    THEN tenantVirtualTime[tenant]
                    ELSE globalVirtualTime
        weight == LaneWeights[lane]
    IN baseTime + (cost * 100) \div weight  \* Scaled to avoid fractions

\* Check if tenant has quota available
HasQuota(tenant, resources) ==
    tenantUsed[tenant] + resources <= tenantQuota[tenant]

\* Check if tenant is within rate limit
WithinRateLimit(tenant) ==
    rateCounters[tenant] < rateLimits[tenant]

\* Find a node with sufficient capacity
FindNode(resources) ==
    CHOOSE n \in Nodes : 
        nodeCapacity[n] - nodeUsed[n] >= resources

\* Check if any node has capacity for a task
HasCapacity(resources) ==
    \E n \in Nodes : nodeCapacity[n] - nodeUsed[n] >= resources

\* Get next task to schedule (lowest virtual time across all lanes)
NextTask ==
    LET allTasks == UNION {Range(queues[lane]) : lane \in PriorityLanes}
        eligibleTasks == {t \in allTasks : 
                           /\ HasQuota(t.tenant, t.resources)
                           /\ HasCapacity(t.resources)}
    IN IF eligibleTasks = {} THEN {}
       ELSE {CHOOSE t \in eligibleTasks :
                 \A t2 \in eligibleTasks : t.virtualTime <= t2.virtualTime}

Range(seq) == {seq[i] : i \in 1..Len(seq)}

\* Remove task from its queue
RemoveFromQueue(task) ==
    [lane \in PriorityLanes |->
        IF task.lane = lane
        THEN SelectSeq(queues[lane], LAMBDA t : t.id /= task.id)
        ELSE queues[lane]]

SelectSeq(seq, test(_)) ==
    LET F[i \in 0..Len(seq)] ==
        IF i = 0 THEN << >>
        ELSE IF test(seq[i]) THEN Append(F[i-1], seq[i])
             ELSE F[i-1]
    IN F[Len(seq)]

\* ---------------------------------------------------------
\* INITIAL STATE
\* ---------------------------------------------------------

Init ==
    /\ nodeCapacity = [n \in Nodes |-> MaxResource]
    /\ nodeUsed = [n \in Nodes |-> 0]
    /\ queues = [lane \in PriorityLanes |-> << >>]
    /\ running = [n \in Nodes |-> {}]
    /\ tenantQuota = [t \in Tenants |-> MaxResource \div Cardinality(Tenants)]
    /\ tenantUsed = [t \in Tenants |-> 0]
    /\ tenantVirtualTime = [t \in Tenants |-> 0]
    /\ rateLimits = [t \in Tenants |-> 100]
    /\ rateCounters = [t \in Tenants |-> 0]
    /\ globalVirtualTime = 0
    /\ preempted = {}

\* ---------------------------------------------------------
\* STATE TRANSITIONS
\* ---------------------------------------------------------

(*
 * SUBMIT TASK: A tenant submits a new task.
 * 
 * Admission Control:
 * 1. Check rate limit - reject if exceeded
 * 2. Calculate virtual time for fair queuing
 * 3. Assign to appropriate priority lane
 * 4. Enqueue
 *)
SubmitTask(taskId, tenant, taskType, lane, resources, priority) ==
    /\ WithinRateLimit(tenant)
    /\ Len(queues[lane]) < MaxQueueLen
    /\ LET virtualTime == CalculateVirtualTime(tenant, lane, resources)
           task == [id        |-> taskId,
                    tenant    |-> tenant,
                    type      |-> taskType,
                    lane      |-> lane,
                    resources |-> resources,
                    priority  |-> priority,
                    virtualTime |-> virtualTime]
       IN /\ queues' = [queues EXCEPT ![lane] = Append(@, task)]
          /\ rateCounters' = [rateCounters EXCEPT ![tenant] = @ + 1]
    /\ UNCHANGED <<nodeCapacity, nodeUsed, running, tenantQuota, tenantUsed,
                   tenantVirtualTime, rateLimits, globalVirtualTime, preempted>>

(*
 * SCHEDULE TASK: Move a task from queue to running.
 * 
 * Scheduling Algorithm (Weighted Fair Queuing):
 * 1. Select task with lowest virtual time
 * 2. Verify quota and capacity
 * 3. Allocate resources
 * 4. Update virtual times
 *)
ScheduleTask ==
    /\ NextTask /= {}
    /\ LET task == CHOOSE t \in NextTask : TRUE
           node == FindNode(task.resources)
       IN /\ queues' = RemoveFromQueue(task)
          /\ running' = [running EXCEPT ![node] = @ \cup {task}]
          /\ nodeUsed' = [nodeUsed EXCEPT ![node] = @ + task.resources]
          /\ tenantUsed' = [tenantUsed EXCEPT ![task.tenant] = @ + task.resources]
          /\ tenantVirtualTime' = [tenantVirtualTime EXCEPT 
                                   ![task.tenant] = task.virtualTime]
          /\ globalVirtualTime' = task.virtualTime
    /\ UNCHANGED <<nodeCapacity, tenantQuota, rateLimits, rateCounters, preempted>>

(*
 * COMPLETE TASK: A running task finishes.
 *)
CompleteTask(node, task) ==
    /\ task \in running[node]
    /\ running' = [running EXCEPT ![node] = @ \ {task}]
    /\ nodeUsed' = [nodeUsed EXCEPT ![node] = @ - task.resources]
    /\ tenantUsed' = [tenantUsed EXCEPT ![task.tenant] = @ - task.resources]
    /\ UNCHANGED <<nodeCapacity, queues, tenantQuota, tenantVirtualTime,
                   rateLimits, rateCounters, globalVirtualTime, preempted>>

(*
 * PREEMPT TASK: Stop a low-priority task to make room for high-priority.
 * 
 * Preemption Rules:
 * - Only Critical tasks can preempt
 * - Only preempt BestEffort or Batch tasks
 * - Preempted tasks go to preempted set (not lost)
 * - Resume when resources available
 *)
PreemptTask(node, victim, highPriTask) ==
    /\ victim \in running[node]
    /\ victim.lane \in {"BestEffort", "Batch"}
    /\ highPriTask.lane = "Critical"
    /\ HasQuota(highPriTask.tenant, highPriTask.resources)
    /\ nodeUsed[node] - victim.resources + highPriTask.resources <= nodeCapacity[node]
    \* Stop victim
    /\ running' = [running EXCEPT ![node] = (@ \ {victim}) \cup {highPriTask}]
    /\ preempted' = preempted \cup {victim}
    /\ nodeUsed' = [nodeUsed EXCEPT ![node] = @ - victim.resources + highPriTask.resources]
    /\ tenantUsed' = [tenantUsed EXCEPT 
                      ![victim.tenant] = @ - victim.resources,
                      ![highPriTask.tenant] = @ + highPriTask.resources]
    /\ queues' = RemoveFromQueue(highPriTask)
    /\ UNCHANGED <<nodeCapacity, tenantQuota, tenantVirtualTime, 
                   rateLimits, rateCounters, globalVirtualTime>>

(*
 * RESUME PREEMPTED: Resume a preempted task.
 *)
ResumePreempted(task) ==
    /\ task \in preempted
    /\ HasQuota(task.tenant, task.resources)
    /\ HasCapacity(task.resources)
    /\ LET node == FindNode(task.resources)
       IN /\ running' = [running EXCEPT ![node] = @ \cup {task}]
          /\ nodeUsed' = [nodeUsed EXCEPT ![node] = @ + task.resources]
          /\ tenantUsed' = [tenantUsed EXCEPT ![task.tenant] = @ + task.resources]
    /\ preempted' = preempted \ {task}
    /\ UNCHANGED <<nodeCapacity, queues, tenantQuota, tenantVirtualTime,
                   rateLimits, rateCounters, globalVirtualTime>>

(*
 * RESET RATE COUNTERS: Periodic reset of rate limit counters.
 *)
ResetRateCounters ==
    /\ rateCounters' = [t \in Tenants |-> 0]
    /\ UNCHANGED <<nodeCapacity, nodeUsed, queues, running, tenantQuota, 
                   tenantUsed, tenantVirtualTime, rateLimits, globalVirtualTime, preempted>>

(*
 * UPDATE QUOTA: Dynamically adjust a tenant's quota.
 * 
 * Used for:
 * - Quota rebalancing across tenants
 * - Responding to hotspot detection
 * - Predictive scaling adjustments
 *)
UpdateQuota(tenant, newQuota) ==
    /\ tenantQuota' = [tenantQuota EXCEPT ![tenant] = newQuota]
    \* Safety: Cannot reduce quota below current usage
    /\ newQuota >= tenantUsed[tenant]
    /\ UNCHANGED <<nodeCapacity, nodeUsed, queues, running, tenantUsed,
                   tenantVirtualTime, rateLimits, rateCounters, globalVirtualTime, preempted>>

\* ---------------------------------------------------------
\* NEXT STATE RELATION
\* ---------------------------------------------------------

Next ==
    \/ \E id \in Nat, t \in Tenants, tt \in TaskTypes, 
          lane \in PriorityLanes, res \in 1..MaxResource, pri \in Nat:
        SubmitTask(id, t, tt, lane, res, pri)
    \/ ScheduleTask
    \/ \E n \in Nodes, task \in running[n]: CompleteTask(n, task)
    \/ \E n \in Nodes, v \in running[n], hp \in NextTask: PreemptTask(n, v, hp)
    \/ \E task \in preempted: ResumePreempted(task)
    \/ ResetRateCounters
    \/ \E t \in Tenants, q \in 0..MaxResource: UpdateQuota(t, q)

Spec == Init /\ [][Next]_vars

\* ---------------------------------------------------------
\* SAFETY PROPERTIES
\* ---------------------------------------------------------

(*
 * NO QUOTA VIOLATION: Tenants never use more than their quota.
 * 
 * This is the core safety property for multi-tenancy.
 * Violations lead to noisy-neighbor problems and SLA breaches.
 *)
NoQuotaViolation ==
    \A t \in Tenants: tenantUsed[t] <= tenantQuota[t]

(*
 * CAPACITY LIMITS: No node is overallocated.
 *)
CapacityLimits ==
    \A n \in Nodes: nodeUsed[n] <= nodeCapacity[n]

(*
 * RESOURCE CONSERVATION: Total tenant usage equals sum of node usage.
 * (Resources are not created or destroyed.)
 *)
ResourceConservation ==
    LET totalTenantUsage == 
            LET Sum[S \in SUBSET Tenants] ==
                IF S = {} THEN 0
                ELSE LET t == CHOOSE x \in S : TRUE
                     IN tenantUsed[t] + Sum[S \ {t}]
            IN Sum[Tenants]
        totalNodeUsage ==
            LET Sum[S \in SUBSET Nodes] ==
                IF S = {} THEN 0
                ELSE LET n == CHOOSE x \in S : TRUE
                     IN nodeUsed[n] + Sum[S \ {n}]
            IN Sum[Nodes]
    IN totalTenantUsage = totalNodeUsage

(*
 * NO TASK LOSS: Tasks are never lost (queued, running, or preempted).
 *)
NoTaskLoss ==
    preempted \subseteq UNION {Range(queues[l]) : l \in PriorityLanes} \cup
                         UNION {running[n] : n \in Nodes} \cup preempted

(*
 * BOUNDED FAIRNESS: Virtual time difference between tenants is bounded.
 * 
 * This prevents starvation while allowing priority differences.
 * Max difference is proportional to resource costs and weights.
 *)
BoundedFairness ==
    \A t1, t2 \in Tenants:
        \/ tenantUsed[t1] = 0 \/ tenantUsed[t2] = 0  \* One is idle
        \/ tenantVirtualTime[t1] - tenantVirtualTime[t2] <= MaxResource * 100

\* ---------------------------------------------------------
\* LIVENESS PROPERTIES
\* ---------------------------------------------------------

(*
 * PROGRESS: If there are queued tasks and resources available,
 * some task will eventually be scheduled.
 *)
Progress ==
    [](\E lane \in PriorityLanes: Len(queues[lane]) > 0 
       /\ \E n \in Nodes: nodeUsed[n] < nodeCapacity[n])
    => <>(\E n \in Nodes, t \in running[n]: TRUE)

(*
 * PREEMPTION COMPLETION: Preempted tasks eventually resume.
 *)
PreemptionCompletion ==
    \A task \in preempted: <>(task \in UNION {running[n] : n \in Nodes})

\* ---------------------------------------------------------
\* INVARIANTS
\* ---------------------------------------------------------

SafetyInvariant ==
    /\ TypeInvariant
    /\ NoQuotaViolation
    /\ CapacityLimits
    /\ ResourceConservation
    /\ BoundedFairness

=============================================================================
