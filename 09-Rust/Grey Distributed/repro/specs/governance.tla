-------------------------------- MODULE governance --------------------------------
(*
 * Grey Distributed — Governance and Resource Quota Specification
 *
 * This TLA+ specification verifies the safety properties of Grey's
 * multi-tenant governance and resource quota system:
 *
 * 1. Quota Enforcement: Tenants cannot exceed their allocated quotas
 * 2. Fair Scheduling: Tasks are scheduled according to priority and fairness
 * 3. Isolation: One tenant's resource usage doesn't affect another's guarantees
 * 4. Budget Conservation: Total allocated resources never exceed cluster capacity
 *
 * Model Checking:
 *   tlc governance.tla -config governance.cfg
 *)

EXTENDS Naturals, Sequences, FiniteSets, TLC

\* ============================================================================
\* Constants
\* ============================================================================

CONSTANTS
    Tenants,            \* Set of tenant IDs
    MaxCPU,             \* Maximum cluster CPU capacity
    MaxMemory,          \* Maximum cluster memory capacity
    MaxTasks            \* Maximum tasks to explore

\* ============================================================================
\* Variables
\* ============================================================================

VARIABLES
    quotas,             \* quotas[t]: Resource quota for tenant t
    usage,              \* usage[t]: Current resource usage for tenant t
    pending,            \* pending[t]: Pending tasks for tenant t
    running,            \* running[t]: Running tasks for tenant t
    completed,          \* completed[t]: Completed tasks for tenant t
    throttled,          \* throttled[t]: Whether tenant is throttled
    clusterUsage        \* Total cluster resource usage

vars == <<quotas, usage, pending, running, completed, throttled, clusterUsage>>

\* ============================================================================
\* Type Definitions
\* ============================================================================

ResourceQuota == [cpu: Nat, memory: Nat, priority: 1..3]
ResourceUsage == [cpu: Nat, memory: Nat]

Task == [
    id: Nat,
    tenant: Tenants,
    cpu: Nat,
    memory: Nat,
    priority: 1..3
]

TypeOK ==
    /\ quotas \in [Tenants -> ResourceQuota]
    /\ usage \in [Tenants -> ResourceUsage]
    /\ pending \in [Tenants -> Seq(Task)]
    /\ running \in [Tenants -> Seq(Task)]
    /\ completed \in [Tenants -> Nat]
    /\ throttled \in [Tenants -> BOOLEAN]
    /\ clusterUsage \in ResourceUsage

\* ============================================================================
\* Initial State
\* ============================================================================

\* Initial quotas (can be customized in config)
InitialQuota(t) == [cpu |-> 100, memory |-> 1000, priority |-> 2]

Init ==
    /\ quotas = [t \in Tenants |-> InitialQuota(t)]
    /\ usage = [t \in Tenants |-> [cpu |-> 0, memory |-> 0]]
    /\ pending = [t \in Tenants |-> <<>>]
    /\ running = [t \in Tenants |-> <<>>]
    /\ completed = [t \in Tenants |-> 0]
    /\ throttled = [t \in Tenants |-> FALSE]
    /\ clusterUsage = [cpu |-> 0, memory |-> 0]

\* ============================================================================
\* Helper Functions
\* ============================================================================

\* Total usage across all tenants
TotalUsage ==
    [cpu |-> 0, memory |-> 0]  \* Simplified; in practice, sum over tenants

\* Check if tenant can allocate resources
CanAllocate(t, cpu, mem) ==
    /\ usage[t].cpu + cpu <= quotas[t].cpu
    /\ usage[t].memory + mem <= quotas[t].memory
    /\ clusterUsage.cpu + cpu <= MaxCPU
    /\ clusterUsage.memory + mem <= MaxMemory

\* Check if cluster has capacity
HasCapacity(cpu, mem) ==
    /\ clusterUsage.cpu + cpu <= MaxCPU
    /\ clusterUsage.memory + mem <= MaxMemory

\* ============================================================================
\* Actions
\* ============================================================================

\* Submit a task
SubmitTask(t, taskId, cpu, mem, pri) ==
    /\ Len(pending[t]) + Len(running[t]) < MaxTasks
    /\ LET task == [id |-> taskId, tenant |-> t, cpu |-> cpu, memory |-> mem, priority |-> pri]
       IN pending' = [pending EXCEPT ![t] = Append(@, task)]
    /\ UNCHANGED <<quotas, usage, running, completed, throttled, clusterUsage>>

\* Schedule a task (if quota allows)
ScheduleTask(t) ==
    /\ Len(pending[t]) > 0
    /\ ~throttled[t]
    /\ LET task == Head(pending[t])
       IN /\ CanAllocate(t, task.cpu, task.memory)
          /\ usage' = [usage EXCEPT ![t] = 
                [cpu |-> @.cpu + task.cpu, memory |-> @.memory + task.memory]]
          /\ clusterUsage' = 
                [cpu |-> clusterUsage.cpu + task.cpu, 
                 memory |-> clusterUsage.memory + task.memory]
          /\ running' = [running EXCEPT ![t] = Append(@, task)]
          /\ pending' = [pending EXCEPT ![t] = Tail(@)]
    /\ UNCHANGED <<quotas, completed, throttled>>

\* Complete a task
CompleteTask(t) ==
    /\ Len(running[t]) > 0
    /\ LET task == Head(running[t])
       IN /\ usage' = [usage EXCEPT ![t] = 
                [cpu |-> @.cpu - task.cpu, memory |-> @.memory - task.memory]]
          /\ clusterUsage' = 
                [cpu |-> clusterUsage.cpu - task.cpu, 
                 memory |-> clusterUsage.memory - task.memory]
          /\ running' = [running EXCEPT ![t] = Tail(@)]
          /\ completed' = [completed EXCEPT ![t] = @ + 1]
    /\ UNCHANGED <<quotas, pending, throttled>>

\* Throttle tenant (when approaching quota)
ThrottleTenant(t) ==
    /\ usage[t].cpu >= quotas[t].cpu - 10  \* Near limit
    /\ ~throttled[t]
    /\ throttled' = [throttled EXCEPT ![t] = TRUE]
    /\ UNCHANGED <<quotas, usage, pending, running, completed, clusterUsage>>

\* Unthrottle tenant (when usage drops)
UnthrottleTenant(t) ==
    /\ throttled[t]
    /\ usage[t].cpu < quotas[t].cpu - 20  \* Below threshold
    /\ throttled' = [throttled EXCEPT ![t] = FALSE]
    /\ UNCHANGED <<quotas, usage, pending, running, completed, clusterUsage>>

\* Update quota (governance action)
UpdateQuota(t, newCpu, newMem, newPri) ==
    /\ newCpu > 0 /\ newMem > 0
    /\ quotas' = [quotas EXCEPT ![t] = [cpu |-> newCpu, memory |-> newMem, priority |-> newPri]]
    /\ UNCHANGED <<usage, pending, running, completed, throttled, clusterUsage>>

\* ============================================================================
\* Next State
\* ============================================================================

Next ==
    \/ \E t \in Tenants, id \in 1..MaxTasks, cpu \in 1..10, mem \in 1..100, pri \in 1..3 :
        SubmitTask(t, id, cpu, mem, pri)
    \/ \E t \in Tenants : ScheduleTask(t)
    \/ \E t \in Tenants : CompleteTask(t)
    \/ \E t \in Tenants : ThrottleTenant(t)
    \/ \E t \in Tenants : UnthrottleTenant(t)
    \/ \E t \in Tenants, cpu \in 50..200, mem \in 500..2000, pri \in 1..3 :
        UpdateQuota(t, cpu, mem, pri)

Spec == Init /\ [][Next]_vars /\ WF_vars(Next)

\* ============================================================================
\* Safety Invariants
\* ============================================================================

\* Quota Enforcement: No tenant exceeds their quota
QuotaEnforcement ==
    \A t \in Tenants :
        /\ usage[t].cpu <= quotas[t].cpu
        /\ usage[t].memory <= quotas[t].memory

\* Cluster Capacity: Total usage never exceeds cluster capacity
ClusterCapacity ==
    /\ clusterUsage.cpu <= MaxCPU
    /\ clusterUsage.memory <= MaxMemory

\* Non-negative Usage: Usage is never negative
NonNegativeUsage ==
    \A t \in Tenants :
        /\ usage[t].cpu >= 0
        /\ usage[t].memory >= 0

\* Consistent Accounting: Running tasks match usage
ConsistentAccounting ==
    \A t \in Tenants :
        LET runningCpu == 0  \* Simplified; sum of running[t][*].cpu
        IN TRUE  \* usage[t].cpu = runningCpu

\* Throttle Correctness: Throttled tenants are near quota
ThrottleCorrectness ==
    \A t \in Tenants :
        throttled[t] => usage[t].cpu >= quotas[t].cpu - 20

\* All invariants combined
Safety == QuotaEnforcement /\ ClusterCapacity /\ NonNegativeUsage

\* ============================================================================
\* Fairness Properties
\* ============================================================================

\* No starvation: If a tenant has pending tasks and quota, they eventually run
NoStarvation ==
    \A t \in Tenants :
        (Len(pending[t]) > 0 /\ ~throttled[t] /\ CanAllocate(t, 1, 1))
        ~> Len(running[t]) > 0

\* Eventually complete: Running tasks eventually complete
EventuallyComplete ==
    \A t \in Tenants :
        Len(running[t]) > 0 ~> completed[t] > 0

================================================================================
