package com.greylegacy.service.compensation;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.List;

/**
 * Maintains a stack of compensating actions and executes them in LIFO
 * order when a multi-step workflow fails.
 *
 * <p>This is the core building block of the Saga (orchestration) pattern
 * used in Grey Legacy for operations that span multiple systems (database,
 * JMS, SOAP endpoints, external payment gateways) where distributed
 * transactions (XA/2PC) are not feasible.</p>
 *
 * <h3>Usage Pattern:</h3>
 * <pre>{@code
 * CompensationChain chain = new CompensationChain();
 * try {
 *     step1();
 *     chain.register("Release funds", () -> undoStep1());
 *
 *     step2();
 *     chain.register("Cancel payment", () -> undoStep2());
 *
 *     step3();  // If this throws...
 *
 *     chain.clear();  // All steps succeeded — discard compensations
 * } catch (Exception e) {
 *     chain.compensate();  // ...undo step2, then step1
 * }
 * }</pre>
 *
 * <h3>Design Decisions:</h3>
 * <ul>
 *   <li><b>LIFO order:</b> Compensations run in reverse registration order
 *       to unwind state changes in the correct sequence.</li>
 *   <li><b>Best-effort:</b> A failing compensation does NOT abort remaining
 *       compensations. All are attempted.</li>
 *   <li><b>Not thread-safe:</b> Each workflow creates its own chain within
 *       a single thread/transaction scope.</li>
 *   <li><b>Idempotent compensations:</b> Callers should design compensating
 *       actions to be safely re-executable.</li>
 * </ul>
 *
 * @author Grey Legacy Architecture Team
 * @see ClaimPayoutCoordinator
 */
public class CompensationChain {

    private static final Logger log = LoggerFactory.getLogger(CompensationChain.class);

    /**
     * A named compensating action. The description is used for logging
     * and audit trail purposes.
     */
    private static class CompensationStep {
        final String description;
        final Runnable action;

        CompensationStep(String description, Runnable action) {
            this.description = description;
            this.action = action;
        }
    }

    /** Stack of compensating actions — LIFO execution on compensate(). */
    private final Deque<CompensationStep> compensations = new ArrayDeque<>();

    /**
     * Registers a compensating action to be executed on failure.
     * Actions are stacked — the last registered is the first executed.
     *
     * @param description human-readable description (for logging/audit)
     * @param compensation the compensating action to execute on failure
     */
    public void register(String description, Runnable compensation) {
        if (compensation == null) {
            throw new IllegalArgumentException("Compensation action must not be null");
        }
        compensations.push(new CompensationStep(description, compensation));
        log.debug("Registered compensation step {}: {}", compensations.size(), description);
    }

    /**
     * Convenience overload that uses a generic description.
     *
     * @param compensation the compensating action
     */
    public void register(Runnable compensation) {
        register("Step " + (compensations.size() + 1), compensation);
    }

    /**
     * Executes all registered compensations in LIFO order.
     *
     * <p>Each compensation is attempted independently. If one fails, the
     * error is logged and the chain continues with the next compensation.
     * Failed compensation descriptions are collected and returned so the
     * caller can raise alerts or record audit entries.</p>
     *
     * @return list of failure descriptions (empty if all compensations succeeded)
     */
    public List<String> compensate() {
        List<String> failures = new ArrayList<>();
        int total = compensations.size();
        int index = 0;

        log.warn("Initiating compensation chain: {} steps to undo", total);

        while (!compensations.isEmpty()) {
            CompensationStep step = compensations.pop();
            index++;
            try {
                log.info("Executing compensation {}/{}: {}", index, total, step.description);
                step.action.run();
                log.info("Compensation {}/{} succeeded: {}", index, total, step.description);
            } catch (Exception e) {
                String failureMsg = String.format(
                    "Compensation %d/%d failed [%s]: %s",
                    index, total, step.description, e.getMessage()
                );
                log.error(failureMsg, e);
                failures.add(failureMsg);
                // Continue — do NOT abort remaining compensations.
                // A partial undo is better than no undo at all.
            }
        }

        if (failures.isEmpty()) {
            log.info("All {} compensations completed successfully", total);
        } else {
            log.error("{} of {} compensations FAILED — manual intervention required", 
                      failures.size(), total);
        }

        return failures;
    }

    /**
     * Returns the number of compensation steps currently registered.
     * Useful for logging which step failed in the forward workflow.
     *
     * @return count of registered (not yet executed) compensations
     */
    public int getStepsCompleted() {
        return compensations.size();
    }

    /**
     * Clears all registered compensations without executing them.
     * Called when the entire forward workflow succeeds and no compensation
     * is needed.
     */
    public void clear() {
        int cleared = compensations.size();
        compensations.clear();
        log.debug("Cleared {} compensation steps (workflow succeeded)", cleared);
    }

    /**
     * Returns true if there are compensations registered.
     *
     * @return true if compensations are pending
     */
    public boolean hasPendingCompensations() {
        return !compensations.isEmpty();
    }
}
