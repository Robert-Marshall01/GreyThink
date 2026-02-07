// ─── Grey++ Backend Runner Layer ─────────────────────────────────────────────
// BackendRunner represents execution contexts for JS, C++, and RISC-V,
// applies stub execution rules, and outputs backend execution results.
//
// Self-test:  npx tsx src/ast/grey_runner.ts --test
//
// No external dependencies — pure in-memory graph/arrays.

// ─── Types ──────────────────────────────────────────────────────────────────

export interface ExecContext {
    backend: string;
    program: string;
    status: 'ready' | 'running' | 'completed';
    createdAt: string;
}

export interface ExecRule {
    backend?: string;
    program?: string;
    action: string;
    appliedAt: string;
}

export interface ExecResultEntry {
    backend: string;
    output: string;
    exitCode: number;
    generatedAt: string;
}

export interface BackendRunnerReport {
    _type: 'BackendRunnerReport';
    totalContexts: number;
    totalRules: number;
    totalResults: number;
    contexts: ExecContext[];
    rules: ExecRule[];
    results: ExecResultEntry[];
    summary: string;
    meta: { stub: true };
}

// ── BackendRunner ───────────────────────────────────────────────────────────

export class BackendRunner {
    private contexts: ExecContext[] = [];
    private rules: ExecRule[] = [];
    private results: ExecResultEntry[] = [];

    /** Register an execution context for a given backend. */
    addContext(backend: string, program: string): ExecContext {
        const ctx: ExecContext = {
            backend,
            program,
            status: 'ready',
            createdAt: new Date().toISOString(),
        };
        this.contexts.push(ctx);
        return ctx;
    }

    /** Apply a stub execution rule to the runner. */
    applyExec(rule: { backend?: string; program?: string; action: string }): ExecRule {
        const entry: ExecRule = {
            ...rule,
            appliedAt: new Date().toISOString(),
        };
        this.rules.push(entry);
        return entry;
    }

    /** Simulate execution for a backend and produce a result entry. */
    toResult(backend: string): ExecResultEntry {
        const ctx = this.contexts.find(c => c.backend === backend && c.status === 'ready');
        if (ctx) ctx.status = 'completed';

        const result: ExecResultEntry = {
            backend,
            output: `[${backend}] Simulated execution — output returned.`,
            exitCode: 0,
            generatedAt: new Date().toISOString(),
        };
        this.results.push(result);
        return result;
    }

    /** Get all registered contexts, optionally filtered by backend. */
    getContexts(backend?: string): ExecContext[] {
        if (backend) return this.contexts.filter(c => c.backend === backend);
        return [...this.contexts];
    }

    /** Get all applied rules. */
    getRules(): ExecRule[] {
        return [...this.rules];
    }

    /** Get all execution results. */
    getResults(): ExecResultEntry[] {
        return [...this.results];
    }

    /** Generate a consolidated backend runner report. */
    generateRunnerReport(): BackendRunnerReport {
        const summary = this.contexts.length === 0 && this.rules.length === 0
            ? 'No execution contexts registered yet — run execmulti.* commands first.'
            : `Runner report: ${this.contexts.length} context(s), ${this.rules.length} rule(s), ${this.results.length} result(s) across [${[...new Set(this.contexts.map(c => c.backend))].join(', ')}].`;

        return {
            _type: 'BackendRunnerReport',
            totalContexts: this.contexts.length,
            totalRules: this.rules.length,
            totalResults: this.results.length,
            contexts: [...this.contexts],
            rules: [...this.rules],
            results: [...this.results],
            summary,
            meta: { stub: true },
        };
    }

    /** Clear all state. */
    clear(): void {
        this.contexts = [];
        this.rules = [];
        this.results = [];
    }
}

// ─── Self-test ──────────────────────────────────────────────────────────────

if (process.argv[1]?.includes('grey_runner') && process.argv.includes('--test')) {
    console.log('── grey_runner self-test ──');

    const runner = new BackendRunner();

    // addContext
    const c1 = runner.addContext('js', 'console.log("hello")');
    console.assert(c1.backend === 'js', 'context backend');
    console.assert(c1.program === 'console.log("hello")', 'context program');
    console.assert(c1.status === 'ready', 'context status');
    console.assert(c1.createdAt !== undefined, 'context timestamp');
    console.log('  ✓ addContext');

    // More contexts
    runner.addContext('cpp', 'int main(){return 0;}');
    runner.addContext('riscv', 'addi x1, x0, 5');
    const allContexts = runner.getContexts();
    console.assert(allContexts.length === 3, 'all contexts');
    console.log('  ✓ multiple contexts');

    // getContexts (filtered)
    const jsOnly = runner.getContexts('js');
    console.assert(jsOnly.length === 1, 'js contexts');
    const cppOnly = runner.getContexts('cpp');
    console.assert(cppOnly.length === 1, 'cpp contexts');
    console.log('  ✓ getContexts (filtered)');

    // applyExec
    const r1 = runner.applyExec({ backend: 'js', program: 'test()', action: 'simulate-run' });
    console.assert(r1.backend === 'js', 'rule backend');
    console.assert(r1.action === 'simulate-run', 'rule action');
    console.assert(r1.appliedAt !== undefined, 'rule timestamp');
    console.log('  ✓ applyExec');

    // More rules
    runner.applyExec({ action: 'compile-and-run', backend: 'cpp' });
    const allRules = runner.getRules();
    console.assert(allRules.length === 2, 'all rules');
    console.log('  ✓ multiple rules');

    // toResult
    const res = runner.toResult('js');
    console.assert(res.backend === 'js', 'result backend');
    console.assert(res.exitCode === 0, 'result exit code');
    console.assert(res.output.includes('[js]'), 'result output');
    console.assert(res.generatedAt !== undefined, 'result timestamp');
    console.log('  ✓ toResult');

    // Context status updated
    const updatedCtx = runner.getContexts('js')[0];
    console.assert(updatedCtx.status === 'completed', 'context status updated');
    console.log('  ✓ context status updated');

    // More results
    runner.toResult('cpp');
    runner.toResult('riscv');
    const allResults = runner.getResults();
    console.assert(allResults.length === 3, 'all results');
    console.log('  ✓ multiple results');

    // generateRunnerReport
    const report = runner.generateRunnerReport();
    console.assert(report._type === 'BackendRunnerReport', 'report type');
    console.assert(report.totalContexts === 3, 'report contexts');
    console.assert(report.totalRules === 2, 'report rules');
    console.assert(report.totalResults === 3, 'report results');
    console.assert(report.summary.includes('3 context(s)'), 'report summary contexts');
    console.assert(report.summary.includes('2 rule(s)'), 'report summary rules');
    console.log('  ✓ generateRunnerReport');

    // clear
    runner.clear();
    console.assert(runner.getContexts().length === 0, 'clear contexts');
    console.assert(runner.getRules().length === 0, 'clear rules');
    console.assert(runner.getResults().length === 0, 'clear results');
    console.log('  ✓ clear');

    // empty report
    const emptyReport = runner.generateRunnerReport();
    console.assert(emptyReport.totalContexts === 0, 'empty contexts');
    console.assert(emptyReport.summary.includes('No execution contexts'), 'empty summary');
    console.log('  ✓ empty report');

    console.log('── all grey_runner tests passed ──');
}
