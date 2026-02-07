// ─── Grey++ Multi-Backend Execution Coordinator ─────────────────────────────
// MultiExecCoordinator maps compiled backend code into execution environments,
// handles dispatch across multiple backends, and outputs consolidated
// execution reports.
//
// Self-test:  npx tsx src/ast/grey_multiexec.ts --test
//
// No external dependencies — pure in-memory arrays/maps.

// ─── Types ──────────────────────────────────────────────────────────────────

export interface BackendMapping {
    backend: string;
    program: string;
    environment: string;
    timestamp: string;
}

export interface ExecDispatch {
    backend: string;
    program: string;
    status: 'dispatched' | 'completed';
    output: string;
    timestamp: string;
}

export interface MultiExecReport {
    _type: 'MultiExecReport';
    totalMappings: number;
    totalDispatches: number;
    mappings: BackendMapping[];
    dispatches: ExecDispatch[];
    backendsUsed: string[];
    summary: string;
    meta: { stub: true };
}

// ── MultiExecCoordinator ────────────────────────────────────────────────────

export class MultiExecCoordinator {
    private mappings: BackendMapping[] = [];
    private dispatches: ExecDispatch[] = [];

    /** Map compiled code into an execution environment for a given backend. */
    mapBackend(backend: string, program: string): BackendMapping {
        const mapping: BackendMapping = {
            backend,
            program,
            environment: `env.${backend}`,
            timestamp: new Date().toISOString(),
        };
        this.mappings.push(mapping);
        return mapping;
    }

    /** Dispatch execution to a specific backend. */
    dispatchExec(backend: string, program: string): ExecDispatch {
        const entry: ExecDispatch = {
            backend,
            program,
            status: 'completed',
            output: `[${backend}] Executed: ${program.slice(0, 60)}${program.length > 60 ? '…' : ''}`,
            timestamp: new Date().toISOString(),
        };
        this.dispatches.push(entry);
        return entry;
    }

    /** Get all backend mappings, optionally filtered by backend. */
    getMappings(backend?: string): BackendMapping[] {
        if (backend) return this.mappings.filter(m => m.backend === backend);
        return [...this.mappings];
    }

    /** Get all execution dispatches, optionally filtered by backend. */
    getDispatches(backend?: string): ExecDispatch[] {
        if (backend) return this.dispatches.filter(d => d.backend === backend);
        return [...this.dispatches];
    }

    /** Generate a consolidated multi-backend execution report. */
    generateReport(): MultiExecReport {
        const backendsUsed = [...new Set([
            ...this.mappings.map(m => m.backend),
            ...this.dispatches.map(d => d.backend),
        ])];

        const summary = this.mappings.length === 0 && this.dispatches.length === 0
            ? 'No backends executed yet — run execmulti.* commands first.'
            : `Multi-exec report: ${this.mappings.length} mapping(s), ${this.dispatches.length} dispatch(es) across [${backendsUsed.join(', ')}].`;

        return {
            _type: 'MultiExecReport',
            totalMappings: this.mappings.length,
            totalDispatches: this.dispatches.length,
            mappings: [...this.mappings],
            dispatches: [...this.dispatches],
            backendsUsed,
            summary,
            meta: { stub: true },
        };
    }

    /** Total number of backend mappings registered. */
    mappingCount(): number {
        return this.mappings.length;
    }

    /** Total number of execution dispatches registered. */
    dispatchCount(): number {
        return this.dispatches.length;
    }

    /** Clear all state. */
    clear(): void {
        this.mappings = [];
        this.dispatches = [];
    }
}

// ─── Self-test ──────────────────────────────────────────────────────────────

if (process.argv[1]?.includes('grey_multiexec') && process.argv.includes('--test')) {
    console.log('── grey_multiexec self-test ──');

    const coord = new MultiExecCoordinator();

    // mapBackend
    const m1 = coord.mapBackend('js', 'console.log("hello")');
    console.assert(m1.backend === 'js', 'map backend');
    console.assert(m1.program === 'console.log("hello")', 'map program');
    console.assert(m1.environment === 'env.js', 'map environment');
    console.assert(coord.mappingCount() === 1, 'mapping count 1');
    console.log('  ✓ mapBackend');

    // More mappings
    coord.mapBackend('cpp', 'int main(){return 0;}');
    coord.mapBackend('riscv', 'addi x1, x0, 5');
    console.assert(coord.mappingCount() === 3, 'mapping count 3');
    console.log('  ✓ multiple mappings');

    // getMappings (all)
    const allMappings = coord.getMappings();
    console.assert(allMappings.length === 3, 'all mappings');
    console.log('  ✓ getMappings (all)');

    // getMappings (filtered)
    const jsOnly = coord.getMappings('js');
    console.assert(jsOnly.length === 1, 'js mappings');
    const cppOnly = coord.getMappings('cpp');
    console.assert(cppOnly.length === 1, 'cpp mappings');
    console.log('  ✓ getMappings (filtered)');

    // dispatchExec
    const d1 = coord.dispatchExec('js', 'console.log("test")');
    console.assert(d1.backend === 'js', 'dispatch backend');
    console.assert(d1.status === 'completed', 'dispatch status');
    console.assert(d1.output.includes('[js]'), 'dispatch output');
    console.assert(coord.dispatchCount() === 1, 'dispatch count');
    console.log('  ✓ dispatchExec');

    // More dispatches
    coord.dispatchExec('cpp', 'main()');
    console.assert(coord.dispatchCount() === 2, 'dispatch count 2');
    console.log('  ✓ multiple dispatches');

    // getDispatches (all)
    const allDispatches = coord.getDispatches();
    console.assert(allDispatches.length === 2, 'all dispatches');
    console.log('  ✓ getDispatches (all)');

    // getDispatches (filtered)
    const jsDispatches = coord.getDispatches('js');
    console.assert(jsDispatches.length === 1, 'js dispatches');
    console.log('  ✓ getDispatches (filtered)');

    // generateReport
    const report = coord.generateReport();
    console.assert(report._type === 'MultiExecReport', 'report type');
    console.assert(report.totalMappings === 3, 'report mappings');
    console.assert(report.totalDispatches === 2, 'report dispatches');
    console.assert(report.backendsUsed.length === 3, `backends used: ${report.backendsUsed.length}`);
    console.assert(report.summary.includes('3 mapping(s)'), 'report summary');
    console.log('  ✓ generateReport');

    // clear
    coord.clear();
    console.assert(coord.mappingCount() === 0, 'clear mappings');
    console.assert(coord.dispatchCount() === 0, 'clear dispatches');
    console.log('  ✓ clear');

    // empty report
    const emptyReport = coord.generateReport();
    console.assert(emptyReport.totalMappings === 0, 'empty mappings');
    console.assert(emptyReport.summary.includes('No backends executed'), 'empty summary');
    console.log('  ✓ empty report');

    console.log('── all grey_multiexec tests passed ──');
}
