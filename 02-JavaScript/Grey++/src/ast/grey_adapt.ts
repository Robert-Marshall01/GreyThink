// ─── Grey++ Adapt Coordinator ────────────────────────────────────────────────
// Collects meta-reports from self-optimization runs, applies adaptive
// refinements (stub), and outputs consolidated optimization reports.
// Pure in-memory — no external dependencies, no real AI, no real I/O.
//
// Self-test:  npx tsx src/ast/grey_adapt.ts --test
//
// Extend by plugging real adaptive refinement backends in a future stage.

// ─── Types ──────────────────────────────────────────────────────────────────

export interface AdaptEntry {
    id: string;
    domain: string;
    kind: string;
    data: unknown;
    timestamp: string;
}

export interface AdaptRefinement {
    id: string;
    entryId: string;
    action: string;
    detail: string;
    applied: boolean;
    timestamp: string;
}

export interface AdaptReport {
    _type: 'AdaptReport';
    totalEntries: number;
    totalRefinements: number;
    domains: string[];
    entries: AdaptEntry[];
    refinements: AdaptRefinement[];
    summary: string;
}

// ═══════════════════════════════════════════════════════════════════════════
//  AdaptCoordinator
// ═══════════════════════════════════════════════════════════════════════════

export class AdaptCoordinator {
    private entries: AdaptEntry[] = [];
    private refinements: AdaptRefinement[] = [];
    private entrySeq = 0;
    private refinementSeq = 0;

    // ── Collection ──────────────────────────────────────────────────────

    /**
     * Collect a meta-report from a self-optimization run.
     * Returns the created entry.
     */
    collect(domain: string, kind: string, data: unknown): AdaptEntry {
        const entry: AdaptEntry = {
            id: `adapt-entry-${++this.entrySeq}`,
            domain,
            kind,
            data,
            timestamp: new Date().toISOString(),
        };
        this.entries.push(entry);
        return entry;
    }

    /**
     * Get all entries, optionally filtered by domain.
     */
    getEntries(domain?: string): AdaptEntry[] {
        if (domain) return this.entries.filter(e => e.domain === domain);
        return [...this.entries];
    }

    /**
     * Get a single entry by id.
     */
    getEntry(id: string): AdaptEntry | null {
        return this.entries.find(e => e.id === id) ?? null;
    }

    // ── Refinement ──────────────────────────────────────────────────────

    /**
     * Apply an adaptive refinement to a collected entry.
     * In the stub, this just records the refinement intent.
     */
    applyRefinement(entryId: string, action: string, detail: string): AdaptRefinement {
        const entry = this.getEntry(entryId);
        if (!entry) {
            throw new Error(`AdaptCoordinator: no entry with id "${entryId}"`);
        }

        const refinement: AdaptRefinement = {
            id: `adapt-ref-${++this.refinementSeq}`,
            entryId,
            action,
            detail,
            applied: true,
            timestamp: new Date().toISOString(),
        };
        this.refinements.push(refinement);
        return refinement;
    }

    /**
     * Get all refinements, optionally filtered by entry id.
     */
    getRefinements(entryId?: string): AdaptRefinement[] {
        if (entryId) return this.refinements.filter(r => r.entryId === entryId);
        return [...this.refinements];
    }

    // ── Reporting ───────────────────────────────────────────────────────

    /**
     * Get a consolidated optimization report.
     */
    getReport(): AdaptReport {
        const domains = [...new Set(this.entries.map(e => e.domain))];
        return {
            _type: 'AdaptReport',
            totalEntries: this.entries.length,
            totalRefinements: this.refinements.length,
            domains,
            entries: [...this.entries],
            refinements: [...this.refinements],
            summary: `${this.entries.length} entry/entries collected across ${domains.length} domain(s), ${this.refinements.length} refinement(s) applied`,
        };
    }

    // ── History ─────────────────────────────────────────────────────────

    /**
     * Get full history (entries + refinements).
     */
    getHistory(): { entries: AdaptEntry[]; refinements: AdaptRefinement[] } {
        return {
            entries: [...this.entries],
            refinements: [...this.refinements],
        };
    }

    // ── Clear ───────────────────────────────────────────────────────────

    /**
     * Clear all collected entries and refinements.
     */
    clear(): void {
        this.entries = [];
        this.refinements = [];
        this.entrySeq = 0;
        this.refinementSeq = 0;
    }
}

// ─── Self-test ──────────────────────────────────────────────────────────────

if (process.argv[1]?.includes('grey_adapt') && process.argv.includes('--test')) {
    console.log('── grey_adapt self-test ──');

    const coord = new AdaptCoordinator();

    // Collect entries
    const e1 = coord.collect('selfopt', 'pipeline', { target: 'p1', strategy: 'latency' });
    console.assert(e1.id === 'adapt-entry-1', `entry id: ${e1.id}`);
    console.assert(e1.domain === 'selfopt', 'entry domain');
    console.assert(e1.kind === 'pipeline', 'entry kind');
    console.log('  ✓ collect entry');

    const e2 = coord.collect('selfopt', 'trust', { target: 'sec.perm', threshold: 0.9 });
    console.assert(e2.id === 'adapt-entry-2', 'second entry id');
    console.log('  ✓ collect second entry');

    const e3 = coord.collect('orch', 'cross', { modules: ['a', 'b'] });
    console.assert(e3.domain === 'orch', 'different domain');
    console.log('  ✓ collect different domain');

    // Get entries
    console.assert(coord.getEntries().length === 3, 'all entries');
    console.assert(coord.getEntries('selfopt').length === 2, 'filtered entries');
    console.assert(coord.getEntries('orch').length === 1, 'filtered orch');
    console.assert(coord.getEntry('adapt-entry-1') !== null, 'get by id');
    console.assert(coord.getEntry('nonexistent') === null, 'get missing');
    console.log('  ✓ get entries');

    // Apply refinement
    const r1 = coord.applyRefinement('adapt-entry-1', 'reorder', 'Move step 3 before step 2');
    console.assert(r1.id === 'adapt-ref-1', 'refinement id');
    console.assert(r1.entryId === 'adapt-entry-1', 'refinement entry id');
    console.assert(r1.action === 'reorder', 'refinement action');
    console.assert(r1.applied === true, 'refinement applied');
    console.log('  ✓ apply refinement');

    const r2 = coord.applyRefinement('adapt-entry-2', 'adjust-threshold', 'Lower threshold to 0.85');
    console.assert(r2.id === 'adapt-ref-2', 'second refinement id');
    console.log('  ✓ apply second refinement');

    // Refinement to missing entry throws
    try {
        coord.applyRefinement('nonexistent', 'x', 'y');
        console.assert(false, 'should throw');
    } catch (e: any) {
        console.assert(e.message.includes('no entry'), 'missing entry error');
        console.log('  ✓ refinement to missing entry throws');
    }

    // Get refinements
    console.assert(coord.getRefinements().length === 2, 'all refinements');
    console.assert(coord.getRefinements('adapt-entry-1').length === 1, 'filtered refinements');
    console.assert(coord.getRefinements('adapt-entry-2').length === 1, 'filtered refinements 2');
    console.log('  ✓ get refinements');

    // Report
    const report = coord.getReport();
    console.assert(report._type === 'AdaptReport', 'report type');
    console.assert(report.totalEntries === 3, 'report entries');
    console.assert(report.totalRefinements === 2, 'report refinements');
    console.assert(report.domains.length === 2, 'report domains');
    console.assert(report.domains.includes('selfopt'), 'report has selfopt');
    console.assert(report.domains.includes('orch'), 'report has orch');
    console.assert(report.summary.includes('3'), 'report summary entries');
    console.assert(report.summary.includes('2 domain'), 'report summary domains');
    console.log('  ✓ report');

    // History
    const history = coord.getHistory();
    console.assert(history.entries.length === 3, 'history entries');
    console.assert(history.refinements.length === 2, 'history refinements');
    console.log('  ✓ history');

    // Clear
    coord.clear();
    console.assert(coord.getEntries().length === 0, 'cleared entries');
    console.assert(coord.getRefinements().length === 0, 'cleared refinements');
    console.assert(coord.getReport().totalEntries === 0, 'cleared report');
    console.log('  ✓ clear');

    // Post-clear IDs reset
    const e4 = coord.collect('test', 'x', {});
    console.assert(e4.id === 'adapt-entry-1', 'id reset after clear');
    console.log('  ✓ id reset after clear');

    console.log('── all grey_adapt tests passed ──');
}
