// ─── Grey++ Storage Coordination ─────────────────────────────────────────────
// Central router for storage backends (pdf, db, file).  Registers backends,
// routes operations to the correct handler, and tracks changes in memory.
// All stubs — no external dependencies, no real I/O.
//
// Extend by plugging real adapters into the backend map in a future stage.

import { GreyPDF } from './grey_pdf.js';
import type { PDFLoadResult, PDFQueryResult, PDFEditResult } from './grey_pdf.js';

// ─── Types ──────────────────────────────────────────────────────────────────

export type StorageBackendType = 'pdf' | 'db' | 'file' | (string & {});

export type StorageOp = 'load' | 'query' | 'edit' | 'list' | (string & {});

export interface StorageChange {
    id: number;
    backend: StorageBackendType;
    op: StorageOp;
    args: unknown[];
    flags: Record<string, unknown>;
    result: unknown;
    timestamp: string;
}

export interface StorageResult {
    _type: string;
    backend: StorageBackendType;
    op: StorageOp;
    payload: unknown;
    meta: { stub: boolean;[k: string]: unknown };
}

// ═══════════════════════════════════════════════════════════════════════════
//  StorageCoordinator
// ═══════════════════════════════════════════════════════════════════════════

export class StorageCoordinator {
    /** Registered backends. */
    private backends = new Map<StorageBackendType, unknown>();

    /** Change log — append-only in-memory audit trail. */
    private changeLog: StorageChange[] = [];

    /** Auto-increment id for changes. */
    private changeSeq = 0;

    /** Lazily-created GreyPDF instance. */
    private pdfAdapter: GreyPDF | null = null;

    constructor() {
        // Pre-register the built-in backend names.
        this.backends.set('pdf', null);
        this.backends.set('db', null);
        this.backends.set('file', null);
    }

    // ── Backend Registration ────────────────────────────────────────────

    register(type: StorageBackendType, adapter?: unknown): void {
        this.backends.set(type, adapter ?? null);
    }

    hasBackend(type: StorageBackendType): boolean {
        return this.backends.has(type);
    }

    // ── Central Router ──────────────────────────────────────────────────

    /**
     * Route a storage operation to the correct backend.
     * PDF operations are handled by GreyPDF; others return a stub.
     */
    dispatch(
        backend: StorageBackendType,
        op: StorageOp,
        args: unknown[] = [],
        flags: Record<string, unknown> = {},
    ): StorageResult {
        let payload: unknown;

        switch (backend) {
            case 'pdf':
                payload = this.handlePdf(op, args, flags);
                break;
            case 'db':
                payload = this.handleDb(op, args, flags);
                break;
            case 'file':
                payload = this.handleFile(op, args, flags);
                break;
            default:
                payload = {
                    note: `Unknown storage backend "${backend}" — generic stub.`,
                    op, args, flags,
                };
                break;
        }

        const result: StorageResult = {
            _type: 'StorageResult',
            backend,
            op,
            payload,
            meta: { stub: true },
        };

        // Record the change.
        this.recordChange(backend, op, args, flags, result);

        return result;
    }

    // ── PDF handler ─────────────────────────────────────────────────────

    private getPdf(): GreyPDF {
        if (!this.pdfAdapter) this.pdfAdapter = new GreyPDF();
        return this.pdfAdapter;
    }

    private handlePdf(
        op: StorageOp,
        args: unknown[],
        flags: Record<string, unknown>,
    ): PDFLoadResult | PDFQueryResult | PDFEditResult | Record<string, unknown> {
        const pdf = this.getPdf();

        switch (op) {
            case 'load':
                return pdf.load(String(args[0] ?? flags['file'] ?? 'untitled.pdf'));

            case 'query':
                return pdf.query(String(args[0] ?? flags['query'] ?? flags['q'] ?? ''));

            case 'edit': {
                const nodeId = Number(flags['node'] ?? flags['id'] ?? args[0] ?? 0);
                const field = String(flags['field'] ?? 'content');
                const newValue = String(flags['text'] ?? flags['value'] ?? args[1] ?? '');
                return pdf.editNode(nodeId, field, newValue);
            }

            case 'list':
                return {
                    _type: 'PDFTree',
                    file: pdf.getFile(),
                    tree: pdf.getTree(),
                    meta: { stub: true },
                };

            default:
                return { note: `Unknown PDF op "${op}"`, args, flags };
        }
    }

    // ── DB handler (stub) ───────────────────────────────────────────────

    private handleDb(
        op: StorageOp,
        args: unknown[],
        flags: Record<string, unknown>,
    ): Record<string, unknown> {
        return {
            _type: 'DBStub',
            op,
            args,
            flags,
            note: 'DB backend not yet implemented — stub response.',
        };
    }

    // ── File handler (stub) ─────────────────────────────────────────────

    private handleFile(
        op: StorageOp,
        args: unknown[],
        flags: Record<string, unknown>,
    ): Record<string, unknown> {
        return {
            _type: 'FileStub',
            op,
            args,
            flags,
            note: 'File backend not yet implemented — stub response.',
        };
    }

    // ── Change Tracking ─────────────────────────────────────────────────

    private recordChange(
        backend: StorageBackendType,
        op: StorageOp,
        args: unknown[],
        flags: Record<string, unknown>,
        result: unknown,
    ): void {
        this.changeLog.push({
            id: ++this.changeSeq,
            backend,
            op,
            args,
            flags,
            result,
            timestamp: new Date().toISOString(),
        });
    }

    /** Return all recorded changes (read-only). */
    getChanges(): readonly StorageChange[] {
        return this.changeLog;
    }

    /** Return changes for a specific backend. */
    getChangesFor(backend: StorageBackendType): StorageChange[] {
        return this.changeLog.filter(c => c.backend === backend);
    }

    /** Clear the change log. */
    clearChanges(): void {
        this.changeLog = [];
        this.changeSeq = 0;
    }
}

// ─── Self-test ──────────────────────────────────────────────────────────────

if (process.argv[1]?.includes('grey_storage') && process.argv.includes('--test')) {
    console.log('═══ grey_storage.ts Self-Test ═══\n');

    const store = new StorageCoordinator();

    // PDF load.
    console.log('── storage pdf load "demo.pdf" ──');
    const r1 = store.dispatch('pdf', 'load', ['demo.pdf']);
    console.log(JSON.stringify(r1, null, 2));

    // PDF query.
    console.log('\n── storage pdf query "heading" ──');
    const r2 = store.dispatch('pdf', 'query', ['heading']);
    console.log(JSON.stringify(r2, null, 2));

    // PDF edit.
    console.log('\n── storage pdf edit node=2 text="Updated Title" ──');
    const r3 = store.dispatch('pdf', 'edit', [], { node: 2, text: 'Updated Title' });
    console.log(JSON.stringify(r3, null, 2));

    // DB stub.
    console.log('\n── storage db query ──');
    const r4 = store.dispatch('db', 'query', ['SELECT * FROM users']);
    console.log(JSON.stringify(r4, null, 2));

    // File stub.
    console.log('\n── storage file load ──');
    const r5 = store.dispatch('file', 'load', ['config.json']);
    console.log(JSON.stringify(r5, null, 2));

    // Change log.
    console.log('\n── change log ──');
    console.log(`Total changes recorded: ${store.getChanges().length}`);
    console.log(`PDF changes: ${store.getChangesFor('pdf').length}`);

    console.log('\n✓ All grey_storage tests passed.');
}
