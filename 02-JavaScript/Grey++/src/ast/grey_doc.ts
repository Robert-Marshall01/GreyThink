// ─── Grey++ Document Primitives ──────────────────────────────────────────────
// Isolated executors for every DocNode primitive.  Each document operation
// is a pure function that receives resolved arguments and returns a
// structured stub result — no real PDF parsing, no real rendering engine,
// no external deps.
//
// Extend by swapping the stub bodies for real implementations (pdf-parse,
// Puppeteer, semantic diff engines) in a future stage.

import type { DocPrimitive } from './grey_ast.js';

// ─── Result types ───────────────────────────────────────────────────────────

export interface DocResult {
    _type: string;
    primitive: DocPrimitive;
    [key: string]: unknown;
}

// ═══════════════════════════════════════════════════════════════════════════
//  Document Parse Stub
// ═══════════════════════════════════════════════════════════════════════════

/**
 * doc.parse — parse a document file into a semantic tree (stub).
 *
 *   doc.parse file="report.pdf"
 *   doc.parse file="notes.md" format=markdown
 *
 * Returns a placeholder "PDF parsed" result.  No real parsing.
 */
export function docParse(
    args: unknown[],
    flags: Record<string, unknown>,
): DocResult {
    const file = String(flags['file'] ?? args[0] ?? 'unknown.pdf');
    const format = String(flags['format'] ?? 'pdf');
    const docId = `doc-${Date.now().toString(36)}`;

    return {
        _type: 'DocParse',
        primitive: 'parse',
        docId,
        file,
        format,
        pageCount: 3,
        nodes: 8,
        status: 'parsed',
        response: `PDF parsed — "${file}" (${format}, 3 pages, 8 semantic nodes).`,
        meta: { stub: true, note: 'No real PDF parsing — placeholder tree.' },
    };
}

// ═══════════════════════════════════════════════════════════════════════════
//  Document Render Stub
// ═══════════════════════════════════════════════════════════════════════════

/**
 * doc.render — render a parsed document to a target format (stub).
 *
 *   doc.render doc="report" format=html
 *   doc.render doc="report" format=markdown
 *
 * Returns a placeholder "PDF rendered" result.  No real rendering.
 */
export function docRender(
    args: unknown[],
    flags: Record<string, unknown>,
): DocResult {
    const doc = String(flags['doc'] ?? args[0] ?? 'unknown');
    const format = String(flags['format'] ?? 'html');
    const renderId = `render-${Date.now().toString(36)}`;

    return {
        _type: 'DocRender',
        primitive: 'render',
        renderId,
        doc,
        format,
        outputSize: '2.4 KB',
        status: 'rendered',
        response: `PDF rendered — "${doc}" → ${format} (2.4 KB).`,
        meta: { stub: true, note: 'No real rendering engine — placeholder output.' },
    };
}

// ═══════════════════════════════════════════════════════════════════════════
//  Document Edit Stub
// ═══════════════════════════════════════════════════════════════════════════

/**
 * doc.edit — apply a semantic edit to a parsed document (stub).
 *
 *   doc.edit doc="report" target=heading.1 value="New Title"
 *   doc.edit doc="report" target=paragraph.3 value="Updated text"
 *
 * Returns a placeholder "Semantic edit applied" result.  No real editing.
 */
export function docEdit(
    args: unknown[],
    flags: Record<string, unknown>,
): DocResult {
    const doc = String(flags['doc'] ?? args[0] ?? 'unknown');
    const target = String(flags['target'] ?? args[1] ?? 'node.0');
    const value = String(flags['value'] ?? args[2] ?? '<no value>');
    const editId = `edit-${Date.now().toString(36)}`;

    return {
        _type: 'DocEdit',
        primitive: 'edit',
        editId,
        doc,
        target,
        oldValue: '<previous content>',
        newValue: value,
        status: 'applied',
        response: `Semantic edit applied — "${doc}" ${target} → "${value}".`,
        meta: { stub: true, note: 'No real semantic editing — placeholder result.' },
    };
}

// ─── Central dispatcher ─────────────────────────────────────────────────────
// Maps a DocPrimitive string to the matching executor.  Unknown primitives
// fall through to a generic stub.

export function dispatchDoc(
    primitive: DocPrimitive,
    args: unknown[],
    flags: Record<string, unknown>,
): DocResult {
    switch (primitive) {
        case 'parse': return docParse(args, flags);
        case 'render': return docRender(args, flags);
        case 'edit': return docEdit(args, flags);
        default:
            return {
                _type: 'DocResult',
                primitive,
                args,
                flags,
                response: `Unknown doc primitive "${primitive}" — generic stub.`,
                meta: { stub: true },
            };
    }
}

// ─── Self-test ──────────────────────────────────────────────────────────────

if (process.argv[1]?.includes('grey_doc') && process.argv.includes('--test')) {
    console.log('═══ grey_doc.ts Self-Test ═══\n');

    console.log('── doc.parse (file) ──');
    console.log(JSON.stringify(docParse([], { file: 'report.pdf' }), null, 2));

    console.log('\n── doc.parse (file + format) ──');
    console.log(JSON.stringify(docParse([], { file: 'notes.md', format: 'markdown' }), null, 2));

    console.log('\n── doc.render (doc + format) ──');
    console.log(JSON.stringify(docRender([], { doc: 'report', format: 'html' }), null, 2));

    console.log('\n── doc.render (doc + markdown) ──');
    console.log(JSON.stringify(docRender([], { doc: 'report', format: 'markdown' }), null, 2));

    console.log('\n── doc.edit (doc + target + value) ──');
    console.log(JSON.stringify(docEdit([], { doc: 'report', target: 'heading.1', value: 'New Title' }), null, 2));

    console.log('\n── doc.edit (doc + paragraph) ──');
    console.log(JSON.stringify(docEdit([], { doc: 'report', target: 'paragraph.3', value: 'Updated text' }), null, 2));

    console.log('\n── dispatch (unknown) ──');
    console.log(JSON.stringify(dispatchDoc('merge', ['a', 'b'], { mode: 'concat' }), null, 2));

    console.log('\n✓ All grey_doc tests passed.');
}
