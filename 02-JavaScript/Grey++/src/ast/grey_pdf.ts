// ─── Grey++ PDF Core ─────────────────────────────────────────────────────────
// Stub PDF adapter that models a PDF document as a semantic tree.  No real
// parsing — every operation returns structured placeholders so the pipeline
// is testable end-to-end.
//
// Extend by wiring in a real PDF library (pdf-parse, pdfjs-dist, etc.) in a
// future stage.  The public API surface stays identical.

// ─── Semantic Node Types ────────────────────────────────────────────────────

export type PDFNodeKind = 'heading' | 'text' | 'table' | 'image' | 'list' | 'page';

export interface PDFNode {
    id: number;
    kind: PDFNodeKind;
    content: string;
    children: PDFNode[];
    meta: Record<string, unknown>;
}

// ─── Query / Edit Result Types ──────────────────────────────────────────────

export interface PDFQueryResult {
    _type: 'PDFQueryResult';
    query: string;
    matches: PDFNode[];
    meta: { stub: boolean };
}

export interface PDFEditResult {
    _type: 'PDFEditResult';
    nodeId: number;
    field: string;
    oldValue: string;
    newValue: string;
    meta: { stub: boolean };
}

export interface PDFLoadResult {
    _type: 'PDFLoadResult';
    file: string;
    pageCount: number;
    tree: PDFNode;
    meta: { stub: boolean };
}

// ═══════════════════════════════════════════════════════════════════════════
//  GreyPDF
// ═══════════════════════════════════════════════════════════════════════════

export class GreyPDF {
    /** Current file path (stub). */
    private file: string | null = null;

    /** Semantic tree representing the loaded document. */
    private tree: PDFNode | null = null;

    /** Auto-increment id for nodes. */
    private nextId = 1;

    // ── Load ────────────────────────────────────────────────────────────

    /**
     * Load a PDF file (stub).  Builds a placeholder semantic tree with
     * sample headings, text, and a table node.
     */
    load(filePath: string): PDFLoadResult {
        this.file = filePath;
        this.nextId = 1;

        // Build a representative placeholder tree.
        this.tree = this.buildStubTree(filePath);

        return {
            _type: 'PDFLoadResult',
            file: filePath,
            pageCount: 3,
            tree: this.tree,
            meta: { stub: true },
        };
    }

    // ── Query ───────────────────────────────────────────────────────────

    /**
     * Search the semantic tree for nodes matching a query string.
     * Stub: returns nodes whose `kind` or `content` contains the query.
     */
    query(queryStr: string): PDFQueryResult {
        if (!this.tree) {
            return { _type: 'PDFQueryResult', query: queryStr, matches: [], meta: { stub: true } };
        }

        const q = queryStr.toLowerCase();
        const matches: PDFNode[] = [];
        this.walkTree(this.tree, (node) => {
            if (node.kind.includes(q) || node.content.toLowerCase().includes(q)) {
                matches.push(node);
            }
        });

        return { _type: 'PDFQueryResult', query: queryStr, matches, meta: { stub: true } };
    }

    // ── Edit ────────────────────────────────────────────────────────────

    /**
     * Edit a node's content by id.
     * Returns the old and new values for traceability.
     */
    editNode(nodeId: number, field: string, newValue: string): PDFEditResult {
        const node = this.findById(nodeId);
        const oldValue = node ? (node as any)[field] ?? '' : '<not found>';

        if (node && field in node) {
            (node as any)[field] = newValue;
        }

        return {
            _type: 'PDFEditResult',
            nodeId,
            field,
            oldValue: String(oldValue),
            newValue,
            meta: { stub: true },
        };
    }

    // ── Accessors ───────────────────────────────────────────────────────

    getTree(): PDFNode | null {
        return this.tree;
    }

    getFile(): string | null {
        return this.file;
    }

    // ── Internal helpers ────────────────────────────────────────────────

    private buildStubTree(file: string): PDFNode {
        const root: PDFNode = this.makeNode('page', `Document: ${file}`, [
            this.makeNode('heading', 'Chapter 1: Introduction'),
            this.makeNode('text', 'This is a placeholder paragraph for the loaded PDF document.'),
            this.makeNode('heading', 'Chapter 2: Data'),
            this.makeNode('table', 'Table 1: Sales Summary', [
                this.makeNode('text', 'Q1: $100k'),
                this.makeNode('text', 'Q2: $150k'),
            ]),
            this.makeNode('heading', 'Chapter 3: Conclusion'),
            this.makeNode('text', 'Summary and next steps.'),
        ]);
        return root;
    }

    private makeNode(kind: PDFNodeKind, content: string, children: PDFNode[] = []): PDFNode {
        return { id: this.nextId++, kind, content, children, meta: {} };
    }

    private walkTree(node: PDFNode, visitor: (n: PDFNode) => void): void {
        visitor(node);
        for (const child of node.children) {
            this.walkTree(child, visitor);
        }
    }

    private findById(id: number): PDFNode | null {
        if (!this.tree) return null;
        let found: PDFNode | null = null;
        this.walkTree(this.tree, (n) => { if (n.id === id) found = n; });
        return found;
    }
}

// ═══════════════════════════════════════════════════════════════════════════
//  GreyPDFCoordinator
// ═══════════════════════════════════════════════════════════════════════════
// Higher-level coordinator that manages document registration, edit tracking,
// and render pipeline orchestration.  Works alongside the lower-level GreyPDF
// class — each registered document can optionally be backed by a GreyPDF
// instance for semantic tree operations.
//
// No external deps — all state is in-memory Maps / arrays.

export interface DocRegistration {
    name: string;
    meta: Record<string, unknown>;
    registeredAt: string;
    editCount: number;
}

export interface EditRecord {
    editId: string;
    docName: string;
    action: string;
    target: string;
    value: string;
    timestamp: string;
}

export interface RenderPipelineResult {
    _type: 'RenderPipelineResult';
    docName: string;
    steps: string[];
    editCount: number;
    status: string;
    meta: { stub: boolean };
}

export class GreyPDFCoordinator {
    /** Registered documents by name. */
    private documents = new Map<string, DocRegistration>();

    /** Edit history across all documents. */
    private editHistory: EditRecord[] = [];

    // ── Register Document ───────────────────────────────────────────────

    /**
     * Register a document for coordination.  If a document with the same
     * name already exists, the registration is updated.
     */
    registerDocument(
        name: string,
        meta: Record<string, unknown> = {},
    ): DocRegistration {
        const reg: DocRegistration = {
            name,
            meta,
            registeredAt: new Date().toISOString(),
            editCount: 0,
        };
        this.documents.set(name, reg);
        return reg;
    }

    // ── Track Edit ──────────────────────────────────────────────────────

    /**
     * Record a semantic edit against a registered document.  The edit is
     * stored in a global edit history for auditing and replay.
     */
    trackEdit(
        docName: string,
        action: string,
        target: string,
        value: string,
    ): EditRecord {
        const record: EditRecord = {
            editId: `edit-${Date.now().toString(36)}-${this.editHistory.length}`,
            docName,
            action,
            target,
            value,
            timestamp: new Date().toISOString(),
        };
        this.editHistory.push(record);

        // Increment edit count on the document registration.
        const doc = this.documents.get(docName);
        if (doc) {
            doc.editCount++;
        }

        return record;
    }

    // ── Manage Render Pipeline ──────────────────────────────────────────

    /**
     * Orchestrate a render pipeline for a registered document.  The
     * pipeline validates registration, replays pending edits, and
     * produces a render-ready result (stub).
     */
    manageRenderPipeline(docName: string): RenderPipelineResult {
        const doc = this.documents.get(docName);

        if (!doc) {
            return {
                _type: 'RenderPipelineResult',
                docName,
                steps: ['validate:failed'],
                editCount: 0,
                status: 'error',
                meta: { stub: true },
            };
        }

        const docEdits = this.editHistory.filter(e => e.docName === docName);

        return {
            _type: 'RenderPipelineResult',
            docName,
            steps: [
                'validate:passed',
                `replay:${docEdits.length} edits`,
                'semantic-transform:applied',
                'render:complete',
            ],
            editCount: docEdits.length,
            status: 'rendered',
            meta: { stub: true },
        };
    }

    // ── Snapshot ─────────────────────────────────────────────────────────

    /**
     * Return a snapshot of all registered documents and the edit history.
     */
    snapshot(): {
        documents: DocRegistration[];
        editHistory: EditRecord[];
        totalEdits: number;
    } {
        return {
            documents: Array.from(this.documents.values()),
            editHistory: [...this.editHistory],
            totalEdits: this.editHistory.length,
        };
    }
}

// ─── Self-test ──────────────────────────────────────────────────────────────

if (process.argv[1]?.includes('grey_pdf') && process.argv.includes('--test')) {
    console.log('═══ grey_pdf.ts Self-Test ═══\n');

    const pdf = new GreyPDF();

    // Load.
    const loadResult = pdf.load('demo.pdf');
    console.log('── load ──');
    console.log(JSON.stringify(loadResult, null, 2));

    // Query headings.
    console.log('\n── query "heading" ──');
    const qr = pdf.query('heading');
    console.log(JSON.stringify(qr, null, 2));

    // Query text containing "placeholder".
    console.log('\n── query "placeholder" ──');
    console.log(JSON.stringify(pdf.query('placeholder'), null, 2));

    // Edit node 2 (first heading).
    console.log('\n── edit node=2 content="Updated Title" ──');
    const editResult = pdf.editNode(2, 'content', 'Updated Title');
    console.log(JSON.stringify(editResult, null, 2));

    // Verify edit applied.
    const afterEdit = pdf.query('Updated Title');
    console.log('\n── verify edit ──');
    console.log('Found updated node:', afterEdit.matches.length > 0 ? 'PASS' : 'FAIL');

    // ── GreyPDFCoordinator tests ──

    console.log('\n── GreyPDFCoordinator ──');
    const coordinator = new GreyPDFCoordinator();

    console.log('\n── registerDocument ──');
    const regResult = coordinator.registerDocument('report', { author: 'Alice', pages: 12 });
    console.log(JSON.stringify(regResult, null, 2));

    console.log('\n── registerDocument (second) ──');
    console.log(JSON.stringify(coordinator.registerDocument('invoice', { type: 'billing' }), null, 2));

    console.log('\n── trackEdit ──');
    const editTrack = coordinator.trackEdit('report', 'update', 'heading.1', 'New Title');
    console.log(JSON.stringify(editTrack, null, 2));

    console.log('\n── trackEdit (second) ──');
    console.log(JSON.stringify(coordinator.trackEdit('report', 'delete', 'paragraph.3', ''), null, 2));

    console.log('\n── manageRenderPipeline ──');
    const renderResult = coordinator.manageRenderPipeline('report');
    console.log(JSON.stringify(renderResult, null, 2));

    console.log('\n── manageRenderPipeline (unknown doc) ──');
    console.log(JSON.stringify(coordinator.manageRenderPipeline('missing'), null, 2));

    console.log('\n── snapshot ──');
    console.log(JSON.stringify(coordinator.snapshot(), null, 2));

    console.log('\n✓ All grey_pdf tests passed.');
}
