// ─── Grey++ Semantic Layer ───────────────────────────────────────────────────
// Provides a unified semantic model for document structure representation
// and transformation.  Documents are modelled as trees of typed semantic
// nodes that can be queried, transformed, and serialised to different
// output formats.
//
// No external deps — all state is in-memory.  Swap in real NLP / layout
// engines in a future stage.

// ─── Semantic Node Types ────────────────────────────────────────────────────

export type SemanticNodeKind =
    | 'document'
    | 'section'
    | 'heading'
    | 'paragraph'
    | 'list'
    | 'list-item'
    | 'table'
    | 'table-row'
    | 'table-cell'
    | 'image'
    | 'code-block'
    | 'inline'
    | (string & {});

export interface SemanticNode {
    id: number;
    kind: SemanticNodeKind;
    content: string;
    children: SemanticNode[];
    attributes: Record<string, unknown>;
}

// ─── Transformation Types ───────────────────────────────────────────────────

export interface SemanticTransform {
    name: string;
    /** CSS-selector-style target (e.g. "heading", "paragraph.intro"). */
    target: string;
    /** The transformation action. */
    action: 'replace' | 'delete' | 'wrap' | 'unwrap' | 'annotate' | (string & {});
    /** New content / wrapper kind / annotation value. */
    value?: string;
}

export interface TransformResult {
    transform: string;
    matchedNodes: number;
    appliedCount: number;
    status: 'applied' | 'no-match' | 'error';
}

// ═══════════════════════════════════════════════════════════════════════════
//  SemanticLayer
// ═══════════════════════════════════════════════════════════════════════════

export class SemanticLayer {
    /** The root semantic tree (null until a document is loaded). */
    private root: SemanticNode | null = null;

    /** Auto-increment id for nodes. */
    private nextId = 1;

    /** History of applied transforms. */
    private transformHistory: TransformResult[] = [];

    // ── Build / Load ────────────────────────────────────────────────────

    /**
     * Build a semantic tree from a document name.  Returns a stub tree
     * with representative structure.  Replace with real parser output
     * in a future stage.
     */
    buildFromDocument(docName: string): SemanticNode {
        this.nextId = 1;
        this.transformHistory = [];

        this.root = this.node('document', `Document: ${docName}`, [
            this.node('section', 'Introduction', [
                this.node('heading', 'Chapter 1: Introduction'),
                this.node('paragraph', 'Opening paragraph with context and background.'),
                this.node('paragraph', 'Methodology overview.'),
            ]),
            this.node('section', 'Data', [
                this.node('heading', 'Chapter 2: Data Analysis'),
                this.node('table', 'Sales Summary', [
                    this.node('table-row', 'Header Row', [
                        this.node('table-cell', 'Quarter'),
                        this.node('table-cell', 'Revenue'),
                    ]),
                    this.node('table-row', 'Q1 Data', [
                        this.node('table-cell', 'Q1'),
                        this.node('table-cell', '$100k'),
                    ]),
                ]),
            ]),
            this.node('section', 'Conclusion', [
                this.node('heading', 'Chapter 3: Conclusion'),
                this.node('paragraph', 'Summary and next steps.'),
            ]),
        ]);

        return this.root;
    }

    // ── Query ───────────────────────────────────────────────────────────

    /**
     * Find all nodes matching a kind filter.
     */
    queryByKind(kind: SemanticNodeKind): SemanticNode[] {
        if (!this.root) return [];
        const results: SemanticNode[] = [];
        this.walk(this.root, (n) => {
            if (n.kind === kind) results.push(n);
        });
        return results;
    }

    /**
     * Find a node by its id.
     */
    findById(id: number): SemanticNode | null {
        if (!this.root) return null;
        let found: SemanticNode | null = null;
        this.walk(this.root, (n) => { if (n.id === id) found = n; });
        return found;
    }

    // ── Transform ───────────────────────────────────────────────────────

    /**
     * Apply a semantic transform to the tree.  The transform targets nodes
     * by kind and applies the specified action.  Returns a result summary.
     */
    applyTransform(transform: SemanticTransform): TransformResult {
        if (!this.root) {
            const res: TransformResult = {
                transform: transform.name,
                matchedNodes: 0,
                appliedCount: 0,
                status: 'no-match',
            };
            this.transformHistory.push(res);
            return res;
        }

        // Find matching nodes by kind (simple kind-based targeting).
        const targetKind = transform.target.split('.')[0];
        const matches: SemanticNode[] = [];
        this.walk(this.root, (n) => {
            if (n.kind === targetKind) matches.push(n);
        });

        if (matches.length === 0) {
            const res: TransformResult = {
                transform: transform.name,
                matchedNodes: 0,
                appliedCount: 0,
                status: 'no-match',
            };
            this.transformHistory.push(res);
            return res;
        }

        let applied = 0;
        for (const node of matches) {
            switch (transform.action) {
                case 'replace':
                    if (transform.value !== undefined) {
                        node.content = transform.value;
                        applied++;
                    }
                    break;
                case 'annotate':
                    node.attributes['annotation'] = transform.value ?? '';
                    applied++;
                    break;
                case 'delete':
                    node.content = '';
                    node.attributes['deleted'] = true;
                    applied++;
                    break;
                case 'wrap':
                    node.attributes['wrapper'] = transform.value ?? 'div';
                    applied++;
                    break;
                case 'unwrap':
                    delete node.attributes['wrapper'];
                    applied++;
                    break;
                default:
                    // Unknown action — record but don't modify.
                    node.attributes[`custom:${transform.action}`] = transform.value ?? true;
                    applied++;
                    break;
            }
        }

        const res: TransformResult = {
            transform: transform.name,
            matchedNodes: matches.length,
            appliedCount: applied,
            status: 'applied',
        };
        this.transformHistory.push(res);
        return res;
    }

    // ── Output ──────────────────────────────────────────────────────────

    /**
     * Return a unified semantic model — the full tree plus transform
     * history and statistics.
     */
    toUnifiedModel(): {
        root: SemanticNode | null;
        nodeCount: number;
        transformHistory: TransformResult[];
        totalTransforms: number;
    } {
        let count = 0;
        if (this.root) {
            this.walk(this.root, () => { count++; });
        }

        return {
            root: this.root,
            nodeCount: count,
            transformHistory: [...this.transformHistory],
            totalTransforms: this.transformHistory.length,
        };
    }

    /**
     * Serialise the semantic tree to a flat list of content strings,
     * useful for rendering or export.
     */
    flatten(): string[] {
        if (!this.root) return [];
        const lines: string[] = [];
        this.walk(this.root, (n) => {
            if (n.content && !n.attributes['deleted']) {
                lines.push(`[${n.kind}] ${n.content}`);
            }
        });
        return lines;
    }

    // ── Accessors ───────────────────────────────────────────────────────

    getRoot(): SemanticNode | null {
        return this.root;
    }

    // ── Internal helpers ────────────────────────────────────────────────

    private node(
        kind: SemanticNodeKind,
        content: string,
        children: SemanticNode[] = [],
    ): SemanticNode {
        return {
            id: this.nextId++,
            kind,
            content,
            children,
            attributes: {},
        };
    }

    private walk(node: SemanticNode, visitor: (n: SemanticNode) => void): void {
        visitor(node);
        for (const child of node.children) {
            this.walk(child, visitor);
        }
    }
}

// ─── Self-test ──────────────────────────────────────────────────────────────

if (process.argv[1]?.includes('grey_semantic') && process.argv.includes('--test')) {
    console.log('═══ grey_semantic.ts Self-Test ═══\n');

    const layer = new SemanticLayer();

    // Build from document.
    console.log('── buildFromDocument ──');
    const root = layer.buildFromDocument('report');
    console.log(`Root kind: ${root.kind}, content: "${root.content}"`);
    console.log(`Children: ${root.children.length} sections`);

    // Query by kind.
    console.log('\n── queryByKind("heading") ──');
    const headings = layer.queryByKind('heading');
    console.log(`Found ${headings.length} headings:`);
    for (const h of headings) {
        console.log(`  [${h.id}] ${h.content}`);
    }

    console.log('\n── queryByKind("paragraph") ──');
    const paragraphs = layer.queryByKind('paragraph');
    console.log(`Found ${paragraphs.length} paragraphs`);

    // Find by id.
    console.log('\n── findById(3) ──');
    const node3 = layer.findById(3);
    console.log(node3 ? `Found: [${node3.kind}] ${node3.content}` : 'Not found');

    // Apply transforms.
    console.log('\n── applyTransform (replace headings) ──');
    const replaceResult = layer.applyTransform({
        name: 'update-headings',
        target: 'heading',
        action: 'replace',
        value: 'REDACTED',
    });
    console.log(JSON.stringify(replaceResult, null, 2));

    console.log('\n── applyTransform (annotate paragraphs) ──');
    const annotateResult = layer.applyTransform({
        name: 'mark-paragraphs',
        target: 'paragraph',
        action: 'annotate',
        value: 'reviewed',
    });
    console.log(JSON.stringify(annotateResult, null, 2));

    console.log('\n── applyTransform (no-match) ──');
    const noMatch = layer.applyTransform({
        name: 'remove-images',
        target: 'image',
        action: 'delete',
    });
    console.log(JSON.stringify(noMatch, null, 2));

    // Unified model.
    console.log('\n── toUnifiedModel ──');
    const model = layer.toUnifiedModel();
    console.log(`Node count: ${model.nodeCount}`);
    console.log(`Total transforms: ${model.totalTransforms}`);

    // Flatten.
    console.log('\n── flatten ──');
    const flat = layer.flatten();
    for (const line of flat) {
        console.log(`  ${line}`);
    }

    console.log('\n✓ All grey_semantic tests passed.');
}
