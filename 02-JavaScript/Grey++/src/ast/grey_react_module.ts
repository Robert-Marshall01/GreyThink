// ─── Grey++ React Module Adapter ─────────────────────────────────────────────
// Maps a Grey++ ModuleNode (framework: 'react') into a virtual React
// component tree.  Stage 1 supports only <div> and <span> elements.
//
// No bundler, no JSX transform, no background services — just a pure
// data-structure-in → data-structure-out translator that can be consumed
// by any renderer (SSR string, test harness, or a real React runtime).
//
// Usage:
//   import { ReactModule } from './grey_react_module.js';
//   const mod = new ReactModule(moduleNode);
//   const vdom = mod.render();           // → VNode tree
//   const html = mod.toHTML();           // → HTML string
//   const src  = mod.toComponentSource(); // → JSX source code

import type { ASTNode, ModuleNode, FunctionNode } from './grey_ast.js';

// ═══════════════════════════════════════════════════════════════════════════
//  VIRTUAL DOM
// ═══════════════════════════════════════════════════════════════════════════

/** Supported element tags (Stage 1). */
export type ElementTag = 'div' | 'span';

/** A single virtual-DOM node — mirrors React.createElement's shape. */
export interface VNode {
    tag: ElementTag;
    props: Record<string, unknown>;
    children: (VNode | string)[];
}

/** Create a VNode. */
export function h(
    tag: ElementTag,
    props: Record<string, unknown> = {},
    ...children: (VNode | string)[]
): VNode {
    return { tag, props, children };
}

// ═══════════════════════════════════════════════════════════════════════════
//  AST → VNode COMPILER
// ═══════════════════════════════════════════════════════════════════════════

const SUPPORTED_TAGS = new Set<string>(['div', 'span']);

/**
 * Walk a Grey++ AST subtree and produce a VNode tree.
 *
 * Recognised patterns inside `exports`:
 *
 *   CallExpr("div",  [ ...children ])
 *   CallExpr("span", [ ...children ])
 *
 * String / number literals become text children.
 * Nested call-exprs become nested VNodes.
 * Everything else is serialised as a text placeholder.
 */
function astToVNode(node: ASTNode): VNode | string {
    // ── Text leaves ──
    if (node.kind === 'StringLit') return (node as any).value as string;
    if (node.kind === 'NumberLit') return String((node as any).value);
    if (node.kind === 'BoolLit') return String((node as any).value);
    if (node.kind === 'Identifier') return `{${(node as any).name}}`;

    // ── Element call:  div("class", child1, child2, …) ──
    if (node.kind === 'CallExpr') {
        const callee: string = (node as any).callee;
        const args: ASTNode[] = (node as any).args ?? [];

        if (SUPPORTED_TAGS.has(callee)) {
            // First arg may be a props object literal; the rest are children.
            let props: Record<string, unknown> = {};
            let childArgs = args;

            if (args.length > 0 && args[0].kind === 'ObjectLit') {
                props = objectLitToRecord(args[0]);
                childArgs = args.slice(1);
            }

            const children = childArgs.map(astToVNode);
            return h(callee as ElementTag, props, ...children);
        }

        // Unknown call — render as placeholder text.
        return `<${callee}(…)>`;
    }

    // ── Array literal → fragment-like list of children ──
    if (node.kind === 'ArrayLit') {
        const elements: ASTNode[] = (node as any).elements ?? [];
        return h('div', { _fragment: true }, ...elements.map(astToVNode));
    }

    // ── Fallback ──
    return `<unsupported:${node.kind}>`;
}

/** Convert an ObjectLit AST node into a plain Record. */
function objectLitToRecord(node: ASTNode): Record<string, unknown> {
    const entries: { key: string; value: ASTNode }[] = (node as any).entries ?? [];
    const out: Record<string, unknown> = {};
    for (const { key, value } of entries) {
        if (value.kind === 'StringLit') out[key] = (value as any).value;
        else if (value.kind === 'NumberLit') out[key] = (value as any).value;
        else if (value.kind === 'BoolLit') out[key] = (value as any).value;
        else out[key] = `{${key}}`;
    }
    return out;
}

// ═══════════════════════════════════════════════════════════════════════════
//  VNode → HTML SERIALISER
// ═══════════════════════════════════════════════════════════════════════════

function propsToAttrs(props: Record<string, unknown>): string {
    const parts: string[] = [];
    for (const [k, v] of Object.entries(props)) {
        if (k.startsWith('_')) continue; // internal flags
        if (k === 'className') {
            parts.push(`class="${esc(String(v))}"`);
        } else {
            parts.push(`${k}="${esc(String(v))}"`);
        }
    }
    return parts.length ? ' ' + parts.join(' ') : '';
}

function esc(s: string): string {
    return s.replace(/&/g, '&amp;').replace(/"/g, '&quot;').replace(/</g, '&lt;').replace(/>/g, '&gt;');
}

function vnodeToHTML(node: VNode | string, indent = 0): string {
    if (typeof node === 'string') return '  '.repeat(indent) + esc(node);
    const pad = '  '.repeat(indent);
    const attrs = propsToAttrs(node.props);
    if (node.children.length === 0) {
        return `${pad}<${node.tag}${attrs} />`;
    }
    if (node.children.length === 1 && typeof node.children[0] === 'string') {
        return `${pad}<${node.tag}${attrs}>${esc(node.children[0] as string)}</${node.tag}>`;
    }
    const inner = node.children.map(c => vnodeToHTML(c, indent + 1)).join('\n');
    return `${pad}<${node.tag}${attrs}>\n${inner}\n${pad}</${node.tag}>`;
}

// ═══════════════════════════════════════════════════════════════════════════
//  VNode → JSX SOURCE
// ═══════════════════════════════════════════════════════════════════════════

function vnodeToJSX(node: VNode | string, indent = 0): string {
    if (typeof node === 'string') {
        // If it looks like a binding `{varName}`, keep braces; otherwise quote.
        if (node.startsWith('{') && node.endsWith('}')) return '  '.repeat(indent) + node;
        return '  '.repeat(indent) + `"${node}"`;
    }
    const pad = '  '.repeat(indent);
    const propsStr = jsxProps(node.props);
    if (node.children.length === 0) {
        return `${pad}<${node.tag}${propsStr} />`;
    }
    if (node.children.length === 1 && typeof node.children[0] === 'string') {
        const text = node.children[0] as string;
        const content = text.startsWith('{') ? text : text;
        return `${pad}<${node.tag}${propsStr}>${content}</${node.tag}>`;
    }
    const inner = node.children.map(c => vnodeToJSX(c, indent + 2)).join('\n');
    return `${pad}<${node.tag}${propsStr}>\n${inner}\n${pad}</${node.tag}>`;
}

function jsxProps(props: Record<string, unknown>): string {
    const parts: string[] = [];
    for (const [k, v] of Object.entries(props)) {
        if (k.startsWith('_')) continue;
        if (typeof v === 'string') parts.push(`${k}="${v}"`);
        else parts.push(`${k}={${JSON.stringify(v)}}`);
    }
    return parts.length ? ' ' + parts.join(' ') : '';
}

// ═══════════════════════════════════════════════════════════════════════════
//  ReactModule  — public API
// ═══════════════════════════════════════════════════════════════════════════

export class ReactModule {
    readonly name: string;
    private exportNodes: ASTNode[];

    constructor(node: ModuleNode) {
        if (node.framework && node.framework !== 'react') {
            throw new Error(`ReactModule only handles framework "react", got "${node.framework}"`);
        }
        this.name = node.name;
        this.exportNodes = node.exports;
    }

    // ── Render to VNode tree ──────────────────────────────────────────────

    /** Compile the module's exports into a single VNode tree. */
    render(): VNode {
        const children: (VNode | string)[] = [];

        for (const exp of this.exportNodes) {
            // If export is a FunctionNode, treat its body as the component render.
            if (exp.kind === 'Function') {
                const fn = exp as unknown as FunctionNode;
                for (const stmt of fn.body) {
                    const child = this.compileStmt(stmt);
                    if (child !== null) children.push(child);
                }
            } else {
                const child = astToVNode(exp);
                children.push(child);
            }
        }

        // Wrap in a root <div>.
        return h('div', { 'data-component': this.name }, ...children);
    }

    // ── Output formats ────────────────────────────────────────────────────

    /** Render to an HTML string. */
    toHTML(): string {
        return vnodeToHTML(this.render());
    }

    /** Render to JSX component source code. */
    toComponentSource(): string {
        const jsx = vnodeToJSX(this.render(), 2);
        return [
            `// Auto-generated by Grey++ ReactModule adapter`,
            `// Module: ${this.name}`,
            ``,
            `export default function ${this.name}() {`,
            `  return (`,
            jsx,
            `  );`,
            `}`,
        ].join('\n');
    }

    // ── Internals ─────────────────────────────────────────────────────────

    private compileStmt(node: ASTNode): VNode | string | null {
        // ReturnStmt → compile the returned value.
        if (node.kind === 'ReturnStmt') {
            return astToVNode((node as any).value);
        }
        // Bare expression (call to div/span/etc.)
        return astToVNode(node);
    }
}

// ═══════════════════════════════════════════════════════════════════════════
//  SELF-TEST  (run with: npx tsx src/ast/grey_react_module.ts)
// ═══════════════════════════════════════════════════════════════════════════

if (typeof process !== 'undefined' && process.argv[1]?.endsWith('grey_react_module.ts')) {
    // Build a sample ModuleNode by hand.
    const sampleModule: ModuleNode = {
        kind: 'Module' as any,
        name: 'Dashboard',
        framework: 'react',
        exports: [
            {
                kind: 'Function' as any,
                name: 'Dashboard',
                params: [],
                body: [
                    {
                        kind: 'ReturnStmt' as any,
                        value: {
                            kind: 'CallExpr' as any,
                            callee: 'div',
                            args: [
                                { kind: 'ObjectLit' as any, entries: [{ key: 'className', value: { kind: 'StringLit' as any, value: 'dashboard' } }] },
                                {
                                    kind: 'CallExpr' as any,
                                    callee: 'span',
                                    args: [
                                        { kind: 'ObjectLit' as any, entries: [{ key: 'className', value: { kind: 'StringLit' as any, value: 'title' } }] },
                                        { kind: 'StringLit' as any, value: 'Hello Grey++' },
                                    ],
                                },
                                {
                                    kind: 'CallExpr' as any,
                                    callee: 'div',
                                    args: [
                                        { kind: 'StringLit' as any, value: 'Welcome to the dashboard' },
                                    ],
                                },
                            ],
                        },
                    },
                ],
            },
        ],
        imports: [],
    };

    const mod = new ReactModule(sampleModule);

    console.log('═══ VNode Tree ═══');
    console.dir(mod.render(), { depth: null });

    console.log('\n═══ HTML Output ═══');
    console.log(mod.toHTML());

    console.log('\n═══ JSX Component Source ═══');
    console.log(mod.toComponentSource());
}
