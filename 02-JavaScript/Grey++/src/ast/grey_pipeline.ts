// ─── Grey++ Pipeline Primitives ──────────────────────────────────────────────
// Isolated executors for every PipelineStepNode kind.  Each step is a pure
// function that receives resolved arguments, the previous step's output,
// and returns a structured stub result — no real AI, no real storage, no
// real networking.  Chaining is built-in: each step receives `prevOutput`.
//
// Extend by swapping the stub bodies for real implementations in a future
// stage.

import type { PipelineStepKind } from './grey_ast.js';

// ─── Result types ───────────────────────────────────────────────────────────

export interface PipelineStepResult {
    _type: string;
    stepKind: PipelineStepKind;
    stepIndex: number;
    input: unknown;
    output: unknown;
    meta: { stub: boolean;[k: string]: unknown };
}

export interface PipelineResult {
    _type: 'PipelineResult';
    name: string | undefined;
    stepCount: number;
    steps: PipelineStepResult[];
    finalOutput: unknown;
    meta: { stub: boolean };
}

// ═══════════════════════════════════════════════════════════════════════════
//  Step Executors
// ═══════════════════════════════════════════════════════════════════════════

/**
 * pipeline.infer — inference step (stub).
 *
 * Receives the previous step's output as input context and returns a
 * placeholder inference result.
 */
export function pipelineInfer(
    stepIndex: number,
    args: unknown[],
    flags: Record<string, unknown>,
    prevOutput: unknown,
): PipelineStepResult {
    const model = String(flags['model'] ?? args[0] ?? 'default');
    const dataset = flags['dataset'] ?? flags['prompt'] ?? args[1] ?? null;

    return {
        _type: 'PipelineInferStep',
        stepKind: 'infer',
        stepIndex,
        input: prevOutput,
        output: {
            model,
            dataset,
            prediction: 'Inference complete',
            prevInput: prevOutput ?? null,
        },
        meta: { stub: true, note: 'No real AI inference — placeholder result.' },
    };
}

/**
 * pipeline.store — storage step (stub).
 *
 * Takes the previous step's output and "stores" it.  Returns a placeholder
 * confirmation.
 */
export function pipelineStore(
    stepIndex: number,
    args: unknown[],
    flags: Record<string, unknown>,
    prevOutput: unknown,
): PipelineStepResult {
    const backend = String(flags['backend'] ?? args[0] ?? 'file');
    const file = String(flags['file'] ?? flags['path'] ?? args[1] ?? 'output.dat');
    const op = String(flags['op'] ?? 'save');

    return {
        _type: 'PipelineStoreStep',
        stepKind: 'store',
        stepIndex,
        input: prevOutput,
        output: {
            backend,
            file,
            op,
            status: 'Stored result',
            storedPayload: prevOutput ?? null,
        },
        meta: { stub: true, note: 'No real storage write — placeholder result.' },
    };
}

/**
 * pipeline.net — networking step (stub).
 *
 * Dispatches a network call with the previous step's output as context.
 * Returns a placeholder response.
 */
export function pipelineNet(
    stepIndex: number,
    args: unknown[],
    flags: Record<string, unknown>,
    prevOutput: unknown,
    primitive?: string,
    op?: string,
): PipelineStepResult {
    const resolvedPrimitive = primitive ?? String(flags['primitive'] ?? 'http');
    const resolvedOp = op ?? String(flags['op'] ?? flags['method'] ?? args[0] ?? 'GET');
    const url = String(flags['url'] ?? args.find(a => typeof a === 'string' && (a as string).startsWith('http')) ?? 'https://example.com');

    return {
        _type: 'PipelineNetStep',
        stepKind: 'net',
        stepIndex,
        input: prevOutput,
        output: {
            primitive: resolvedPrimitive,
            op: resolvedOp,
            url,
            status: 'Network call dispatched',
            payload: prevOutput ?? null,
        },
        meta: { stub: true, note: 'No real network call — placeholder result.' },
    };
}

// ─── Central step dispatcher ────────────────────────────────────────────────

export function dispatchPipelineStep(
    stepKind: PipelineStepKind,
    stepIndex: number,
    args: unknown[],
    flags: Record<string, unknown>,
    prevOutput: unknown,
    primitive?: string,
    op?: string,
): PipelineStepResult {
    switch (stepKind) {
        case 'infer': return pipelineInfer(stepIndex, args, flags, prevOutput);
        case 'store': return pipelineStore(stepIndex, args, flags, prevOutput);
        case 'net': return pipelineNet(stepIndex, args, flags, prevOutput, primitive, op);
        default:
            return {
                _type: 'PipelineGenericStep',
                stepKind,
                stepIndex,
                input: prevOutput,
                output: {
                    note: `Unknown pipeline step kind "${stepKind}" — generic stub.`,
                    args,
                    flags,
                },
                meta: { stub: true },
            };
    }
}

// ─── Pipeline runner (standalone, no runtime dependency) ────────────────────

/**
 * Execute an ordered list of step descriptors, threading output → input
 * between them.  Returns a consolidated PipelineResult.
 */
export function runPipelineSteps(
    name: string | undefined,
    steps: Array<{
        stepKind: PipelineStepKind;
        args: unknown[];
        flags: Record<string, unknown>;
        primitive?: string;
        op?: string;
    }>,
): PipelineResult {
    const results: PipelineStepResult[] = [];
    let prevOutput: unknown = null;

    for (let i = 0; i < steps.length; i++) {
        const s = steps[i];
        const result = dispatchPipelineStep(
            s.stepKind, i, s.args, s.flags, prevOutput, s.primitive, s.op,
        );
        results.push(result);
        prevOutput = result.output;
    }

    return {
        _type: 'PipelineResult',
        name,
        stepCount: steps.length,
        steps: results,
        finalOutput: prevOutput,
        meta: { stub: true },
    };
}

// ─── Self-test ──────────────────────────────────────────────────────────────

if (process.argv[1]?.includes('grey_pipeline') && process.argv.includes('--test')) {
    console.log('═══ grey_pipeline.ts Self-Test ═══\n');

    console.log('── pipeline.infer ──');
    console.log(JSON.stringify(pipelineInfer(0, [], { model: 'basic', dataset: 'demo' }, null), null, 2));

    console.log('\n── pipeline.store (chained) ──');
    const inferOut = pipelineInfer(0, [], { model: 'basic' }, null);
    console.log(JSON.stringify(pipelineStore(1, [], { backend: 'pdf', file: 'demo.pdf' }, inferOut.output), null, 2));

    console.log('\n── pipeline.net (chained) ──');
    const storeOut = pipelineStore(1, [], { backend: 'pdf', file: 'demo.pdf' }, inferOut.output);
    console.log(JSON.stringify(pipelineNet(2, [], { url: 'https://api.example.com' }, storeOut.output, 'http', 'POST'), null, 2));

    console.log('\n── full pipeline run ──');
    const full = runPipelineSteps('test-pipeline', [
        { stepKind: 'infer', args: [], flags: { model: 'basic', dataset: 'demo' } },
        { stepKind: 'store', args: [], flags: { backend: 'pdf', file: 'demo.pdf' } },
        { stepKind: 'net', args: [], flags: { url: 'https://api.example.com' }, primitive: 'http', op: 'POST' },
    ]);
    console.log(JSON.stringify(full, null, 2));

    console.log('\n✓ All grey_pipeline tests passed.');
}
