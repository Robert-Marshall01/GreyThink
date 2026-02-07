#!/usr/bin/env node
// ─── Grey++ Entry Point ─────────────────────────────────────────────────────
// Usage:
//   grey                           → start the REPL
//   grey run <file.greypp>         → execute a .greypp file
//   grey run <file.greypp> --ast   → execute and dump AST
//   grey run <file.greypp> -v      → verbose mode
//   grey compile <file.greypp>     → compile to standalone .js
//   grey compile <file.greypp> -o  → compile to specific output path

import { startREPL } from './repl/repl.js';
import { runFile, GREYPP_EXTENSIONS, PRINTED } from './runner.js';
import { compileFile } from './compiler.js';

const args = process.argv.slice(2);
const command = args[0];

if (command === 'run' && args[1]) {
    // ── File execution mode ──
    const filePath = args[1];
    const flags = args.slice(2);
    try {
        const result = runFile(filePath, {
            verbose: flags.includes('--verbose') || flags.includes('-v'),
            dumpAst: flags.includes('--ast'),
        });
        if (result !== undefined && result !== PRINTED) {
            console.log(result);
        }
    } catch (err) {
        console.error(`Error: ${err.message}`);
        process.exit(1);
    }
} else if (command === 'compile' && args[1]) {
    // ── Compile mode ──
    const filePath = args[1];
    const flags = args.slice(2);
    const outIndex = flags.indexOf('-o');
    const outFile = outIndex !== -1 ? flags[outIndex + 1] : undefined;
    try {
        const emitted = compileFile(filePath, {
            verbose: flags.includes('--verbose') || flags.includes('-v'),
            dumpAst: flags.includes('--ast'),
            outFile,
        });
        console.log(`Compiled → ${emitted}`);
    } catch (err) {
        console.error(`Error: ${err.message}`);
        process.exit(1);
    }
} else if (command === 'help' || command === '--help' || command === '-h') {
    console.log('Grey++ v0.1.0 — The Universal Programming Language\n');
    console.log('Usage:');
    console.log('  grey                                Start the interactive REPL');
    console.log('  grey run <file.greypp>              Execute a Grey++ source file');
    console.log('  grey run <file.greypp> --ast        Execute and dump the AST');
    console.log('  grey run <file.greypp> -v           Verbose mode');
    console.log('  grey compile <file.greypp>          Compile to standalone .js');
    console.log('  grey compile <file.greypp> -o out.js Compile to a specific file');
    console.log('  grey compile <file.greypp> -v       Verbose compilation');
    console.log('  grey help                           Show this help message\n');
    console.log(`Supported file extensions: ${GREYPP_EXTENSIONS.join(', ')}`);
} else {
    // ── REPL mode (default) ──
    startREPL();
}
