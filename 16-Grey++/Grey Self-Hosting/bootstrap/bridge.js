/**
 * Grey++ Self-Hosting Bootstrap Bridge
 * 
 * This Node.js script connects the self-hosted Grey++ compiler front-end
 * to the existing Grey++ backend. It:
 * 
 *   1. Compiles the self-hosted front-end using the existing Grey++ interpreter
 *   2. Feeds Grey++ source code into the self-hosted front-end
 *   3. Captures the AST/IR output (JSON)
 *   4. Passes it to the existing Grey++ backend for code generation
 * 
 * Usage:
 *   node bootstrap/bridge.js <input.greypp> [--ast] [--tokens] [--json]
 * 
 * This is the bootstrapping pipeline:
 *   existing compiler → compiles selfhost.greypp → self-hosted front-end
 *   self-hosted front-end → parses input.greypp → AST (JSON)
 *   AST (JSON) → existing backend → executable
 */

import { execSync } from 'child_process';
import { readFileSync, writeFileSync, existsSync } from 'fs';
import { resolve, dirname, basename, join } from 'path';
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

// Paths
const SELFHOST_DIR = resolve(__dirname, '..');
const GREYPP_DIR = resolve(SELFHOST_DIR, '..', 'Grey++');
const GREYPP_CLI = join(GREYPP_DIR, 'src', 'index.js');
const SELFHOST_SRC = join(SELFHOST_DIR, 'src', 'selfhost.greypp');

function printUsage() {
    console.log(`
Grey++ Self-Hosting Bootstrap Bridge
=====================================

Usage:
  node bootstrap/bridge.js <input.greypp> [options]

Options:
  --ast      Print the AST tree
  --tokens   Print the token stream
  --json     Output raw AST JSON
  --compile  Compile using existing backend (pass-through)
  --help     Show this help message

Examples:
  node bootstrap/bridge.js examples/demo.greypp --ast
  node bootstrap/bridge.js examples/demo.greypp --json > output.json
  node bootstrap/bridge.js examples/demo.greypp --compile -o output.js

The bridge runs the self-hosted Grey++ front-end (written in Grey++)
to parse the input, then hands the AST to the existing backend.
    `.trim());
}

function main() {
    const args = process.argv.slice(2);

    if (args.includes('--help') || args.length === 0) {
        printUsage();
        process.exit(0);
    }

    // Parse arguments
    const inputFile = args.find(a => !a.startsWith('--') && !a.startsWith('-o'));
    const showAst = args.includes('--ast');
    const showTokens = args.includes('--tokens');
    const showJson = args.includes('--json');
    const doCompile = args.includes('--compile');

    if (!inputFile) {
        console.error('Error: No input file specified');
        printUsage();
        process.exit(1);
    }

    const resolvedInput = resolve(inputFile);
    if (!existsSync(resolvedInput)) {
        console.error(`Error: File not found: ${resolvedInput}`);
        process.exit(1);
    }

    // Check that the Grey++ interpreter exists
    if (!existsSync(GREYPP_CLI)) {
        console.error(`Error: Grey++ interpreter not found at: ${GREYPP_CLI}`);
        console.error('Make sure the Grey++ project is in the expected location.');
        process.exit(1);
    }

    // Check that selfhost.greypp exists
    if (!existsSync(SELFHOST_SRC)) {
        console.error(`Error: Self-host source not found: ${SELFHOST_SRC}`);
        process.exit(1);
    }

    console.log('Grey++ Self-Hosting Bootstrap Bridge');
    console.log('====================================');
    console.log(`Input:     ${resolvedInput}`);
    console.log(`Self-host: ${SELFHOST_SRC}`);
    console.log(`Backend:   ${GREYPP_CLI}`);
    console.log('');

    // Read the input source
    const inputSource = readFileSync(resolvedInput, 'utf-8');
    console.log(`Source: ${inputSource.length} bytes, ${inputSource.split('\n').length} lines`);

    // Create a temporary Grey++ script that:
    //   1. Includes all self-host code (inline)
    //   2. Reads the input source (embedded as a string)
    //   3. Runs the compile_frontend pipeline
    //   4. Outputs the result
    const escapedSource = inputSource
        .replace(/\\/g, '\\\\')
        .replace(/"/g, '\\"')
        .replace(/\n/g, '\\n')
        .replace(/\r/g, '\\r')
        .replace(/\t/g, '\\t');

    const selfhostCode = readFileSync(SELFHOST_SRC, 'utf-8');

    // Strip the demo section (everything after "SECTION 10: DEMONSTRATIONS")
    const demoMarker = '// SECTION 10: DEMONSTRATIONS';
    const demoIdx = selfhostCode.indexOf(demoMarker);
    const coreCode = demoIdx > 0
        ? selfhostCode.substring(0, demoIdx)
        : selfhostCode;

    // Build the bridge script
    const bridgeScript = `
${coreCode}

// ── Bridge: Parse input and output results ──────────────────────────────
fn __bridge_input() { "${escapedSource}" }

fn __bridge_run() {
    fn source() { __bridge_input() }
    
    ${showTokens ? `
    section("Token Stream")
    fn tokens() { tokenize(source()) }
    forEach(tokens(), fn(tok) {
        print(str(pad_end(tok_type(tok), 12, " "), " ", json_stringify(tok_val(tok)),
                  "  [", tok_line(tok), ":", tok_col(tok), "]"))
    })
    ` : ''}
    
    fn result() { compile_frontend(source()) }
    
    ${showAst ? `
    section("AST Tree")
    print_ast(get(result(), "ast"), 0)
    ` : ''}
    
    ${showJson ? `
    section("AST JSON")
    print(get(result(), "json"))
    ` : ''}
    
    result()
}

__bridge_run()
`;

    // Write temporary bridge script
    const tmpFile = join(SELFHOST_DIR, '.bridge_tmp.greypp');
    writeFileSync(tmpFile, bridgeScript, 'utf-8');

    try {
        // Run the bridge script through the existing Grey++ interpreter
        console.log('\nRunning self-hosted front-end...\n');
        const output = execSync(`node "${GREYPP_CLI}" run "${tmpFile}"`, {
            encoding: 'utf-8',
            maxBuffer: 10 * 1024 * 1024, // 10MB buffer
            cwd: SELFHOST_DIR
        });
        console.log(output);
    } catch (err) {
        console.error('Bridge execution failed:');
        console.error(err.stderr || err.message);
        process.exit(1);
    } finally {
        // Clean up temp file
        try {
            const { unlinkSync } = await import('fs');
            unlinkSync(tmpFile);
        } catch { /* ignore cleanup errors */ }
    }

    if (doCompile) {
        console.log('\nPass-through compilation via existing backend...');
        try {
            const output = execSync(
                `node "${GREYPP_CLI}" compile "${resolvedInput}"`,
                { encoding: 'utf-8', cwd: SELFHOST_DIR }
            );
            console.log(output);
        } catch (err) {
            console.error('Compilation failed:', err.message);
            process.exit(1);
        }
    }
}

main();
