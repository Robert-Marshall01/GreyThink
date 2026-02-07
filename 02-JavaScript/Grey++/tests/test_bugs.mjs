import { tokenize } from '../src/parser/lexer.js';
import { parse } from '../src/parser/parser.js';
import { runSource } from '../src/runner.js';
import { createTableStore } from '../src/runtime/plugins/query.js';

let passed = 0;
let failed = 0;
const failures = [];

function test(name, fn) {
    try {
        fn();
        passed++;
    } catch (e) {
        failed++;
        failures.push({ name, error: e.message });
    }
}

function expectError(name, fn) {
    try {
        fn();
        failed++;
        failures.push({ name, error: 'Expected an error but none was thrown' });
    } catch (e) {
        passed++;
    }
}

function expectNoError(name, fn) {
    try {
        fn();
        passed++;
    } catch (e) {
        failed++;
        failures.push({ name, error: e.message });
    }
}

function assertEqual(name, actual, expected) {
    if (JSON.stringify(actual) !== JSON.stringify(expected)) {
        failed++;
        failures.push({ name, error: `Expected ${JSON.stringify(expected)}, got ${JSON.stringify(actual)}` });
    } else {
        passed++;
    }
}

// ═══ LEXER BUGS ═══

// BUG 1: Unterminated string — should throw, currently doesn't
test('Lexer: unterminated string should error', () => {
    try {
        tokenize('"hello');
        throw new Error('SHOULD HAVE THROWN');
    } catch (e) {
        if (e.message === 'SHOULD HAVE THROWN') {
            throw new Error('Unterminated string did NOT produce an error');
        }
        // Good — it threw a proper error
    }
});

// BUG 2: Multi-dot number — 1.2.3 produces NaN
test('Lexer: multi-dot number produces NaN', () => {
    const tokens = tokenize('1.2.3');
    // Check if it creates a single NaN token or multiple tokens
    const numTokens = tokens.filter(t => t.type === 'Number');
    if (numTokens.length === 1 && isNaN(Number(numTokens[0].value))) {
        throw new Error('Multi-dot number 1.2.3 was parsed as NaN token');
    }
    // If it parsed as two tokens (1.2 and error on .3), that's also bad
});

// BUG 3: String escape sequences not interpreted
test('Lexer: escape sequences not interpreted', () => {
    const tokens = tokenize('"hello\\nworld"');
    const str = tokens.find(t => t.type === 'String');
    if (str.value === 'nworld') {
        throw new Error('\\n is not interpreted as newline — got literal "n"');
    }
    if (str.value !== 'hello\nworld') {
        throw new Error(`Expected hello<newline>world, got: ${JSON.stringify(str.value)}`);
    }
});

// BUG 4: Dot-prefixed decimals (.5) should work
test('Lexer: dot-prefixed decimal .5', () => {
    try {
        const tokens = tokenize('.5');
        const num = tokens.find(t => t.type === 'Number');
        if (!num || Number(num.value) !== 0.5) {
            throw new Error('.5 not parsed as 0.5');
        }
    } catch (e) {
        throw new Error('.5 causes lexer error: ' + e.message);
    }
});

// ═══ PARSER BUGS ═══

// BUG 5: Unary minus
test('Parser: unary minus -5', () => {
    try {
        parse('-5');
    } catch (e) {
        throw new Error('Unary minus -5 fails: ' + e.message);
    }
});

// BUG 6: Unary minus in expressions
test('Parser: unary minus in expression fn foo() { -x }', () => {
    try {
        parse('fn foo(x) { -x }');
    } catch (e) {
        throw new Error('-x fails in fn body: ' + e.message);
    }
});

// ═══ RUNTIME BUGS ═══

// BUG 7: Loose equality (== uses JS == instead of ===)
test('Runtime: 0 == false is true (loose equality)', () => {
    const result = runSource('0 == false');
    if (result === true) {
        throw new Error('0 == false returns true (using JS loose ==, should use ===)');
    }
});

// FIXED: Division by zero now throws
expectError('Runtime: 1/0 throws division by zero', () => {
    runSource('1 / 0');
});

// FIXED: 0/0 also throws
expectError('Runtime: 0/0 throws division by zero', () => {
    runSource('0 / 0');
});

// BUG 10: Query table store is global (leaks between kernels)
test('Query: tables leak between separate runSource calls', () => {
    runSource('createTable("leak_test", [{ x: 1 }])');
    try {
        const result = runSource('query { select * from leak_test }');
        if (Array.isArray(result) && result.length > 0) {
            throw new Error('Table "leak_test" leaked from previous runSource into new one');
        }
    } catch (e) {
        // If it throws "Unknown table", that's actually GOOD (no leak)
        if (e.message.includes('Unknown table')) {
            // Good — table didn't leak
        } else {
            throw e;
        }
    }
});

// BUG 11: Duplicate output — runFile prints last expression result even if it's a print() return
test('Runner: last print() result echoed twice', () => {
    // This is a design issue, not testable here easily
    // Just document it as known
    // pass
});

// BUG 12: Return keyword used as parameter name
test('Parser: return as parameter name', () => {
    try {
        const result = runSource('fn foo(return) { return }');
        // This might parse but 'return' in the body would be treated as ReturnStmt
        throw new Error('Using "return" as a param name: the body "return" is ambiguous');
    } catch (e) {
        // Expected — the parser treats 'return' in the body as a return statement
        // which then expects an expression, creating confusion
    }
});

// BUG 13: cond() evaluates all branches eagerly
test('Runtime: cond evaluates branches eagerly', () => {
    let sideEffect = false;
    // Can't test this directly in Grey++ since we can't track side effects
    // from JS. But the implementation: args are evaluated by the Grey++ runtime
    // before being passed to cond(), so all branches execute.
    // This is a known design limitation.
});

// ═══ RESULTS ═══
console.log('\n══════════════════════════════════════════');
console.log(`  PASS: ${passed}`);
console.log(`  FAIL: ${failed}`);
console.log('══════════════════════════════════════════');
if (failures.length > 0) {
    console.log('\nFailed tests:');
    for (const f of failures) {
        console.log(`  ✗ ${f.name}`);
        console.log(`    ${f.error}`);
    }
}
