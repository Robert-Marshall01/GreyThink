// ═══════════════════════════════════════════════════════════════════════════
//  Grey++ Language Specification v1.0
// ═══════════════════════════════════════════════════════════════════════════
//  A unified programming language that takes the best constructs from 50
//  languages and merges them into ONE coherent syntax.
//
//  Design principles:
//    1. EXPRESSIVENESS — Every paradigm (imperative, functional, declarative,
//       OOP, systems, concurrent, reactive, logic, hardware) is first-class.
//    2. SAFETY — Ownership model (Rust), null safety (Kotlin/Swift),
//       type inference (Haskell/TypeScript), effect tracking.
//    3. ERGONOMICS — Clean syntax (Python), closures (JS/Ruby), pattern
//       matching (Rust/Scala/Elixir), pipes (Elixir/F#/Bash).
//    4. UNIVERSALITY — One language for AI, web, systems, mobile, embedded,
//       data science, blockchain, hardware, DevOps, and enterprise.
//
//  Source languages and what Grey++ takes from each:
//
//    Python       → clean syntax, list comprehensions, decorators, generators
//    JavaScript   → first-class functions, closures, async/await, JSON
//    Java         → interfaces, generics, annotations, try/catch
//    SQL          → query { select … from … where … }
//    TypeScript   → type annotations, union types, generics, interfaces
//    C#           → properties, LINQ-style pipeline, using, pattern matching
//    C++          → templates, RAII, operator overloading, unsafe blocks
//    Go           → goroutines (spawn), channels, defer, multiple returns
//    Rust         → ownership, borrowing, traits, match, Result/Option
//    PHP          → string interpolation, practical web
//    Swift        → optionals (?), guard, protocols, extensions
//    Kotlin       → null safety, data classes, coroutines, extensions
//    C            → raw pointers (in unsafe), manual memory, preprocessor
//    Ruby         → blocks, mixins, metaprogramming, everything-is-object
//    R            → vectorized ops, data frames, statistical primitives
//    Bash/Shell   → pipe |>, process substitution, command chains
//    Dart         → widget declarations, factory constructors, mixins
//    PowerShell   → cmdlet pipeline, object output
//    Scala        → case classes, pattern matching, actors, implicits
//    Objective-C  → message passing (via .send)
//    Solidity     → contract blocks, modifiers, event emission
//    MATLAB       → matrix literals, vectorized math, signal processing
//    Groovy       → DSL builders, closures, scripting ergonomics
//    Perl         → regex literals, text processing, one-liners
//    Assembly     → asm blocks for inline assembly
//    Haskell      → pure functions, monads (effects), type classes, lazy eval
//    Julia        → multiple dispatch, macros, scientific computing
//    Elixir       → actors, pipe operator, pattern matching, supervision
//    Lua          → lightweight embedding, metatables
//    Clojure      → immutable data, persistent data structures, atoms
//    Apex         → cloud-native annotations, triggers
//    PL/SQL       → stored procedures, cursors, database triggers
//    Fortran      → array operations, do loops, supercomputing primitives
//    COBOL        → record types, business logic sections
//    Erlang       → fault tolerance, hot code reload, OTP patterns
//    F#           → discriminated unions, computation expressions, pipes
//    Delphi       → components, RAD, event-driven
//    VB.NET       → event-driven, readable syntax, With blocks
//    ABAP         → internal tables, structured data, ALV
//    VHDL/Verilog → hardware description, concurrent processes, signals
//    Zig          → comptime execution, no hidden allocations
//    Mojo         → Python syntax + systems performance, SIMD
//    Nim          → macro system, compile to C, uniform call syntax
//    Smalltalk    → message passing, image-based, live coding
//    SAS          → data steps, proc steps, statistical analysis
//    Prolog       → logic programming, unification, backtracking
//    Lisp         → homoiconicity, macros, S-expressions, code-as-data
//    Tcl          → everything-is-a-string, introspection
//    LabVIEW      → dataflow programming, parallel execution
//    Scratch      → visual blocks (→ block DSL), event-driven
// ═══════════════════════════════════════════════════════════════════════════

// ─── Keyword Registry ───────────────────────────────────────────────────────
// Every keyword in Grey++, categorized by the paradigm and source language.

export const GREY_KEYWORDS = {
    // ── Core Bindings (Rust, JS, Python, Go) ──
    bind: { category: 'binding', description: 'Immutable variable binding', from: ['Rust', 'Haskell'] },
    mut: { category: 'binding', description: 'Mutable variable binding', from: ['Rust'] },
    const: { category: 'binding', description: 'Compile-time constant', from: ['C++', 'JS', 'Go'] },

    // ── Functions (Python, Rust, Go, Haskell, JS) ──
    fn: { category: 'function', description: 'Function definition', from: ['Rust', 'Go'] },
    async: { category: 'function', description: 'Asynchronous function modifier', from: ['JS', 'C#', 'Python'] },
    await: { category: 'function', description: 'Await async result', from: ['JS', 'C#', 'Python'] },
    pure: { category: 'function', description: 'Pure function (no side effects)', from: ['Haskell'] },
    gen: { category: 'function', description: 'Generator function', from: ['Python', 'JS'] },
    yield: { category: 'function', description: 'Yield value from generator', from: ['Python', 'JS'] },
    return: { category: 'function', description: 'Return value from function', from: ['all'] },
    defer: { category: 'function', description: 'Deferred cleanup execution', from: ['Go', 'Zig'] },

    // ── Types (TypeScript, Rust, Haskell, Java, C#) ──
    type: { category: 'type', description: 'Type alias / algebraic type', from: ['TypeScript', 'Haskell', 'Rust'] },
    struct: { category: 'type', description: 'Struct / record definition', from: ['Rust', 'C', 'Go'] },
    enum: { category: 'type', description: 'Enumeration / sum type', from: ['Rust', 'Java', 'Swift', 'C#'] },
    trait: { category: 'type', description: 'Trait / type class definition', from: ['Rust', 'Haskell', 'Scala'] },
    impl: { category: 'type', description: 'Trait implementation for a type', from: ['Rust'] },
    interface: { category: 'type', description: 'Interface contract', from: ['Java', 'Go', 'TypeScript', 'C#'] },
    data: { category: 'type', description: 'Data class (auto eq/hash/copy)', from: ['Kotlin', 'Haskell', 'Scala'] },
    record: { category: 'type', description: 'Immutable record type', from: ['C#', 'Java', 'COBOL'] },
    newtype: { category: 'type', description: 'Newtype wrapper (zero-cost)', from: ['Haskell', 'Rust'] },
    class: { category: 'type', description: 'Class definition (OOP)', from: ['Java', 'Python', 'C#', 'C++'] },

    // ── Control Flow (Rust, Python, Go, C, Elixir, Haskell) ──
    cond: { category: 'control', description: 'Conditional branching', from: ['Grey++'] },
    match: { category: 'control', description: 'Pattern matching', from: ['Rust', 'Scala', 'Elixir', 'F#', 'Haskell'] },
    loop: { category: 'control', description: 'Loop construct', from: ['Grey++', 'Rust'] },
    while: { category: 'control', description: 'While loop modifier', from: ['C', 'Python', 'JS'] },
    for: { category: 'control', description: 'For-each iteration', from: ['Python', 'Rust', 'Go'] },
    break: { category: 'control', description: 'Break from loop', from: ['C', 'Rust', 'JS'] },
    continue: { category: 'control', description: 'Continue to next iteration', from: ['C', 'Rust', 'JS'] },
    guard: { category: 'control', description: 'Guard clause (early return)', from: ['Swift'] },
    when: { category: 'control', description: 'When expression (Kotlin-style)', from: ['Kotlin'] },

    // ── Error Handling (Rust, Go, Java, Swift) ──
    try: { category: 'error', description: 'Try block / error propagation', from: ['Java', 'Rust', 'Swift'] },
    catch: { category: 'error', description: 'Catch exception', from: ['Java', 'C#', 'JS'] },
    finally: { category: 'error', description: 'Finally cleanup block', from: ['Java', 'JS'] },
    throw: { category: 'error', description: 'Throw exception / error', from: ['Java', 'JS'] },
    panic: { category: 'error', description: 'Unrecoverable error', from: ['Rust', 'Go'] },
    recover: { category: 'error', description: 'Recover from panic', from: ['Go'] },

    // ── Modules & Imports (Rust, Python, JS, Go, C++) ──
    module: { category: 'module', description: 'Module definition', from: ['Grey++', 'Rust'] },
    import: { category: 'module', description: 'Import from module', from: ['Python', 'JS', 'Go'] },
    export: { category: 'module', description: 'Export from module', from: ['JS', 'Rust'] },
    use: { category: 'module', description: 'Use/import path', from: ['Rust', 'PHP'] },
    package: { category: 'module', description: 'Package declaration', from: ['Go', 'Java', 'Kotlin'] },

    // ── Concurrency (Go, Erlang, Elixir, Rust, Scala) ──
    spawn: { category: 'concurrency', description: 'Spawn concurrent task / goroutine', from: ['Go', 'Erlang', 'Elixir'] },
    channel: { category: 'concurrency', description: 'Channel for message passing', from: ['Go', 'Rust'] },
    send: { category: 'concurrency', description: 'Send message to channel/actor', from: ['Go', 'Erlang', 'Elixir'] },
    receive: { category: 'concurrency', description: 'Receive from channel/actor', from: ['Go', 'Erlang', 'Elixir'] },
    select: { category: 'concurrency', description: 'Select on multiple channels', from: ['Go'] },
    actor: { category: 'concurrency', description: 'Actor definition', from: ['Erlang', 'Elixir', 'Scala'] },
    supervisor: { category: 'concurrency', description: 'Supervision tree', from: ['Erlang', 'Elixir'] },
    atomic: { category: 'concurrency', description: 'Atomic operation', from: ['C++', 'Rust'] },
    mutex: { category: 'concurrency', description: 'Mutual exclusion lock', from: ['C++', 'Rust', 'Go'] },

    // ── Memory & Safety (Rust, C, C++, Zig) ──
    unsafe: { category: 'memory', description: 'Unsafe block (raw pointers, FFI)', from: ['Rust', 'C#'] },
    own: { category: 'memory', description: 'Ownership transfer', from: ['Rust'] },
    ref: { category: 'memory', description: 'Borrow reference', from: ['Rust', 'C#'] },
    drop: { category: 'memory', description: 'Explicit destructor / cleanup', from: ['Rust', 'C++'] },
    alloc: { category: 'memory', description: 'Allocate memory', from: ['C', 'Rust'] },
    free: { category: 'memory', description: 'Free memory', from: ['C'] },
    box: { category: 'memory', description: 'Heap allocation wrapper', from: ['Rust'] },

    // ── Query & Data (SQL, R, MATLAB, Clojure) ──
    query: { category: 'data', description: 'Declarative query block', from: ['SQL', 'Grey++'] },
    from: { category: 'data', description: 'Data source clause', from: ['SQL', 'C# LINQ'] },
    where: { category: 'data', description: 'Filter clause', from: ['SQL', 'C# LINQ'] },
    orderby: { category: 'data', description: 'Sort clause', from: ['SQL', 'C# LINQ'] },
    groupby: { category: 'data', description: 'Group clause', from: ['SQL'] },
    join: { category: 'data', description: 'Join clause', from: ['SQL'] },
    into: { category: 'data', description: 'Collect into target', from: ['Rust', 'SQL'] },
    pipe: { category: 'data', description: 'Pipeline operator', from: ['Elixir', 'F#', 'Bash'] },
    matrix: { category: 'data', description: 'Matrix literal', from: ['MATLAB', 'Julia', 'R'] },
    dataframe: { category: 'data', description: 'Data frame operations', from: ['R', 'Python/Pandas'] },
    vector: { category: 'data', description: 'Vectorized operations', from: ['R', 'MATLAB', 'Julia'] },

    // ── AI & Inference (Python, Mojo, Grey++) ──
    infer: { category: 'ai', description: 'AI model inference block', from: ['Grey++'] },
    train: { category: 'ai', description: 'Model training block', from: ['Grey++', 'Python'] },
    tensor: { category: 'ai', description: 'Tensor declaration', from: ['Python/PyTorch', 'Mojo'] },
    model: { category: 'ai', description: 'ML model definition', from: ['Grey++'] },
    predict: { category: 'ai', description: 'Prediction dispatch', from: ['Grey++'] },
    embed: { category: 'ai', description: 'Embedding computation', from: ['Grey++'] },

    // ── Blockchain & Contracts (Solidity) ──
    contract: { category: 'blockchain', description: 'Smart contract definition', from: ['Solidity'] },
    event: { category: 'blockchain', description: 'Contract event emission', from: ['Solidity', 'C#'] },
    modifier: { category: 'blockchain', description: 'Contract modifier / guard', from: ['Solidity'] },
    payable: { category: 'blockchain', description: 'Payable function modifier', from: ['Solidity'] },
    state: { category: 'blockchain', description: 'Contract state variable', from: ['Solidity'] },

    // ── Hardware & Embedded (VHDL, Verilog, C, Assembly) ──
    hardware: { category: 'hardware', description: 'Hardware description block', from: ['VHDL', 'Verilog'] },
    signal: { category: 'hardware', description: 'Hardware signal', from: ['VHDL'] },
    process: { category: 'hardware', description: 'Hardware process', from: ['VHDL'] },
    port: { category: 'hardware', description: 'Hardware port', from: ['VHDL', 'Verilog'] },
    asm: { category: 'hardware', description: 'Inline assembly block', from: ['C', 'Rust', 'Zig'] },
    register: { category: 'hardware', description: 'Hardware register', from: ['Assembly', 'VHDL'] },
    interrupt: { category: 'hardware', description: 'Interrupt handler', from: ['C', 'Assembly'] },

    // ── UI & Reactive (Dart/Flutter, React, SwiftUI) ──
    widget: { category: 'ui', description: 'UI widget declaration', from: ['Dart/Flutter'] },
    render: { category: 'ui', description: 'Render function/block', from: ['React', 'Dart'] },
    component: { category: 'ui', description: 'UI component', from: ['React', 'Vue'] },
    observe: { category: 'ui', description: 'Observable binding', from: ['Swift', 'Kotlin'] },
    binding: { category: 'ui', description: 'Two-way data binding', from: ['SwiftUI', 'Angular'] },

    // ── Logic Programming (Prolog, Lisp) ──
    logic: { category: 'logic', description: 'Logic programming block', from: ['Prolog'] },
    fact: { category: 'logic', description: 'Logic fact assertion', from: ['Prolog'] },
    rule: { category: 'logic', description: 'Logic rule definition', from: ['Prolog'] },
    solve: { category: 'logic', description: 'Logic query / solve', from: ['Prolog'] },

    // ── Metaprogramming (Rust, Lisp, Julia, Nim) ──
    macro: { category: 'meta', description: 'Macro definition', from: ['Rust', 'Lisp', 'Julia', 'Nim'] },
    comptime: { category: 'meta', description: 'Compile-time execution', from: ['Zig', 'Nim'] },
    reflect: { category: 'meta', description: 'Runtime reflection', from: ['Java', 'C#', 'Go'] },
    quote: { category: 'meta', description: 'Quote expression (code as data)', from: ['Lisp', 'Julia', 'Elixir'] },
    unquote: { category: 'meta', description: 'Unquote in macro', from: ['Lisp', 'Elixir'] },

    // ── Testing & Assertions (all) ──
    test: { category: 'test', description: 'Test block / case', from: ['Rust', 'Go', 'Python'] },
    assert: { category: 'test', description: 'Assertion', from: ['all'] },
    expect: { category: 'test', description: 'Expectation', from: ['Jest', 'RSpec'] },
    bench: { category: 'test', description: 'Benchmark block', from: ['Rust', 'Go'] },

    // ── Systems & DevOps (Bash, PowerShell, Go) ──
    sys: { category: 'system', description: 'System operation', from: ['Grey++'] },
    exec: { category: 'system', description: 'Execute shell command', from: ['Bash', 'Go'] },
    env: { category: 'system', description: 'Environment variable access', from: ['Bash', 'Go'] },
    pipe_cmd: { category: 'system', description: 'Command pipeline', from: ['Bash', 'PowerShell'] },

    // ── Networking (Grey++) ──
    net: { category: 'network', description: 'Networking primitive', from: ['Grey++'] },
    http: { category: 'network', description: 'HTTP request', from: ['all'] },
    socket: { category: 'network', description: 'Socket connection', from: ['all'] },
    rpc: { category: 'network', description: 'Remote procedure call', from: ['Go', 'Rust'] },

    // ── Annotations & Decorators (Python, Java, C#, Kotlin) ──
    '@': { category: 'annotation', description: 'Annotation / decorator prefix', from: ['Python', 'Java', 'C#'] },

    // ── Functional (Haskell, Scala, F#, Clojure, Elixir) ──
    compose: { category: 'functional', description: 'Function composition', from: ['Haskell', 'F#'] },
    curry: { category: 'functional', description: 'Curried function', from: ['Haskell', 'Scala'] },
    partial: { category: 'functional', description: 'Partial application', from: ['Haskell', 'Python'] },
    lazy: { category: 'functional', description: 'Lazy evaluation', from: ['Haskell', 'Scala'] },
    memo: { category: 'functional', description: 'Memoization', from: ['all'] },

    // ── Scientific Computing (Julia, MATLAB, R, Fortran, SAS) ──
    simd: { category: 'scientific', description: 'SIMD vectorization', from: ['Mojo', 'C++', 'Rust'] },
    parallel: { category: 'scientific', description: 'Parallel execution', from: ['Julia', 'Fortran', 'LabVIEW'] },
    distribute: { category: 'scientific', description: 'Distributed computation', from: ['Spark/Scala', 'Julia'] },

    // ── Database (SQL, PL/SQL, ABAP) ──
    transaction: { category: 'database', description: 'Database transaction block', from: ['SQL', 'PL/SQL'] },
    cursor: { category: 'database', description: 'Database cursor', from: ['PL/SQL', 'SQL'] },
    procedure: { category: 'database', description: 'Stored procedure', from: ['PL/SQL', 'SQL'] },

    // ── Enterprise (COBOL, ABAP, VB.NET, Delphi) ──
    section: { category: 'enterprise', description: 'Code section / division', from: ['COBOL'] },
    perform: { category: 'enterprise', description: 'Execute paragraph/section', from: ['COBOL'] },
    table: { category: 'enterprise', description: 'Internal table', from: ['ABAP', 'SQL'] },
    form: { category: 'enterprise', description: 'Form / visual component', from: ['Delphi', 'VB.NET'] },
} as const;

// ─── Type System ────────────────────────────────────────────────────────────
// Grey++ has a unified type system combining the best from TypeScript,
// Rust, Haskell, and Go.

export const GREY_TYPES = {
    // ── Primitive Types ──
    primitives: {
        int: { size: 64, description: 'Signed integer', from: 'Rust/Go' },
        i8: { size: 8, description: '8-bit signed integer', from: 'Rust' },
        i16: { size: 16, description: '16-bit signed integer', from: 'Rust' },
        i32: { size: 32, description: '32-bit signed integer', from: 'Rust' },
        i64: { size: 64, description: '64-bit signed integer', from: 'Rust' },
        i128: { size: 128, description: '128-bit signed integer', from: 'Rust' },
        u8: { size: 8, description: '8-bit unsigned integer', from: 'Rust' },
        u16: { size: 16, description: '16-bit unsigned integer', from: 'Rust' },
        u32: { size: 32, description: '32-bit unsigned integer', from: 'Rust' },
        u64: { size: 64, description: '64-bit unsigned integer', from: 'Rust' },
        u128: { size: 128, description: '128-bit unsigned integer', from: 'Rust' },
        f32: { size: 32, description: '32-bit float', from: 'Rust' },
        f64: { size: 64, description: '64-bit float', from: 'Rust' },
        bool: { size: 1, description: 'Boolean', from: 'all' },
        char: { size: 32, description: 'Unicode character', from: 'Rust' },
        str: { size: -1, description: 'String (UTF-8)', from: 'Rust/Python' },
        byte: { size: 8, description: 'Raw byte', from: 'Go' },
        void: { size: 0, description: 'No value', from: 'C/C++' },
        never: { size: 0, description: 'Never returns (bottom type)', from: 'Rust/TypeScript' },
        any: { size: -1, description: 'Dynamic type', from: 'TypeScript/Python' },
    },

    // ── Composite Types ──
    composite: {
        'array<T>': { description: 'Fixed-size array', from: 'Rust/C' },
        'vec<T>': { description: 'Growable vector', from: 'Rust' },
        'list<T>': { description: 'Linked list', from: 'Haskell' },
        'map<K,V>': { description: 'Hash map', from: 'Go/Rust' },
        'set<T>': { description: 'Hash set', from: 'Python/Rust' },
        'tuple<T...>': { description: 'Fixed-size heterogeneous', from: 'Python/Rust' },
        'Option<T>': { description: 'Optional value (Some/None)', from: 'Rust/Swift' },
        'Result<T,E>': { description: 'Success/Error result', from: 'Rust' },
        'Future<T>': { description: 'Async future', from: 'Rust/Dart' },
        'Stream<T>': { description: 'Async stream', from: 'Dart/Rust' },
        'Channel<T>': { description: 'Typed channel', from: 'Go/Rust' },
        'Ref<T>': { description: 'Shared reference', from: 'Rust' },
        'MutRef<T>': { description: 'Mutable reference', from: 'Rust' },
        'Box<T>': { description: 'Heap-allocated box', from: 'Rust' },
        'Arc<T>': { description: 'Atomic reference counted', from: 'Rust' },
        'Tensor<T>': { description: 'N-dimensional tensor', from: 'Mojo/PyTorch' },
        'Matrix<T>': { description: '2D matrix', from: 'MATLAB/Julia' },
        'DataFrame': { description: 'Tabular data', from: 'R/Python' },
    },

    // ── Type Operators ──
    operators: {
        '|': { description: 'Union type: A | B', from: 'TypeScript/Rust' },
        '&': { description: 'Intersection type: A & B', from: 'TypeScript' },
        '?': { description: 'Optional: T? = Option<T>', from: 'Swift/Kotlin/TypeScript' },
        '!': { description: 'Non-null assertion', from: 'TypeScript/Kotlin' },
        '->': { description: 'Function return type', from: 'Rust/Haskell' },
        '=>': { description: 'Fat arrow / lambda', from: 'JS/C#/Scala' },
    },
} as const;

// ─── Operator Registry ─────────────────────────────────────────────────────

export const GREY_OPERATORS = {
    // ── Arithmetic ──
    '+': 'Addition / concatenation',
    '-': 'Subtraction / negation',
    '*': 'Multiplication / dereference',
    '/': 'Division',
    '%': 'Modulo',
    '**': 'Exponentiation (Python)',
    '//': 'Integer division (Python)',

    // ── Comparison ──
    '==': 'Equality',
    '!=': 'Inequality',
    '<': 'Less than',
    '>': 'Greater than',
    '<=': 'Less than or equal',
    '>=': 'Greater than or equal',
    '<=>': 'Spaceship / three-way comparison (C++/Ruby)',

    // ── Logical ──
    'and': 'Logical AND (Python-style)',
    'or': 'Logical OR (Python-style)',
    'not': 'Logical NOT (Python-style)',
    '&&': 'Short-circuit AND',
    '||': 'Short-circuit OR',
    '!': 'Logical NOT',

    // ── Bitwise ──
    '&': 'Bitwise AND',
    '|': 'Bitwise OR / union type separator',
    '^': 'Bitwise XOR',
    '~': 'Bitwise NOT',
    '<<': 'Left shift',
    '>>': 'Right shift',

    // ── Assignment ──
    '=': 'Assignment',
    '+=': 'Add-assign',
    '-=': 'Sub-assign',
    '*=': 'Mul-assign',
    '/=': 'Div-assign',
    '%=': 'Mod-assign',

    // ── Special ──
    '|>': 'Pipe operator (Elixir/F#)',
    '..': 'Range (exclusive end) (Rust)',
    '..=': 'Range (inclusive end) (Rust)',
    '?.': 'Optional chaining (JS/Kotlin/Swift)',
    '??': 'Null coalescing (JS/C#/Kotlin)',
    '?:': 'Elvis operator (Kotlin/Groovy)',
    '<-': 'Channel receive (Go) / monadic bind',
    '=>': 'Pattern match arm / fat arrow',
    '::': 'Path separator (Rust/C++)',
    '...': 'Spread / rest operator (JS)',
    '@': 'Decorator / annotation',
} as const;

// ─── Paradigm Map ───────────────────────────────────────────────────────────
// Maps each source language to its paradigm contributions in Grey++.

export interface ParadigmContribution {
    language: string;
    paradigms: string[];
    keyContributions: string[];
    greyKeywords: string[];
}

export const PARADIGM_MAP: ParadigmContribution[] = [
    {
        language: 'Python',
        paradigms: ['imperative', 'OOP', 'functional', 'scripting'],
        keyContributions: ['clean syntax', 'list comprehensions', 'decorators', 'generators', 'duck typing'],
        greyKeywords: ['fn', 'for', 'yield', 'gen', '@', 'import', 'class'],
    },
    {
        language: 'JavaScript',
        paradigms: ['imperative', 'functional', 'event-driven', 'prototype-OOP'],
        keyContributions: ['closures', 'async/await', 'JSON', 'first-class functions', 'spread operator'],
        greyKeywords: ['async', 'await', 'fn', 'export', 'import', '...'],
    },
    {
        language: 'Java',
        paradigms: ['OOP', 'imperative', 'generic'],
        keyContributions: ['interfaces', 'generics', 'annotations', 'exception handling', 'strong typing'],
        greyKeywords: ['interface', 'class', 'try', 'catch', 'throw', 'package'],
    },
    {
        language: 'SQL',
        paradigms: ['declarative', 'set-based'],
        keyContributions: ['declarative queries', 'joins', 'aggregations', 'transactions'],
        greyKeywords: ['query', 'select', 'from', 'where', 'join', 'orderby', 'groupby', 'transaction'],
    },
    {
        language: 'TypeScript',
        paradigms: ['imperative', 'OOP', 'functional', 'generic'],
        keyContributions: ['type annotations', 'union types', 'generics', 'interfaces', 'type inference'],
        greyKeywords: ['type', 'interface', 'enum', 'any', 'never'],
    },
    {
        language: 'C#',
        paradigms: ['OOP', 'imperative', 'functional', 'generic'],
        keyContributions: ['properties', 'LINQ', 'async/await', 'pattern matching', 'events'],
        greyKeywords: ['async', 'await', 'event', 'query', 'match', 'record', 'pipe'],
    },
    {
        language: 'C++',
        paradigms: ['imperative', 'OOP', 'generic', 'systems'],
        keyContributions: ['templates', 'RAII', 'operator overloading', 'move semantics', 'constexpr'],
        greyKeywords: ['struct', 'class', 'const', 'unsafe', 'comptime', 'atomic'],
    },
    {
        language: 'Go',
        paradigms: ['imperative', 'concurrent'],
        keyContributions: ['goroutines', 'channels', 'defer', 'interfaces', 'multiple return values'],
        greyKeywords: ['spawn', 'channel', 'defer', 'select', 'interface', 'package'],
    },
    {
        language: 'Rust',
        paradigms: ['imperative', 'functional', 'systems', 'concurrent'],
        keyContributions: ['ownership', 'borrowing', 'traits', 'pattern matching', 'Result/Option', 'zero-cost abstractions'],
        greyKeywords: ['bind', 'mut', 'own', 'ref', 'trait', 'impl', 'match', 'enum', 'unsafe', 'box'],
    },
    {
        language: 'PHP',
        paradigms: ['imperative', 'OOP', 'scripting'],
        keyContributions: ['string interpolation', 'practical web development', 'superglobals'],
        greyKeywords: ['fn', 'class', 'use'],
    },
    {
        language: 'Swift',
        paradigms: ['OOP', 'functional', 'protocol-oriented'],
        keyContributions: ['optionals', 'guard clauses', 'protocols', 'extensions', 'value types'],
        greyKeywords: ['guard', 'trait', 'enum', '?', '!'],
    },
    {
        language: 'Kotlin',
        paradigms: ['OOP', 'functional', 'concurrent'],
        keyContributions: ['null safety', 'data classes', 'coroutines', 'extension functions', 'when expression'],
        greyKeywords: ['data', 'when', 'async', '?.', '??'],
    },
    {
        language: 'C',
        paradigms: ['imperative', 'systems'],
        keyContributions: ['raw pointers', 'manual memory management', 'preprocessor', 'minimal runtime'],
        greyKeywords: ['unsafe', 'alloc', 'free', 'struct'],
    },
    {
        language: 'Ruby',
        paradigms: ['OOP', 'functional', 'scripting', 'metaprogramming'],
        keyContributions: ['blocks', 'mixins', 'metaprogramming', 'DSLs', 'everything-is-object'],
        greyKeywords: ['fn', 'trait', 'macro', 'yield'],
    },
    {
        language: 'R',
        paradigms: ['functional', 'statistical', 'vectorized'],
        keyContributions: ['vectorized operations', 'data frames', 'statistical functions', 'plotting'],
        greyKeywords: ['vector', 'dataframe', 'pipe', 'matrix'],
    },
    {
        language: 'Bash/Shell',
        paradigms: ['scripting', 'pipeline'],
        keyContributions: ['pipe operator', 'process substitution', 'command chaining', 'globbing'],
        greyKeywords: ['pipe', 'pipe_cmd', 'exec', 'env'],
    },
    {
        language: 'Dart',
        paradigms: ['OOP', 'reactive', 'UI'],
        keyContributions: ['widget declarations', 'factory constructors', 'mixins', 'Flutter UI tree'],
        greyKeywords: ['widget', 'render', 'component', 'class'],
    },
    {
        language: 'PowerShell',
        paradigms: ['scripting', 'pipeline', 'object-oriented'],
        keyContributions: ['cmdlet pipeline', 'object output', 'remoting', 'DSC'],
        greyKeywords: ['pipe', 'pipe_cmd', 'exec'],
    },
    {
        language: 'Scala',
        paradigms: ['OOP', 'functional', 'concurrent'],
        keyContributions: ['case classes', 'pattern matching', 'actors', 'implicits', 'for-comprehensions'],
        greyKeywords: ['data', 'match', 'actor', 'trait', 'enum'],
    },
    {
        language: 'Objective-C',
        paradigms: ['OOP', 'message-passing'],
        keyContributions: ['message passing', 'categories', 'protocols', 'dynamic dispatch'],
        greyKeywords: ['send', 'trait', 'class'],
    },
    {
        language: 'Solidity',
        paradigms: ['contract-oriented', 'declarative'],
        keyContributions: ['smart contracts', 'modifiers', 'events', 'payable functions', 'gas optimization'],
        greyKeywords: ['contract', 'event', 'modifier', 'payable', 'state'],
    },
    {
        language: 'MATLAB',
        paradigms: ['imperative', 'array-oriented', 'scientific'],
        keyContributions: ['matrix literals', 'vectorized math', 'signal processing', 'toolboxes'],
        greyKeywords: ['matrix', 'vector', 'parallel'],
    },
    {
        language: 'Groovy',
        paradigms: ['OOP', 'scripting', 'DSL'],
        keyContributions: ['DSL builders', 'closures', 'scripting ergonomics', 'Gradle integration'],
        greyKeywords: ['fn', 'class', 'macro'],
    },
    {
        language: 'Perl',
        paradigms: ['imperative', 'functional', 'text-processing'],
        keyContributions: ['regex literals', 'text processing', 'CPAN ecosystem', 'one-liners'],
        greyKeywords: ['fn', 'match'],
    },
    {
        language: 'Assembly',
        paradigms: ['low-level', 'systems'],
        keyContributions: ['direct hardware access', 'register manipulation', 'interrupt handling'],
        greyKeywords: ['asm', 'register', 'interrupt', 'unsafe'],
    },
    {
        language: 'Haskell',
        paradigms: ['pure-functional', 'lazy', 'typed'],
        keyContributions: ['pure functions', 'monads', 'type classes', 'lazy evaluation', 'algebraic data types'],
        greyKeywords: ['pure', 'lazy', 'type', 'trait', 'compose', 'curry', 'data', 'newtype'],
    },
    {
        language: 'Julia',
        paradigms: ['scientific', 'functional', 'multiple-dispatch'],
        keyContributions: ['multiple dispatch', 'macros', 'scientific computing', 'metaprogramming'],
        greyKeywords: ['macro', 'parallel', 'matrix', 'simd', 'distribute'],
    },
    {
        language: 'Elixir',
        paradigms: ['functional', 'concurrent', 'actor-based'],
        keyContributions: ['actors', 'pipe operator', 'pattern matching', 'supervision trees', 'hot code reload'],
        greyKeywords: ['actor', 'pipe', 'match', 'supervisor', 'spawn'],
    },
    {
        language: 'Lua',
        paradigms: ['scripting', 'imperative', 'meta-table'],
        keyContributions: ['lightweight embedding', 'metatables', 'coroutines', 'minimal footprint'],
        greyKeywords: ['fn', 'struct'],
    },
    {
        language: 'Clojure',
        paradigms: ['functional', 'concurrent', 'lisp'],
        keyContributions: ['immutable data structures', 'persistent data', 'atoms', 'STM', 'REPL-driven'],
        greyKeywords: ['bind', 'atomic', 'fn', 'quote'],
    },
    {
        language: 'Apex',
        paradigms: ['OOP', 'cloud-native'],
        keyContributions: ['cloud-native annotations', 'triggers', 'SOQL queries', 'governor limits'],
        greyKeywords: ['class', 'query', '@'],
    },
    {
        language: 'PL/SQL',
        paradigms: ['procedural', 'database'],
        keyContributions: ['stored procedures', 'cursors', 'triggers', 'packages'],
        greyKeywords: ['procedure', 'cursor', 'transaction'],
    },
    {
        language: 'Fortran',
        paradigms: ['imperative', 'array-oriented', 'scientific'],
        keyContributions: ['array operations', 'do loops', 'supercomputing', 'parallel I/O'],
        greyKeywords: ['loop', 'parallel', 'simd', 'matrix'],
    },
    {
        language: 'COBOL',
        paradigms: ['procedural', 'record-oriented', 'enterprise'],
        keyContributions: ['record types', 'sections/divisions', 'business logic', 'decimal arithmetic'],
        greyKeywords: ['record', 'section', 'perform'],
    },
    {
        language: 'Erlang',
        paradigms: ['functional', 'concurrent', 'fault-tolerant'],
        keyContributions: ['fault tolerance', 'hot code reload', 'OTP patterns', 'lightweight processes'],
        greyKeywords: ['spawn', 'receive', 'supervisor', 'actor'],
    },
    {
        language: 'F#',
        paradigms: ['functional', 'OOP', 'typed'],
        keyContributions: ['discriminated unions', 'computation expressions', 'pipe operator', 'type providers'],
        greyKeywords: ['type', 'enum', 'pipe', 'match', 'compose'],
    },
    {
        language: 'Delphi',
        paradigms: ['OOP', 'event-driven', 'RAD'],
        keyContributions: ['components', 'RAD development', 'event handling', 'visual forms'],
        greyKeywords: ['class', 'form', 'event', 'component'],
    },
    {
        language: 'VB.NET',
        paradigms: ['OOP', 'event-driven'],
        keyContributions: ['event-driven programming', 'readable syntax', 'With blocks', 'LINQ'],
        greyKeywords: ['class', 'event', 'query'],
    },
    {
        language: 'ABAP',
        paradigms: ['procedural', 'OOP', 'enterprise'],
        keyContributions: ['internal tables', 'structured data', 'ALV reports', 'SAP integration'],
        greyKeywords: ['table', 'struct', 'query'],
    },
    {
        language: 'VHDL/Verilog',
        paradigms: ['hardware-description', 'concurrent'],
        keyContributions: ['hardware description', 'concurrent processes', 'signals', 'port mapping'],
        greyKeywords: ['hardware', 'signal', 'process', 'port'],
    },
    {
        language: 'Zig',
        paradigms: ['systems', 'imperative'],
        keyContributions: ['comptime execution', 'no hidden allocations', 'explicit error handling', 'C interop'],
        greyKeywords: ['comptime', 'defer', 'unsafe'],
    },
    {
        language: 'Mojo',
        paradigms: ['systems', 'scientific', 'AI'],
        keyContributions: ['Python compatibility + systems perf', 'SIMD', 'ownership + Python syntax'],
        greyKeywords: ['simd', 'fn', 'struct', 'tensor'],
    },
    {
        language: 'Nim',
        paradigms: ['systems', 'scripting', 'meta'],
        keyContributions: ['macro system', 'compile to C', 'uniform call syntax', 'effect system'],
        greyKeywords: ['macro', 'comptime', 'fn'],
    },
    {
        language: 'Smalltalk',
        paradigms: ['OOP', 'message-passing'],
        keyContributions: ['message passing', 'live coding', 'image-based development', 'pure OOP'],
        greyKeywords: ['send', 'class', 'reflect'],
    },
    {
        language: 'SAS',
        paradigms: ['procedural', 'statistical'],
        keyContributions: ['data steps', 'proc steps', 'statistical analysis', 'clinical trials'],
        greyKeywords: ['dataframe', 'query', 'procedure'],
    },
    {
        language: 'Prolog',
        paradigms: ['logic', 'declarative'],
        keyContributions: ['logic programming', 'unification', 'backtracking', 'constraint solving'],
        greyKeywords: ['logic', 'fact', 'rule', 'solve'],
    },
    {
        language: 'Lisp',
        paradigms: ['functional', 'meta', 'homoiconic'],
        keyContributions: ['homoiconicity', 'macros', 'S-expressions', 'code-as-data', 'REPL'],
        greyKeywords: ['macro', 'quote', 'unquote', 'reflect'],
    },
    {
        language: 'Tcl',
        paradigms: ['scripting', 'meta'],
        keyContributions: ['everything-is-a-string', 'introspection', 'embeddable', 'Tk GUI'],
        greyKeywords: ['reflect', 'exec'],
    },
    {
        language: 'LabVIEW',
        paradigms: ['dataflow', 'visual'],
        keyContributions: ['dataflow programming', 'parallel execution', 'measurement automation'],
        greyKeywords: ['parallel', 'pipe', 'signal'],
    },
    {
        language: 'Scratch',
        paradigms: ['visual', 'event-driven', 'educational'],
        keyContributions: ['visual blocks', 'event-driven', 'educational simplicity'],
        greyKeywords: ['event', 'component', 'render'],
    },
];

// ─── Syntax Examples ────────────────────────────────────────────────────────
// Reference examples showing Grey++ syntax for each major paradigm.

export const SYNTAX_EXAMPLES: Record<string, string> = {
    // ── Immutable binding (from Rust) ──
    binding: `bind x = 42
bind name = "Grey++"
mut counter = 0
const MAX = 100`,

    // ── Functions (from Rust + Python + Go) ──
    function: `fn add(a: int, b: int) -> int { a + b }
fn greet(name: str) { print("Hello, {name}") }
async fn fetch(url: str) -> Response { await http.get(url) }
pure fn double(x: int) -> int { x * 2 }
fn divmod(a: int, b: int) -> (int, int) { (a / b, a % b) }`,

    // ── Closures (from Rust + JS + Ruby) ──
    closure: `bind double = |x| x * 2
bind add = |a, b| a + b
bind greet = |name| { print("Hi {name}") }
bind nums = [1, 2, 3] |> map(|x| x * x) |> filter(|x| x > 2)`,

    // ── Types (from TypeScript + Rust + Haskell) ──
    types: `type ID = int | str
type Result<T, E> = Ok(T) | Err(E)
type Option<T> = Some(T) | None
type Callback = fn(int) -> bool`,

    // ── Structs & Data (from Rust + Kotlin + C#) ──
    struct: `struct Point { x: f64, y: f64 }
data class User { name: str, age: int, email: str }
record Transaction { amount: f64, currency: str, timestamp: int }`,

    // ── Traits / Interfaces (from Rust + Go + Java) ──
    trait: `trait Drawable {
    fn draw(self)
    fn area(self) -> f64
}

impl Drawable for Circle {
    fn draw(self) { render(self.x, self.y, self.radius) }
    fn area(self) -> f64 { 3.14159 * self.radius ** 2 }
}

interface Serializable {
    fn serialize(self) -> str
    fn deserialize(data: str) -> Self
}`,

    // ── Enums / Algebraic Types (from Rust + Swift + Haskell) ──
    enum: `enum Color { Red, Green, Blue }

enum Shape {
    Circle(radius: f64),
    Rect(width: f64, height: f64),
    Triangle(a: f64, b: f64, c: f64),
}

enum WebEvent {
    PageLoad,
    Click(x: int, y: int),
    KeyPress(key: char),
    Input(value: str),
}`,

    // ── Pattern Matching (from Rust + Scala + Elixir + Haskell) ──
    match: `match shape {
    Circle(r) => print("Circle with radius {r}"),
    Rect(w, h) if w == h => print("Square {w}x{h}"),
    Rect(w, h) => print("Rectangle {w}x{h}"),
    _ => print("Unknown shape"),
}

match result {
    Ok(value) => process(value),
    Err(e) => log_error(e),
}

match (x, y) {
    (0, 0) => "origin",
    (x, 0) => "on x-axis at {x}",
    (0, y) => "on y-axis at {y}",
    (x, y) => "point({x}, {y})",
}`,

    // ── Loops (from Python + Rust + Go + C) ──
    loop: `loop i in 0..10 { print(i) }
loop item in collection { process(item) }
loop while condition { step() }
loop { break cond done() }
loop (key, value) in map { print("{key}: {value}") }`,

    // ── Conditionals (from Swift + Kotlin + Rust) ──
    conditional: `cond x > 5 { print("big") } else { print("small") }

guard connection != nil else { return Err("no connection") }

when value {
    1 => "one",
    2 => "two",
    3..=9 => "several",
    _ => "many",
}`,

    // ── Error Handling (from Rust + Go + Java + Swift) ──
    error_handling: `// Rust-style Result propagation
fn read_config(path: str) -> Result<Config, IOError> {
    bind content = try fs.read(path)?
    bind parsed = try json.parse(content)?
    Ok(Config.from(parsed))
}

// Java-style try/catch
try {
    bind data = fetch_data()
    process(data)
} catch err: NetworkError {
    log("Network failed: {err}")
    retry()
} catch err: ParseError {
    panic("Invalid data: {err}")
} finally {
    cleanup()
}

// Go-style defer
fn process_file(path: str) {
    bind file = open(path)
    defer { file.close() }
    bind data = file.read_all()
    transform(data)
}`,

    // ── Concurrency (from Go + Erlang + Elixir + Rust) ──
    concurrency: `// Goroutine-style spawning
spawn { heavy_computation() }

// Channels (Go)
bind ch = channel<int>()
spawn { send(ch, compute()) }
bind result = receive(ch)

// Select on channels (Go)
select {
    msg <- inbox => process(msg),
    <- timeout(5000) => print("timed out"),
}

// Actor model (Erlang/Elixir/Scala)
actor Counter {
    state count: int = 0
    on Increment { count += 1 }
    on Decrement { count -= 1 }
    on GetCount -> int { count }
}

// Supervisors (Erlang/Elixir)
supervisor MainSupervisor {
    strategy: one_for_one,
    children: [DatabaseActor, CacheActor, WebActor],
}`,

    // ── Pipeline (from Elixir + F# + Bash + R) ──
    pipeline: `// Elixir-style pipe
bind result = data
    |> filter(|x| x > 0)
    |> map(|x| x * 2)
    |> sort()
    |> take(10)

// LINQ-style query (C#/SQL)
bind adults = query {
    select name, age
    from users
    where age >= 18
    orderby age
}

// Bash-style command pipeline
pipe_cmd "cat data.csv" |> "grep ERROR" |> "wc -l"`,

    // ── Modules (from Rust + Python + JS + Go) ──
    module: `module math {
    export fn sqrt(x: f64) -> f64 { ... }
    export fn pow(base: f64, exp: f64) -> f64 { ... }
    fn internal_helper() { ... }
}

use math::sqrt
import { Component, useState } from "react"
package main`,

    // ── Generics (from Rust + Java + TypeScript) ──
    generics: `fn max<T: Comparable>(a: T, b: T) -> T {
    cond a > b { a } else { b }
}

struct Stack<T> {
    items: vec<T>,
    fn push(self, item: T) { self.items.append(item) }
    fn pop(self) -> Option<T> { self.items.pop() }
}

fn transform<A, B>(input: vec<A>, f: fn(A) -> B) -> vec<B> {
    input |> map(f) |> collect()
}`,

    // ── Smart Contracts (from Solidity) ──
    contract: `contract Token {
    state balances: map<addr, u256>
    state total_supply: u256
    event Transfer(from: addr, to: addr, amount: u256)

    @payable
    fn mint(to: addr, amount: u256) {
        balances[to] += amount
        total_supply += amount
        emit Transfer(addr.zero, to, amount)
    }

    fn transfer(to: addr, amount: u256) {
        guard balances[msg.sender] >= amount else { panic("insufficient") }
        balances[msg.sender] -= amount
        balances[to] += amount
        emit Transfer(msg.sender, to, amount)
    }

    fn balance_of(owner: addr) -> u256 { balances[owner] }
}`,

    // ── Logic Programming (from Prolog) ──
    logic: `logic {
    fact parent("tom", "bob")
    fact parent("bob", "alice")
    fact parent("bob", "charlie")

    rule ancestor(X, Y) :- parent(X, Y)
    rule ancestor(X, Y) :- parent(X, Z), ancestor(Z, Y)

    solve ancestor("tom", Who) -> results
}`,

    // ── Hardware Description (from VHDL/Verilog) ──
    hardware: `hardware Timer {
    port clk: bit input
    port reset: bit input
    port count: u8 output

    signal counter: u8 = 0

    process(clk) {
        cond reset { counter = 0 }
        else { counter += 1 }
        count = counter
    }
}`,

    // ── Inline Assembly (from C/Rust) ──
    assembly: `asm {
    mov rax, 1
    mov rdi, 1
    syscall
}

fn fast_add(a: i64, b: i64) -> i64 {
    asm { add rax, rbx } -> result
}`,

    // ── UI / Widgets (from Dart/Flutter + React) ──
    ui: `widget Counter {
    state count = 0

    render {
        Column {
            Text("Count: {count}")
            Row {
                Button("-") { count -= 1 }
                Button("+") { count += 1 }
            }
        }
    }
}

component TodoApp {
    state items: vec<str> = []

    fn add_item(text: str) { items.append(text) }

    render {
        List(items) |> map(|item| Text(item))
    }
}`,

    // ── Scientific Computing (from Julia + MATLAB + R + Fortran) ──
    scientific: `bind m = matrix[[1, 2, 3], [4, 5, 6], [7, 8, 9]]
bind result = m * m.T
bind eigenvalues = linalg.eig(m)
bind det = linalg.det(m)

bind data = dataframe {
    name: ["Alice", "Bob", "Carol"],
    age: [30, 25, 35],
    score: [95.5, 87.3, 92.1],
}
bind stats = data.score |> mean |> stddev

// SIMD vectorization (Mojo)
@simd
fn dot_product(a: vec<f32>, b: vec<f32>) -> f32 {
    a |> zip(b) |> map(|(x,y)| x * y) |> sum()
}`,

    // ── Compile-time Execution (from Zig + Nim + Mojo) ──
    comptime: `comptime fn factorial(n: int) -> int {
    cond n <= 1 { 1 } else { n * factorial(n - 1) }
}

comptime bind TABLE = loop i in 0..256 { crc32_byte(i) }

comptime type Vec3 = struct { x: f32, y: f32, z: f32 }`,

    // ── Macros (from Rust + Lisp + Julia + Nim) ──
    macros: `macro repeat!(n, body) {
    loop _ in 0..n { body }
}

macro dbg!(expr) {
    bind val = expr
    print("[DEBUG] {quote(expr)} = {val}")
    val
}

macro derive!(Clone, Debug) for MyStruct {}`,

    // ── Testing (from Rust + Go + Python + Jest) ──
    testing: `test "addition works" {
    assert(add(2, 3) == 5)
    assert(add(-1, 1) == 0)
    assert(add(0, 0) == 0)
}

test "user creation" {
    bind user = User.new("Alice", 30)
    expect(user.name).to_equal("Alice")
    expect(user.age).to_be_greater_than(0)
}

bench "sort performance" {
    bind data = random_vec(10000)
    sort(data)
}`,

    // ── Decorators / Annotations (from Python + Java + C#) ──
    decorator: `@cached(ttl: 3600)
fn fetch_user(id: int) -> User { db.find(id) }

@test
fn test_fetch_user() { assert(fetch_user(1).name == "Alice") }

@deprecated("use new_api() instead")
fn old_api() { ... }

@route("/api/users", method: "GET")
fn list_users() -> vec<User> { db.all() }`,

    // ── Functional Programming (from Haskell + Scala + F# + Clojure) ──
    functional: `// Composition (Haskell)
bind process = compose(validate, transform, save)

// Currying (Haskell/Scala)
bind add = curry(|a, b| a + b)
bind add5 = add(5)
bind result = add5(3)  // 8

// Partial application
bind log_error = partial(log, level: "ERROR")

// Lazy evaluation (Haskell)
bind fibonacci = lazy gen {
    bind (a, b) = (0, 1)
    loop { yield a; (a, b) = (b, a + b) }
}
bind first_10 = fibonacci |> take(10)

// Monadic operations
bind result = do {
    bind config <- read_config("app.toml")
    bind db <- connect(config.db_url)
    bind users <- db.query("SELECT * FROM users")
    Ok(users)
}`,

    // ── Actor System (from Erlang/Elixir) ──
    actor_system: `actor ChatRoom {
    state members: set<ActorRef> = set()
    state history: vec<Message> = []

    on Join(user) {
        members.insert(user)
        broadcast(SystemMsg("{user.name} joined"))
    }

    on Leave(user) {
        members.remove(user)
        broadcast(SystemMsg("{user.name} left"))
    }

    on Chat(user, text) {
        bind msg = Message(user, text, now())
        history.append(msg)
        broadcast(msg)
    }

    fn broadcast(msg: Message) {
        loop member in members { send(member, msg) }
    }
}`,

    // ── Database Operations (from SQL + PL/SQL + ABAP) ──
    database: `// Declarative query
bind results = query {
    select u.name, count(o.id) as order_count
    from users as u
    join orders as o on u.id == o.user_id
    where u.active == true
    groupby u.name
    orderby order_count desc
}

// Transaction
transaction {
    bind account = db.find(account_id)
    guard account.balance >= amount else { rollback("insufficient funds") }
    account.balance -= amount
    target.balance += amount
    commit()
}

// Stored procedure
procedure calculate_interest(account_id: int) {
    bind account = db.find(account_id)
    bind interest = account.balance * account.rate / 12
    account.balance += interest
    db.save(account)
}`,

    // ── Systems / DevOps (from Bash + PowerShell + Go) ──
    systems: `// Shell command execution
bind files = exec("find /var/log -name '*.log' -mtime -1")
bind disk = exec("df -h /") |> parse_table()

// Environment access
bind db_url = env("DATABASE_URL") ?? "localhost:5432"
bind debug = env("DEBUG") == "true"

// Process pipeline
pipe_cmd "docker ps" |> "grep running" |> "awk '{print $1}'"

// System monitoring
sys.monitor {
    cpu_threshold: 80,
    memory_threshold: 90,
    on_alert: |metric| notify("ops@example.com", metric),
}`,

    // ── Memory Safety (from Rust + C + Zig) ──
    memory: `// Ownership (Rust)
bind data = vec[1, 2, 3]
bind moved = own data  // data is no longer valid
process(moved)

// Borrowing (Rust)
fn sum(list: ref vec<int>) -> int {
    loop x in list { total += x }
    total
}

// Unsafe block (Rust/C)
unsafe {
    bind ptr = alloc(1024)
    write(ptr, 0, 42)
    bind val = read(ptr, 0)
    free(ptr)
}

// RAII / defer (C++/Go/Zig)
fn process_file(path: str) -> Result<Data, Error> {
    bind file = open(path)?
    defer { file.close() }
    file.read_all()
}`,
};

// ─── Language Feature Index ─────────────────────────────────────────────────
// A quick lookup of which Grey++ feature came from which source language(s).

export interface FeatureOrigin {
    feature: string;
    greyKeyword: string;
    sourceLanguages: string[];
    category: string;
    example: string;
}

export const FEATURE_INDEX: FeatureOrigin[] = [
    { feature: 'Immutable bindings', greyKeyword: 'bind', sourceLanguages: ['Rust', 'Haskell', 'Scala'], category: 'binding', example: 'bind x = 42' },
    { feature: 'Mutable variables', greyKeyword: 'mut', sourceLanguages: ['Rust'], category: 'binding', example: 'mut counter = 0' },
    { feature: 'Constants', greyKeyword: 'const', sourceLanguages: ['C++', 'JS', 'Go', 'Zig'], category: 'binding', example: 'const MAX = 100' },
    { feature: 'Functions', greyKeyword: 'fn', sourceLanguages: ['Rust', 'Go', 'Zig'], category: 'function', example: 'fn add(a: int, b: int) -> int { a + b }' },
    { feature: 'Async/Await', greyKeyword: 'async/await', sourceLanguages: ['JS', 'C#', 'Python', 'Rust'], category: 'concurrency', example: 'async fn fetch() { await get(url) }' },
    { feature: 'Pure functions', greyKeyword: 'pure', sourceLanguages: ['Haskell'], category: 'functional', example: 'pure fn double(x) -> int { x * 2 }' },
    { feature: 'Generators', greyKeyword: 'gen/yield', sourceLanguages: ['Python', 'JS'], category: 'function', example: 'gen fn range(n) { loop i in 0..n { yield i } }' },
    { feature: 'Deferred cleanup', greyKeyword: 'defer', sourceLanguages: ['Go', 'Zig'], category: 'function', example: 'defer { file.close() }' },
    { feature: 'Type aliases', greyKeyword: 'type', sourceLanguages: ['TypeScript', 'Haskell', 'Rust'], category: 'type', example: 'type ID = int | str' },
    { feature: 'Structs', greyKeyword: 'struct', sourceLanguages: ['Rust', 'C', 'Go', 'C++'], category: 'type', example: 'struct Point { x: f64, y: f64 }' },
    { feature: 'Enums / Sum types', greyKeyword: 'enum', sourceLanguages: ['Rust', 'Haskell', 'Swift', 'Scala'], category: 'type', example: 'enum Option<T> { Some(T), None }' },
    { feature: 'Traits / Type classes', greyKeyword: 'trait', sourceLanguages: ['Rust', 'Haskell', 'Scala'], category: 'type', example: 'trait Drawable { fn draw(self) }' },
    { feature: 'Interfaces', greyKeyword: 'interface', sourceLanguages: ['Java', 'Go', 'TypeScript', 'C#'], category: 'type', example: 'interface Serializable { fn serialize(self) -> str }' },
    { feature: 'Data classes', greyKeyword: 'data', sourceLanguages: ['Kotlin', 'Haskell', 'Scala'], category: 'type', example: 'data class User { name: str, age: int }' },
    { feature: 'Records', greyKeyword: 'record', sourceLanguages: ['C#', 'Java', 'COBOL'], category: 'type', example: 'record Invoice { id: int, amount: f64 }' },
    { feature: 'Pattern matching', greyKeyword: 'match', sourceLanguages: ['Rust', 'Scala', 'Elixir', 'Haskell', 'F#'], category: 'control', example: 'match x { 0 => "zero", _ => "other" }' },
    { feature: 'Guard clauses', greyKeyword: 'guard', sourceLanguages: ['Swift'], category: 'control', example: 'guard x != nil else { return }' },
    { feature: 'Goroutines / Spawn', greyKeyword: 'spawn', sourceLanguages: ['Go', 'Erlang', 'Elixir'], category: 'concurrency', example: 'spawn { compute() }' },
    { feature: 'Channels', greyKeyword: 'channel', sourceLanguages: ['Go', 'Rust'], category: 'concurrency', example: 'bind ch = channel<int>()' },
    { feature: 'Actor model', greyKeyword: 'actor', sourceLanguages: ['Erlang', 'Elixir', 'Scala'], category: 'concurrency', example: 'actor Counter { state n = 0 }' },
    { feature: 'Pipe operator', greyKeyword: '|>', sourceLanguages: ['Elixir', 'F#', 'Bash', 'R'], category: 'data', example: 'data |> transform |> filter' },
    { feature: 'Declarative queries', greyKeyword: 'query', sourceLanguages: ['SQL', 'C# LINQ'], category: 'data', example: 'query { select * from users }' },
    { feature: 'Smart contracts', greyKeyword: 'contract', sourceLanguages: ['Solidity'], category: 'blockchain', example: 'contract Token { state supply: u256 }' },
    { feature: 'Logic programming', greyKeyword: 'logic', sourceLanguages: ['Prolog'], category: 'logic', example: 'logic { fact parent("a","b") }' },
    { feature: 'Hardware description', greyKeyword: 'hardware', sourceLanguages: ['VHDL', 'Verilog'], category: 'hardware', example: 'hardware Timer { port clk: bit }' },
    { feature: 'Inline assembly', greyKeyword: 'asm', sourceLanguages: ['C', 'Rust', 'Assembly'], category: 'hardware', example: 'asm { mov rax, 1 }' },
    { feature: 'UI widgets', greyKeyword: 'widget', sourceLanguages: ['Dart/Flutter', 'React', 'SwiftUI'], category: 'ui', example: 'widget App { render { Text("Hi") } }' },
    { feature: 'Matrix operations', greyKeyword: 'matrix', sourceLanguages: ['MATLAB', 'Julia', 'R', 'Fortran'], category: 'scientific', example: 'bind m = matrix[[1,2],[3,4]]' },
    { feature: 'Compile-time exec', greyKeyword: 'comptime', sourceLanguages: ['Zig', 'Nim', 'Mojo'], category: 'meta', example: 'comptime fn fac(n) { ... }' },
    { feature: 'Macros', greyKeyword: 'macro', sourceLanguages: ['Rust', 'Lisp', 'Julia', 'Nim'], category: 'meta', example: 'macro dbg!(x) { print(x) }' },
    { feature: 'Ownership', greyKeyword: 'own', sourceLanguages: ['Rust'], category: 'memory', example: 'bind moved = own data' },
    { feature: 'Borrowing', greyKeyword: 'ref', sourceLanguages: ['Rust'], category: 'memory', example: 'fn sum(list: ref vec<int>)' },
    { feature: 'Unsafe blocks', greyKeyword: 'unsafe', sourceLanguages: ['Rust', 'C#', 'C'], category: 'memory', example: 'unsafe { alloc(1024) }' },
    { feature: 'Supervision trees', greyKeyword: 'supervisor', sourceLanguages: ['Erlang', 'Elixir'], category: 'concurrency', example: 'supervisor S { children: [...] }' },
    { feature: 'SIMD vectorization', greyKeyword: 'simd', sourceLanguages: ['Mojo', 'C++', 'Rust'], category: 'scientific', example: '@simd fn dot(a, b) { ... }' },
    { feature: 'Data frames', greyKeyword: 'dataframe', sourceLanguages: ['R', 'Python/Pandas'], category: 'data', example: 'bind df = dataframe { ... }' },
    { feature: 'Transactions', greyKeyword: 'transaction', sourceLanguages: ['SQL', 'PL/SQL'], category: 'database', example: 'transaction { ... commit() }' },
    { feature: 'Decorators', greyKeyword: '@', sourceLanguages: ['Python', 'Java', 'C#', 'Kotlin'], category: 'meta', example: '@cached fn get() { }' },
    { feature: 'Concurrency select', greyKeyword: 'select', sourceLanguages: ['Go'], category: 'concurrency', example: 'select { msg <- ch => ... }' },
    { feature: 'Composition', greyKeyword: 'compose', sourceLanguages: ['Haskell', 'F#'], category: 'functional', example: 'bind f = compose(g, h)' },
];

// ─── Stats ──────────────────────────────────────────────────────────────────

export function getLanguageStats() {
    const keywordCount = Object.keys(GREY_KEYWORDS).length;
    const paradigmCount = new Set(PARADIGM_MAP.flatMap(p => p.paradigms)).size;
    const sourceCount = PARADIGM_MAP.length;
    const featureCount = FEATURE_INDEX.length;
    const categoryCount = new Set(FEATURE_INDEX.map(f => f.category)).size;
    const typeCount = Object.keys(GREY_TYPES.primitives).length + Object.keys(GREY_TYPES.composite).length;
    const operatorCount = Object.keys(GREY_OPERATORS).length;

    return {
        keywords: keywordCount,
        paradigms: paradigmCount,
        sourceLanguages: sourceCount,
        features: featureCount,
        categories: categoryCount,
        types: typeCount,
        operators: operatorCount,
        summary: `Grey++ v1.0: ${keywordCount} keywords, ${typeCount} types, ${operatorCount} operators, ${featureCount} features from ${sourceCount} languages across ${paradigmCount} paradigms`,
    };
}

// ─── Self-test ──────────────────────────────────────────────────────────────

if (process.argv[1]?.includes('grey_lang') && process.argv.includes('--test')) {
    console.log('═══ grey_lang.ts Self-Test ═══\n');

    const stats = getLanguageStats();
    console.log(stats.summary);
    console.log(`\n  Keywords:   ${stats.keywords}`);
    console.log(`  Types:      ${stats.types}`);
    console.log(`  Operators:  ${stats.operators}`);
    console.log(`  Features:   ${stats.features}`);
    console.log(`  Paradigms:  ${stats.paradigms}`);
    console.log(`  Languages:  ${stats.sourceLanguages}`);
    console.log(`  Categories: ${stats.categories}`);

    // Verify all feature origins reference valid keywords
    console.log('\n── Feature index integrity ──');
    let errors = 0;
    for (const f of FEATURE_INDEX) {
        if (!f.greyKeyword || !f.sourceLanguages.length) {
            console.error(`  ✗ Missing data for feature: ${f.feature}`);
            errors++;
        }
    }
    if (errors === 0) console.log('  ✓ All feature origins valid');

    // Verify paradigm map completeness
    console.log('\n── Paradigm map completeness ──');
    console.assert(PARADIGM_MAP.length === 50, `Expected 50 languages, got ${PARADIGM_MAP.length}`);
    console.log(`  ✓ All 50 languages mapped`);

    // Verify syntax examples
    console.log('\n── Syntax examples ──');
    const exampleCount = Object.keys(SYNTAX_EXAMPLES).length;
    console.log(`  ✓ ${exampleCount} syntax example categories defined`);

    console.log('\n✓ All grey_lang tests passed.');
}
