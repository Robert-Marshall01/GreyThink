// ─── Grey++ Translation Layer ────────────────────────────────────────────────
// Maps Grey++ NormalizedNode back to language-specific code for ALL 50+
// unified languages.  Stub-only — produces syntactically representative
// code fragments, not full compilation output.
//
// Self-test:  npx tsx src/ast/grey_translate.ts --test

import type {
    NormalizedNode,
    NormBindNode,
    NormFnNode,
    NormLoopNode,
    NormCondNode,
    NormStructNode,
    NormModuleNode,
    NormExprNode,
    NormBlockNode,
} from './grey_ast.js';

// ─── Types ──────────────────────────────────────────────────────────────────

export type TargetLanguage =
    // Group 1: Core Application & Systems
    | 'python' | 'javascript' | 'typescript' | 'java' | 'go' | 'rust' | 'csharp' | 'cpp' | 'swift' | 'kotlin'
    // Group 2: Data & Infrastructure
    | 'sql' | 'ruby' | 'php' | 'bash' | 'powershell' | 'r' | 'scala' | 'julia' | 'dart' | 'objc'
    // Group 3: Functional & Niche
    | 'elixir' | 'clojure' | 'haskell' | 'erlang' | 'fsharp' | 'ocaml' | 'lisp' | 'prolog' | 'solidity' | 'zig'
    // Group 4: Low-Level, Embedded & Legacy
    | 'c' | 'fortran' | 'vhdl' | 'matlab' | 'pascal' | 'perl' | 'apex'
    // Group 5: Modern & Emerging
    | 'mojo' | 'lua' | 'groovy' | 'elm' | 'nim' | 'crystal' | 'd' | 'vbnet' | 'smalltalk'
    // Grey++
    | 'grey';

/** All supported target languages for translateAll() */
export const ALL_TARGET_LANGUAGES: TargetLanguage[] = [
    'python', 'javascript', 'typescript', 'java', 'go', 'rust', 'csharp', 'cpp', 'swift', 'kotlin',
    'sql', 'ruby', 'php', 'bash', 'powershell', 'r', 'scala', 'julia', 'dart', 'objc',
    'elixir', 'clojure', 'haskell', 'erlang', 'fsharp', 'ocaml', 'lisp', 'prolog', 'solidity', 'zig',
    'c', 'fortran', 'vhdl', 'matlab', 'pascal', 'perl', 'apex',
    'mojo', 'lua', 'groovy', 'elm', 'nim', 'crystal', 'd', 'vbnet', 'smalltalk',
    'grey',
];

// ─── Language Family Type ───────────────────────────────────────────────────
type LangFamily = 'clike' | 'pylike' | 'mllike' | 'lisplike' | 'shelllike' | 'other';

interface LangTraits {
    printCall: string;
    terminator: string;
    family: LangFamily;
}

const LANG_TRAITS: Record<TargetLanguage, LangTraits> = {
    python: { printCall: 'print', terminator: '', family: 'pylike' },
    javascript: { printCall: 'console.log', terminator: ';', family: 'clike' },
    typescript: { printCall: 'console.log', terminator: ';', family: 'clike' },
    java: { printCall: 'System.out.println', terminator: ';', family: 'clike' },
    go: { printCall: 'fmt.Println', terminator: '', family: 'clike' },
    rust: { printCall: 'println!', terminator: ';', family: 'clike' },
    csharp: { printCall: 'Console.WriteLine', terminator: ';', family: 'clike' },
    cpp: { printCall: 'std::cout <<', terminator: ';', family: 'clike' },
    swift: { printCall: 'print', terminator: '', family: 'clike' },
    kotlin: { printCall: 'println', terminator: '', family: 'clike' },
    sql: { printCall: 'PRINT', terminator: ';', family: 'other' },
    ruby: { printCall: 'puts', terminator: '', family: 'pylike' },
    php: { printCall: 'echo', terminator: ';', family: 'clike' },
    bash: { printCall: 'echo', terminator: '', family: 'shelllike' },
    powershell: { printCall: 'Write-Output', terminator: '', family: 'shelllike' },
    r: { printCall: 'print', terminator: '', family: 'other' },
    scala: { printCall: 'println', terminator: '', family: 'clike' },
    julia: { printCall: 'println', terminator: '', family: 'pylike' },
    dart: { printCall: 'print', terminator: ';', family: 'clike' },
    objc: { printCall: 'NSLog', terminator: ';', family: 'clike' },
    elixir: { printCall: 'IO.puts', terminator: '', family: 'pylike' },
    clojure: { printCall: 'println', terminator: '', family: 'lisplike' },
    haskell: { printCall: 'putStrLn', terminator: '', family: 'mllike' },
    erlang: { printCall: 'io:format', terminator: '.', family: 'other' },
    fsharp: { printCall: 'printfn', terminator: '', family: 'mllike' },
    ocaml: { printCall: 'print_endline', terminator: '', family: 'mllike' },
    lisp: { printCall: 'format t', terminator: '', family: 'lisplike' },
    prolog: { printCall: 'write', terminator: '.', family: 'other' },
    solidity: { printCall: 'emit', terminator: ';', family: 'clike' },
    zig: { printCall: 'std.debug.print', terminator: ';', family: 'clike' },
    c: { printCall: 'printf', terminator: ';', family: 'clike' },
    fortran: { printCall: 'print *,', terminator: '', family: 'other' },
    vhdl: { printCall: 'report', terminator: ';', family: 'other' },
    matlab: { printCall: 'disp', terminator: ';', family: 'other' },
    pascal: { printCall: 'WriteLn', terminator: ';', family: 'other' },
    perl: { printCall: 'print', terminator: ';', family: 'clike' },
    apex: { printCall: 'System.debug', terminator: ';', family: 'clike' },
    mojo: { printCall: 'print', terminator: '', family: 'pylike' },
    lua: { printCall: 'print', terminator: '', family: 'other' },
    groovy: { printCall: 'println', terminator: '', family: 'clike' },
    elm: { printCall: 'Debug.log', terminator: '', family: 'mllike' },
    nim: { printCall: 'echo', terminator: '', family: 'pylike' },
    crystal: { printCall: 'puts', terminator: '', family: 'pylike' },
    d: { printCall: 'writeln', terminator: ';', family: 'clike' },
    vbnet: { printCall: 'Console.WriteLine', terminator: '', family: 'other' },
    smalltalk: { printCall: 'Transcript show:', terminator: '.', family: 'other' },
    grey: { printCall: 'print', terminator: '', family: 'clike' },
};

export interface TranslationOutput {
    _type: 'TranslationOutput';
    target: TargetLanguage;
    code: string;
    sourceLang: string;
    construct: string;
    meta: { stub: boolean };
}

// ═══════════════════════════════════════════════════════════════════════════
//  Translation Class
// ═══════════════════════════════════════════════════════════════════════════

export class Translation {

    /**
     * Map Grey++ universal `print(...)` to language-specific output calls.
     * Supports all 50+ unified languages via trait lookup.
     */
    private mapPrint(body: string, target: TargetLanguage): string {
        const pc = LANG_TRAITS[target].printCall;
        if (pc === 'print') return body;
        if (pc === 'std::cout <<') {
            return body.replace(/\bprint\s*\(("(?:[^"\\]|\\.)*"|'(?:[^'\\]|\\.)*'|[^)]+)\)/g,
                (_, arg) => `std::cout << ${arg.trim()}`);
        }
        if (pc === 'println!') {
            return body.replace(/\bprint\s*\((.+?)\)/g, (_, arg) => `println!(${arg.trim()})`);
        }
        if (pc === 'printf') {
            return body.replace(/\bprint\s*\((.+?)\)/g, (_, arg) => `printf(${arg.trim()})`);
        }
        if (pc === 'format t') {
            return body.replace(/\bprint\s*\((.+?)\)/g, (_, arg) => `(format t ${arg.trim()})`);
        }
        if (pc === 'io:format') {
            return body.replace(/\bprint\s*\((.+?)\)/g, (_, arg) => `io:format(${arg.trim()})`);
        }
        if (pc === 'print *,') {
            return body.replace(/\bprint\s*\((.+?)\)/g, (_, arg) => `print *, ${arg.trim()}`);
        }
        if (pc === 'Transcript show:') {
            return body.replace(/\bprint\s*\((.+?)\)/g, (_, arg) => `Transcript show: ${arg.trim()}`);
        }
        return body.replace(/\bprint\s*\(/g, `${pc}(`);
    }

    /** Translate a NormalizedNode to target language code. */
    translate(node: NormalizedNode, target: TargetLanguage): TranslationOutput {
        let code: string;

        switch (node.nkind) {
            case 'NormBind': code = this.translateBind(node, target); break;
            case 'NormFn': code = this.translateFn(node, target); break;
            case 'NormLoop': code = this.translateLoop(node, target); break;
            case 'NormCond': code = this.translateCond(node, target); break;
            case 'NormStruct': code = this.translateStruct(node, target); break;
            case 'NormModule': code = this.translateModule(node, target); break;
            case 'NormExpr': code = this.translateExpr(node, target); break;
            case 'NormBlock': code = this.translateBlock(node, target); break;
            default: code = `// unsupported construct`;
        }

        return {
            _type: 'TranslationOutput',
            target,
            code,
            sourceLang: node.sourceLang,
            construct: node.nkind,
            meta: { stub: true },
        };
    }

    /** Translate NormalizedNode to all supported targets. */
    translateAll(node: NormalizedNode): TranslationOutput[] {
        return ALL_TARGET_LANGUAGES.map(t => this.translate(node, t));
    }

    /** Translate to a specific language group. */
    translateGroup(node: NormalizedNode, group: 1 | 2 | 3 | 4 | 5): TranslationOutput[] {
        const groups: Record<number, TargetLanguage[]> = {
            1: ['python', 'javascript', 'typescript', 'java', 'go', 'rust', 'csharp', 'cpp', 'swift', 'kotlin'],
            2: ['sql', 'ruby', 'php', 'bash', 'powershell', 'r', 'scala', 'julia', 'dart', 'objc'],
            3: ['elixir', 'clojure', 'haskell', 'erlang', 'fsharp', 'ocaml', 'lisp', 'prolog', 'solidity', 'zig'],
            4: ['c', 'fortran', 'vhdl', 'matlab', 'pascal', 'perl', 'apex'],
            5: ['mojo', 'lua', 'groovy', 'elm', 'nim', 'crystal', 'd', 'vbnet', 'smalltalk'],
        };
        return (groups[group] || []).map(t => this.translate(node, t));
    }

    // ── bind → variable declaration ─────────────────────────────────────

    private translateBind(node: NormBindNode, target: TargetLanguage): string {
        const v = node.value;
        const n = node.name;

        switch (target) {
            case 'python': return `${n} = ${v}`;
            case 'javascript': case 'typescript':
                return `${node.mutable ? 'let' : 'const'} ${n} = ${v};`;
            case 'java': return `${node.mutable ? '' : 'final '}${node.typeAnnotation ?? 'var'} ${n} = ${v};`;
            case 'go': return node.mutable ? `${n} := ${v}` : `const ${n} = ${v}`;
            case 'rust': return `let ${node.mutable ? 'mut ' : ''}${n}${node.typeAnnotation ? ': ' + node.typeAnnotation : ''} = ${v};`;
            case 'csharp': return `${node.mutable ? 'var' : 'readonly var'} ${n} = ${v};`;
            case 'cpp': return `${node.typeAnnotation ?? 'auto'} ${n} = ${v};`;
            case 'swift': return `${node.mutable ? 'var' : 'let'} ${n} = ${v}`;
            case 'kotlin': return `${node.mutable ? 'var' : 'val'} ${n} = ${v}`;
            case 'sql': return `DECLARE @${n} = ${v};`;
            case 'ruby': return `${n} = ${v}`;
            case 'php': return `$${n} = ${v};`;
            case 'bash': return `${n}=${v}`;
            case 'powershell': return `$${n} = ${v}`;
            case 'r': return `${n} <- ${v}`;
            case 'scala': return `${node.mutable ? 'var' : 'val'} ${n} = ${v}`;
            case 'julia': return `${node.mutable ? '' : 'const '}${n} = ${v}`;
            case 'dart': return `${node.mutable ? 'var' : 'final'} ${n} = ${v};`;
            case 'objc': return `${node.typeAnnotation ?? 'id'} ${n} = ${v};`;
            case 'elixir': return `${n} = ${v}`;
            case 'clojure': return `(def ${n} ${v})`;
            case 'haskell': return `${n} = ${v}`;
            case 'erlang': return `${n} = ${v}.`;
            case 'fsharp': return `let ${node.mutable ? 'mutable ' : ''}${n} = ${v}`;
            case 'ocaml': return `let ${n} = ${v}`;
            case 'lisp': return `(${node.mutable ? 'defvar' : 'defconstant'} ${n} ${v})`;
            case 'prolog': return `${n}(${v}).`;
            case 'solidity': return `${node.typeAnnotation ?? 'uint256'} ${n} = ${v};`;
            case 'zig': return `${node.mutable ? 'var' : 'const'} ${n} = ${v};`;
            case 'c': return `${node.typeAnnotation ?? 'int'} ${n} = ${v};`;
            case 'fortran': return `${node.typeAnnotation ?? 'integer'} :: ${n} = ${v}`;
            case 'vhdl': return `signal ${n} : ${node.typeAnnotation ?? 'std_logic'} := ${v};`;
            case 'matlab': return `${n} = ${v};`;
            case 'pascal': return `${node.mutable ? 'var' : 'const'} ${n} = ${v};`;
            case 'perl': return `my $${n} = ${v};`;
            case 'apex': return `${node.typeAnnotation ?? 'Object'} ${n} = ${v};`;
            case 'mojo': return `${node.mutable ? 'var' : 'let'} ${n} = ${v}`;
            case 'lua': return `local ${n} = ${v}`;
            case 'groovy': return `${node.mutable ? 'def' : 'final'} ${n} = ${v}`;
            case 'elm': return `${n} = ${v}`;
            case 'nim': return `${node.mutable ? 'var' : 'let'} ${n} = ${v}`;
            case 'crystal': return `${n} = ${v}`;
            case 'd': return `${node.mutable ? 'auto' : 'immutable'} ${n} = ${v};`;
            case 'vbnet': return `Dim ${n} = ${v}`;
            case 'smalltalk': return `${n} := ${v}.`;
            case 'grey': return `bind ${n} = ${v}`;
        }
    }

    // ── fn → function definition ────────────────────────────────────────

    private translateFn(node: NormFnNode, target: TargetLanguage): string {
        const name = node.name ?? 'anonymous';
        const params = node.params.join(', ');
        const body = this.mapPrint(node.body, target);
        const ret = node.returnType ?? 'void';
        const bSemi = body.endsWith(';') ? body : body + ';';

        switch (target) {
            case 'python': return `def ${name}(${params}): ${body}`;
            case 'javascript': return `function ${name}(${params}){ ${body}; }`;
            case 'typescript': return `function ${name}(${params}): ${ret} { ${body}; }`;
            case 'java': return `${ret} ${name}(${params}) { ${bSemi} }`;
            case 'go': return `func ${name}(${params}) { ${body} }`;
            case 'rust': return `fn ${name}(${params})${ret !== 'void' ? ' -> ' + ret : ''} { ${bSemi} }`;
            case 'csharp': return `${ret} ${name}(${params}) { ${bSemi} }`;
            case 'cpp': return `${ret} ${name}(${params}){ ${bSemi} }`;
            case 'swift': return `func ${name}(${params})${ret !== 'void' ? ' -> ' + ret : ''} { ${body} }`;
            case 'kotlin': return `fun ${name}(${params})${ret !== 'void' ? ': ' + ret : ''} { ${body} }`;
            case 'sql': return `CREATE FUNCTION ${name}(${params}) RETURNS ${ret} AS BEGIN ${body}; END;`;
            case 'ruby': return `def ${name}(${params})\n  ${body}\nend`;
            case 'php': return `function ${name}(${params}) { ${bSemi} }`;
            case 'bash': return `${name}() { ${body}; }`;
            case 'powershell': return `function ${name}(${params ? '$' + params.replace(/,\s*/g, ', $') : ''}) { ${body} }`;
            case 'r': return `${name} <- function(${params}) { ${body} }`;
            case 'scala': return `def ${name}(${params}) = { ${body} }`;
            case 'julia': return `function ${name}(${params})\n  ${body}\nend`;
            case 'dart': return `${ret} ${name}(${params}) { ${bSemi} }`;
            case 'objc': return `- (${ret})${name} { ${bSemi} }`;
            case 'elixir': return `def ${name}(${params}) do\n  ${body}\nend`;
            case 'clojure': return `(defn ${name} [${params.replace(/,\s*/g, ' ')}] ${body})`;
            case 'haskell': return `${name} ${params.replace(/,\s*/g, ' ')} = ${body}`;
            case 'erlang': return `${name}(${params}) -> ${body}.`;
            case 'fsharp': return `let ${name} ${params.replace(/,\s*/g, ' ')} = ${body}`;
            case 'ocaml': return `let ${name} ${params.replace(/,\s*/g, ' ')} = ${body}`;
            case 'lisp': return `(defun ${name} (${params.replace(/,\s*/g, ' ')}) ${body})`;
            case 'prolog': return `${name}(${params}) :- ${body}.`;
            case 'solidity': return `function ${name}(${params}) public { ${bSemi} }`;
            case 'zig': return `fn ${name}(${params}) void { ${bSemi} }`;
            case 'c': return `${ret} ${name}(${params}) { ${bSemi} }`;
            case 'fortran': return `subroutine ${name}(${params})\n  ${body}\nend subroutine`;
            case 'vhdl': return `process(${params}) begin ${body}; end process;`;
            case 'matlab': return `function ${name}(${params})\n  ${body}\nend`;
            case 'pascal': return `procedure ${name}(${params}); begin ${bSemi} end;`;
            case 'perl': return `sub ${name} { ${bSemi} }`;
            case 'apex': return `${ret} ${name}(${params}) { ${bSemi} }`;
            case 'mojo': return `fn ${name}(${params}): ${body}`;
            case 'lua': return `function ${name}(${params})\n  ${body}\nend`;
            case 'groovy': return `def ${name}(${params}) { ${body} }`;
            case 'elm': return `${name} ${params.replace(/,\s*/g, ' ')} = ${body}`;
            case 'nim': return `proc ${name}(${params}) = ${body}`;
            case 'crystal': return `def ${name}(${params})\n  ${body}\nend`;
            case 'd': return `${ret} ${name}(${params}) { ${bSemi} }`;
            case 'vbnet': return `Sub ${name}(${params})\n  ${body}\nEnd Sub`;
            case 'smalltalk': return `${name}\n  ${body}`;
            case 'grey': return `fn ${name}(${params}) { ${body} }`;
            default: return `// fn ${name}(${params}) = ${body}`;
        }
    }

    // ── loop → iteration construct ──────────────────────────────────────

    private translateLoop(node: NormLoopNode, target: TargetLanguage): string {
        const body = this.mapPrint(node.body, target);
        const b = node.binding ?? '_';
        const iter = node.iterable ?? '...';
        const bSemi = body.endsWith(';') ? body : body + ';';
        const rangeMatch = node.iterable?.match(/^range\((\d+),(\w+|\d+)\)$/);

        if (node.variant === 'for') {
            switch (target) {
                case 'python': return `for ${b} in ${iter}: ${body}`;
                case 'javascript': case 'typescript':
                    if (rangeMatch) return `for(let ${b}=${rangeMatch[1]};${b}<${rangeMatch[2]};${b}++){ ${body}; }`;
                    return `for (const ${b} of ${iter}) { ${body} }`;
                case 'java':
                    if (rangeMatch) return `for(int ${b}=${rangeMatch[1]};${b}<${rangeMatch[2]};${b}++){ ${bSemi} }`;
                    return `for (var ${b} : ${iter}) { ${bSemi} }`;
                case 'go': return `for _, ${b} := range ${iter} { ${body} }`;
                case 'rust': return `for ${b} in ${iter} { ${bSemi} }`;
                case 'csharp': return `foreach (var ${b} in ${iter}) { ${bSemi} }`;
                case 'cpp':
                    if (rangeMatch) return `for(int ${b}=${rangeMatch[1]};${b}<${rangeMatch[2]};${b}++){ ${bSemi} }`;
                    return `for (auto ${b} : ${iter}) { ${body} }`;
                case 'swift': return `for ${b} in ${iter} { ${body} }`;
                case 'kotlin': return `for (${b} in ${iter}) { ${body} }`;
                case 'ruby': return `${iter}.each do |${b}|\n  ${body}\nend`;
                case 'php': return `foreach (${iter} as $${b}) { ${bSemi} }`;
                case 'bash': return `for ${b} in ${iter}; do ${body}; done`;
                case 'powershell': return `foreach ($${b} in ${iter}) { ${body} }`;
                case 'r': return `for (${b} in ${iter}) { ${body} }`;
                case 'scala': return `for (${b} <- ${iter}) { ${body} }`;
                case 'julia': return `for ${b} in ${iter}\n  ${body}\nend`;
                case 'dart': return `for (var ${b} in ${iter}) { ${bSemi} }`;
                case 'objc': return `for (id ${b} in ${iter}) { ${bSemi} }`;
                case 'elixir': return `for ${b} <- ${iter} do\n  ${body}\nend`;
                case 'clojure': return `(doseq [${b} ${iter}] ${body})`;
                case 'haskell': return `forM_ ${iter} $ \\${b} -> ${body}`;
                case 'erlang': return `lists:foreach(fun(${b}) -> ${body} end, ${iter}).`;
                case 'fsharp': return `for ${b} in ${iter} do ${body}`;
                case 'ocaml': return `List.iter (fun ${b} -> ${body}) ${iter}`;
                case 'lisp': return `(dolist (${b} ${iter}) ${body})`;
                case 'prolog': return `forall(member(${b}, ${iter}), ${body}).`;
                case 'solidity':
                    if (rangeMatch) return `for(uint ${b}=${rangeMatch[1]};${b}<${rangeMatch[2]};${b}++){ ${bSemi} }`;
                    return `for (uint ${b} = 0; ${b} < ${iter}.length; ${b}++) { ${bSemi} }`;
                case 'zig': return `for (${iter}) |${b}| { ${bSemi} }`;
                case 'c':
                    if (rangeMatch) return `for(int ${b}=${rangeMatch[1]};${b}<${rangeMatch[2]};${b}++){ ${bSemi} }`;
                    return `for (int _i=0; _i<sizeof(${iter})/sizeof(${iter}[0]); _i++) { ${bSemi} }`;
                case 'fortran': return `do ${b} = 1, size(${iter})\n  ${body}\nend do`;
                case 'vhdl': return `for ${b} in ${iter} loop ${body}; end loop;`;
                case 'matlab': return `for ${b} = ${iter}\n  ${body}\nend`;
                case 'pascal': return `for ${b} := 1 to Length(${iter}) do begin ${bSemi} end;`;
                case 'perl': return `foreach my $${b} (${iter}) { ${bSemi} }`;
                case 'apex': return `for (Object ${b} : ${iter}) { ${bSemi} }`;
                case 'mojo': return `for ${b} in ${iter}: ${body}`;
                case 'lua': return `for _, ${b} in ipairs(${iter}) do\n  ${body}\nend`;
                case 'groovy': return `for (${b} in ${iter}) { ${body} }`;
                case 'elm': return `List.map (\\${b} -> ${body}) ${iter}`;
                case 'nim': return `for ${b} in ${iter}: ${body}`;
                case 'crystal': return `${iter}.each do |${b}|\n  ${body}\nend`;
                case 'd': return `foreach (${b}; ${iter}) { ${bSemi} }`;
                case 'vbnet': return `For Each ${b} In ${iter}\n  ${body}\nNext`;
                case 'smalltalk': return `${iter} do: [:${b} | ${body}].`;
                case 'sql': return `-- loop over ${iter}`;
                case 'grey': return `loop ${b} in ${iter} { ${body} }`;
                default: return `// for ${b} in ${iter}: ${body}`;
            }
        }

        // while / infinite loops
        const cond = node.condition ?? 'true';
        const t = LANG_TRAITS[target];

        if (node.variant === 'while') {
            switch (t.family) {
                case 'clike': return `while (${cond}) { ${bSemi} }`;
                case 'pylike': return `while ${cond}: ${body}`;
                case 'mllike': return `while ${cond} do ${body}`;
                case 'lisplike': return `(loop while ${cond} do ${body})`;
                case 'shelllike': return `while ${cond}; do ${body}; done`;
                default:
                    if (target === 'grey') return `loop while ${cond} { ${body} }`;
                    return `while (${cond}) { ${bSemi} }`;
            }
        }

        // Infinite loop
        switch (t.family) {
            case 'clike': return `while (true) { ${bSemi} }`;
            case 'pylike': return `while True: ${body}`;
            case 'lisplike': return `(loop ${body})`;
            default:
                if (target === 'rust') return `loop { ${bSemi} }`;
                if (target === 'grey') return `loop { ${body} }`;
                return `while (true) { ${bSemi} }`;
        }
    }

    // ── cond → conditional branching ────────────────────────────────────

    private translateCond(node: NormCondNode, target: TargetLanguage): string {
        const thenBody = this.mapPrint(node.thenBranch, target);
        const elseBody = node.elseBranch ? this.mapPrint(node.elseBranch, target) : undefined;
        const t = LANG_TRAITS[target];

        switch (t.family) {
            case 'pylike': {
                const eb = elseBody ? `\nelse: ${elseBody}` : '';
                if (target === 'elixir') return `if ${node.condition} do\n  ${thenBody}${elseBody ? '\nelse\n  ' + elseBody : ''}\nend`;
                if (target === 'ruby' || target === 'crystal') return `if ${node.condition}\n  ${thenBody}${elseBody ? '\nelse\n  ' + elseBody : ''}\nend`;
                if (target === 'julia') return `if ${node.condition}\n  ${thenBody}${elseBody ? '\nelse\n  ' + elseBody : ''}\nend`;
                return `if ${node.condition}: ${thenBody}${eb}`;
            }
            case 'mllike': {
                const eb = elseBody ? ` else ${elseBody}` : '';
                return `if ${node.condition} then ${thenBody}${eb}`;
            }
            case 'lisplike':
                return `(if ${node.condition} ${thenBody}${elseBody ? ' ' + elseBody : ''})`;
            case 'shelllike': {
                const eb = elseBody ? `; else ${elseBody}` : '';
                if (target === 'bash') return `if [ ${node.condition} ]; then ${thenBody}${eb}; fi`;
                if (target === 'powershell') return `if (${node.condition}) { ${thenBody} }${elseBody ? ' else { ' + elseBody + ' }' : ''}`;
                return `if ${node.condition}; then ${thenBody}${eb}; fi`;
            }
            case 'clike':
            default: {
                const eb = elseBody ? ` else { ${elseBody} }` : '';
                if (target === 'grey') return `cond ${node.condition} { ${thenBody} }${eb}`;
                if (target === 'rust') {
                    const rThen = thenBody.endsWith(';') ? thenBody : thenBody + ';';
                    const rElse = elseBody ? (elseBody.endsWith(';') ? elseBody : elseBody + ';') : undefined;
                    return `if ${node.condition} { ${rThen} }${rElse ? ' else { ' + rElse + ' }' : ''}`;
                }
                if (target === 'go' || target === 'swift') return `if ${node.condition} { ${thenBody} }${eb}`;
                if (target === 'erlang') return `if ${node.condition} -> ${thenBody}${elseBody ? '; true -> ' + elseBody : ''} end.`;
                if (target === 'prolog') return `(${node.condition} -> ${thenBody}${elseBody ? ' ; ' + elseBody : ''}).`;
                if (target === 'fortran') return `if (${node.condition}) then\n  ${thenBody}${elseBody ? '\nelse\n  ' + elseBody : ''}\nend if`;
                if (target === 'pascal') return `if ${node.condition} then begin ${thenBody} end${elseBody ? ' else begin ' + elseBody + ' end' : ''};`;
                if (target === 'vbnet') return `If ${node.condition} Then\n  ${thenBody}${elseBody ? '\nElse\n  ' + elseBody : ''}\nEnd If`;
                if (target === 'smalltalk') return `(${node.condition}) ifTrue: [${thenBody}]${elseBody ? ' ifFalse: [' + elseBody + ']' : ''}.`;
                if (target === 'vhdl') return `if ${node.condition} then ${thenBody};${elseBody ? ' else ' + elseBody + ';' : ''} end if;`;
                if (target === 'matlab') return `if ${node.condition}\n  ${thenBody}${elseBody ? '\nelse\n  ' + elseBody : ''}\nend`;
                if (target === 'lua') return `if ${node.condition} then\n  ${thenBody}${elseBody ? '\nelse\n  ' + elseBody : ''}\nend`;
                if (target === 'perl') return `if (${node.condition}) { ${thenBody} }${eb}`;
                return `if (${node.condition}) { ${thenBody} }${eb}`;
            }
        }
    }

    // ── struct → class / struct definition ─────────────────────────────

    private translateStruct(node: NormStructNode, target: TargetLanguage): string {
        const fields = node.fields;
        const fNames = fields.map(f => f.name);
        const fDecls = fields.map(f => `${f.type ?? 'auto'} ${f.name};`).join(' ');

        switch (target) {
            case 'python': { const p = ['self', ...fNames].join(', '); const b = fields.map(f => `self.${f.name} = ${f.name}`).join('; '); return `class ${node.name}:\n  def __init__(${p}): ${b}`; }
            case 'javascript': { const b = fields.map(f => `this.${f.name} = ${f.name};`).join(' '); return `class ${node.name} { constructor(${fNames.join(', ')}){ ${b} } }`; }
            case 'typescript': { const p = fields.map(f => `${f.name}: ${f.type ?? 'any'}`).join(', '); return `class ${node.name} { constructor(${p}){ ${fields.map(f => `this.${f.name} = ${f.name};`).join(' ')} } }`; }
            case 'java': { const fd = fields.map(f => `${f.type ?? 'Object'} ${f.name};`).join(' '); return `class ${node.name} { ${fd} }`; }
            case 'go': { const fd = fields.map(f => `${f.name} ${f.type ?? 'interface{}'}`).join('\n  '); return `type ${node.name} struct {\n  ${fd}\n}`; }
            case 'rust': { const fd = fields.map(f => `${f.name}: ${f.type ?? 'i32'}`).join(', '); return `struct ${node.name} { ${fd} }`; }
            case 'csharp': { const fd = fields.map(f => `public ${f.type ?? 'object'} ${f.name} { get; set; }`).join(' '); return `class ${node.name} { ${fd} }`; }
            case 'cpp': return `struct ${node.name} { ${fDecls} };`;
            case 'swift': { const fd = fields.map(f => `var ${f.name}: ${f.type ?? 'Any'}`).join('\n  '); return `struct ${node.name} {\n  ${fd}\n}`; }
            case 'kotlin': { const fd = fields.map(f => `val ${f.name}: ${f.type ?? 'Any'}`).join(', '); return `data class ${node.name}(${fd})`; }
            case 'sql': { const fd = fields.map(f => `${f.name} ${f.type ?? 'TEXT'}`).join(', '); return `CREATE TABLE ${node.name} (${fd});`; }
            case 'ruby': { const attrs = fNames.map(f => `:${f}`).join(', '); return `class ${node.name}\n  attr_accessor ${attrs}\nend`; }
            case 'php': { const fd = fields.map(f => `public $${f.name};`).join(' '); return `class ${node.name} { ${fd} }`; }
            case 'scala': { const fd = fields.map(f => `${f.name}: ${f.type ?? 'Any'}`).join(', '); return `case class ${node.name}(${fd})`; }
            case 'julia': { const fd = fields.map(f => `${f.name}::${f.type ?? 'Any'}`).join('\n  '); return `struct ${node.name}\n  ${fd}\nend`; }
            case 'dart': { const fd = fields.map(f => `${f.type ?? 'dynamic'} ${f.name};`).join(' '); return `class ${node.name} { ${fd} }`; }
            case 'elixir': { const fd = fNames.map(f => `:${f}`).join(', '); return `defmodule ${node.name} do\n  defstruct [${fd}]\nend`; }
            case 'clojure': return `(defrecord ${node.name} [${fNames.join(' ')}])`;
            case 'haskell': { const fd = fields.map(f => `${f.name} :: ${f.type ?? 'Int'}`).join(', '); return `data ${node.name} = ${node.name} { ${fd} }`; }
            case 'erlang': { const fd = fNames.join(', '); return `-record(${node.name.toLowerCase()}, {${fd}}).`; }
            case 'fsharp': { const fd = fields.map(f => `${f.name}: ${f.type ?? 'int'}`).join('; '); return `type ${node.name} = { ${fd} }`; }
            case 'ocaml': { const fd = fields.map(f => `${f.name}: ${f.type ?? 'int'}`).join('; '); return `type ${node.name} = { ${fd} }`; }
            case 'lisp': return `(defstruct ${node.name} ${fNames.join(' ')})`;
            case 'solidity': { const fd = fields.map(f => `${f.type ?? 'uint256'} ${f.name};`).join(' '); return `struct ${node.name} { ${fd} }`; }
            case 'zig': { const fd = fields.map(f => `${f.name}: ${f.type ?? 'u32'}`).join(', '); return `const ${node.name} = struct { ${fd} };`; }
            case 'c': return `struct ${node.name} { ${fDecls} };`;
            case 'fortran': { const fd = fields.map(f => `${f.type ?? 'integer'} :: ${f.name}`).join('\n  '); return `type :: ${node.name}\n  ${fd}\nend type`; }
            case 'pascal': { const fd = fields.map(f => `${f.name}: ${f.type ?? 'Integer'};`).join(' '); return `type ${node.name} = record ${fd} end;`; }
            case 'perl': return `package ${node.name}; use Moo; ${fields.map(f => `has '${f.name}' => (is => 'rw');`).join(' ')}`;
            case 'nim': { const fd = fields.map(f => `${f.name}: ${f.type ?? 'int'}`).join('\n    '); return `type ${node.name} = object\n    ${fd}`; }
            case 'crystal': { const fd = fields.map(f => `property ${f.name} : ${f.type ?? 'String'}`).join('\n  '); return `class ${node.name}\n  ${fd}\nend`; }
            case 'd': return `struct ${node.name} { ${fDecls} }`;
            case 'vbnet': { const fd = fields.map(f => `Public ${f.name} As ${f.type ?? 'Object'}`).join('\n  '); return `Class ${node.name}\n  ${fd}\nEnd Class`; }
            case 'smalltalk': return `Object subclass: #${node.name} instanceVariableNames: '${fNames.join(' ')}'.`;
            case 'grey': { const fs = fields.map(f => f.type ? `${f.name}: ${f.type}` : f.name).join(', '); return `struct ${node.name} { ${fs} }`; }
            default: return `// struct ${node.name} { ${fNames.join(', ')} }`;
        }
    }

    // ── module → import / module declaration ────────────────────────────

    private translateModule(node: NormModuleNode, target: TargetLanguage): string {
        const n = node.name;
        const imps = node.imports;
        const impStr = imps.join(', ');

        switch (target) {
            case 'python': return imps.length === 1 && imps[0] === n ? `import ${n}` : `from ${n} import ${impStr}`;
            case 'javascript': case 'typescript':
                return node.exports?.length ? `export { ${node.exports.join(', ')} };` : `import { ${impStr} } from '${n}';`;
            case 'java': return `import ${n};`;
            case 'go': return `import "${n}"`;
            case 'rust': return imps.length > 0 ? `use ${imps[0]};` : `mod ${n};`;
            case 'csharp': return `using ${n};`;
            case 'cpp': return `#include <${n}>`;
            case 'swift': return `import ${n}`;
            case 'kotlin': return `import ${n}`;
            case 'sql': return `USE ${n};`;
            case 'ruby': return `require '${n}'`;
            case 'php': return `use ${n};`;
            case 'bash': return `source ${n}`;
            case 'powershell': return `Import-Module ${n}`;
            case 'r': return `library(${n})`;
            case 'scala': return `import ${n}`;
            case 'julia': return `using ${n}`;
            case 'dart': return `import '${n}';`;
            case 'objc': return `#import <${n}>`;
            case 'elixir': return `use ${n}`;
            case 'clojure': return `(require '[${n}])`;
            case 'haskell': return `import ${n}`;
            case 'erlang': return `-import(${n}).`;
            case 'fsharp': return `open ${n}`;
            case 'ocaml': return `open ${n}`;
            case 'lisp': return `(require '${n})`;
            case 'prolog': return `:- use_module(library(${n})).`;
            case 'solidity': return `import "${n}";`;
            case 'zig': return `const ${imps[0] || n} = @import("${n}");`;
            case 'c': return `#include <${n}>`;
            case 'fortran': return `use ${n}`;
            case 'vhdl': return `library ${n}; use ${n}.all;`;
            case 'matlab': return `import ${n}.*`;
            case 'pascal': return `uses ${n};`;
            case 'perl': return `use ${n};`;
            case 'apex': return `// import ${n}`;
            case 'mojo': return `from ${n} import ${impStr || '*'}`;
            case 'lua': return `local ${imps[0] || n} = require("${n}")`;
            case 'groovy': return `import ${n}`;
            case 'elm': return `import ${n}`;
            case 'nim': return `import ${n}`;
            case 'crystal': return `require "${n}"`;
            case 'd': return `import ${n};`;
            case 'vbnet': return `Imports ${n}`;
            case 'smalltalk': return `"load: ${n}"`;
            case 'grey': return `module ${n} { ${impStr} }`;
            default: return `// import ${n}`;
        }
    }

    // ── expr → pass-through ─────────────────────────────────────────────

    private translateExpr(node: NormExprNode, target: TargetLanguage): string {
        const expr = this.mapPrint(node.expression, target);
        const t = LANG_TRAITS[target] ?? LANG_TRAITS['grey'];
        return t.terminator ? `${expr}${t.terminator}` : expr;
    }

    // ── block → sequence ────────────────────────────────────────────────

    private translateBlock(node: NormBlockNode, target: TargetLanguage): string {
        return node.children.map(child => this.translate(child, target).code).join('\n');
    }
}

// ─── Self-test ──────────────────────────────────────────────────────────────

if (process.argv[1]?.includes('grey_translate') && process.argv.includes('--test')) {
    console.log('═══ grey_translate.ts Self-Test ═══\n');

    const translator = new Translation();

    // ── bind translation ──
    console.log('── bind translations ──');
    const bindNode: NormBindNode = {
        nkind: 'NormBind', sourceLang: 'python', sourceText: 'x = 42',
        name: 'x', mutable: true, value: '42',
    };

    const jsOut = translator.translate(bindNode, 'javascript');
    console.assert(jsOut.code === 'let x = 42;', `js bind: ${jsOut.code}`);
    const pyOut = translator.translate(bindNode, 'python');
    console.assert(pyOut.code === 'x = 42', `py bind: ${pyOut.code}`);
    const cppOut = translator.translate(bindNode, 'cpp');
    console.assert(cppOut.code === 'auto x = 42;', `cpp bind: ${cppOut.code}`);
    const rsOut = translator.translate(bindNode, 'rust');
    console.assert(rsOut.code === 'let mut x = 42;', `rust bind: ${rsOut.code}`);
    const grOut = translator.translate(bindNode, 'grey');
    console.assert(grOut.code === 'bind x = 42', `grey bind: ${grOut.code}`);
    console.log('  ✓ bind → js/python/cpp/rust/grey');

    // ── fn translation with print mapping ──
    console.log('── fn translations (with print) ──');
    const fnNode: NormFnNode = {
        nkind: 'NormFn', sourceLang: 'python', sourceText: 'def hello(): print("hi")',
        name: 'hello', params: [], body: 'print("hi")',
    };

    const fnJS = translator.translate(fnNode, 'javascript');
    console.assert(fnJS.code.includes('console.log("hi")'), `fn→js: ${fnJS.code}`);
    console.assert(fnJS.code.startsWith('function hello()'), `fn→js shape: ${fnJS.code}`);
    const fnPy = translator.translate(fnNode, 'python');
    console.assert(fnPy.code.includes('print("hi")'), `fn→py: ${fnPy.code}`);
    const fnCpp = translator.translate(fnNode, 'cpp');
    console.assert(fnCpp.code.includes('std::cout << "hi"'), `fn→cpp: ${fnCpp.code}`);
    console.assert(fnCpp.code.startsWith('void hello()'), `fn→cpp shape: ${fnCpp.code}`);
    const fnGrey = translator.translate(fnNode, 'grey');
    console.assert(fnGrey.code.includes('print("hi")'), `fn→grey: ${fnGrey.code}`);
    console.log('  ✓ fn with print mapping → all targets');

    // ── loop translation ──
    console.log('── loop translations ──');
    const loopNode: NormLoopNode = {
        nkind: 'NormLoop', sourceLang: 'python', sourceText: 'for i in range(10): print(i)',
        variant: 'for', binding: 'i', iterable: 'range(0,10)', body: 'print(i)',
    };

    const loopJS = translator.translate(loopNode, 'javascript');
    console.assert(loopJS.code.includes('console.log(i)'), `loop→js print: ${loopJS.code}`);
    const loopPy = translator.translate(loopNode, 'python');
    console.assert(loopPy.code.includes('print(i)'), `loop→py: ${loopPy.code}`);
    const loopGrey = translator.translate(loopNode, 'grey');
    console.assert(loopGrey.code.includes('loop i in'), `loop→grey: ${loopGrey.code}`);
    console.log('  ✓ loop → all targets');

    // ── cond translation ──
    console.log('── cond translations ──');
    const condNode: NormCondNode = {
        nkind: 'NormCond', sourceLang: 'python', sourceText: 'if x > 5: print("big")',
        condition: 'x > 5', thenBranch: 'print("big")', elseBranch: 'print("small")',
    };

    const condJS = translator.translate(condNode, 'javascript');
    console.assert(condJS.code.includes('console.log("big")'), `cond→js then: ${condJS.code}`);
    console.assert(condJS.code.includes('console.log("small")'), `cond→js else: ${condJS.code}`);
    const condPy = translator.translate(condNode, 'python');
    console.assert(condPy.code.includes('print("big")'), `cond→py: ${condPy.code}`);
    const condGrey = translator.translate(condNode, 'grey');
    console.assert(condGrey.code.includes('cond x > 5'), `cond→grey: ${condGrey.code}`);
    console.log('  ✓ cond → all targets');

    // ── translateAll ──
    console.log('── translateAll ──');
    const allOutputs = translator.translateAll(bindNode);
    const expectedCount = ALL_TARGET_LANGUAGES.length;
    console.assert(allOutputs.length === expectedCount, `${expectedCount} targets, got ${allOutputs.length}`);
    for (const out of allOutputs) {
        console.assert(out._type === 'TranslationOutput', 'has _type');
        console.assert(out.meta.stub === true, 'stub meta');
    }
    console.log(`  ✓ translateAll produces ${expectedCount} outputs`);

    // ── translateGroup ──
    console.log('── translateGroup ──');
    const groupOutputs = translator.translateGroup(bindNode, 1);
    console.assert(groupOutputs.length > 0, 'core group has outputs');
    console.log(`  ✓ translateGroup("core") produces ${groupOutputs.length} outputs`);

    console.log('\n✓ All grey_translate tests passed.');
}
