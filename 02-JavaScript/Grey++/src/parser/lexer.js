// ─── Grey++ Lexer / Tokenizer ────────────────────────────────────────────────
// Converts raw source text into a flat token stream consumed by the parser.

/** @enum {string} */
export const TokenType = Object.freeze({
    // Keywords
    Fn: 'Fn',
    Query: 'Query',
    Select: 'Select',
    From: 'From',
    Where: 'Where',
    Return: 'Return',
    True: 'True',
    False: 'False',

    // Literals & identifiers
    Number: 'Number',
    String: 'String',
    Ident: 'Ident',

    // Operators & punctuation
    Plus: 'Plus',
    Minus: 'Minus',
    Star: 'Star',
    Slash: 'Slash',
    Eq: 'Eq',       // =
    EqEq: 'EqEq',     // ==
    BangEq: 'BangEq',   // !=
    Lt: 'Lt',
    Gt: 'Gt',
    LtEq: 'LtEq',
    GtEq: 'GtEq',
    LParen: 'LParen',
    RParen: 'RParen',
    LBrace: 'LBrace',
    RBrace: 'RBrace',
    Comma: 'Comma',
    Semi: 'Semi',
    Colon: 'Colon',
    LBracket: 'LBracket',
    RBracket: 'RBracket',

    // Meta
    EOF: 'EOF',
});

const KEYWORDS = {
    fn: TokenType.Fn,
    query: TokenType.Query,
    select: TokenType.Select,
    from: TokenType.From,
    where: TokenType.Where,
    return: TokenType.Return,
    true: TokenType.True,
    false: TokenType.False,
};

/**
 * @param {string} source
 * @returns {{ type: string, value: string }[]}
 */
export function tokenize(source) {
    /** @type {{ type: string, value: string }[]} */
    const tokens = [];
    let i = 0;

    while (i < source.length) {
        const ch = source[i];

        // ── whitespace ──
        if (/\s/.test(ch)) { i++; continue; }

        // ── single-line comment ──
        if (ch === '/' && source[i + 1] === '/') {
            while (i < source.length && source[i] !== '\n') i++;
            continue;
        }

        // ── number (supports leading dot like .5) ──
        if (/[0-9]/.test(ch) || (ch === '.' && i + 1 < source.length && /[0-9]/.test(source[i + 1]))) {
            let num = '';
            let hasDot = false;
            while (i < source.length && (/[0-9]/.test(source[i]) || (source[i] === '.' && !hasDot))) {
                if (source[i] === '.') hasDot = true;
                num += source[i++];
            }
            tokens.push({ type: TokenType.Number, value: num });
            continue;
        }

        // ── string (double-quoted or single-quoted) ──
        if (ch === '"' || ch === "'") {
            const quote = ch;
            const startPos = i;
            i++; // skip opening quote
            let str = '';
            while (i < source.length && source[i] !== quote) {
                if (source[i] === '\\') {
                    i++; // skip backslash
                    if (i >= source.length) break;
                    // Interpret escape sequences
                    switch (source[i]) {
                        case 'n': str += '\n'; break;
                        case 't': str += '\t'; break;
                        case 'r': str += '\r'; break;
                        case '\\': str += '\\'; break;
                        case "'": str += "'"; break;
                        case '"': str += '"'; break;
                        case '0': str += '\0'; break;
                        default: str += source[i]; break;
                    }
                    i++;
                    continue;
                }
                str += source[i++];
            }
            if (i >= source.length) {
                throw new SyntaxError(`Unterminated string starting at position ${startPos}`);
            }
            i++; // skip closing quote
            tokens.push({ type: TokenType.String, value: str });
            continue;
        }

        // ── identifier / keyword ──
        if (/[a-zA-Z_]/.test(ch)) {
            let word = '';
            while (i < source.length && /[a-zA-Z0-9_]/.test(source[i])) { word += source[i++]; }
            const type = KEYWORDS[word] ?? TokenType.Ident;
            tokens.push({ type, value: word });
            continue;
        }

        // ── two-char operators ──
        const two = source.slice(i, i + 2);
        if (two === '==') { tokens.push({ type: TokenType.EqEq, value: two }); i += 2; continue; }
        if (two === '!=') { tokens.push({ type: TokenType.BangEq, value: two }); i += 2; continue; }
        if (two === '<=') { tokens.push({ type: TokenType.LtEq, value: two }); i += 2; continue; }
        if (two === '>=') { tokens.push({ type: TokenType.GtEq, value: two }); i += 2; continue; }

        // ── single-char operators & punctuation ──
        const SINGLE = {
            '+': TokenType.Plus, '-': TokenType.Minus,
            '*': TokenType.Star, '/': TokenType.Slash,
            '=': TokenType.Eq, '<': TokenType.Lt,
            '>': TokenType.Gt, '(': TokenType.LParen,
            ')': TokenType.RParen, '{': TokenType.LBrace,
            '}': TokenType.RBrace, ',': TokenType.Comma,
            ';': TokenType.Semi, ':': TokenType.Colon,
            '[': TokenType.LBracket, ']': TokenType.RBracket,
        };
        if (SINGLE[ch]) {
            tokens.push({ type: SINGLE[ch], value: ch });
            i++;
            continue;
        }

        throw new SyntaxError(`Unexpected character '${ch}' at position ${i}`);
    }

    tokens.push({ type: TokenType.EOF, value: '' });
    return tokens;
}
