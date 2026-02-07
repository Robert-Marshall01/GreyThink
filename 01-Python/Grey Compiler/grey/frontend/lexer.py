"""
Lexer (Scanner) for the Grey language.

Converts raw source text into a stream of tokens.
Handles:
  - Keywords, identifiers, numbers (int/float/hex/binary/octal)
  - String literals with escape sequences
  - Operators and punctuation
  - Single-line (//) and multi-line (/* */) comments
  - Whitespace removal
  - Error tokens for malformed input
"""

from typing import Optional
from .tokens import Token, TokenType, SourceLocation, KEYWORDS


class LexerError(Exception):
    """Raised when the lexer encounters invalid input."""

    def __init__(self, message: str, location: SourceLocation):
        self.location = location
        super().__init__(f"{location}: {message}")


class Lexer:
    """
    Scans source code and produces a token stream.

    Usage:
        lexer = Lexer(source_code, filename="main.grey")
        tokens = lexer.tokenize()
    """

    def __init__(self, source: str, filename: str = "<stdin>"):
        self.source = source
        self.filename = filename
        self.pos = 0          # current position in source
        self.line = 1         # current line number
        self.column = 1       # current column number
        self.tokens: list[Token] = []
        self.errors: list[LexerError] = []

    # ── Helpers ──────────────────────────────────────────────

    def _current(self) -> str:
        """Return current character or empty string at EOF."""
        if self.pos < len(self.source):
            return self.source[self.pos]
        return ''

    def _peek(self, offset: int = 1) -> str:
        """Look ahead by offset characters."""
        idx = self.pos + offset
        if idx < len(self.source):
            return self.source[idx]
        return ''

    def _advance(self) -> str:
        """Consume and return the current character."""
        ch = self._current()
        if ch == '\n':
            self.line += 1
            self.column = 1
        else:
            self.column += 1
        self.pos += 1
        return ch

    def _at_end(self) -> bool:
        return self.pos >= len(self.source)

    def _location(self) -> SourceLocation:
        return SourceLocation(self.filename, self.line, self.column, self.pos)

    def _make_token(self, ttype: TokenType, value, lexeme: str,
                    loc: SourceLocation) -> Token:
        return Token(ttype, value, lexeme, loc)

    def _error(self, message: str, loc: Optional[SourceLocation] = None):
        loc = loc or self._location()
        err = LexerError(message, loc)
        self.errors.append(err)
        return err

    # ── Whitespace & Comments ────────────────────────────────

    def _skip_whitespace(self):
        """Skip spaces, tabs, carriage returns."""
        while not self._at_end() and self._current() in ' \t\r':
            self._advance()

    def _skip_line_comment(self):
        """Skip from // to end of line."""
        while not self._at_end() and self._current() != '\n':
            self._advance()

    def _skip_block_comment(self):
        """Skip from /* to */, supporting nesting."""
        depth = 1
        while not self._at_end() and depth > 0:
            if self._current() == '/' and self._peek() == '*':
                self._advance()
                self._advance()
                depth += 1
            elif self._current() == '*' and self._peek() == '/':
                self._advance()
                self._advance()
                depth -= 1
            else:
                self._advance()
        if depth > 0:
            self._error("Unterminated block comment")

    def _skip_whitespace_and_comments(self):
        """Skip all whitespace and comments."""
        while not self._at_end():
            ch = self._current()
            if ch in ' \t\r':
                self._skip_whitespace()
            elif ch == '\n':
                self._advance()
            elif ch == '/' and self._peek() == '/':
                self._skip_line_comment()
            elif ch == '/' and self._peek() == '*':
                self._advance()  # skip /
                self._advance()  # skip *
                self._skip_block_comment()
            else:
                break

    # ── Number Scanning ──────────────────────────────────────

    def _scan_number(self) -> Token:
        """Scan integer or float literal (decimal, hex, binary, octal)."""
        loc = self._location()
        start = self.pos

        # Check for hex, binary, octal prefixes
        if self._current() == '0' and self._peek() in ('x', 'X', 'b', 'B', 'o', 'O'):
            self._advance()  # skip '0'
            prefix = self._advance()  # skip the letter

            if prefix in ('x', 'X'):
                while not self._at_end() and (self._current() in '0123456789abcdefABCDEF_'):
                    self._advance()
                lexeme = self.source[start:self.pos]
                value = int(lexeme.replace('_', ''), 16)
                return self._make_token(TokenType.INTEGER, value, lexeme, loc)

            elif prefix in ('b', 'B'):
                while not self._at_end() and self._current() in '01_':
                    self._advance()
                lexeme = self.source[start:self.pos]
                value = int(lexeme.replace('_', ''), 2)
                return self._make_token(TokenType.INTEGER, value, lexeme, loc)

            elif prefix in ('o', 'O'):
                while not self._at_end() and self._current() in '01234567_':
                    self._advance()
                lexeme = self.source[start:self.pos]
                value = int(lexeme.replace('_', ''), 8)
                return self._make_token(TokenType.INTEGER, value, lexeme, loc)

        # Decimal number
        while not self._at_end() and (self._current().isdigit() or self._current() == '_'):
            self._advance()

        is_float = False

        # Check for decimal point (but not range operator ..)
        if self._current() == '.' and self._peek() != '.':
            is_float = True
            self._advance()  # consume '.'
            while not self._at_end() and (self._current().isdigit() or self._current() == '_'):
                self._advance()

        # Check for exponent
        if self._current() in ('e', 'E'):
            is_float = True
            self._advance()
            if self._current() in ('+', '-'):
                self._advance()
            if not self._current().isdigit():
                self._error("Expected digit in exponent", loc)
            while not self._at_end() and (self._current().isdigit() or self._current() == '_'):
                self._advance()

        lexeme = self.source[start:self.pos]
        clean = lexeme.replace('_', '')

        if is_float:
            return self._make_token(TokenType.FLOAT, float(clean), lexeme, loc)
        else:
            return self._make_token(TokenType.INTEGER, int(clean), lexeme, loc)

    # ── String Scanning ──────────────────────────────────────

    def _scan_string(self) -> Token:
        """Scan a string literal with escape sequences."""
        loc = self._location()
        quote = self._advance()  # consume opening quote
        value_parts: list[str] = []
        start = self.pos

        ESCAPE_MAP = {
            'n': '\n', 't': '\t', 'r': '\r', '\\': '\\',
            '0': '\0', "'": "'", '"': '"',
            'a': '\a', 'b': '\b', 'f': '\f', 'v': '\v',
        }

        while not self._at_end():
            ch = self._current()

            if ch == quote:
                self._advance()  # consume closing quote
                lexeme = self.source[start - 1:self.pos]
                return self._make_token(TokenType.STRING, ''.join(value_parts),
                                        lexeme, loc)

            if ch == '\n':
                self._error("Unterminated string literal", loc)
                break

            if ch == '\\':
                self._advance()
                esc = self._current()
                if esc in ESCAPE_MAP:
                    value_parts.append(ESCAPE_MAP[esc])
                    self._advance()
                elif esc == 'x':
                    # Hex escape: \xFF
                    self._advance()
                    hex_str = ''
                    for _ in range(2):
                        if self._current() in '0123456789abcdefABCDEF':
                            hex_str += self._advance()
                        else:
                            break
                    if hex_str:
                        value_parts.append(chr(int(hex_str, 16)))
                    else:
                        self._error("Invalid hex escape sequence", loc)
                elif esc == 'u':
                    # Unicode escape: \u{XXXX}
                    self._advance()
                    if self._current() == '{':
                        self._advance()
                        hex_str = ''
                        while self._current() != '}' and not self._at_end():
                            hex_str += self._advance()
                        if self._current() == '}':
                            self._advance()
                        if hex_str:
                            value_parts.append(chr(int(hex_str, 16)))
                        else:
                            self._error("Invalid unicode escape", loc)
                    else:
                        self._error("Expected '{' after \\u", loc)
                else:
                    self._error(f"Unknown escape sequence: \\{esc}", loc)
                    self._advance()
            else:
                value_parts.append(ch)
                self._advance()

        self._error("Unterminated string literal", loc)
        lexeme = self.source[start - 1:self.pos]
        return self._make_token(TokenType.STRING, ''.join(value_parts), lexeme, loc)

    # ── Identifier / Keyword Scanning ────────────────────────

    def _scan_identifier(self) -> Token:
        """Scan an identifier or keyword."""
        loc = self._location()
        start = self.pos

        while not self._at_end() and (self._current().isalnum() or self._current() == '_'):
            self._advance()

        lexeme = self.source[start:self.pos]

        # Check if it's a keyword
        if lexeme in KEYWORDS:
            ttype = KEYWORDS[lexeme]
            # Handle boolean literal values
            if ttype == TokenType.BOOL_TRUE:
                return self._make_token(ttype, True, lexeme, loc)
            elif ttype == TokenType.BOOL_FALSE:
                return self._make_token(ttype, False, lexeme, loc)
            elif ttype == TokenType.NIL:
                return self._make_token(ttype, None, lexeme, loc)
            return self._make_token(ttype, lexeme, lexeme, loc)

        return self._make_token(TokenType.IDENTIFIER, lexeme, lexeme, loc)

    # ── Operator / Punctuation Scanning ──────────────────────

    def _scan_operator(self) -> Token:
        """Scan operators and punctuation."""
        loc = self._location()
        ch = self._advance()

        # Two-character operators
        next_ch = self._current()
        two_char = ch + next_ch

        TWO_CHAR_OPS = {
            '==': TokenType.EQ,
            '!=': TokenType.NEQ,
            '<=': TokenType.LTE,
            '>=': TokenType.GTE,
            '->': TokenType.ARROW,
            '=>': TokenType.FAT_ARROW,
            '+=': TokenType.PLUS_ASSIGN,
            '-=': TokenType.MINUS_ASSIGN,
            '*=': TokenType.STAR_ASSIGN,
            '/=': TokenType.SLASH_ASSIGN,
            '%=': TokenType.PERCENT_ASSIGN,
            '<<': TokenType.SHIFT_LEFT,
            '>>': TokenType.SHIFT_RIGHT,
            '..': TokenType.DOT_DOT,
            '**': TokenType.POWER,
        }

        if two_char in TWO_CHAR_OPS:
            self._advance()
            # Check for three-char operators
            if two_char == '..' and self._current() == '.':
                self._advance()
                return self._make_token(TokenType.ELLIPSIS, '...', '...', loc)
            return self._make_token(TWO_CHAR_OPS[two_char], two_char, two_char, loc)

        # Single-character operators
        SINGLE_OPS = {
            '+': TokenType.PLUS,
            '-': TokenType.MINUS,
            '*': TokenType.STAR,
            '/': TokenType.SLASH,
            '%': TokenType.PERCENT,
            '=': TokenType.ASSIGN,
            '<': TokenType.LT,
            '>': TokenType.GT,
            '&': TokenType.AMPERSAND,
            '|': TokenType.PIPE,
            '^': TokenType.CARET,
            '~': TokenType.TILDE,
            '!': TokenType.NOT,
            '(': TokenType.LPAREN,
            ')': TokenType.RPAREN,
            '[': TokenType.LBRACKET,
            ']': TokenType.RBRACKET,
            '{': TokenType.LBRACE,
            '}': TokenType.RBRACE,
            ',': TokenType.COMMA,
            '.': TokenType.DOT,
            ':': TokenType.COLON,
            ';': TokenType.SEMICOLON,
            '#': TokenType.HASH,
            '@': TokenType.AT,
            '?': TokenType.QUESTION,
        }

        if ch in SINGLE_OPS:
            return self._make_token(SINGLE_OPS[ch], ch, ch, loc)

        # Unknown character
        self._error(f"Unexpected character: {ch!r}")
        return self._make_token(TokenType.ERROR, ch, ch, loc)

    # ── Main Tokenize Loop ───────────────────────────────────

    def tokenize(self) -> list[Token]:
        """
        Scan the entire source file and return all tokens.
        Returns a list ending with an EOF token.
        """
        self.tokens = []
        self.errors = []

        while not self._at_end():
            self._skip_whitespace_and_comments()

            if self._at_end():
                break

            ch = self._current()

            if ch.isdigit():
                self.tokens.append(self._scan_number())
            elif ch == '"' or ch == "'":
                self.tokens.append(self._scan_string())
            elif ch.isalpha() or ch == '_':
                self.tokens.append(self._scan_identifier())
            else:
                self.tokens.append(self._scan_operator())

        # Add EOF token
        eof_loc = self._location()
        self.tokens.append(Token(TokenType.EOF, None, '', eof_loc))

        return self.tokens

    # Alias for convenience
    scan_tokens = tokenize

    def tokenize_with_errors(self) -> tuple[list[Token], list[LexerError]]:
        """Tokenize and return both tokens and any errors encountered."""
        tokens = self.tokenize()
        return tokens, self.errors
