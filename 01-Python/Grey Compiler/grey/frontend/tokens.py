"""
Token definitions for the Grey language.

Defines all token types and the Token class used by the lexer
to produce a token stream from source code.
"""

from enum import Enum, auto
from dataclasses import dataclass
from typing import Any, Optional


class TokenType(Enum):
    """All token types recognized by the Grey lexer."""

    # --- Literals ---
    INTEGER = auto()        # 42, 0xFF, 0b1010, 0o77
    FLOAT = auto()          # 3.14, 1e10, 2.5e-3
    STRING = auto()         # "hello", 'world'
    CHAR = auto()           # 'a' (single char in single quotes when unambiguous)
    BOOL_TRUE = auto()      # true
    BOOL_FALSE = auto()     # false
    NIL = auto()            # nil

    # --- Identifiers ---
    IDENTIFIER = auto()     # variable/function/type names

    # --- Keywords ---
    LET = auto()            # let
    MUT = auto()            # mut
    CONST = auto()          # const
    FN = auto()             # fn
    RETURN = auto()         # return
    IF = auto()             # if
    ELSE = auto()           # else
    ELIF = auto()           # elif
    WHILE = auto()          # while
    FOR = auto()            # for
    IN = auto()             # in
    BREAK = auto()          # break
    CONTINUE = auto()       # continue
    STRUCT = auto()         # struct
    ENUM = auto()           # enum
    IMPL = auto()           # impl
    TRAIT = auto()          # trait
    MATCH = auto()          # match
    IMPORT = auto()         # import
    FROM = auto()           # from
    AS = auto()             # as
    PUB = auto()            # pub
    SELF = auto()           # self
    TYPE = auto()           # type (alias)
    AND = auto()            # and
    OR = auto()             # or
    NOT = auto()            # not

    # --- Type keywords ---
    INT_TYPE = auto()       # int
    FLOAT_TYPE = auto()     # float
    STRING_TYPE = auto()    # string
    BOOL_TYPE = auto()      # bool
    CHAR_TYPE = auto()      # char
    VOID_TYPE = auto()      # void
    ARRAY_TYPE = auto()     # array

    # --- Operators ---
    PLUS = auto()           # +
    MINUS = auto()          # -
    STAR = auto()           # *
    SLASH = auto()          # /
    PERCENT = auto()        # %
    POWER = auto()          # **
    AMPERSAND = auto()      # &
    PIPE = auto()           # |
    CARET = auto()          # ^
    TILDE = auto()          # ~
    SHIFT_LEFT = auto()     # <<
    SHIFT_RIGHT = auto()    # >>

    # --- Comparison ---
    EQ = auto()             # ==
    NEQ = auto()            # !=
    LT = auto()             # <
    GT = auto()             # >
    LTE = auto()            # <=
    GTE = auto()            # >=

    # --- Assignment ---
    ASSIGN = auto()         # =
    PLUS_ASSIGN = auto()    # +=
    MINUS_ASSIGN = auto()   # -=
    STAR_ASSIGN = auto()    # *=
    SLASH_ASSIGN = auto()   # /=
    PERCENT_ASSIGN = auto() # %=

    # --- Punctuation ---
    LPAREN = auto()         # (
    RPAREN = auto()         # )
    LBRACKET = auto()       # [
    RBRACKET = auto()       # ]
    LBRACE = auto()         # {
    RBRACE = auto()         # }
    COMMA = auto()          # ,
    DOT = auto()            # .
    COLON = auto()          # :
    SEMICOLON = auto()      # ;
    ARROW = auto()          # ->
    FAT_ARROW = auto()      # =>
    DOT_DOT = auto()        # ..
    ELLIPSIS = auto()       # ...
    HASH = auto()           # #
    AT = auto()             # @
    QUESTION = auto()       # ?

    # --- Special ---
    NEWLINE = auto()        # \n (if significant)
    EOF = auto()            # end of file
    ERROR = auto()          # lexer error token


# Keyword mapping
KEYWORDS: dict[str, TokenType] = {
    'let':      TokenType.LET,
    'mut':      TokenType.MUT,
    'const':    TokenType.CONST,
    'fn':       TokenType.FN,
    'return':   TokenType.RETURN,
    'if':       TokenType.IF,
    'else':     TokenType.ELSE,
    'elif':     TokenType.ELIF,
    'while':    TokenType.WHILE,
    'for':      TokenType.FOR,
    'in':       TokenType.IN,
    'break':    TokenType.BREAK,
    'continue': TokenType.CONTINUE,
    'struct':   TokenType.STRUCT,
    'enum':     TokenType.ENUM,
    'impl':     TokenType.IMPL,
    'trait':    TokenType.TRAIT,
    'match':    TokenType.MATCH,
    'import':   TokenType.IMPORT,
    'from':     TokenType.FROM,
    'as':       TokenType.AS,
    'pub':      TokenType.PUB,
    'self':     TokenType.SELF,
    'type':     TokenType.TYPE,
    'and':      TokenType.AND,
    'or':       TokenType.OR,
    'not':      TokenType.NOT,
    'true':     TokenType.BOOL_TRUE,
    'false':    TokenType.BOOL_FALSE,
    'nil':      TokenType.NIL,
    'int':      TokenType.INT_TYPE,
    'float':    TokenType.FLOAT_TYPE,
    'string':   TokenType.STRING_TYPE,
    'bool':     TokenType.BOOL_TYPE,
    'char':     TokenType.CHAR_TYPE,
    'void':     TokenType.VOID_TYPE,
    'array':    TokenType.ARRAY_TYPE,
}


@dataclass
class SourceLocation:
    """Tracks where a token appears in source code."""
    file: str
    line: int
    column: int
    offset: int  # byte offset from start of file

    def __repr__(self) -> str:
        return f"{self.file}:{self.line}:{self.column}"


@dataclass
class Token:
    """
    A single token produced by the lexer.

    Attributes:
        type:     The TokenType classification
        value:    The literal value (string text, numeric value, etc.)
        lexeme:   The original source text that produced this token
        location: Where in the source file this token was found
    """
    type: TokenType
    value: Any
    lexeme: str
    location: SourceLocation

    def __repr__(self) -> str:
        return f"Token({self.type.name}, {self.value!r}, {self.location})"

    @property
    def line(self) -> int:
        return self.location.line

    @property
    def column(self) -> int:
        return self.location.column

    def is_type(self, *types: TokenType) -> bool:
        """Check if this token matches any of the given types."""
        return self.type in types

    def is_keyword(self) -> bool:
        """Check if this token is a keyword."""
        return self.type in KEYWORDS.values()

    def is_literal(self) -> bool:
        """Check if this token is a literal value."""
        return self.type in (
            TokenType.INTEGER, TokenType.FLOAT, TokenType.STRING,
            TokenType.CHAR, TokenType.BOOL_TRUE, TokenType.BOOL_FALSE,
            TokenType.NIL,
        )

    def is_operator(self) -> bool:
        """Check if this token is an operator."""
        return self.type in (
            TokenType.PLUS, TokenType.MINUS, TokenType.STAR,
            TokenType.SLASH, TokenType.PERCENT, TokenType.POWER,
            TokenType.AMPERSAND, TokenType.PIPE, TokenType.CARET,
            TokenType.TILDE, TokenType.SHIFT_LEFT, TokenType.SHIFT_RIGHT,
            TokenType.EQ, TokenType.NEQ, TokenType.LT, TokenType.GT,
            TokenType.LTE, TokenType.GTE, TokenType.AND, TokenType.OR,
            TokenType.NOT,
        )

    def is_assignment(self) -> bool:
        """Check if this token is an assignment operator."""
        return self.type in (
            TokenType.ASSIGN, TokenType.PLUS_ASSIGN, TokenType.MINUS_ASSIGN,
            TokenType.STAR_ASSIGN, TokenType.SLASH_ASSIGN,
            TokenType.PERCENT_ASSIGN,
        )
