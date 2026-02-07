"""
Grey Code Formatter.

Automatically formats Grey source code according to the standard style:
  - Consistent indentation (4 spaces)
  - Proper brace placement (K&R style)
  - Normalized whitespace around operators
  - Consistent semicolons
  - Blank line management
  - Maximum line length enforcement
"""

from __future__ import annotations
from ..frontend.lexer import Lexer
from ..frontend.tokens import TokenType, Token


class GreyFormatter:
    """
    Formats Grey source code into a canonical style.

    Usage:
        formatter = GreyFormatter()
        formatted = formatter.format(source)
    """

    def __init__(
        self,
        indent_size: int = 4,
        max_line_length: int = 100,
        use_tabs: bool = False,
        trailing_newline: bool = True,
        blank_line_between_functions: int = 1,
    ):
        self.indent_size = indent_size
        self.max_line_length = max_line_length
        self.use_tabs = use_tabs
        self.trailing_newline = trailing_newline
        self.blank_line_between_functions = blank_line_between_functions

    def format(self, source: str) -> str:
        """Format Grey source code."""
        lines = source.split("\n")
        formatted_lines = []
        indent_level = 0
        prev_line_type = None
        in_multiline_comment = False

        for raw_line in lines:
            stripped = raw_line.strip()

            # Handle multi-line comments
            if in_multiline_comment:
                formatted_lines.append(self._indent(indent_level) + stripped)
                if "*/" in stripped:
                    in_multiline_comment = False
                continue

            if "/*" in stripped and "*/" not in stripped:
                in_multiline_comment = True
                formatted_lines.append(self._indent(indent_level) + stripped)
                continue

            # Empty lines
            if not stripped:
                # Collapse multiple blank lines into one
                if formatted_lines and formatted_lines[-1].strip() == "":
                    continue
                formatted_lines.append("")
                prev_line_type = "blank"
                continue

            # Detect line type
            line_type = self._classify_line(stripped)

            # Insert blank lines before function/struct/enum/impl declarations
            if line_type in ("fn_decl", "struct_decl", "enum_decl", "impl_decl", "trait_decl"):
                if prev_line_type and prev_line_type != "blank":
                    for _ in range(self.blank_line_between_functions):
                        formatted_lines.append("")

            # Adjust indent for closing braces BEFORE formatting this line
            if stripped.startswith("}"):
                indent_level = max(0, indent_level - 1)

            # Format the line content
            formatted = self._format_line(stripped)

            # Apply indentation
            indented = self._indent(indent_level) + formatted
            formatted_lines.append(indented)

            # Adjust indent for opening braces AFTER adding this line
            if stripped.endswith("{"):
                indent_level += 1

            # Handle single-line else/elif on same line as closing brace
            if stripped.startswith("} else") or stripped.startswith("} elif"):
                pass  # indent already handled

            prev_line_type = line_type

        result = "\n".join(formatted_lines)

        # Clean up trailing whitespace
        result = "\n".join(line.rstrip() for line in result.split("\n"))

        # Remove excess trailing blank lines
        while result.endswith("\n\n"):
            result = result[:-1]

        if self.trailing_newline and not result.endswith("\n"):
            result += "\n"

        return result

    def format_file(self, filepath: str) -> str:
        """Format a Grey source file and return the result."""
        with open(filepath, "r") as f:
            source = f.read()
        return self.format(source)

    def format_file_in_place(self, filepath: str):
        """Format a Grey source file in place."""
        formatted = self.format_file(filepath)
        with open(filepath, "w") as f:
            f.write(formatted)

    # ── Line Classification ──────────────────────────────────

    def _classify_line(self, stripped: str) -> str:
        """Classify a line for blank line insertion logic."""
        if stripped.startswith("fn "):
            return "fn_decl"
        if stripped.startswith("struct "):
            return "struct_decl"
        if stripped.startswith("enum "):
            return "enum_decl"
        if stripped.startswith("impl "):
            return "impl_decl"
        if stripped.startswith("trait "):
            return "trait_decl"
        if stripped.startswith("//"):
            return "comment"
        if stripped.startswith("import ") or stripped.startswith("from "):
            return "import"
        if stripped.startswith("let ") or stripped.startswith("mut ") or stripped.startswith("const "):
            return "var_decl"
        if stripped == "}":
            return "close_brace"
        return "statement"

    # ── Line Formatting ──────────────────────────────────────

    def _format_line(self, line: str) -> str:
        """Format a single line of code (without indentation)."""
        # Don't touch comments
        if line.startswith("//"):
            return line

        # Normalize whitespace around operators
        line = self._normalize_operators(line)

        # Normalize spaces after keywords
        line = self._normalize_keywords(line)

        # Normalize commas
        line = self._normalize_commas(line)

        # Normalize semicolons
        line = self._normalize_semicolons(line)

        # Collapse multiple spaces (but not in strings)
        line = self._collapse_spaces(line)

        return line

    def _normalize_operators(self, line: str) -> str:
        """Ensure spaces around binary operators."""
        result = []
        i = 0
        in_string = False
        string_char = None

        while i < len(line):
            ch = line[i]

            # Track string state
            if not in_string and ch in ('"', "'"):
                in_string = True
                string_char = ch
                result.append(ch)
                i += 1
                continue
            if in_string:
                result.append(ch)
                if ch == string_char and (i == 0 or line[i - 1] != "\\"):
                    in_string = False
                i += 1
                continue

            # Two-char operators
            if i + 1 < len(line):
                two = line[i:i + 2]
                if two in ("==", "!=", "<=", ">=", "&&", "||", "->", "+=", "-=", "*=", "/=", "<<", ">>"):
                    # Ensure space before
                    if result and result[-1] != " ":
                        result.append(" ")
                    result.append(two)
                    # Ensure space after
                    if i + 2 < len(line) and line[i + 2] != " ":
                        result.append(" ")
                    i += 2
                    continue

            # Single-char operators (not unary context)
            if ch in ("=", "+", "-", "*", "/", "%", "<", ">", "&", "|", "^"):
                # Check for unary minus/plus (after operator, paren, comma, or start)
                if ch in ("-", "+") and (not result or result[-1] in (" ", "(", ",", "=", "[", "{")):
                    result.append(ch)
                    i += 1
                    continue

                # Skip if part of -> or other multi-char op already handled
                if result and result[-1] != " ":
                    result.append(" ")
                result.append(ch)
                if i + 1 < len(line) and line[i + 1] not in (" ", "=", ch):
                    result.append(" ")
                i += 1
                continue

            result.append(ch)
            i += 1

        return "".join(result)

    def _normalize_keywords(self, line: str) -> str:
        """Ensure exactly one space after keywords."""
        keywords = [
            "fn", "let", "mut", "const", "if", "else", "elif",
            "while", "for", "in", "return", "match", "struct",
            "enum", "impl", "trait", "import", "from", "type",
        ]
        for kw in keywords:
            prefix = kw + "  "
            while prefix in line:
                line = line.replace(prefix, kw + " ")
        return line

    def _normalize_commas(self, line: str) -> str:
        """Ensure space after commas but not before."""
        result = []
        in_string = False
        for i, ch in enumerate(line):
            if ch in ('"', "'") and (i == 0 or line[i - 1] != "\\"):
                in_string = not in_string
            if not in_string and ch == ",":
                # Remove space before comma
                while result and result[-1] == " ":
                    result.pop()
                result.append(",")
                # Add space after comma if not already
                if i + 1 < len(line) and line[i + 1] != " ":
                    result.append(" ")
            else:
                result.append(ch)
        return "".join(result)

    def _normalize_semicolons(self, line: str) -> str:
        """Remove space before semicolons."""
        result = []
        in_string = False
        for i, ch in enumerate(line):
            if ch in ('"', "'") and (i == 0 or line[i - 1] != "\\"):
                in_string = not in_string
            if not in_string and ch == ";":
                while result and result[-1] == " ":
                    result.pop()
                result.append(";")
            else:
                result.append(ch)
        return "".join(result)

    def _collapse_spaces(self, line: str) -> str:
        """Collapse multiple spaces into one (outside strings)."""
        result = []
        in_string = False
        prev_space = False

        for i, ch in enumerate(line):
            if ch in ('"', "'") and (i == 0 or line[i - 1] != "\\"):
                in_string = not in_string
            if not in_string and ch == " ":
                if not prev_space:
                    result.append(ch)
                prev_space = True
            else:
                result.append(ch)
                prev_space = False

        return "".join(result)

    # ── Helpers ──────────────────────────────────────────────

    def _indent(self, level: int) -> str:
        """Generate indentation string."""
        if self.use_tabs:
            return "\t" * level
        return " " * (self.indent_size * level)
