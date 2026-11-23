#!/usr/bin/env python3
"""Turns grammar.bnf into a LaTeX file for the `syntax` package (syntax-mdw).

Usage: bnf2tex.py [grammar.bnf [chapters/grammar.tex]]   (defaults: next to this script's parent folder)

Long rules and rules written over several lines get one alternative per line
(\\alt); short ones keep the alternatives on one line with `|`.
Section headers of the form `(* ---- N. Title ... ---- *)` become headings.
"""
import re
import sys
from pathlib import Path

INLINE_LIMIT = 70      # a rule longer than this (in source characters) uses \alt
INLINE_ALTERNATIVES = 4

TOKEN = re.compile(r"""
    (?P<comment>\(\*.*?\*\))
  | (?P<string>"(?:\\.|[^"\\])*")
  | (?P<class><[a-z\-]+>)
  | (?P<name>[A-Za-z][A-Za-z0-9]*)
  | (?P<open>[\[\]{}()])
  | (?P<bar>\|)
  | (?P<space>\s+)
  | (?P<other>.)
""", re.X | re.S)

TEX_ESCAPES = {
    "<": r"\textless{}", ">": r"\textgreater{}", "|": r"\textbar{}",
    '"': r"\textquotedbl{}", "'": r"\textquotesingle{}", "\\": r"\textbackslash{}",
    "{": r"\{", "}": r"\}", "#": r"\#", "%": r"\%", "&": r"\&", "_": r"\_",
    "^": r"\^{}", "~": r"\~{}", "$": r"\$",
}


def escape(text):
    return "".join(TEX_ESCAPES.get(character, character) for character in text)


def unquote(string):
    body = string[1:-1]
    return re.sub(r"\\(.)", r"\1", body)


def comment_text(comment):
    return re.sub(r"\s+", " ", comment[2:-2]).strip()


def tex_comment(text):
    return r"\hfill{\itshape\small " + escape(text) + "}"


def render(token, kind):
    """Text of one token in the grammar environment."""
    if kind == "string":
        return r"\lit{" + escape(unquote(token)) + "}"
    if kind == "class":
        return "<" + token[1:-1] + ">"
    if kind == "name":
        return "<" + token + ">"
    if kind == "open":
        return {"{": r"\{", "}": r"\}"}.get(token, token)
    return token


def tokenize(text):
    for match in TOKEN.finditer(text):
        yield match.lastgroup, match.group()


def split_alternatives(body):
    """Splits a rule body on top-level `|`; returns (text, comment) pairs."""
    alternatives = [[]]
    depth = 0
    for kind, token in tokenize(body):
        if kind == "open":
            depth += token in "[{("
            depth -= token in "]})"
        if kind == "bar" and depth == 0:
            alternatives.append([])
            continue
        if kind != "space":
            alternatives[-1].append((kind, token))
    result = []
    for alternative in alternatives:
        parts = [render(token, kind) for kind, token in alternative if kind != "comment"]
        comments = [comment_text(token) for kind, token in alternative if kind == "comment"]
        text = " ".join(parts) if parts else r"$\varepsilon$"
        result.append((text, " ".join(comments)))
    return result


def render_rule(name, body):
    alternatives = split_alternatives(body)
    only_terminals = all(text.startswith("\\lit{") and text.count("\\lit{") == 1 and "<" not in text for text, _ in alternatives)
    long_rule = not only_terminals and (
        len(alternatives) > INLINE_ALTERNATIVES
        or len(body) > INLINE_LIMIT
        or "\n" in body.strip()
    )
    head = f"<{name}> ::= "
    if not long_rule:
        text = " | ".join(text for text, _ in alternatives)
        comment = " ".join(c for _, c in alternatives if c)
        return head + text + (" " + tex_comment(comment) if comment else "")
    lines = []
    for index, (text, comment) in enumerate(alternatives):
        line = text + (" " + tex_comment(comment) if comment else "")
        lines.append(head + line if index == 0 else r"  \alt " + line)
    return "\n".join(lines)


def split_sections(source):
    """Yields (title or None, text) for each section of the file."""
    pattern = re.compile(r"\(\*\s*-{5,}\s*\n\s*(\d+\.\s+[^\n]*?)\s*(?:\([^)\n]*\))?\s*\n(.*?)-{5,}\s*\*\)", re.S)
    position = 0
    title = None
    for match in pattern.finditer(source):
        yield title, source[position:match.start()]
        title = re.sub(r"\s*\(.*\)\s*$", "", match.group(1))
        position = match.end()
    yield title, source[position:]


RULE = re.compile(r"^([A-Z][A-Za-z0-9]*)\s*::=(.*?)(?=^[A-Z][A-Za-z0-9]*\s*::=|\Z|^\(\*)", re.S | re.M)


def convert_section(text):
    rules = [render_rule(name, body.strip()) for name, body in RULE.findall(text)]
    return "\n\n".join(rules)


def main():
    thesis = Path(__file__).resolve().parent.parent
    source_path = Path(sys.argv[1]) if len(sys.argv) > 1 else thesis / "grammar.bnf"
    target_path = Path(sys.argv[2]) if len(sys.argv) > 2 else thesis / "chapters" / "grammar.tex"
    source = source_path.read_text(encoding="utf-8")
    # the header and the closing conditions are comments; only sections with rules matter
    output = [
        f"% Generated from {source_path.name} by scripts/bnf2tex.py. Do not edit by hand.",
        "% Usage: \\input{chapters/grammar} inside an appendix or a section.",
        "",
    ]
    for title, text in split_sections(source):
        body = convert_section(text)
        if not body:
            continue
        if title:
            output.append(f"\\subsection*{{{escape(title)}}}")
            output.append("")
        output.append("\\begin{grammar}")
        output.append("")
        output.append(body)
        output.append("")
        output.append("\\end{grammar}")
        output.append("")
    target_path.write_text("\n".join(output), encoding="utf-8")
    print(f"{target_path}: {len(output)} lines")


if __name__ == "__main__":
    main()
