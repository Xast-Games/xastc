#!/usr/bin/env python3
"""Sort Haskell imports: qualified ones first, then groups by top-level module.

Usage: sort_imports.py [--check] [PATH ...]
PATH is a .hs file or a directory (searched recursively). Default: src app test.
"""
import re
import sys
from pathlib import Path

IMPORT = re.compile(r"import\s+(?:\{-#\s*SOURCE\s*#-\}\s+)?(?:safe\s+)?(?:qualified\s+)?(?:\"[^\"]*\"\s+)?([A-Z][\w.']*)")


def is_import(line):
    return re.match(r"import\s", line) is not None


def module_name(entry):
    match = IMPORT.match(next(l for l in entry if is_import(l)))
    return match.group(1) if match else ""


def split_entries(lines):
    """Group lines into entries: leading comments + import + continuation lines."""
    entries, pending = [], []
    for line in lines:
        if not line.strip():
            continue
        if is_import(line):
            entries.append(pending + [line])
            pending = []
        elif line[0].isspace() and entries and not pending:
            entries[-1].append(line)
        else:
            pending.append(line)
    if pending:
        entries.append(pending)  # trailing comments, kept last
    return entries


def is_qualified(entry):
    header = next(l for l in entry if is_import(l))
    return re.search(r"\bqualified\b", header.split("(")[0]) is not None


def format_block(lines):
    entries = [e for e in split_entries(lines) if any(is_import(l) for l in e)]
    qualified = [(module_name(e), e) for e in entries if is_qualified(e)]
    groups = {}
    for entry in entries:
        if not is_qualified(entry):
            name = module_name(entry)
            groups.setdefault(name.split(".")[0], []).append((name, entry))
    chunks = [sorted(qualified, key=lambda pair: pair[0])] if qualified else []
    chunks += [sorted(groups[top], key=lambda pair: pair[0]) for top in sorted(groups)]
    return ["\n".join(line for _, entry in chunk for line in entry) for chunk in chunks]


def import_block_end(lines, start):
    """Index of the last line of the import block that begins at start."""
    end = start
    in_import = False
    for i in range(start, len(lines)):
        line = lines[i]
        if is_import(line):
            end, in_import = i, True
        elif line[:1].isspace() and in_import:
            end = i  # continuation of a multi-line import
        elif not line.strip() or line.startswith("--") or line.startswith("{-"):
            in_import = False  # blanks and comments only count if an import follows
        else:
            break
    return end


def sort_source(text):
    lines = text.split("\n")
    start = next((i for i, l in enumerate(lines) if is_import(l)), None)
    if start is None:
        return text
    end = import_block_end(lines, start)
    new_block = "\n\n".join(format_block(lines[start : end + 1])).split("\n")
    return "\n".join(lines[:start] + new_block + lines[end + 1 :])


def collect(paths):
    for path in paths:
        path = Path(path)
        if path.is_dir():
            yield from sorted(path.rglob("*.hs"))
        elif path.is_file():
            yield path


def main():
    args = sys.argv[1:]
    check = "--check" in args
    paths = [a for a in args if a != "--check"] or ["src", "app", "test"]
    dirty = 0
    for path in collect(paths):
        text = path.read_text()
        result = sort_source(text)
        if result != text:
            dirty += 1
            print(("would change " if check else "sorted ") + str(path))
            if not check:
                path.write_text(result)
    sys.exit(1 if check and dirty else 0)


if __name__ == "__main__":
    main()
