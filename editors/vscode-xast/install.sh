#!/usr/bin/env bash
# Packages the extension and installs it into VS Code. Run again after changing the grammar.
set -euo pipefail
cd "$(dirname "$0")"
python3 package.py
code --install-extension xast-syntax.vsix --force
echo "Installed. Run 'Developer: Reload Window' in VS Code."
