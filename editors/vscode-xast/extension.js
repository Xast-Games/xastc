// Semantic tokens for Xast: module names as `namespace` and generics in
// signatures as `typeParameter`, like the Scala and Haskell language servers,
// so themes give them the same colors (e.g. magenta italic modules).
const vscode = require('vscode');

const legend = new vscode.SemanticTokensLegend(['namespace', 'typeParameter'], []);
const NAMESPACE = 0;
const TYPE_PARAMETER = 1;

const UPPER = "[A-Z][A-Za-z0-9_']*";
const MODULE_PATH = `${UPPER}(?:\\.${UPPER})*`;

// e.g. `module Std.Option exports`, `use Std.Bool *`, `use Std.Option as O`
const MODULE_LINE = new RegExp(`^\\s*(?:module|use)\\s+(${MODULE_PATH})(?:\\s+as\\s+(${UPPER}))?`);
// e.g. the `M` of `M.inc` or `O.Some`
const QUALIFIER = new RegExp(`\\b(${UPPER})\\.(?=[A-Za-z_])`, 'g');
// e.g. `fn map (fn(a) -> b, Option a) -> Option b;`: the part after the name
const SIGNATURE = /^\s*(?:extern\s+)?fn\s+[a-z_][A-Za-z0-9_']*(\s*\([^=]*->[^=]*;)/;
const LOWER_WORD = /\b[a-z][A-Za-z0-9_']*\b/g;
const NOT_GENERICS = new Set(['fn', 'number', 'concatenative']);

/** The line without its comment, e.g. `use Std.Bool *` for `use Std.Bool * -- bools`. */
function withoutComment(line) {
   const comment = line.indexOf('--');
   return comment === -1 ? line : line.slice(0, comment);
}

/** Tokens of one line, as [start, length, type]. */
function lineTokens(line) {
   const code = withoutComment(line);

   const moduleLine = MODULE_LINE.exec(code);
   if (moduleLine) {
      const tokens = [[code.indexOf(moduleLine[1]), moduleLine[1].length, NAMESPACE]];
      if (moduleLine[2]) {
         tokens.push([code.lastIndexOf(moduleLine[2]), moduleLine[2].length, NAMESPACE]);
      }
      return tokens;
   }

   const tokens = [];
   for (const qualifier of code.matchAll(QUALIFIER)) {
      tokens.push([qualifier.index, qualifier[1].length, NAMESPACE]);
   }

   const signature = SIGNATURE.exec(code);
   if (signature) {
      const start = code.length - signature[1].length;
      for (const word of signature[1].matchAll(LOWER_WORD)) {
         if (!NOT_GENERICS.has(word[0])) {
            tokens.push([start + word.index, word[0].length, TYPE_PARAMETER]);
         }
      }
   }

   return tokens.sort((first, second) => first[0] - second[0]);
}

const provider = {
   provideDocumentSemanticTokens(document) {
      const builder = new vscode.SemanticTokensBuilder(legend);
      for (let lineNumber = 0; lineNumber < document.lineCount; lineNumber++) {
         for (const [start, length, type] of lineTokens(document.lineAt(lineNumber).text)) {
            builder.push(lineNumber, start, length, type, 0);
         }
      }
      return builder.build();
   },
};

function activate(context) {
   context.subscriptions.push(
      vscode.languages.registerDocumentSemanticTokensProvider({ language: 'xast' }, provider, legend)
   );
}

module.exports = { activate, lineTokens };
