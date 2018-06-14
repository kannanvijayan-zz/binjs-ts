
# BinaryAST - TypeScript Encoder

This is an independent, simple implementation of an encoder
for the BinaryAST format.

The main AST schema is defined in `src/schema.ts` and derived
from `es6.webidl`.

The `src/parse_js.ts` file contains code to lift a parsed
JSON representation of the AST (produced by the shift parser),
into the typed schema, as well as identifying the scopes in
which names are bound, which names are captured in inner
function scopes, building the string table, etc.

The main program logic is defind in `src/index.ts`.

The actual encoding logic is not yet implemented, but will
go into `src/encode_binast.ts`

## Usage

After checkout, run `npm install` to initialize the `node_modules`
directory.

Then, `npm run build` will compile the TypeScript to JS and
place the results in the `dist` subdirectory.

Then, `npm run encode <FILE>` will "encode" a file (i.e.
run the generated `dist/index.js` script on the given `<FILE>`.
