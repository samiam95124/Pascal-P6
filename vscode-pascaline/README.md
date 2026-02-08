# Pascaline

Language support and debugging for the Pascaline programming language
(ISO 7185 Pascal extended) in Visual Studio Code.

## Features

### Syntax Highlighting

Full syntax coloring for Pascaline and ISO 7185 Pascal, including keywords,
types, operators, string escape sequences, numeric literals (decimal, hex,
octal, binary), and all three comment styles (`{ }`, `(* *)`, `!`).

### Language Server

- **Diagnostics** -- real-time error checking on save and open
- **Go to Definition** -- navigate to symbol declarations
- **Find All References** -- locate every use of a symbol
- **Hover** -- display type signatures, constant values, and documentation
- **Document Outline** -- structured symbol view in the Outline panel
- **Signature Help** -- parameter hints when calling procedures and functions
- **Code Completion** -- context-aware identifier suggestions
- **Call Hierarchy** -- view incoming and outgoing calls for any procedure

### Debugger

Source-level debugging via the Pascal-P6 interpreter (`pint`):

- Breakpoints
- Step in, step over, step out, continue
- Variable inspection (locals, globals, parameters)
- Call stack
- Expression evaluation

### Documentation Viewer

Right-click **Open Pasdoc Documentation** to view generated HTML documentation
for the current source file in a VS Code webview panel, scrolled to the
selected symbol.

## Requirements

This extension requires the Pascal-P6 toolchain to be installed:

- `source/parser` -- used by the language server for diagnostics
- `utils/passym` -- used by the language server for symbol information
- `pint` -- used by the debugger
- `pc` -- used by the debugger to compile before debugging

By default, the extension looks for these tools relative to the workspace root.
Paths can be configured in settings.

## Extension Settings

- `pascaline.parserPath` -- Path to the parser executable
- `pascaline.symPath` -- Path to the passym executable
- `pascaline.checkOnSave` -- Run parser on file save (default: true)
- `pascaline.checkOnOpen` -- Run parser on file open (default: true)

## More Information

- [Pascal-P6 on GitHub](https://github.com/samiam95124/Pascal-P6)
- [VS Code Extension Implementation Assessment](https://github.com/samiam95124/Pascal-P6/discussions/323)
