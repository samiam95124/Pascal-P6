import {
    createConnection,
    TextDocuments,
    Diagnostic,
    DiagnosticSeverity,
    ProposedFeatures,
    InitializeParams,
    InitializeResult,
    TextDocumentSyncKind,
    DefinitionParams,
    ReferenceParams,
    HoverParams,
    Hover,
    MarkupKind,
    Location,
    Range
} from 'vscode-languageserver/node';

import { TextDocument } from 'vscode-languageserver-textdocument';

import { ParserRunner } from './parserRunner';
import { SymRunner, SymbolInfo, SymResult } from './symRunner';

const connection = createConnection(ProposedFeatures.all);
const documents = new TextDocuments(TextDocument);

let parserRunner: ParserRunner | undefined;
let symRunner: SymRunner | undefined;

// Per-file symbol/reference cache
const symbolCache = new Map<string, SymResult>();

interface Settings {
    parserPath?: string;
    symPath?: string;
    checkOnSave: boolean;
    checkOnOpen: boolean;
}

let settings: Settings = {
    checkOnSave: true,
    checkOnOpen: true
};

connection.onInitialize((params: InitializeParams): InitializeResult => {
    const workspaceFolders = params.workspaceFolders;
    const workspaceRoot = workspaceFolders?.[0]?.uri
        ? decodeURIComponent(workspaceFolders[0].uri.replace('file://', ''))
        : undefined;

    // Try to find the parser
    const parserPath = ParserRunner.findParser(settings.parserPath, workspaceRoot);
    if (parserPath) {
        parserRunner = new ParserRunner(parserPath);
        connection.console.log(`Pascaline: using parser at ${parserPath}`);
    } else {
        connection.console.warn(
            'Pascaline: parser executable not found. ' +
            'Set pascaline.parserPath or ensure source/parser exists in workspace.'
        );
    }

    // Try to find passym
    const symPath = SymRunner.findSym(settings.symPath, workspaceRoot);
    if (symPath) {
        symRunner = new SymRunner(symPath);
        connection.console.log(`Pascaline: using passym at ${symPath}`);
    } else {
        connection.console.warn(
            'Pascaline: passym executable not found. ' +
            'Go-to-definition will be unavailable.'
        );
    }

    return {
        capabilities: {
            textDocumentSync: {
                openClose: true,
                save: true,
                change: TextDocumentSyncKind.None
            },
            definitionProvider: !!symRunner,
            referencesProvider: !!symRunner,
            hoverProvider: !!symRunner
        }
    };
});

connection.onInitialized(() => {
    connection.workspace.getConfiguration('pascaline').then((config: any) => {
        if (config) {
            if (config.parserPath) settings.parserPath = config.parserPath;
            if (config.symPath) settings.symPath = config.symPath;
            if (config.checkOnSave !== undefined) settings.checkOnSave = config.checkOnSave;
            if (config.checkOnOpen !== undefined) settings.checkOnOpen = config.checkOnOpen;

            if (config.parserPath && !parserRunner) {
                const parserPath = ParserRunner.findParser(config.parserPath);
                if (parserPath) {
                    parserRunner = new ParserRunner(parserPath);
                    connection.console.log(`Pascaline: using parser at ${parserPath}`);
                }
            }
            if (config.symPath && !symRunner) {
                const symPath = SymRunner.findSym(config.symPath);
                if (symPath) {
                    symRunner = new SymRunner(symPath);
                    connection.console.log(`Pascaline: using passym at ${symPath}`);
                }
            }
        }
    });
});

// Check on open
documents.onDidOpen(async (event) => {
    if (settings.checkOnOpen) {
        await validateDocument(event.document);
    }
    await updateSymbols(event.document);
});

// Check on save
documents.onDidSave(async (event) => {
    if (settings.checkOnSave) {
        await validateDocument(event.document);
    }
    await updateSymbols(event.document);
});

// Clear caches when document is closed
documents.onDidClose((event) => {
    connection.sendDiagnostics({ uri: event.document.uri, diagnostics: [] });
    symbolCache.delete(event.document.uri);
});

// Go-to-definition
connection.onDefinition((params: DefinitionParams): Location | null => {
    const document = documents.get(params.textDocument.uri);
    if (!document) return null;

    const cached = symbolCache.get(document.uri);
    if (!cached || cached.symbols.length === 0) return null;
    const symbols = cached.symbols;

    // Extract the word at the cursor position
    const line = document.getText(
        Range.create(params.position.line, 0, params.position.line + 1, 0)
    );
    const word = getWordAt(line, params.position.character);
    if (!word) return null;

    // Find matching symbols (case-insensitive, Pascal is case-insensitive)
    const wordLower = word.toLowerCase();
    const matches = symbols.filter(
        s => s.name.toLowerCase() === wordLower && s.klass !== 'alias'
    );

    if (matches.length === 0) {
        // Try including aliases
        const aliasMatches = symbols.filter(
            s => s.name.toLowerCase() === wordLower
        );
        if (aliasMatches.length === 0) return null;
        return symbolToLocation(aliasMatches[0], document, params.position.line);
    }

    // If single match, return it
    if (matches.length === 1) {
        return symbolToLocation(matches[0], document, params.position.line);
    }

    // Multiple matches: pick the innermost scope whose declaration is before cursor
    const cursorLine = params.position.line + 1; // convert to 1-indexed
    let best: SymbolInfo | undefined;
    for (const sym of matches) {
        if (sym.line <= cursorLine) {
            if (!best || sym.level > best.level ||
                (sym.level === best.level && sym.line > best.line)) {
                best = sym;
            }
        }
    }

    if (!best) best = matches[0]; // fallback to first match
    return symbolToLocation(best, document, params.position.line);
});

// Find all references
connection.onReferences((params: ReferenceParams): Location[] | null => {
    const document = documents.get(params.textDocument.uri);
    if (!document) return null;

    const cached = symbolCache.get(document.uri);
    if (!cached) return null;

    // Extract the word at the cursor position
    const line = document.getText(
        Range.create(params.position.line, 0, params.position.line + 1, 0)
    );
    const word = getWordAt(line, params.position.character);
    if (!word) return null;

    const wordLower = word.toLowerCase();
    const locations: Location[] = [];

    // If includeDeclaration, add matching symbol declarations
    if (params.context.includeDeclaration) {
        for (const sym of cached.symbols) {
            if (sym.name.toLowerCase() === wordLower && sym.klass !== 'alias') {
                locations.push(symbolToLocation(sym, document, params.position.line));
            }
        }
    }

    // Add all reference sites that match by name
    for (const ref of cached.references) {
        if (ref.name.toLowerCase() === wordLower) {
            const refLine = ref.refLine - 1; // 0-indexed
            const lineText = document.getText(
                Range.create(refLine, 0, refLine + 1, 0)
            );
            const col = findIdentifierColumn(lineText, ref.name);
            locations.push(Location.create(
                document.uri,
                Range.create(refLine, col, refLine, col + ref.name.length)
            ));
        }
    }

    return locations.length > 0 ? locations : null;
});

// Hover information
connection.onHover((params: HoverParams): Hover | null => {
    const document = documents.get(params.textDocument.uri);
    if (!document) return null;

    const cached = symbolCache.get(document.uri);
    if (!cached || cached.symbols.length === 0) return null;
    const symbols = cached.symbols;

    // Extract the word at the cursor position
    const line = document.getText(
        Range.create(params.position.line, 0, params.position.line + 1, 0)
    );
    const word = getWordAt(line, params.position.character);
    if (!word) return null;

    // Find matching symbol (same scope resolution as onDefinition)
    const wordLower = word.toLowerCase();
    const matches = symbols.filter(
        s => s.name.toLowerCase() === wordLower && s.klass !== 'alias'
    );
    if (matches.length === 0) return null;

    let best: SymbolInfo;
    if (matches.length === 1) {
        best = matches[0];
    } else {
        const cursorLine = params.position.line + 1;
        let b: SymbolInfo | undefined;
        for (const sym of matches) {
            if (sym.line <= cursorLine) {
                if (!b || sym.level > b.level ||
                    (sym.level === b.level && sym.line > b.line)) {
                    b = sym;
                }
            }
        }
        best = b || matches[0];
    }

    // Format hover content
    let text: string;
    switch (best.klass) {
        case 'vars':
        case 'field':
            text = best.typeName
                ? `var ${best.name}: ${best.typeName}`
                : `var ${best.name}`;
            break;
        case 'konst': {
            let valueStr = best.value;
            if (valueStr && best.typeName) {
                if (best.typeName === 'boolean') {
                    valueStr = valueStr === '1' ? 'true' : 'false';
                } else if (best.typeName === 'char') {
                    const code = parseInt(valueStr);
                    if (!isNaN(code) && code >= 32 && code < 127) {
                        valueStr = `'${String.fromCharCode(code)}'`;
                    }
                }
            }
            text = best.typeName
                ? `const ${best.name}: ${best.typeName}`
                : `const ${best.name}`;
            if (valueStr) {
                text += ` = ${valueStr}`;
            }
            break;
        }
        case 'types': {
            // Try to show the actual definition from the source line
            const typeDeclLine = best.line - 1;
            const typeDeclText = document.getText(
                Range.create(typeDeclLine, 0, typeDeclLine + 1, 0)
            );
            const defn = extractDefinition(typeDeclText, best.name);
            text = defn
                ? `type ${best.name} = ${defn}`
                : `type ${best.name}`;
            break;
        }
        case 'proc':
            text = best.typeName
                ? `procedure ${best.name}${best.typeName}`
                : `procedure ${best.name}`;
            break;
        case 'func':
            text = best.typeName
                ? `function ${best.name}${best.typeName}`
                : `function ${best.name}`;
            break;
        default:
            text = `${best.klass} ${best.name}`;
    }

    // Extract trailing comment from declaration line
    const declLine0 = best.line - 1;
    const declText = document.getText(
        Range.create(declLine0, 0, declLine0 + 1, 0)
    );
    const comment = extractComment(declText);

    let markdown = '```pascaline\n' + text + '\n```';
    if (comment) {
        markdown += '\n\n' + comment;
    }

    return {
        contents: {
            kind: MarkupKind.Markdown,
            value: markdown
        }
    };
});

/** Extract a trailing Pascal comment from a source line: { ... } or (* ... *) */
function extractComment(lineText: string): string | undefined {
    // Look for { ... } comment (but not star-decoration lines like {*** })
    let idx = lineText.indexOf('{');
    if (idx >= 0) {
        // Skip star-decoration comments
        const afterBrace = lineText.substring(idx + 1).trimStart();
        if (afterBrace.startsWith('***')) return undefined;
        const end = lineText.indexOf('}', idx);
        const commentText = end >= 0
            ? lineText.substring(idx + 1, end).trim()
            : lineText.substring(idx + 1).trim();
        return commentText || undefined;
    }
    // Look for (* ... *) comment
    idx = lineText.indexOf('(*');
    if (idx >= 0) {
        const end = lineText.indexOf('*)', idx);
        const commentText = end >= 0
            ? lineText.substring(idx + 2, end).trim()
            : lineText.substring(idx + 2).trim();
        return commentText || undefined;
    }
    return undefined;
}

/** Extract the definition part after '=' from a type or const declaration line */
function extractDefinition(lineText: string, name: string): string | undefined {
    const lower = lineText.toLowerCase();
    const nameLower = name.toLowerCase();
    const nameIdx = lower.indexOf(nameLower);
    if (nameIdx < 0) return undefined;

    // Find '=' after the name
    const eqIdx = lineText.indexOf('=', nameIdx + nameLower.length);
    if (eqIdx < 0) return undefined;

    let rest = lineText.substring(eqIdx + 1);
    // Remove trailing comment
    let commentIdx = rest.indexOf('{');
    if (commentIdx < 0) commentIdx = rest.indexOf('(*');
    if (commentIdx >= 0) rest = rest.substring(0, commentIdx);
    // Remove trailing semicolon and whitespace
    rest = rest.replace(/;\s*$/, '').trim();

    return rest || undefined;
}

function symbolToLocation(sym: SymbolInfo, document: TextDocument, _cursorLine: number): Location {
    const declLine = sym.line - 1; // 0-indexed

    // Find the exact column by searching for the identifier on the declaration line
    const lineText = document.getText(
        Range.create(declLine, 0, declLine + 1, 0)
    );
    const col = findIdentifierColumn(lineText, sym.name);

    return Location.create(
        document.uri,
        Range.create(declLine, col, declLine, col + sym.name.length)
    );
}

function findIdentifierColumn(lineText: string, name: string): number {
    // Case-insensitive search for the identifier as a whole word
    const lower = lineText.toLowerCase();
    const nameLower = name.toLowerCase();
    let pos = 0;
    while (pos < lower.length) {
        const idx = lower.indexOf(nameLower, pos);
        if (idx < 0) break;

        // Check word boundaries
        const before = idx > 0 ? lower[idx - 1] : ' ';
        const after = idx + nameLower.length < lower.length
            ? lower[idx + nameLower.length] : ' ';

        if (!isIdentChar(before) && !isIdentChar(after)) {
            return idx;
        }
        pos = idx + 1;
    }
    return 0; // fallback to start of line
}

function isIdentChar(ch: string): boolean {
    return /[a-z0-9_]/i.test(ch);
}

function getWordAt(line: string, character: number): string | undefined {
    if (character >= line.length) return undefined;

    let start = character;
    while (start > 0 && isIdentChar(line[start - 1])) start--;
    let end = character;
    while (end < line.length && isIdentChar(line[end])) end++;

    const word = line.substring(start, end);
    return word.length > 0 ? word : undefined;
}

async function validateDocument(document: TextDocument): Promise<void> {
    if (!parserRunner) return;

    const uri = document.uri;
    const filePath = decodeURIComponent(uri.replace('file://', ''));
    if (!filePath.endsWith('.pas')) return;

    try {
        const errors = await parserRunner.run(filePath);
        const diagnostics: Diagnostic[] = errors.map(err => {
            const line = Math.max(0, err.line - 1);
            return {
                severity: DiagnosticSeverity.Error,
                range: {
                    start: { line, character: err.column },
                    end: { line, character: err.column + 1 }
                },
                message: err.message,
                code: err.code,
                source: 'pascaline'
            };
        });

        connection.sendDiagnostics({ uri, diagnostics });
    } catch (e: any) {
        connection.console.error(`Pascaline: parser failed: ${e.message}`);
    }
}

async function updateSymbols(document: TextDocument): Promise<void> {
    if (!symRunner) return;

    const filePath = decodeURIComponent(document.uri.replace('file://', ''));
    if (!filePath.endsWith('.pas')) return;

    try {
        const result = await symRunner.run(filePath);
        symbolCache.set(document.uri, result);
    } catch (e: any) {
        connection.console.error(`Pascaline: passym failed: ${e.message}`);
    }
}

documents.listen(connection);
connection.listen();
