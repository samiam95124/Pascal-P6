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
    Range,
    DocumentSymbol,
    DocumentSymbolParams,
    SymbolKind,
    SignatureHelp,
    SignatureHelpParams,
    SignatureInformation,
    ParameterInformation,
    CompletionItem,
    CompletionItemKind,
    CompletionParams,
    InsertTextFormat,
    CallHierarchyItem,
    CallHierarchyIncomingCall,
    CallHierarchyOutgoingCall,
    CallHierarchyIncomingCallsParams,
    CallHierarchyOutgoingCallsParams,
    CallHierarchyPrepareParams
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
                change: TextDocumentSyncKind.Incremental
            },
            definitionProvider: !!symRunner,
            referencesProvider: !!symRunner,
            hoverProvider: !!symRunner,
            documentSymbolProvider: !!symRunner,
            signatureHelpProvider: symRunner ? {
                triggerCharacters: ['(', ',']
            } : undefined,
            completionProvider: symRunner ? {
                triggerCharacters: ['.']
            } : undefined,
            callHierarchyProvider: !!symRunner
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

// Document symbols (Outline view)
connection.onDocumentSymbol(async (params: DocumentSymbolParams): Promise<DocumentSymbol[] | null> => {
    try {
        const document = documents.get(params.textDocument.uri);
        if (!document) return null;

        // If symbols aren't cached yet (passym still running), trigger and wait
        let cached = symbolCache.get(document.uri);
        if (!cached && symRunner) {
            await updateSymbols(document);
            cached = symbolCache.get(document.uri);
        }
        if (!cached || cached.symbols.length === 0) return null;

        return buildDocumentSymbols(cached.symbols, document);
    } catch (e: any) {
        connection.console.error(`Pascaline: documentSymbol failed: ${e.message}`);
        return null;
    }
});

// Signature help (parameter hints)
connection.onSignatureHelp((params: SignatureHelpParams): SignatureHelp | null => {
    const document = documents.get(params.textDocument.uri);
    if (!document) return null;

    const cached = symbolCache.get(document.uri);
    if (!cached || cached.symbols.length === 0) return null;

    // Get text from start of document to cursor
    const textToCursor = document.getText(
        Range.create(0, 0, params.position.line, params.position.character)
    );

    // Walk backward to find the enclosing open '(' and the function name
    const callInfo = findCallContext(textToCursor);
    if (!callInfo) return null;

    // Look up the function/procedure in symbol cache
    const nameLower = callInfo.name.toLowerCase();
    const cursorLine1 = params.position.line + 1;
    const matches = cached.symbols.filter(
        s => s.name.toLowerCase() === nameLower &&
            (s.klass === 'proc' || s.klass === 'func')
    );
    if (matches.length === 0) return null;

    // Pick innermost scope match declared before cursor
    let best: SymbolInfo | undefined;
    for (const sym of matches) {
        if (sym.line <= cursorLine1) {
            if (!best || sym.level > best.level ||
                (sym.level === best.level && sym.line > best.line)) {
                best = sym;
            }
        }
    }
    if (!best) best = matches[0];

    if (!best.typeName) return null;

    // Parse the parameter signature
    const paramGroups = parseParamSignature(best.typeName);
    if (paramGroups.length === 0) return null;

    // Build the full label and parameter info
    const keyword = best.klass === 'func' ? 'function' : 'procedure';
    const label = `${keyword} ${best.name}${best.typeName}`;

    const parameters: ParameterInformation[] = paramGroups.map(p => {
        // Find the parameter substring in the label
        const start = label.indexOf(p.label);
        const paramLabel: [number, number] = start >= 0
            ? [start, start + p.label.length]
            : [0, 0];
        return ParameterInformation.create(paramLabel);
    });

    const sig = SignatureInformation.create(label, undefined, ...parameters);

    return {
        signatures: [sig],
        activeSignature: 0,
        activeParameter: Math.min(callInfo.argIndex, paramGroups.length - 1)
    };
});

// Code completion
connection.onCompletion((params: CompletionParams): CompletionItem[] | null => {
    const document = documents.get(params.textDocument.uri);
    if (!document) return null;

    const cached = symbolCache.get(document.uri);
    if (!cached || cached.symbols.length === 0) return null;

    const cursorLine1 = params.position.line + 1;
    const items: CompletionItem[] = [];
    const seen = new Set<string>();

    for (const sym of cached.symbols) {
        if (sym.klass === 'alias') continue;

        // Skip symbols declared after cursor (except level 0 standard predefines)
        if (sym.level > 0 && sym.line > cursorLine1) continue;

        // Deduplicate by lowercase name — keep the innermost/latest declaration
        const key = sym.name.toLowerCase();
        if (seen.has(key)) continue;
        seen.add(key);

        const item: CompletionItem = {
            label: sym.name,
            kind: klassToCompletionKind(sym.klass),
            detail: completionDetail(sym),
            insertText: sym.name,
            insertTextFormat: InsertTextFormat.PlainText
        };

        items.push(item);
    }

    return items.length > 0 ? items : null;
});

// Call Hierarchy
connection.languages.callHierarchy.onPrepare((params: CallHierarchyPrepareParams): CallHierarchyItem[] | null => {
    const document = documents.get(params.textDocument.uri);
    if (!document) return null;

    const cached = symbolCache.get(document.uri);
    if (!cached || cached.symbols.length === 0) return null;

    const line = document.getText(
        Range.create(params.position.line, 0, params.position.line + 1, 0)
    );
    const word = getWordAt(line, params.position.character);
    if (!word) return null;

    const wordLower = word.toLowerCase();
    const matches = cached.symbols.filter(
        s => s.name.toLowerCase() === wordLower &&
            (s.klass === 'proc' || s.klass === 'func')
    );
    if (matches.length === 0) return null;

    return matches.map(sym => {
        const declLine = sym.line - 1;
        const lineText = document.getText(Range.create(declLine, 0, declLine + 1, 0));
        const lineLen = lineText.replace(/\r?\n$/, '').length;
        const col = findIdentifierColumn(lineText, sym.name);
        const endLine = sym.endLine ? sym.endLine - 1 : declLine;
        return {
            name: sym.name,
            kind: SymbolKind.Function,
            uri: document.uri,
            range: Range.create(declLine, 0, endLine, lineLen),
            selectionRange: Range.create(declLine, col, declLine, col + sym.name.length),
            detail: sym.typeName || undefined,
            data: { declLine: sym.line, level: sym.level }
        } as CallHierarchyItem;
    });
});

connection.languages.callHierarchy.onIncomingCalls((params: CallHierarchyIncomingCallsParams): CallHierarchyIncomingCall[] | null => {
    const uri = params.item.uri;
    const cached = symbolCache.get(uri);
    if (!cached) return null;

    const document = documents.get(uri);
    if (!document) return null;

    const targetName = params.item.name.toLowerCase();

    // Build sorted list of proc/func symbols with ranges for containment lookup
    const funcs = cached.symbols
        .filter(s => (s.klass === 'proc' || s.klass === 'func') && s.endLine)
        .sort((a, b) => a.line - b.line);

    // Find all references to the target function
    const targetRefs = cached.references.filter(
        r => r.name.toLowerCase() === targetName
    );

    // Group references by containing function
    const callerMap = new Map<string, { sym: typeof funcs[0]; fromRanges: Range[] }>();

    for (const ref of targetRefs) {
        const container = findContainingFunc(funcs, ref.refLine);
        if (!container) continue;

        const key = `${container.name.toLowerCase()}:${container.line}`;
        if (!callerMap.has(key)) {
            callerMap.set(key, { sym: container, fromRanges: [] });
        }

        const refLine0 = ref.refLine - 1;
        const refLineText = document.getText(Range.create(refLine0, 0, refLine0 + 1, 0));
        const col = findIdentifierColumn(refLineText, ref.name);
        callerMap.get(key)!.fromRanges.push(
            Range.create(refLine0, col, refLine0, col + ref.name.length)
        );
    }

    const results: CallHierarchyIncomingCall[] = [];
    for (const { sym, fromRanges } of callerMap.values()) {
        const declLine = sym.line - 1;
        const lineText = document.getText(Range.create(declLine, 0, declLine + 1, 0));
        const lineLen = lineText.replace(/\r?\n$/, '').length;
        const col = findIdentifierColumn(lineText, sym.name);
        const endLine = sym.endLine ? sym.endLine - 1 : declLine;

        results.push({
            from: {
                name: sym.name,
                kind: SymbolKind.Function,
                uri: uri,
                range: Range.create(declLine, 0, endLine, lineLen),
                selectionRange: Range.create(declLine, col, declLine, col + sym.name.length),
                detail: sym.typeName || undefined
            },
            fromRanges
        });
    }

    return results.length > 0 ? results : null;
});

connection.languages.callHierarchy.onOutgoingCalls((params: CallHierarchyOutgoingCallsParams): CallHierarchyOutgoingCall[] | null => {
    const uri = params.item.uri;
    const cached = symbolCache.get(uri);
    if (!cached) return null;

    const document = documents.get(uri);
    if (!document) return null;

    const data = params.item.data as { declLine: number; level: number } | undefined;
    const callerLine = data?.declLine ?? (params.item.selectionRange.start.line + 1);

    // Find the caller symbol to get its line range
    const caller = cached.symbols.find(
        s => s.name.toLowerCase() === params.item.name.toLowerCase() &&
            (s.klass === 'proc' || s.klass === 'func') &&
            s.line === callerLine
    );
    if (!caller || !caller.endLine) return null;

    // Build a lookup of proc/func symbols by lowercase name
    const funcMap = new Map<string, SymbolInfo[]>();
    for (const sym of cached.symbols) {
        if (sym.klass === 'proc' || sym.klass === 'func') {
            const key = sym.name.toLowerCase();
            if (!funcMap.has(key)) funcMap.set(key, []);
            funcMap.get(key)!.push(sym);
        }
    }

    // Find all references within the caller's body that point to proc/func symbols
    const calleeMap = new Map<string, { sym: SymbolInfo; fromRanges: Range[] }>();

    for (const ref of cached.references) {
        if (ref.refLine < caller.line || ref.refLine > caller.endLine) continue;

        const targets = funcMap.get(ref.name.toLowerCase());
        if (!targets || targets.length === 0) continue;

        // Pick the best matching declaration
        const target = targets.find(t => t.line === ref.declLine) || targets[0];
        const key = `${target.name.toLowerCase()}:${target.line}`;

        if (!calleeMap.has(key)) {
            calleeMap.set(key, { sym: target, fromRanges: [] });
        }

        const refLine0 = ref.refLine - 1;
        const refLineText = document.getText(Range.create(refLine0, 0, refLine0 + 1, 0));
        const col = findIdentifierColumn(refLineText, ref.name);
        calleeMap.get(key)!.fromRanges.push(
            Range.create(refLine0, col, refLine0, col + ref.name.length)
        );
    }

    const results: CallHierarchyOutgoingCall[] = [];
    for (const { sym, fromRanges } of calleeMap.values()) {
        const declLine = sym.line - 1;
        const lineText = document.getText(Range.create(declLine, 0, declLine + 1, 0));
        const lineLen = lineText.replace(/\r?\n$/, '').length;
        const col = findIdentifierColumn(lineText, sym.name);
        const endLine = sym.endLine ? sym.endLine - 1 : declLine;

        results.push({
            to: {
                name: sym.name,
                kind: SymbolKind.Function,
                uri: uri,
                range: Range.create(declLine, 0, endLine, lineLen),
                selectionRange: Range.create(declLine, col, declLine, col + sym.name.length),
                detail: sym.typeName || undefined
            },
            fromRanges
        });
    }

    return results.length > 0 ? results : null;
});

/** Find the proc/func that contains a given line number (1-indexed) */
function findContainingFunc(funcs: SymbolInfo[], line: number): SymbolInfo | undefined {
    let best: SymbolInfo | undefined;
    for (const f of funcs) {
        if (f.line <= line && f.endLine && f.endLine >= line) {
            // Pick innermost (highest level, or latest declaration)
            if (!best || f.level > best.level ||
                (f.level === best.level && f.line > best.line)) {
                best = f;
            }
        }
    }
    return best;
}

function klassToCompletionKind(klass: string): CompletionItemKind {
    switch (klass) {
        case 'proc':   return CompletionItemKind.Function;
        case 'func':   return CompletionItemKind.Function;
        case 'types':  return CompletionItemKind.Struct;
        case 'konst':  return CompletionItemKind.Constant;
        case 'vars':   return CompletionItemKind.Variable;
        case 'field':  return CompletionItemKind.Field;
        case 'fixedt': return CompletionItemKind.TypeParameter;
        default:       return CompletionItemKind.Text;
    }
}

function completionDetail(sym: SymbolInfo): string {
    if (sym.klass === 'konst' && sym.value) {
        return sym.typeName ? `${sym.typeName} = ${sym.value}` : `= ${sym.value}`;
    }
    if (sym.klass === 'proc' || sym.klass === 'func') {
        return sym.typeName || '';
    }
    return sym.typeName || '';
}

/** Walk backward through text to find the function call context at cursor */
function findCallContext(text: string): { name: string; argIndex: number } | null {
    let depth = 0;
    let argIndex = 0;
    let i = text.length - 1;

    // Walk backward, tracking parenthesis depth and counting commas
    while (i >= 0) {
        const ch = text[i];
        if (ch === ')') {
            depth++;
        } else if (ch === '(') {
            if (depth === 0) {
                // Found our open paren — extract the identifier before it
                let end = i;
                // Skip whitespace before '('
                while (end > 0 && /\s/.test(text[end - 1])) end--;
                let start = end;
                while (start > 0 && isIdentChar(text[start - 1])) start--;
                const name = text.substring(start, end);
                if (name.length > 0) {
                    return { name, argIndex };
                }
                return null;
            }
            depth--;
        } else if (ch === ',' && depth === 0) {
            argIndex++;
        }
        // Skip string literals (walk backward past '...')
        if (ch === "'" && i > 0) {
            i--;
            while (i > 0 && text[i] !== "'") i--;
        }
        i--;
    }
    return null;
}

/** Parse a Pascal parameter signature like "(var x: integer; y, z: real): boolean" into parameter groups */
function parseParamSignature(sig: string): { label: string }[] {
    // Extract content between outer parens
    const openParen = sig.indexOf('(');
    if (openParen < 0) return [];
    const closeParen = sig.lastIndexOf(')');
    if (closeParen < 0) return [];
    const inner = sig.substring(openParen + 1, closeParen).trim();
    if (inner.length === 0) return [];

    // Split on ';' respecting nested parens
    const groups: string[] = [];
    let depth = 0;
    let start = 0;
    for (let i = 0; i < inner.length; i++) {
        if (inner[i] === '(') depth++;
        else if (inner[i] === ')') depth--;
        else if (inner[i] === ';' && depth === 0) {
            groups.push(inner.substring(start, i).trim());
            start = i + 1;
        }
    }
    groups.push(inner.substring(start).trim());

    // Expand groups with multiple names (e.g., "x, y: integer" -> two params)
    const params: { label: string }[] = [];
    for (const group of groups) {
        const colonIdx = group.indexOf(':');
        if (colonIdx < 0) {
            // No type (unlikely but handle it)
            params.push({ label: group });
            continue;
        }
        const namesStr = group.substring(0, colonIdx).trim();
        const typeStr = group.substring(colonIdx).trim(); // includes ": type"

        // Check for var/out prefix
        let prefix = '';
        let names = namesStr;
        const prefixMatch = namesStr.match(/^(var|out)\s+/i);
        if (prefixMatch) {
            prefix = prefixMatch[1] + ' ';
            names = namesStr.substring(prefixMatch[0].length);
        }

        const nameList = names.split(',').map(n => n.trim()).filter(n => n.length > 0);
        for (const name of nameList) {
            params.push({ label: `${prefix}${name}${typeStr}` });
        }
    }
    return params;
}

function klassToSymbolKind(klass: string): SymbolKind {
    switch (klass) {
        case 'proc':   return SymbolKind.Function;
        case 'func':   return SymbolKind.Function;
        case 'types':  return SymbolKind.Struct;
        case 'konst':  return SymbolKind.Constant;
        case 'vars':   return SymbolKind.Variable;
        case 'field':  return SymbolKind.Field;
        case 'fixedt': return SymbolKind.TypeParameter;
        default:       return SymbolKind.Variable;
    }
}

function symbolDetail(sym: SymbolInfo): string {
    if (sym.klass === 'konst' && sym.value) {
        return sym.typeName ? `${sym.typeName} = ${sym.value}` : `= ${sym.value}`;
    }
    return sym.typeName || '';
}

function buildDocumentSymbols(symbols: SymbolInfo[], document: TextDocument): DocumentSymbol[] {
    const roots: DocumentSymbol[] = [];
    const stack: { level: number; node: DocumentSymbol }[] = [];
    const lineCount = document.lineCount;

    for (const sym of symbols) {
        // Skip aliases and standard-library symbols (level 0)
        if (sym.klass === 'alias' || sym.level === 0) continue;

        const line = sym.line - 1; // 0-indexed
        if (line < 0 || line >= lineCount) continue;

        const lineText = document.getText(Range.create(line, 0, line + 1, 0));
        const lineLen = lineText.replace(/\r?\n$/, '').length;
        const col = findIdentifierColumn(lineText, sym.name);
        const selEnd = Math.min(col + sym.name.length, lineLen);

        const node: DocumentSymbol = {
            name: sym.name,
            detail: symbolDetail(sym),
            kind: klassToSymbolKind(sym.klass),
            range: Range.create(line, 0, line, lineLen),
            selectionRange: Range.create(line, col, line, selEnd),
            children: []
        };

        // Pop stack until we find a parent with level < this symbol's level
        while (stack.length > 0 && stack[stack.length - 1].level >= sym.level) {
            stack.pop();
        }

        if (stack.length > 0) {
            stack[stack.length - 1].node.children!.push(node);
        } else {
            roots.push(node);
        }

        // Types, procs, and funcs can contain children
        if (sym.klass === 'proc' || sym.klass === 'func' || sym.klass === 'types') {
            stack.push({ level: sym.level, node });
        }
    }

    return roots;
}

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
