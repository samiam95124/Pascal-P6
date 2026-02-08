import { execFile } from 'child_process';
import * as path from 'path';
import * as fs from 'fs';

export interface SymbolInfo {
    name: string;
    klass: string;    // vars, konst, types, field, proc, func, alias, fixedt
    line: number;     // 1-indexed declaration line
    level: number;    // scope level (0=standard, 1=program, 2+=nested)
    typeName?: string; // resolved type name (e.g., "integer", "char", "array")
    value?: string;   // constant value (for konst class)
    endLine?: number; // 1-indexed end line of body (for proc/func)
}

export interface ReferenceInfo {
    name: string;
    refLine: number;   // 1-indexed line where identifier is used
    declLine: number;  // 1-indexed line where identifier is declared
    declLevel: number; // scope level of declaration
}

export interface SymResult {
    symbols: SymbolInfo[];
    references: ReferenceInfo[];
}

/**
 * Spawns the passym executable and parses its symbol table output.
 *
 * Output format (tab-separated):
 *   SYM\t<name>\t<class>\t<line>\t<level>\t<typename>
 *   REF\t<name>\t<use_line>\t<decl_line>\t<decl_level>
 */
export class SymRunner {
    private symPath: string;

    constructor(symPath: string) {
        this.symPath = symPath;
    }

    run(filePath: string): Promise<SymResult> {
        return new Promise((resolve) => {
            const dir = path.dirname(filePath);
            const base = path.basename(filePath, '.pas');

            execFile(this.symPath, [base], {
                cwd: dir,
                timeout: 30000
            }, (error, stdout, _stderr) => {
                if (!stdout) {
                    resolve({ symbols: [], references: [] });
                    return;
                }
                resolve(this.parseOutput(stdout));
            });
        });
    }

    private parseOutput(output: string): SymResult {
        const symbols: SymbolInfo[] = [];
        const references: ReferenceInfo[] = [];
        const lines = output.split('\n');

        for (const line of lines) {
            if (line.startsWith('SYM\t')) {
                const parts = line.split('\t');
                if (parts.length < 5) continue;
                const name = parts[1];
                const klass = parts[2];
                const lineNum = parseInt(parts[3], 10);
                const level = parseInt(parts[4], 10);
                if (!name || isNaN(lineNum) || isNaN(level)) continue;
                const typeName = parts.length >= 6 && parts[5] ? parts[5] : undefined;
                const value = parts.length >= 7 && parts[6] ? parts[6].trim() : undefined;
                const endLineStr = parts.length >= 8 && parts[7] ? parts[7].trim() : undefined;
                const endLine = endLineStr ? parseInt(endLineStr, 10) : undefined;
                symbols.push({ name, klass, line: lineNum, level, typeName, value,
                    endLine: endLine && !isNaN(endLine) ? endLine : undefined });
            } else if (line.startsWith('REF\t')) {
                const parts = line.split('\t');
                if (parts.length < 5) continue;
                const name = parts[1];
                const refLine = parseInt(parts[2], 10);
                const declLine = parseInt(parts[3], 10);
                const declLevel = parseInt(parts[4], 10);
                if (!name || isNaN(refLine) || isNaN(declLine) || isNaN(declLevel)) continue;
                references.push({ name, refLine, declLine, declLevel });
            }
        }

        return { symbols, references };
    }

    /**
     * Find the passym executable. Checks:
     * 1. Explicit path from settings
     * 2. utils/passym relative to workspace root
     * 3. undefined if not found
     */
    static findSym(settingsPath?: string, workspaceRoot?: string): string | undefined {
        if (settingsPath && fs.existsSync(settingsPath)) {
            return settingsPath;
        }
        if (workspaceRoot) {
            const rel = path.join(workspaceRoot, 'utils', 'passym');
            if (fs.existsSync(rel)) {
                return rel;
            }
        }
        return undefined;
    }
}
