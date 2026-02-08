import { execFile } from 'child_process';
import * as path from 'path';
import * as fs from 'fs';

export interface ParserError {
    line: number;      // 1-indexed
    column: number;    // 0-indexed
    code: number;      // error number
    message: string;
}

/**
 * Spawns the Pascal parser executable and parses its error output.
 *
 * Parser output format (with expanded errors enabled by default):
 *
 *   Source line:   <linecount:6><'  ':2><ic:7><' '><source text>
 *   Pointer line:  <linecount:6><' ****  ':9><spaces><^><errnums>
 *   Expanded line: <linecount:6><' ****  ':9><errnum:3><' '><message>
 *
 * The pointer prefix is 15 chars; the source prefix is 16 chars.
 * The caret position (0-indexed) minus 16 gives the 0-indexed column
 * in the original source file.
 */
export class ParserRunner {
    private parserPath: string;

    constructor(parserPath: string) {
        this.parserPath = parserPath;
    }

    /**
     * Run the parser on a source file and return any errors found.
     */
    run(filePath: string): Promise<ParserError[]> {
        return new Promise((resolve) => {
            const dir = path.dirname(filePath);
            const base = path.basename(filePath, '.pas');

            execFile(this.parserPath, [base], {
                cwd: dir,
                timeout: 30000
            }, (error, stdout, _stderr) => {
                if (!stdout) {
                    resolve([]);
                    return;
                }
                resolve(this.parseOutput(stdout));
            });
        });
    }

    /**
     * Parse the parser's stdout to extract errors.
     */
    private parseOutput(output: string): ParserError[] {
        const lines = output.split('\n');
        const errors: ParserError[] = [];

        // Map from error number to message, populated from expanded lines
        const messageMap = new Map<number, string>();

        // First pass: collect error messages from expanded error lines
        // Format: <linecount:6><' ****  ':9><errnum:3><' '><message>
        const expandedRe = /^\s*\d+\s+\*{4}\s+(\d+)\s+(.+)$/;

        for (const line of lines) {
            const em = line.match(expandedRe);
            if (em) {
                const errNum = parseInt(em[1], 10);
                const msg = em[2].trim();
                if (!messageMap.has(errNum)) {
                    messageMap.set(errNum, msg);
                }
            }
        }

        // Also collect from the summary section at the bottom
        // Format: <errnum>  <count> <line_numbers> <message>
        const summaryRe = /^\s*(\d+)\s+\d+\s+[\d,\s]+\s+(.+)$/;
        let inSummary = false;

        for (const line of lines) {
            if (line.includes('Error numbers in listing:')) {
                inSummary = true;
                continue;
            }
            if (inSummary && line.startsWith('---')) {
                continue;
            }
            if (inSummary) {
                const sm = line.match(summaryRe);
                if (sm) {
                    const errNum = parseInt(sm[1], 10);
                    const msg = sm[2].trim();
                    if (!messageMap.has(errNum)) {
                        messageMap.set(errNum, msg);
                    }
                }
            }
        }

        // Second pass: extract error positions from pointer lines
        // Format: <linecount:6><' ****  ':9><spaces><^><errnums>
        // The ^ may appear with comma-separated error numbers: ^104,129
        const pointerRe = /^\s*(\d+)\s+\*{4}\s+/;

        for (const line of lines) {
            const pm = line.match(pointerRe);
            if (!pm) continue;

            // Check this is a pointer line (has ^) not an expanded line
            const caretIdx = line.indexOf('^');
            if (caretIdx < 0) continue;

            const lineNum = parseInt(pm[1], 10);
            const column = Math.max(0, caretIdx - 16);

            // Extract error numbers after the ^
            const afterCaret = line.substring(caretIdx + 1).trim();
            const errNums = afterCaret.split(',').map(s => parseInt(s.trim(), 10));

            for (const errNum of errNums) {
                if (isNaN(errNum)) continue;
                errors.push({
                    line: lineNum,
                    column,
                    code: errNum,
                    message: messageMap.get(errNum) || `Error ${errNum}`
                });
            }
        }

        return errors;
    }

    /**
     * Find the parser executable. Checks:
     * 1. Explicit path from settings
     * 2. source/parser relative to workspace root
     * 3. 'parser' on PATH
     */
    static findParser(settingsPath?: string, workspaceRoot?: string): string | undefined {
        if (settingsPath && fs.existsSync(settingsPath)) {
            return settingsPath;
        }
        if (workspaceRoot) {
            const rel = path.join(workspaceRoot, 'source', 'parser');
            if (fs.existsSync(rel)) {
                return rel;
            }
        }
        return undefined;
    }
}
