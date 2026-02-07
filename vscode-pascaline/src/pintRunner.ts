import { ChildProcess, spawn } from 'child_process';
import { EventEmitter } from 'events';

/**
 * Information about a stopped event from pint.
 */
export interface StoppedInfo {
    /** The source line where execution stopped. */
    line: number;
    /** The reason for stopping: breakpoint, step, entry, pause, or watch. */
    reason: string;
    /** The 3-line source context around the stopped line. */
    sourceContext: SourceLine[];
}

/**
 * A parsed source line from pint debug output.
 * Format: "  NN:  count: [b][t][*] source text"
 */
export interface SourceLine {
    lineNumber: number;
    hitCount: number;
    hasBreakpoint: boolean;
    hasTracepoint: boolean;
    isCurrent: boolean;
    text: string;
}

/**
 * A parsed stack frame from pint's "df" command.
 */
export interface FrameInfo {
    /** Block name (e.g., "q", "y", "test"). */
    name: string;
    /** Type suffix after @ (e.g., "f_i" for function returning integer). */
    typeSuffix: string;
    /** Address in hex. */
    address: string;
    /** Source context lines for this frame. */
    sourceContext: SourceLine[];
    /** Current source line number for this frame. */
    currentLine: number;
}

/**
 * A parsed variable from pint's pl/pp/pg output.
 */
export interface VariableInfo {
    name: string;
    value: string;
}

/**
 * Manages the pint interpreter subprocess and translates between
 * text-based pint debug commands and structured TypeScript objects.
 */
export class PintRunner extends EventEmitter {
    private process: ChildProcess | null = null;
    private outputBuffer: string = '';
    private commandResolve: ((output: string) => void) | null = null;
    private running: boolean = false;
    private disposed: boolean = false;

    /**
     * Spawn pint with debug mode enabled.
     * @param pintPath Path to the pint executable.
     * @param p6File Path to the compiled .p6 intermediate file.
     * @param outFile Path for program output file.
     */
    async start(pintPath: string, p6File: string, outFile: string): Promise<string> {
        return new Promise((resolve, reject) => {
            this.process = spawn(pintPath, [
                p6File, outFile, '--debug', '--debugsrc'
            ]);

            if (!this.process.stdout || !this.process.stderr || !this.process.stdin) {
                reject(new Error('Failed to create pint process streams'));
                return;
            }

            this.process.stdout.setEncoding('utf8');
            this.process.stderr.setEncoding('utf8');

            this.process.stdout.on('data', (data: string) => {
                this.handleOutput(data);
            });

            this.process.stderr.on('data', (data: string) => {
                this.emit('stderr', data);
            });

            this.process.on('error', (err) => {
                reject(err);
            });

            this.process.on('exit', (code, signal) => {
                this.emit('exited', code ?? -1, signal);
                this.process = null;
            });

            // Wait for the initial "debug> " prompt
            this.commandResolve = (output: string) => {
                resolve(output);
            };
        });
    }

    /**
     * Send a command to pint and wait for the response (until next "debug> " prompt).
     */
    async sendCommand(cmd: string): Promise<string> {
        if (!this.process || !this.process.stdin) {
            throw new Error('Pint process not running');
        }

        return new Promise((resolve) => {
            this.commandResolve = resolve;
            this.process!.stdin!.write(cmd + '\n');
        });
    }

    /**
     * Send a run command. This is special because during execution, pint
     * interleaves program output with debug messages. We need to detect
     * either a break, program completion, or user-interrupted stop.
     */
    async sendRun(): Promise<string> {
        this.running = true;
        const result = await this.sendCommand('r');
        this.running = false;
        return result;
    }

    /**
     * Send a step command (s, so, ret) and wait for the response.
     */
    async sendStep(cmd: string): Promise<string> {
        this.running = true;
        const result = await this.sendCommand(cmd);
        this.running = false;
        return result;
    }

    /**
     * Interrupt the running program with SIGINT.
     */
    pause(): void {
        if (this.process) {
            this.process.kill('SIGINT');
        }
    }

    /**
     * Quit pint and clean up.
     */
    dispose(): void {
        if (this.disposed) return;
        this.disposed = true;

        if (this.process) {
            try {
                this.process.stdin?.write('q\n');
            } catch {
                // ignore write errors during shutdown
            }
            setTimeout(() => {
                if (this.process) {
                    this.process.kill('SIGKILL');
                    this.process = null;
                }
            }, 1000);
        }
    }

    /**
     * Handle raw output from pint's stdout.
     * Accumulates text until a "debug> " prompt is found, then resolves
     * the pending command. During run mode, also detects program output,
     * breaks, and completion.
     */
    private handleOutput(data: string): void {
        this.outputBuffer += data;

        // Check for the debug prompt which signals end of a command response
        const promptIndex = this.outputBuffer.indexOf('debug> ');
        if (promptIndex !== -1) {
            const response = this.outputBuffer.substring(0, promptIndex);
            this.outputBuffer = this.outputBuffer.substring(promptIndex + 'debug> '.length);

            // During run, extract program output vs debug output
            if (this.running) {
                this.extractProgramOutput(response);
            }

            // Check for program completion
            if (response.includes('program complete')) {
                this.emit('exited', 0, null);
            }

            // Check for break
            if (response.includes('=== break ===') ||
                response.includes('*** Program stopped by user break')) {
                const stopped = this.parseStoppedContext(response);
                if (stopped) {
                    this.emit('stopped', stopped);
                }
            }

            // Resolve the pending command
            if (this.commandResolve) {
                const resolve = this.commandResolve;
                this.commandResolve = null;
                resolve(response);
            }
        }
    }

    /**
     * Extract and emit program output (non-debug text) from a run response.
     */
    private extractProgramOutput(response: string): void {
        const lines = response.split('\n');
        const programLines: string[] = [];

        for (const line of lines) {
            // Skip debug-specific output patterns
            if (line.match(/^\s*\d+:\s+\d+:/) ||     // source line format
                line.includes('=== break ===') ||
                line.includes('program complete') ||
                line.includes('P6 debug mode') ||
                line.includes('*** Program stopped') ||
                line.includes('Watch variable:') ||
                line.trim() === '') {
                continue;
            }
            programLines.push(line);
        }

        if (programLines.length > 0) {
            this.emit('output', programLines.join('\n') + '\n');
        }
    }

    /**
     * Parse a stopped context from pint output to extract the current line.
     */
    private parseStoppedContext(response: string): StoppedInfo | null {
        const sourceLines = this.parseSourceLines(response);
        if (sourceLines.length === 0) return null;

        const currentLine = sourceLines.find(l => l.isCurrent);
        if (!currentLine) return null;

        let reason = 'step';
        if (response.includes('=== break ===')) {
            reason = currentLine.hasBreakpoint ? 'breakpoint' : 'breakpoint';
        } else if (response.includes('*** Program stopped by user break')) {
            reason = 'pause';
        }

        return {
            line: currentLine.lineNumber,
            reason,
            sourceContext: sourceLines
        };
    }

    // ========================================================================
    // Output parsing helpers
    // ========================================================================

    /**
     * Parse source lines from pint output.
     * Format: "  NN:  count: [b][t][*] source text"
     */
    parseSourceLines(output: string): SourceLine[] {
        const lines = output.split('\n');
        const result: SourceLine[] = [];
        // Match: optional spaces, line number, colon, spaces, hit count, colon,
        // optional flags (b, t, *), then source text
        const pattern = /^\s*(\d+):\s+(\d+):\s*(b?)(t?)(\*?)\s*(.*)/;

        for (const line of lines) {
            const m = line.match(pattern);
            if (m) {
                result.push({
                    lineNumber: parseInt(m[1], 10),
                    hitCount: parseInt(m[2], 10),
                    hasBreakpoint: m[3] === 'b',
                    hasTracepoint: m[4] === 't',
                    isCurrent: m[5] === '*',
                    text: m[6]
                });
            }
        }
        return result;
    }

    /**
     * Parse stack frames from pint's "df" command output.
     * Format:
     *   name@type: addr: XXXXXXXX locals/stack: RANGE (size)
     *     NN: count: [b][t][*] source text
     *     ...
     */
    parseFrameDump(output: string): FrameInfo[] {
        const frames: FrameInfo[] = [];
        const lines = output.split('\n');
        let currentFrame: FrameInfo | null = null;

        // Frame header: "name@type: addr: XXXXXXXX locals/stack: ..."
        // or just "name: addr: XXXXXXXX locals/stack: ..." for program-level
        const framePattern = /^(\w+)(?:@(\w+))?:\s*addr:\s*([0-9A-Fa-f]+)\s+locals\/stack:/;
        const sourcePattern = /^\s*(\d+):\s+(\d+):\s*(b?)(t?)(\*?)\s*(.*)/;

        for (const line of lines) {
            const fm = line.match(framePattern);
            if (fm) {
                currentFrame = {
                    name: fm[1],
                    typeSuffix: fm[2] || '',
                    address: fm[3],
                    sourceContext: [],
                    currentLine: 0
                };
                frames.push(currentFrame);
                continue;
            }

            if (currentFrame) {
                const sm = line.match(sourcePattern);
                if (sm) {
                    const sl: SourceLine = {
                        lineNumber: parseInt(sm[1], 10),
                        hitCount: parseInt(sm[2], 10),
                        hasBreakpoint: sm[3] === 'b',
                        hasTracepoint: sm[4] === 't',
                        isCurrent: sm[5] === '*',
                        text: sm[6]
                    };
                    currentFrame.sourceContext.push(sl);
                    if (sl.isCurrent) {
                        currentFrame.currentLine = sl.lineNumber;
                    }
                }
            }
        }

        return frames;
    }

    /**
     * Parse variable listings from pint's pl/pp/pg output.
     * Format:
     *   Locals for block: name
     *
     *   varname                    value
     *   ...
     *
     * Or for globals:
     *   Globals:
     *   varname                    value
     */
    parseVariables(output: string): VariableInfo[] {
        const vars: VariableInfo[] = [];
        const lines = output.split('\n');

        for (const line of lines) {
            // Skip headers and blank lines
            if (line.trim() === '' ||
                line.startsWith('Locals for block:') ||
                line.startsWith('Parameters for block:') ||
                line.startsWith('Globals:')) {
                continue;
            }

            // Variable line: name followed by spaces then value
            // The name is left-aligned, value is after whitespace
            const m = line.match(/^(\S+)\s{2,}(.+)/);
            if (m) {
                vars.push({
                    name: m[1],
                    value: m[2].trim()
                });
            }
        }

        return vars;
    }

    /**
     * Parse expression result from pint's "p" command output.
     * The value appears on the line(s) after the command.
     */
    parseExpression(output: string): string {
        const lines = output.split('\n').filter(l => l.trim() !== '');
        return lines.join('\n').trim();
    }
}
