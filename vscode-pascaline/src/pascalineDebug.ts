import {
    LoggingDebugSession,
    InitializedEvent,
    StoppedEvent,
    TerminatedEvent,
    OutputEvent,
    Thread,
    StackFrame,
    Scope,
    Source,
    Variable
} from '@vscode/debugadapter';
import { DebugProtocol } from '@vscode/debugprotocol';
import { PintRunner, StoppedInfo } from './pintRunner';
import { execFile } from 'child_process';
import * as path from 'path';
import * as fs from 'fs';

const THREAD_ID = 1;

interface LaunchRequestArguments extends DebugProtocol.LaunchRequestArguments {
    program: string;
    pc?: string;
    pint?: string;
    stopOnEntry?: boolean;
}

/**
 * Pascaline debug session implementing the Debug Adapter Protocol.
 * Translates DAP requests into pint debugger commands and parses responses.
 */
export class PascalineDebugSession extends LoggingDebugSession {
    private pintRunner: PintRunner | null = null;
    private sourceFile: string = '';
    private sourceDir: string = '';
    private programName: string = '';

    /** Breakpoints keyed by source file path. */
    private breakpointsByFile: Map<string, DebugProtocol.SourceBreakpoint[]> = new Map();

    /** Next breakpoint ID for DAP. */
    private breakpointId = 1;

    /** Whether the program has been launched and initial setup is complete. */
    private configDone = false;

    /** Pending configurationDone resolve. */
    private configDoneResolve: (() => void) | null = null;

    constructor() {
        super();
        this.setDebuggerLinesStartAt1(true);
        this.setDebuggerColumnsStartAt1(true);
    }

    /**
     * DAP: Initialize - return capabilities.
     */
    protected initializeRequest(
        response: DebugProtocol.InitializeResponse,
        _args: DebugProtocol.InitializeRequestArguments
    ): void {
        response.body = response.body || {};
        response.body.supportsConfigurationDoneRequest = true;
        response.body.supportsEvaluateForHovers = true;
        response.body.supportsStepBack = false;
        response.body.supportsSetVariable = true;
        response.body.supportsFunctionBreakpoints = false;
        response.body.supportsConditionalBreakpoints = false;
        response.body.supportsHitConditionalBreakpoints = false;
        response.body.supportsLogPoints = false;
        response.body.supportsTerminateRequest = true;

        this.sendResponse(response);
        this.sendEvent(new InitializedEvent());
    }

    /**
     * DAP: ConfigurationDone - breakpoints have been set, we can proceed.
     */
    protected configurationDoneRequest(
        response: DebugProtocol.ConfigurationDoneResponse,
        _args: DebugProtocol.ConfigurationDoneArguments
    ): void {
        this.configDone = true;
        if (this.configDoneResolve) {
            this.configDoneResolve();
        }
        this.sendResponse(response);
    }

    /**
     * Wait for configurationDone if it hasn't happened yet.
     */
    private waitForConfigDone(): Promise<void> {
        if (this.configDone) return Promise.resolve();
        return new Promise(resolve => {
            this.configDoneResolve = resolve;
        });
    }

    /**
     * DAP: Launch - compile the program and start pint in debug mode.
     */
    protected async launchRequest(
        response: DebugProtocol.LaunchResponse,
        args: LaunchRequestArguments
    ): Promise<void> {
        const programPath = args.program;
        const pcPath = args.pc || 'pc';
        const pintPath = args.pint || 'pint';
        const stopOnEntry = args.stopOnEntry !== false;

        // Validate source file exists
        if (!fs.existsSync(programPath)) {
            this.sendErrorResponse(response, 1, `Source file not found: ${programPath}`);
            return;
        }

        this.sourceFile = path.resolve(programPath);
        this.sourceDir = path.dirname(this.sourceFile);
        // Strip .pas extension for the program name
        this.programName = path.basename(this.sourceFile, '.pas');

        // Step 1: Compile with pc
        this.sendEvent(new OutputEvent(`Compiling ${this.programName}...\n`, 'console'));

        try {
            await this.compile(pcPath, this.programName, this.sourceDir);
        } catch (err: any) {
            this.sendEvent(new OutputEvent(`Compilation failed: ${err.message}\n`, 'stderr'));
            this.sendErrorResponse(response, 2, `Compilation failed: ${err.message}`);
            return;
        }

        this.sendEvent(new OutputEvent(`Compilation successful.\n`, 'console'));

        // Step 2: Start pint in debug mode
        const p6File = path.join(this.sourceDir, this.programName + '.p6');
        const outFile = path.join(this.sourceDir, this.programName + '.out');

        this.pintRunner = new PintRunner();

        // Wire up events
        this.pintRunner.on('output', (text: string) => {
            this.sendEvent(new OutputEvent(text, 'stdout'));
        });

        this.pintRunner.on('stderr', (text: string) => {
            this.sendEvent(new OutputEvent(text, 'stderr'));
        });

        this.pintRunner.on('exited', (_code: number) => {
            this.sendEvent(new TerminatedEvent());
        });

        this.pintRunner.on('stopped', (info: StoppedInfo) => {
            this.sendEvent(new StoppedEvent(info.reason, THREAD_ID));
        });

        try {
            const startOutput = await this.pintRunner.start(pintPath, p6File, outFile);

            // Parse initial source context to find the entry point line
            const sourceLines = this.pintRunner.parseSourceLines(startOutput);
            const currentLine = sourceLines.find(l => l.isCurrent);

            this.sendResponse(response);

            // Wait for breakpoints to be configured
            await this.waitForConfigDone();

            // Set any pending breakpoints
            await this.syncBreakpoints();

            if (stopOnEntry && currentLine) {
                // Already stopped at entry point
                this.sendEvent(new StoppedEvent('entry', THREAD_ID));
            } else if (!stopOnEntry) {
                // Run to first breakpoint or completion
                this.continueExecution();
            } else {
                this.sendEvent(new StoppedEvent('entry', THREAD_ID));
            }
        } catch (err: any) {
            this.sendEvent(new OutputEvent(`Failed to start pint: ${err.message}\n`, 'stderr'));
            this.sendErrorResponse(response, 3, `Failed to start pint: ${err.message}`);
        }
    }

    /**
     * Compile the Pascal program using pc.
     */
    private compile(pcPath: string, programName: string, cwd: string): Promise<void> {
        return new Promise((resolve, reject) => {
            execFile(pcPath, [programName, '--debugsrc', '-r'], { cwd }, (error, stdout, stderr) => {
                if (stdout) {
                    this.sendEvent(new OutputEvent(stdout, 'console'));
                }
                if (stderr) {
                    this.sendEvent(new OutputEvent(stderr, 'stderr'));
                }
                if (error) {
                    reject(error);
                } else {
                    resolve();
                }
            });
        });
    }

    /**
     * DAP: SetBreakpoints - set breakpoints for a source file.
     */
    protected async setBreakpointsRequest(
        response: DebugProtocol.SetBreakpointsResponse,
        args: DebugProtocol.SetBreakpointsArguments
    ): Promise<void> {
        const sourcePath = args.source.path || '';
        const requestedBps = args.breakpoints || [];

        // Store breakpoints for this file
        this.breakpointsByFile.set(sourcePath, requestedBps);

        // If pint is running, sync breakpoints now
        if (this.pintRunner && this.configDone) {
            await this.syncBreakpoints();
        }

        // Return all breakpoints as verified (pint will validate on set)
        const breakpoints: DebugProtocol.Breakpoint[] = requestedBps.map(bp => {
            return {
                id: this.breakpointId++,
                verified: true,
                line: bp.line
            };
        });

        response.body = { breakpoints };
        this.sendResponse(response);
    }

    /**
     * Sync all breakpoints to pint: clear all, then set each one.
     */
    private async syncBreakpoints(): Promise<void> {
        if (!this.pintRunner) return;

        // Clear all breakpoints in pint
        await this.pintRunner.sendCommand('c');

        // Set breakpoints for each file
        for (const [filePath, bps] of this.breakpointsByFile) {
            // Determine module name from file path
            const moduleName = path.basename(filePath, '.pas');

            for (const bp of bps) {
                await this.pintRunner.sendCommand(`b ${moduleName} ${bp.line}`);
            }
        }
    }

    /**
     * DAP: Threads - pint is single-threaded, always return one thread.
     */
    protected threadsRequest(response: DebugProtocol.ThreadsResponse): void {
        response.body = {
            threads: [new Thread(THREAD_ID, 'main')]
        };
        this.sendResponse(response);
    }

    /**
     * DAP: StackTrace - get the call stack using pint's "df" command.
     */
    protected async stackTraceRequest(
        response: DebugProtocol.StackTraceResponse,
        _args: DebugProtocol.StackTraceArguments
    ): Promise<void> {
        if (!this.pintRunner) {
            response.body = { stackFrames: [], totalFrames: 0 };
            this.sendResponse(response);
            return;
        }

        const output = await this.pintRunner.sendCommand('df');
        const frames = this.pintRunner.parseFrameDump(output);

        const stackFrames: StackFrame[] = frames.map((frame, index) => {
            const sf = new StackFrame(
                index,
                frame.name + (frame.typeSuffix ? `@${frame.typeSuffix}` : ''),
                new Source(
                    path.basename(this.sourceFile),
                    this.sourceFile
                ),
                frame.currentLine
            );
            return sf;
        });

        response.body = {
            stackFrames,
            totalFrames: stackFrames.length
        };
        this.sendResponse(response);
    }

    /**
     * DAP: Scopes - return locals, parameters, and globals scopes.
     * We encode the scope type into the variablesReference:
     *   frameId * 3 + 1 = locals
     *   frameId * 3 + 2 = parameters
     *   frameId * 3 + 3 = globals
     */
    protected scopesRequest(
        response: DebugProtocol.ScopesResponse,
        args: DebugProtocol.ScopesArguments
    ): void {
        const frameId = args.frameId;
        const scopes: Scope[] = [
            new Scope('Locals', frameId * 3 + 1, false),
            new Scope('Parameters', frameId * 3 + 2, false),
            new Scope('Globals', frameId * 3 + 3, false)
        ];

        response.body = { scopes };
        this.sendResponse(response);
    }

    /**
     * DAP: Variables - fetch variables for a scope using pl/pp/pg.
     */
    protected async variablesRequest(
        response: DebugProtocol.VariablesResponse,
        args: DebugProtocol.VariablesArguments
    ): Promise<void> {
        if (!this.pintRunner) {
            response.body = { variables: [] };
            this.sendResponse(response);
            return;
        }

        const ref = args.variablesReference;
        const scopeType = ((ref - 1) % 3) + 1; // 1=locals, 2=params, 3=globals
        const frameId = Math.floor((ref - 1) / 3);

        let cmd: string;
        if (scopeType === 1) {
            // Locals: use "pl" with nesting level
            cmd = frameId === 0 ? 'pl' : `pl ${frameId + 1}`;
        } else if (scopeType === 2) {
            // Parameters: use "pp" with nesting level
            cmd = frameId === 0 ? 'pp' : `pp ${frameId + 1}`;
        } else {
            // Globals: use "pg"
            cmd = 'pg';
        }

        const output = await this.pintRunner.sendCommand(cmd);
        const vars = this.pintRunner.parseVariables(output);

        const variables: Variable[] = vars.map(v => new Variable(v.name, v.value));

        response.body = { variables };
        this.sendResponse(response);
    }

    /**
     * DAP: Evaluate - evaluate an expression using pint's "p" command.
     */
    protected async evaluateRequest(
        response: DebugProtocol.EvaluateResponse,
        args: DebugProtocol.EvaluateArguments
    ): Promise<void> {
        if (!this.pintRunner) {
            this.sendErrorResponse(response, 4, 'Not connected to pint');
            return;
        }

        const expr = args.expression;
        const output = await this.pintRunner.sendCommand(`p ${expr}`);
        const result = this.pintRunner.parseExpression(output);

        response.body = {
            result: result || '(no value)',
            variablesReference: 0
        };
        this.sendResponse(response);
    }

    /**
     * DAP: SetVariable - set a variable using pint's "st" command.
     */
    protected async setVariableRequest(
        response: DebugProtocol.SetVariableResponse,
        args: DebugProtocol.SetVariableArguments
    ): Promise<void> {
        if (!this.pintRunner) {
            this.sendErrorResponse(response, 5, 'Not connected to pint');
            return;
        }

        await this.pintRunner.sendCommand(`st ${args.name} ${args.value}`);

        // Read back the new value
        const output = await this.pintRunner.sendCommand(`p ${args.name}`);
        const result = this.pintRunner.parseExpression(output);

        response.body = {
            value: result
        };
        this.sendResponse(response);
    }

    /**
     * DAP: Continue - resume execution with pint's "r" command.
     */
    protected async continueRequest(
        response: DebugProtocol.ContinueResponse,
        _args: DebugProtocol.ContinueArguments
    ): Promise<void> {
        response.body = { allThreadsContinued: true };
        this.sendResponse(response);
        this.continueExecution();
    }

    /**
     * Run the program and handle the stopped/exited result.
     */
    private async continueExecution(): Promise<void> {
        if (!this.pintRunner) return;

        const output = await this.pintRunner.sendRun();

        if (output.includes('program complete')) {
            this.sendEvent(new TerminatedEvent());
        } else {
            // Parse the break context
            const stopped = this.parseStoppedFromOutput(output);
            if (stopped) {
                this.sendEvent(new StoppedEvent(stopped.reason, THREAD_ID));
            }
        }
    }

    /**
     * DAP: Next (step over) - pint's "so" command.
     */
    protected async nextRequest(
        response: DebugProtocol.NextResponse,
        _args: DebugProtocol.NextArguments
    ): Promise<void> {
        this.sendResponse(response);
        await this.stepExecution('so');
    }

    /**
     * DAP: StepIn - pint's "s" command.
     */
    protected async stepInRequest(
        response: DebugProtocol.StepInResponse,
        _args: DebugProtocol.StepInArguments
    ): Promise<void> {
        this.sendResponse(response);
        await this.stepExecution('s');
    }

    /**
     * DAP: StepOut - pint's "ret" command.
     */
    protected async stepOutRequest(
        response: DebugProtocol.StepOutResponse,
        _args: DebugProtocol.StepOutArguments
    ): Promise<void> {
        this.sendResponse(response);
        await this.stepExecution('ret');
    }

    /**
     * Execute a step command and emit the appropriate stopped event.
     */
    private async stepExecution(cmd: string): Promise<void> {
        if (!this.pintRunner) return;

        const output = await this.pintRunner.sendStep(cmd);

        if (output.includes('program complete')) {
            this.sendEvent(new TerminatedEvent());
        } else {
            const stopped = this.parseStoppedFromOutput(output);
            if (stopped) {
                this.sendEvent(new StoppedEvent(stopped.reason, THREAD_ID));
            } else {
                // Even if we couldn't parse it, we stopped somewhere
                this.sendEvent(new StoppedEvent('step', THREAD_ID));
            }
        }
    }

    /**
     * DAP: Pause - send SIGINT to pint.
     */
    protected pauseRequest(
        response: DebugProtocol.PauseResponse,
        _args: DebugProtocol.PauseArguments
    ): void {
        if (this.pintRunner) {
            this.pintRunner.pause();
        }
        this.sendResponse(response);
    }

    /**
     * DAP: Terminate - quit pint.
     */
    protected terminateRequest(
        response: DebugProtocol.TerminateResponse,
        _args: DebugProtocol.TerminateArguments
    ): void {
        if (this.pintRunner) {
            this.pintRunner.dispose();
            this.pintRunner = null;
        }
        this.sendResponse(response);
    }

    /**
     * DAP: Disconnect - clean up and exit.
     */
    protected disconnectRequest(
        response: DebugProtocol.DisconnectResponse,
        _args: DebugProtocol.DisconnectArguments
    ): void {
        if (this.pintRunner) {
            this.pintRunner.dispose();
            this.pintRunner = null;
        }
        this.sendResponse(response);
    }

    /**
     * Parse a stopped context from raw pint output.
     */
    private parseStoppedFromOutput(output: string): StoppedInfo | null {
        if (!this.pintRunner) return null;

        const sourceLines = this.pintRunner.parseSourceLines(output);
        if (sourceLines.length === 0) return null;

        const currentLine = sourceLines.find(l => l.isCurrent);
        if (!currentLine) return null;

        let reason = 'step';
        if (output.includes('=== break ===')) {
            reason = 'breakpoint';
        } else if (output.includes('*** Program stopped by user break')) {
            reason = 'pause';
        }

        return {
            line: currentLine.lineNumber,
            reason,
            sourceContext: sourceLines
        };
    }
}
