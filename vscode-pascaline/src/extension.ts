import * as vscode from 'vscode';
import * as path from 'path';
import * as fs from 'fs';
import { PascalineDebugSession } from './pascalineDebug';
import {
    LanguageClient,
    LanguageClientOptions,
    ServerOptions,
    TransportKind
} from 'vscode-languageclient/node';

let client: LanguageClient | undefined;

/* Locate a Pascal-P6 tool. A configured absolute path wins, then the
   workspace bin directory (the normal layout when the workspace is the
   Pascal-P6 tree or a project beside it), then the bare name on PATH. */
function findTool(name: string): string {
    const cfg = vscode.workspace.getConfiguration('pascaline');
    const configured = cfg.get<string>(name);
    if (configured && configured !== name) { return configured; }
    const exe = process.platform === 'win32' ? name + '.exe' : name;
    for (const f of vscode.workspace.workspaceFolders ?? []) {
        let dir = f.uri.fsPath;
        for (;;) {
            const p = path.join(dir, 'bin', exe);
            if (fs.existsSync(p)) { return p; }
            const parent = path.dirname(dir);
            if (parent === dir) { break; }
            dir = parent;
        }
    }
    return name;
}

/* Build tasks for .pas files: compile with pc in the file's directory.
   ProcessExecution runs the compiler directly, with no shell in between,
   so the task works identically on every platform. */
class PascalineTaskProvider implements vscode.TaskProvider {
    provideTasks(): vscode.Task[] {
        const editor = vscode.window.activeTextEditor;
        if (editor && editor.document.languageId === 'pascaline') {
            return [this.buildTask(editor.document.uri.fsPath)];
        }
        return [];
    }

    resolveTask(task: vscode.Task): vscode.Task | undefined {
        const file = (task.definition as any).file as string | undefined;
        if (file) { return this.buildTask(file, task.definition); }
        const editor = vscode.window.activeTextEditor;
        if (editor && editor.document.languageId === 'pascaline') {
            return this.buildTask(editor.document.uri.fsPath, task.definition);
        }
        return undefined;
    }

    private buildTask(file: string,
                      definition?: vscode.TaskDefinition): vscode.Task {
        const dir = path.dirname(file);
        const base = path.basename(file, path.extname(file));
        const def = definition ??
            { type: 'pascaline', task: 'build', file: file };
        const pc = findTool('pc');
        // pc finds its subtools (pcom, pgen) on PATH, so put its own
        // directory there -- otherwise a workspace whose bin is not on the
        // editor's PATH fails the compile with no message.
        const env: { [key: string]: string } = {};
        if (path.isAbsolute(pc)) {
            env['PATH'] = path.dirname(pc) + path.delimiter +
                (process.env.PATH ?? '');
        }
        const exec = new vscode.ProcessExecution(pc, [base],
                                                 { cwd: dir, env: env });
        const task = new vscode.Task(def, vscode.TaskScope.Workspace,
                                     'build ' + base, 'pascaline', exec, []);
        task.group = vscode.TaskGroup.Build;
        return task;
    }
}

/* Debug configurations: let F5 work on a .pas file with no launch.json.
   An empty configuration is filled in as a pint debug launch, and the pc
   and pint tool locations are resolved to real paths so the debugger does
   not depend on the PATH the editor happened to inherit. */
class PascalineConfigProvider implements vscode.DebugConfigurationProvider {
    resolveDebugConfiguration(
        _folder: vscode.WorkspaceFolder | undefined,
        config: vscode.DebugConfiguration
    ): vscode.ProviderResult<vscode.DebugConfiguration> {
        if (!config.type && !config.request && !config.name) {
            const editor = vscode.window.activeTextEditor;
            if (editor && editor.document.languageId === 'pascaline') {
                config.type = 'pascaline';
                config.request = 'launch';
                config.name = 'Debug with pint';
                config.program = editor.document.uri.fsPath;
                config.stopOnEntry = true;
            }
        }
        if (config.type === 'pascaline') {
            if (!config.pc || config.pc === 'pc') {
                config.pc = findTool('pc');
            }
            if (!config.pint || config.pint === 'pint') {
                config.pint = findTool('pint');
            }
        }
        return config;
    }

    provideDebugConfigurations(): vscode.DebugConfiguration[] {
        return [{
            type: 'pascaline',
            request: 'launch',
            name: 'Debug with pint',
            program: '${file}',
            stopOnEntry: true
        }];
    }
}

function openPasdocWebview(htmlPath: string, symbolName: string) {
    const content = fs.readFileSync(htmlPath, 'utf-8');
    const title = path.basename(htmlPath, '.html') + ' - Documentation';

    const panel = vscode.window.createWebviewPanel(
        'pasdoc',
        title,
        vscode.ViewColumn.Beside,
        { enableScripts: true }
    );

    const scrollScript = `
<script>
(function() {
    var symbol = ${JSON.stringify(symbolName)}.toLowerCase();
    if (!symbol) return;
    // Search detail-item h4 elements first (most specific)
    var details = document.querySelectorAll('.detail-item h4');
    for (var i = 0; i < details.length; i++) {
        var text = details[i].textContent.trim().toLowerCase();
        // h4 text is like "symbolname (kind)" — match the leading word
        if (text === symbol || text.startsWith(symbol + ' ')) {
            details[i].parentElement.scrollIntoView({ behavior: 'smooth' });
            details[i].parentElement.style.outline = '3px solid #3498db';
            return;
        }
    }
    // Fallback: search summary .item elements
    var items = document.querySelectorAll('.item');
    for (var i = 0; i < items.length; i++) {
        var text = items[i].textContent.trim().toLowerCase();
        if (text === symbol || text.startsWith(symbol + ' ') ||
            text.startsWith(symbol + ':')) {
            items[i].scrollIntoView({ behavior: 'smooth' });
            items[i].style.outline = '3px solid #3498db';
            return;
        }
    }
})();
</script>`;

    // Inject scroll script before closing </body>
    const html = content.replace('</body>', scrollScript + '</body>');
    panel.webview.html = html;
}

export function activate(context: vscode.ExtensionContext) {
    // Debug adapter
    const factory = new InlineDebugAdapterFactory();
    context.subscriptions.push(
        vscode.debug.registerDebugAdapterDescriptorFactory('pascaline', factory)
    );

    // Build tasks and default debug configurations
    context.subscriptions.push(
        vscode.tasks.registerTaskProvider('pascaline',
                                          new PascalineTaskProvider())
    );
    const configProvider = new PascalineConfigProvider();
    context.subscriptions.push(
        vscode.debug.registerDebugConfigurationProvider('pascaline',
                                                        configProvider)
    );
    context.subscriptions.push(
        vscode.debug.registerDebugConfigurationProvider(
            'pascaline', configProvider,
            vscode.DebugConfigurationProviderTriggerKind.Dynamic)
    );

    // Pasdoc documentation command
    context.subscriptions.push(
        vscode.commands.registerCommand('pascaline.openDocumentation', async () => {
            const editor = vscode.window.activeTextEditor;
            if (!editor) { return; }

            const document = editor.document;
            const wordRange = document.getWordRangeAtPosition(
                editor.selection.active);
            const symbolName = wordRange ? document.getText(wordRange) : '';

            const filePath = document.uri.fsPath;
            if (!filePath.endsWith('.pas')) {
                vscode.window.showErrorMessage(
                    'Open Pasdoc Documentation: not a .pas file.');
                return;
            }

            const dir = path.dirname(filePath);
            const base = path.basename(filePath, '.pas');
            const htmlPath = path.join(dir, base + '.html');

            if (!fs.existsSync(htmlPath)) {
                vscode.window.showErrorMessage(
                    'No documentation found. Run pc with pasdoc to generate '
                    + base + '.html');
                return;
            }

            openPasdocWebview(htmlPath, symbolName);
        })
    );

    // LSP client
    const serverModule = context.asAbsolutePath(path.join('out', 'server.js'));
    const serverOptions: ServerOptions = {
        run: { module: serverModule, transport: TransportKind.ipc },
        debug: { module: serverModule, transport: TransportKind.ipc }
    };
    const clientOptions: LanguageClientOptions = {
        documentSelector: [{ scheme: 'file', language: 'pascaline' }]
    };
    client = new LanguageClient(
        'pascaline',
        'Pascaline Language Server',
        serverOptions,
        clientOptions
    );
    client.start();
}

export function deactivate(): Thenable<void> | undefined {
    if (client) {
        return client.stop();
    }
    return undefined;
}

class InlineDebugAdapterFactory implements vscode.DebugAdapterDescriptorFactory {
    createDebugAdapterDescriptor(
        _session: vscode.DebugSession
    ): vscode.ProviderResult<vscode.DebugAdapterDescriptor> {
        return new vscode.DebugAdapterInlineImplementation(
            new PascalineDebugSession() as any
        );
    }
}
