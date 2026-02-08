import * as vscode from 'vscode';
import * as path from 'path';
import * as fs from 'fs';
import { execFile } from 'child_process';
import { PascalineDebugSession } from './pascalineDebug';
import {
    LanguageClient,
    LanguageClientOptions,
    ServerOptions,
    TransportKind
} from 'vscode-languageclient/node';

let client: LanguageClient | undefined;

function findPasdoc(workspaceRoot?: string): string | undefined {
    const config = vscode.workspace.getConfiguration('pascaline');
    const settingsPath = config.get<string>('pasdocPath');
    if (settingsPath && fs.existsSync(settingsPath)) {
        return settingsPath;
    }
    if (workspaceRoot) {
        const rel = path.join(workspaceRoot, 'bin', 'pasdoc');
        if (fs.existsSync(rel)) {
            return rel;
        }
    }
    return undefined;
}

function runPasdoc(pasdocPath: string, filePath: string,
    wsRoot: string): Promise<void> {
    return new Promise((resolve, reject) => {
        // Run from workspace root with relative path (no .pas extension)
        const relPath = path.relative(wsRoot, filePath)
            .replace(/\.pas$/, '');
        execFile(pasdocPath, [relPath], { cwd: wsRoot, timeout: 30000 },
            (error, stdout, stderr) => {
                if (error) {
                    reject(new Error(
                        stderr || stdout || error.message));
                } else {
                    resolve();
                }
            });
    });
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

            // Generate if missing
            if (!fs.existsSync(htmlPath)) {
                const wsRoot = vscode.workspace.workspaceFolders?.[0]?.uri.fsPath;
                const pasdocPath = findPasdoc(wsRoot);
                if (!pasdocPath) {
                    vscode.window.showErrorMessage(
                        'Cannot find pasdoc executable. Set pascaline.pasdocPath or ensure bin/pasdoc exists in workspace.');
                    return;
                }
                try {
                    await vscode.window.withProgress({
                        location: vscode.ProgressLocation.Notification,
                        title: 'Generating documentation...',
                        cancellable: false
                    }, () => runPasdoc(pasdocPath, filePath, wsRoot!));
                } catch (e: any) {
                    vscode.window.showErrorMessage(
                        'pasdoc failed: ' + e.message);
                    return;
                }
                if (!fs.existsSync(htmlPath)) {
                    vscode.window.showErrorMessage(
                        'pasdoc did not generate ' + base + '.html');
                    return;
                }
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
