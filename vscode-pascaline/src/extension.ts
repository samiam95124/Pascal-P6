import * as vscode from 'vscode';
import { PascalineDebugSession } from './pascalineDebug';

export function activate(context: vscode.ExtensionContext) {
    const factory = new InlineDebugAdapterFactory();
    context.subscriptions.push(
        vscode.debug.registerDebugAdapterDescriptorFactory('pascaline', factory)
    );
}

export function deactivate() {
    // nothing to clean up
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
