// ─────────────────────────────────────────────────────────────────────────────
//  grey_policy.ts  –  Stage 13 · Policy Layer
// ─────────────────────────────────────────────────────────────────────────────
//  Maintains allow/deny lists, applies stub policies, and outputs consolidated
//  trust decisions.  No external dependencies — pure in-memory stub.
//
//  Self-test:  npx tsx src/ast/grey_policy.ts --test
// ─────────────────────────────────────────────────────────────────────────────

// ── Types ───────────────────────────────────────────────────────────────────

export type PolicyAction = 'allow' | 'deny' | 'sandbox' | 'prompt';

export interface PolicyRule {
    pattern: RegExp;
    action: PolicyAction;
    reason: string;
}

export interface PolicyEvaluation {
    command: string;
    action: PolicyAction;
    matchedRule?: string;
    reason: string;
    timestamp: string;
}

export interface ConsolidatedDecision {
    command: string;
    finalAction: PolicyAction;
    evaluations: PolicyEvaluation[];
    timestamp: string;
}

export interface PolicyLayerConfig {
    rules?: PolicyRule[];
    defaultAction?: PolicyAction;
}

// ── Default rules ───────────────────────────────────────────────────────────

const DEFAULT_RULES: PolicyRule[] = [
    // Allow — harmless inspection commands
    { pattern: /^ls\b/, action: 'allow', reason: 'Directory listing is harmless' },
    { pattern: /^pwd$/, action: 'allow', reason: 'Print working directory is harmless' },
    { pattern: /^echo\b/, action: 'allow', reason: 'Echo is harmless' },
    { pattern: /^cat\b/, action: 'allow', reason: 'File reading is harmless' },
    { pattern: /^whoami$/, action: 'allow', reason: 'User identity query is harmless' },
    { pattern: /^date$/, action: 'allow', reason: 'Date query is harmless' },
    { pattern: /^head\b/, action: 'allow', reason: 'Head is harmless' },
    { pattern: /^tail\b/, action: 'allow', reason: 'Tail is harmless' },

    // Sandbox — potentially risky but recoverable
    { pattern: /\brm\b/, action: 'sandbox', reason: 'File deletion requires sandboxing' },
    { pattern: /\bmkdir\b/, action: 'sandbox', reason: 'Directory creation requires sandboxing' },
    { pattern: /\bchmod\b/, action: 'sandbox', reason: 'Permission changes require sandboxing' },
    { pattern: /\bchown\b/, action: 'sandbox', reason: 'Ownership changes require sandboxing' },

    // Deny — destructive or dangerous
    { pattern: /\bsudo\b/, action: 'deny', reason: 'Elevated privileges denied by policy' },
    { pattern: /\bshutdown\b/, action: 'deny', reason: 'System shutdown denied by policy' },
    { pattern: /\breboot\b/, action: 'deny', reason: 'System reboot denied by policy' },
    { pattern: /\bmkfs\b/, action: 'deny', reason: 'Filesystem formatting denied by policy' },
    { pattern: /\bdd\b.*of=\/dev\//, action: 'deny', reason: 'Raw device write denied by policy' },

    // Prompt — needs human review
    { pattern: /\bgit\s+(reset|push\s+--force|clean)/, action: 'prompt', reason: 'Destructive git operations need approval' },
    { pattern: /\bcurl\b.*\|\s*(bash|sh)\b/, action: 'prompt', reason: 'Piped script execution needs approval' },
];

// ── PolicyLayer ─────────────────────────────────────────────────────────────

export class PolicyLayer {
    private rules: PolicyRule[];
    private defaultAction: PolicyAction;
    private evaluationHistory: PolicyEvaluation[] = [];

    constructor(config?: PolicyLayerConfig) {
        this.rules = config?.rules ?? [...DEFAULT_RULES];
        this.defaultAction = config?.defaultAction ?? 'prompt';
    }

    /** Evaluate a command against all policy rules. Returns the first match. */
    evaluate(command: string): PolicyEvaluation {
        const trimmed = command.trim();

        for (const rule of this.rules) {
            if (rule.pattern.test(trimmed)) {
                const evaluation: PolicyEvaluation = {
                    command: trimmed,
                    action: rule.action,
                    matchedRule: rule.pattern.source,
                    reason: rule.reason,
                    timestamp: new Date().toISOString(),
                };
                this.evaluationHistory.push(evaluation);
                return evaluation;
            }
        }

        // No matching rule — use default action
        const evaluation: PolicyEvaluation = {
            command: trimmed,
            action: this.defaultAction,
            reason: `No matching policy rule — defaulting to ${this.defaultAction}`,
            timestamp: new Date().toISOString(),
        };
        this.evaluationHistory.push(evaluation);
        return evaluation;
    }

    /** Consolidate all rule evaluations for a command into a single decision. */
    consolidate(command: string): ConsolidatedDecision {
        const trimmed = command.trim();
        const evaluations: PolicyEvaluation[] = [];

        // Evaluate against ALL rules (not just first match) for a full picture
        for (const rule of this.rules) {
            if (rule.pattern.test(trimmed)) {
                evaluations.push({
                    command: trimmed,
                    action: rule.action,
                    matchedRule: rule.pattern.source,
                    reason: rule.reason,
                    timestamp: new Date().toISOString(),
                });
            }
        }

        // Priority: deny > prompt > sandbox > allow
        let finalAction: PolicyAction = this.defaultAction;
        if (evaluations.length > 0) {
            if (evaluations.some(e => e.action === 'deny')) finalAction = 'deny';
            else if (evaluations.some(e => e.action === 'prompt')) finalAction = 'prompt';
            else if (evaluations.some(e => e.action === 'sandbox')) finalAction = 'sandbox';
            else finalAction = 'allow';
        }

        return {
            command: trimmed,
            finalAction,
            evaluations,
            timestamp: new Date().toISOString(),
        };
    }

    /** Add a new policy rule. */
    addRule(rule: PolicyRule): void {
        this.rules.push(rule);
    }

    /** Get the allow list — rules with action=allow. */
    getAllowList(): PolicyRule[] {
        return this.rules.filter(r => r.action === 'allow');
    }

    /** Get the deny list — rules with action=deny. */
    getDenyList(): PolicyRule[] {
        return this.rules.filter(r => r.action === 'deny');
    }

    /** Get full rule list. */
    getRules(): PolicyRule[] {
        return [...this.rules];
    }

    /** Get all evaluation history. */
    getHistory(): PolicyEvaluation[] {
        return [...this.evaluationHistory];
    }

    /** Clear history. */
    clear(): void {
        this.evaluationHistory = [];
    }
}

// ── Self-test ───────────────────────────────────────────────────────────────

if (process.argv.includes('--test')) {
    console.log('── grey_policy self-test ──');

    const layer = new PolicyLayer();

    // Allow evaluations
    const e1 = layer.evaluate('ls -la');
    console.assert(e1.action === 'allow', 'ls → allow');
    console.assert(e1.matchedRule !== undefined, 'ls matched a rule');
    console.log('  ✓ ls → allow');

    const e2 = layer.evaluate('pwd');
    console.assert(e2.action === 'allow', 'pwd → allow');
    console.log('  ✓ pwd → allow');

    const e3 = layer.evaluate('echo hello');
    console.assert(e3.action === 'allow', 'echo → allow');
    console.log('  ✓ echo → allow');

    // Sandbox evaluations
    const e4 = layer.evaluate('rm -rf testdir');
    console.assert(e4.action === 'sandbox', 'rm → sandbox');
    console.log('  ✓ rm → sandbox');

    const e5 = layer.evaluate('chmod 777 file.txt');
    console.assert(e5.action === 'sandbox', 'chmod → sandbox');
    console.log('  ✓ chmod → sandbox');

    // Deny evaluations
    const e6 = layer.evaluate('sudo apt install');
    console.assert(e6.action === 'deny', 'sudo → deny');
    console.log('  ✓ sudo → deny');

    const e7 = layer.evaluate('shutdown now');
    console.assert(e7.action === 'deny', 'shutdown → deny');
    console.log('  ✓ shutdown → deny');

    // Prompt evaluations
    const e8 = layer.evaluate('git reset --hard');
    console.assert(e8.action === 'prompt', 'git reset → prompt');
    console.log('  ✓ git reset → prompt');

    // Default (unknown command)
    const e9 = layer.evaluate('node server.js');
    console.assert(e9.action === 'prompt', 'unknown → prompt (default)');
    console.assert(e9.matchedRule === undefined, 'no matched rule for unknown');
    console.log('  ✓ unknown → prompt (default)');

    // Consolidation — single-action command
    const c1 = layer.consolidate('ls');
    console.assert(c1.finalAction === 'allow', 'consolidate ls → allow');
    console.assert(c1.evaluations.length >= 1, 'consolidate has evaluations');
    console.log('  ✓ consolidate single-action');

    // Consolidation — command matching multiple rules with different priorities
    // "sudo rm -rf /" matches both deny (sudo) and sandbox (rm)
    const c2 = layer.consolidate('sudo rm -rf /');
    console.assert(c2.finalAction === 'deny', 'deny takes priority over sandbox');
    console.assert(c2.evaluations.length >= 2, 'multiple rule matches');
    console.log('  ✓ consolidate priority: deny > sandbox');

    // Allow/deny lists
    const allowList = layer.getAllowList();
    console.assert(allowList.length > 0, 'allow list non-empty');
    console.assert(allowList.every(r => r.action === 'allow'), 'all allow rules');
    console.log('  ✓ getAllowList');

    const denyList = layer.getDenyList();
    console.assert(denyList.length > 0, 'deny list non-empty');
    console.assert(denyList.every(r => r.action === 'deny'), 'all deny rules');
    console.log('  ✓ getDenyList');

    // Custom rule
    layer.addRule({ pattern: /^npm\s+publish/, action: 'prompt', reason: 'Publishing needs review' });
    const e10 = layer.evaluate('npm publish');
    console.assert(e10.action === 'prompt', 'custom rule: npm publish → prompt');
    console.log('  ✓ custom rule');

    // History
    const hist = layer.getHistory();
    console.assert(hist.length > 0, 'history non-empty');
    console.log('  ✓ history tracking');

    // Custom config
    const minimal = new PolicyLayer({
        rules: [{ pattern: /.*/, action: 'allow', reason: 'allow-all' }],
        defaultAction: 'deny',
    });
    const me = minimal.evaluate('anything');
    console.assert(me.action === 'allow', 'custom config allow-all');
    console.log('  ✓ custom config');

    // Clear
    layer.clear();
    console.assert(layer.getHistory().length === 0, 'clear empties history');
    console.log('  ✓ clear');

    console.log('── all grey_policy tests passed ──');
}
