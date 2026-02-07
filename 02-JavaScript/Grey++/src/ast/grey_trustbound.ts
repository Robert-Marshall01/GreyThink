// ─────────────────────────────────────────────────────────────────────────────
//  grey_trustbound.ts  –  Stage 13 · Trust Boundary Coordinator
// ─────────────────────────────────────────────────────────────────────────────
//  Maintains regex-based maps of harmless vs. risky command patterns.
//  Auto-approves harmless commands and routes risky ones into sandbox or
//  prompt mode.  No external dependencies — pure in-memory stub.
//
//  Self-test:  npx tsx src/ast/grey_trustbound.ts --test
// ─────────────────────────────────────────────────────────────────────────────

// ── Types ───────────────────────────────────────────────────────────────────

export type CommandClassification = 'safe' | 'risky' | 'unknown';
export type RouteMode = 'auto-approve' | 'sandbox' | 'prompt';

export interface ClassificationResult {
    command: string;
    classification: CommandClassification;
    matchedPattern?: string;
    timestamp: string;
}

export interface RouteDecision {
    command: string;
    mode: RouteMode;
    classification: CommandClassification;
    reason: string;
    timestamp: string;
}

export interface TrustBoundaryConfig {
    safePatterns?: RegExp[];
    riskyPatterns?: RegExp[];
    defaultMode?: RouteMode;
}

// ── Default patterns ────────────────────────────────────────────────────────

const DEFAULT_SAFE_PATTERNS: RegExp[] = [
    /^ls\b/,
    /^pwd$/,
    /^echo\b/,
    /^cat\b/,
    /^head\b/,
    /^tail\b/,
    /^wc\b/,
    /^date$/,
    /^whoami$/,
    /^uname\b/,
    /^hostname$/,
    /^which\b/,
    /^env$/,
    /^printenv\b/,
];

const DEFAULT_RISKY_PATTERNS: RegExp[] = [
    /\brm\b/,
    /\brmdir\b/,
    /\bmkfs\b/,
    /\bdd\b/,
    /\bformat\b/,
    /\bshutdown\b/,
    /\breboot\b/,
    /\bkill\b/,
    /\bchmod\s+[0-7]{3,4}\b/,
    /\bchown\b/,
    /\bgit\s+(reset|push\s+--force|clean)/,
    /\bcurl\b.*\|\s*(bash|sh)\b/,
    /\bsudo\b/,
    /\b>\s*\/dev\//,
];

// ── TrustBoundaryCoordinator ────────────────────────────────────────────────

export class TrustBoundaryCoordinator {
    private safePatterns: RegExp[];
    private riskyPatterns: RegExp[];
    private defaultMode: RouteMode;
    private history: RouteDecision[] = [];
    private approvalLog: string[] = [];

    constructor(config?: TrustBoundaryConfig) {
        this.safePatterns = config?.safePatterns ?? [...DEFAULT_SAFE_PATTERNS];
        this.riskyPatterns = config?.riskyPatterns ?? [...DEFAULT_RISKY_PATTERNS];
        this.defaultMode = config?.defaultMode ?? 'prompt';
    }

    /** Classify a command as safe, risky, or unknown based on pattern matches. */
    classify(command: string): CommandClassification {
        const trimmed = command.trim();

        // Check risky first — risky takes precedence over safe
        for (const pat of this.riskyPatterns) {
            if (pat.test(trimmed)) return 'risky';
        }

        for (const pat of this.safePatterns) {
            if (pat.test(trimmed)) return 'safe';
        }

        return 'unknown';
    }

    /** Get a full classification result with metadata. */
    classifyFull(command: string): ClassificationResult {
        const trimmed = command.trim();
        let matchedPattern: string | undefined;

        for (const pat of this.riskyPatterns) {
            if (pat.test(trimmed)) {
                matchedPattern = pat.source;
                return {
                    command: trimmed,
                    classification: 'risky',
                    matchedPattern,
                    timestamp: new Date().toISOString(),
                };
            }
        }

        for (const pat of this.safePatterns) {
            if (pat.test(trimmed)) {
                matchedPattern = pat.source;
                return {
                    command: trimmed,
                    classification: 'safe',
                    matchedPattern,
                    timestamp: new Date().toISOString(),
                };
            }
        }

        return {
            command: trimmed,
            classification: 'unknown',
            timestamp: new Date().toISOString(),
        };
    }

    /** Auto-approve a command (logs it). Only safe commands should be auto-approved. */
    autoApprove(command: string): void {
        this.approvalLog.push(command);
    }

    /** Route a command to a specific mode and record the decision. */
    routeCommand(command: string, mode?: RouteMode): RouteDecision {
        const classification = this.classify(command);
        let resolvedMode: RouteMode;

        if (mode) {
            resolvedMode = mode;
        } else {
            switch (classification) {
                case 'safe': resolvedMode = 'auto-approve'; break;
                case 'risky': resolvedMode = 'sandbox'; break;
                case 'unknown': resolvedMode = this.defaultMode; break;
            }
        }

        const reason = mode
            ? `Explicitly routed to ${mode}`
            : `Auto-routed: classification=${classification}`;

        const decision: RouteDecision = {
            command,
            mode: resolvedMode,
            classification,
            reason,
            timestamp: new Date().toISOString(),
        };

        this.history.push(decision);

        if (resolvedMode === 'auto-approve') {
            this.autoApprove(command);
        }

        return decision;
    }

    /** Register additional safe patterns. */
    addSafePattern(pattern: RegExp): void {
        this.safePatterns.push(pattern);
    }

    /** Register additional risky patterns. */
    addRiskyPattern(pattern: RegExp): void {
        this.riskyPatterns.push(pattern);
    }

    /** Get all route decisions. */
    getHistory(): RouteDecision[] {
        return [...this.history];
    }

    /** Get all auto-approved commands. */
    getApprovalLog(): string[] {
        return [...this.approvalLog];
    }

    /** Clear all history. */
    clear(): void {
        this.history = [];
        this.approvalLog = [];
    }
}

// ── Self-test ───────────────────────────────────────────────────────────────

if (process.argv.includes('--test')) {
    console.log('── grey_trustbound self-test ──');

    const coord = new TrustBoundaryCoordinator();

    // Safe command classification
    console.assert(coord.classify('ls') === 'safe', 'ls is safe');
    console.assert(coord.classify('ls -la') === 'safe', 'ls -la is safe');
    console.assert(coord.classify('pwd') === 'safe', 'pwd is safe');
    console.assert(coord.classify('echo hello') === 'safe', 'echo is safe');
    console.assert(coord.classify('cat file.txt') === 'safe', 'cat is safe');
    console.assert(coord.classify('whoami') === 'safe', 'whoami is safe');
    console.log('  ✓ safe command classification');

    // Risky command classification
    console.assert(coord.classify('rm -rf /') === 'risky', 'rm -rf is risky');
    console.assert(coord.classify('sudo apt install') === 'risky', 'sudo is risky');
    console.assert(coord.classify('shutdown') === 'risky', 'shutdown is risky');
    console.assert(coord.classify('git reset --hard') === 'risky', 'git reset is risky');
    console.assert(coord.classify('kill -9 1234') === 'risky', 'kill is risky');
    console.log('  ✓ risky command classification');

    // Unknown command classification
    console.assert(coord.classify('node server.js') === 'unknown', 'node is unknown');
    console.assert(coord.classify('npm install') === 'unknown', 'npm install is unknown');
    console.log('  ✓ unknown command classification');

    // Full classification result
    const full = coord.classifyFull('rm -rf testdir');
    console.assert(full.classification === 'risky', 'full classification');
    console.assert(full.matchedPattern !== undefined, 'matched pattern present');
    console.log('  ✓ full classification result');

    // Auto-routing
    const d1 = coord.routeCommand('ls');
    console.assert(d1.mode === 'auto-approve', 'ls auto-approved');
    console.assert(d1.classification === 'safe', 'ls classified safe');
    console.log('  ✓ auto-route safe → auto-approve');

    const d2 = coord.routeCommand('rm -rf /tmp');
    console.assert(d2.mode === 'sandbox', 'rm sandboxed');
    console.log('  ✓ auto-route risky → sandbox');

    const d3 = coord.routeCommand('npm start');
    console.assert(d3.mode === 'prompt', 'unknown → prompt (default)');
    console.log('  ✓ auto-route unknown → prompt (default)');

    // Explicit routing
    const d4 = coord.routeCommand('npm test', 'auto-approve');
    console.assert(d4.mode === 'auto-approve', 'explicit auto-approve');
    console.log('  ✓ explicit route override');

    // Approval log
    const log = coord.getApprovalLog();
    console.assert(log.includes('ls'), 'approval log contains ls');
    console.assert(log.includes('npm test'), 'approval log contains npm test');
    console.log('  ✓ approval log tracking');

    // History
    const hist = coord.getHistory();
    console.assert(hist.length === 4, `history has 4 entries, got ${hist.length}`);
    console.log('  ✓ history tracking');

    // Custom patterns
    coord.addSafePattern(/^npm\s+test$/);
    console.assert(coord.classify('npm test') === 'safe', 'custom safe pattern');
    console.log('  ✓ custom safe pattern');

    coord.addRiskyPattern(/^npm\s+publish/);
    console.assert(coord.classify('npm publish') === 'risky', 'custom risky pattern');
    console.log('  ✓ custom risky pattern');

    // Clear
    coord.clear();
    console.assert(coord.getHistory().length === 0, 'clear empties history');
    console.assert(coord.getApprovalLog().length === 0, 'clear empties approval log');
    console.log('  ✓ clear');

    console.log('── all grey_trustbound tests passed ──');
}
