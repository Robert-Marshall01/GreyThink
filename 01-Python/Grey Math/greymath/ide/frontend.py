"""
Grey Math IDE Frontend.

Generates the single-page HTML/CSS/JS application for the IDE.
Multi-pane layout with editor, graph view, console, and inspector.
"""


def get_index_html() -> str:
    """Return the full IDE frontend HTML."""
    return """<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<title>Grey Math IDE</title>
<style>
:root {
    --bg-primary: #1e1e2e;
    --bg-secondary: #181825;
    --bg-tertiary: #11111b;
    --text-primary: #cdd6f4;
    --text-secondary: #a6adc8;
    --text-muted: #6c7086;
    --accent: #89b4fa;
    --accent-hover: #74c7ec;
    --green: #a6e3a1;
    --red: #f38ba8;
    --yellow: #f9e2af;
    --mauve: #cba6f7;
    --border: #313244;
    --surface: #1e1e2e;
    --font-mono: 'JetBrains Mono', 'Fira Code', 'Consolas', monospace;
    --font-sans: 'Inter', -apple-system, sans-serif;
}
* { margin: 0; padding: 0; box-sizing: border-box; }
body {
    font-family: var(--font-sans);
    background: var(--bg-tertiary);
    color: var(--text-primary);
    height: 100vh;
    overflow: hidden;
}

/* ── Top Bar ───────────────────────────────────── */
.topbar {
    height: 40px;
    background: var(--bg-secondary);
    border-bottom: 1px solid var(--border);
    display: flex;
    align-items: center;
    padding: 0 16px;
    gap: 16px;
}
.topbar .logo {
    font-weight: 700;
    font-size: 14px;
    color: var(--accent);
    letter-spacing: 1px;
}
.topbar .nav-item {
    font-size: 12px;
    color: var(--text-secondary);
    cursor: pointer;
    padding: 4px 8px;
    border-radius: 4px;
}
.topbar .nav-item:hover { background: var(--border); color: var(--text-primary); }
.topbar .status {
    margin-left: auto;
    font-size: 11px;
    color: var(--green);
    display: flex;
    align-items: center;
    gap: 6px;
}
.topbar .status .dot {
    width: 6px; height: 6px;
    border-radius: 50%;
    background: var(--green);
}

/* ── Main Layout ───────────────────────────────── */
.main {
    display: grid;
    grid-template-columns: 250px 1fr 350px;
    grid-template-rows: 1fr 250px;
    height: calc(100vh - 40px);
}

/* ── Sidebar ───────────────────────────────────── */
.sidebar {
    background: var(--bg-secondary);
    border-right: 1px solid var(--border);
    grid-row: 1 / 3;
    overflow-y: auto;
    padding: 8px;
}
.sidebar h3 {
    font-size: 11px;
    text-transform: uppercase;
    letter-spacing: 1px;
    color: var(--text-muted);
    padding: 8px;
    margin-top: 8px;
}
.sidebar .tree-item {
    padding: 4px 8px 4px 16px;
    font-size: 13px;
    cursor: pointer;
    border-radius: 4px;
    display: flex;
    align-items: center;
    gap: 6px;
    color: var(--text-secondary);
}
.sidebar .tree-item:hover { background: var(--border); color: var(--text-primary); }
.sidebar .tree-item.active { background: rgba(137,180,250,0.15); color: var(--accent); }
.sidebar .tree-icon { font-size: 14px; }

/* ── Editor Pane ───────────────────────────────── */
.editor-pane {
    background: var(--bg-primary);
    display: flex;
    flex-direction: column;
}
.editor-tabs {
    display: flex;
    background: var(--bg-secondary);
    border-bottom: 1px solid var(--border);
    height: 36px;
}
.tab {
    padding: 0 16px;
    font-size: 12px;
    display: flex;
    align-items: center;
    gap: 6px;
    cursor: pointer;
    border-right: 1px solid var(--border);
    color: var(--text-secondary);
}
.tab.active { background: var(--bg-primary); color: var(--text-primary);
    border-bottom: 2px solid var(--accent); }
.editor-content {
    flex: 1;
    padding: 16px;
    overflow-y: auto;
}
.cell {
    background: var(--bg-secondary);
    border: 1px solid var(--border);
    border-radius: 8px;
    margin-bottom: 12px;
    overflow: hidden;
}
.cell-header {
    display: flex;
    align-items: center;
    padding: 6px 12px;
    background: var(--bg-tertiary);
    font-size: 11px;
    color: var(--text-muted);
    gap: 8px;
}
.cell-type {
    background: var(--accent);
    color: var(--bg-tertiary);
    padding: 1px 6px;
    border-radius: 3px;
    font-weight: 600;
    font-size: 10px;
}
.cell-input {
    padding: 12px;
    font-family: var(--font-mono);
    font-size: 14px;
}
.cell-input textarea {
    width: 100%;
    background: transparent;
    border: none;
    color: var(--text-primary);
    font-family: var(--font-mono);
    font-size: 14px;
    resize: none;
    outline: none;
    min-height: 60px;
}
.cell-output {
    padding: 12px;
    border-top: 1px solid var(--border);
    font-family: var(--font-mono);
    font-size: 13px;
    color: var(--green);
    background: rgba(166,227,161,0.05);
}
.cell-output.error { color: var(--red); background: rgba(243,139,168,0.05); }
.run-btn {
    background: var(--accent);
    color: var(--bg-tertiary);
    border: none;
    padding: 4px 12px;
    border-radius: 4px;
    cursor: pointer;
    font-size: 11px;
    font-weight: 600;
    margin-left: auto;
}
.run-btn:hover { background: var(--accent-hover); }

/* ── Inspector Pane ────────────────────────────── */
.inspector {
    background: var(--bg-secondary);
    border-left: 1px solid var(--border);
    grid-row: 1 / 3;
    overflow-y: auto;
    padding: 12px;
}
.inspector h3 {
    font-size: 12px;
    color: var(--accent);
    margin-bottom: 8px;
    padding-bottom: 6px;
    border-bottom: 1px solid var(--border);
}
.inspector .prop {
    display: flex;
    justify-content: space-between;
    padding: 4px 0;
    font-size: 12px;
}
.inspector .prop-key { color: var(--text-muted); }
.inspector .prop-val { color: var(--text-primary); font-family: var(--font-mono); }
.inspector .section { margin-bottom: 16px; }
.stability-badge {
    padding: 2px 8px;
    border-radius: 4px;
    font-size: 11px;
    font-weight: 600;
}
.stability-badge.stable { background: rgba(166,227,161,0.2); color: var(--green); }
.stability-badge.unstable { background: rgba(243,139,168,0.2); color: var(--red); }
.stability-badge.conditional { background: rgba(249,226,175,0.2); color: var(--yellow); }

/* ── Console ───────────────────────────────────── */
.console {
    background: var(--bg-tertiary);
    border-top: 1px solid var(--border);
    display: flex;
    flex-direction: column;
}
.console-tabs {
    display: flex;
    background: var(--bg-secondary);
    border-bottom: 1px solid var(--border);
    height: 32px;
}
.console-tabs .tab { font-size: 11px; height: 32px; }
.console-output {
    flex: 1;
    padding: 8px 12px;
    font-family: var(--font-mono);
    font-size: 12px;
    overflow-y: auto;
    color: var(--text-secondary);
}
.console-output .log-line { padding: 2px 0; }
.console-output .log-line.info { color: var(--accent); }
.console-output .log-line.warn { color: var(--yellow); }
.console-output .log-line.error { color: var(--red); }
.console-input-row {
    display: flex;
    border-top: 1px solid var(--border);
    padding: 6px 12px;
    gap: 8px;
    align-items: center;
}
.console-input-row .prompt { color: var(--accent); font-family: var(--font-mono); font-size: 13px; }
.console-input-row input {
    flex: 1;
    background: transparent;
    border: none;
    color: var(--text-primary);
    font-family: var(--font-mono);
    font-size: 13px;
    outline: none;
}

/* ── Canvas for visualizations ─────────────────── */
.viz-canvas {
    background: var(--bg-primary);
    border: 1px solid var(--border);
    border-radius: 8px;
    margin: 8px 0;
    height: 200px;
    display: flex;
    align-items: center;
    justify-content: center;
    color: var(--text-muted);
    font-size: 13px;
}
</style>
</head>
<body>
<!-- Top Bar -->
<div class="topbar">
    <span class="logo">GREY MATH</span>
    <span class="nav-item">File</span>
    <span class="nav-item">Edit</span>
    <span class="nav-item">View</span>
    <span class="nav-item">Math</span>
    <span class="nav-item">Run</span>
    <span class="nav-item">Plugins</span>
    <span class="nav-item">Help</span>
    <span class="status"><span class="dot"></span> Connected</span>
</div>

<!-- Main Layout -->
<div class="main">
    <!-- Sidebar -->
    <div class="sidebar">
        <h3>Explorer</h3>
        <div class="tree-item active"><span class="tree-icon">&#128196;</span> workspace.gm</div>
        <div class="tree-item"><span class="tree-icon">&#128196;</span> experiment_01.gm</div>

        <h3>Math Objects</h3>
        <div class="tree-item"><span class="tree-icon">&#120031;</span> Scalar: alpha</div>
        <div class="tree-item"><span class="tree-icon">&#8477;</span> Matrix: A (3x3)</div>
        <div class="tree-item"><span class="tree-icon">&#120029;</span> Operator: T</div>
        <div class="tree-item"><span class="tree-icon">&#120027;</span> Manifold: S2</div>

        <h3>Plugins</h3>
        <div class="tree-item"><span class="tree-icon">&#128268;</span> Functional Analysis</div>
        <div class="tree-item"><span class="tree-icon">&#128268;</span> Diff Geometry</div>
        <div class="tree-item"><span class="tree-icon">&#128268;</span> Category Theory</div>

        <h3>Experiments</h3>
        <div class="tree-item"><span class="tree-icon">&#9654;</span> Spectral Analysis</div>
        <div class="tree-item"><span class="tree-icon">&#9654;</span> Lorenz Attractor</div>
    </div>

    <!-- Editor Pane -->
    <div class="editor-pane">
        <div class="editor-tabs">
            <div class="tab active">workspace.gm</div>
            <div class="tab">Graph View</div>
            <div class="tab">Visualizer</div>
        </div>
        <div class="editor-content" id="editor">
            <div class="cell" id="cell-1">
                <div class="cell-header">
                    <span class="cell-type">MATH</span>
                    <span>Cell 1</span>
                    <button class="run-btn" onclick="runCell(1)">Run</button>
                </div>
                <div class="cell-input">
                    <textarea id="input-1" rows="3"># Define a matrix and compute its spectrum
A = [[4, 1, 0], [1, 3, 1], [0, 1, 2]]
eig(A)</textarea>
                </div>
                <div class="cell-output" id="output-1"></div>
            </div>

            <div class="cell" id="cell-2">
                <div class="cell-header">
                    <span class="cell-type">MATH</span>
                    <span>Cell 2</span>
                    <button class="run-btn" onclick="runCell(2)">Run</button>
                </div>
                <div class="cell-input">
                    <textarea id="input-2" rows="3"># Compute condition number and stability
det(A)</textarea>
                </div>
                <div class="cell-output" id="output-2"></div>
            </div>

            <div class="cell" id="cell-3">
                <div class="cell-header">
                    <span class="cell-type">MATH</span>
                    <span>Cell 3</span>
                    <button class="run-btn" onclick="runCell(3)">Run</button>
                </div>
                <div class="cell-input">
                    <textarea id="input-3" rows="2"># Quick computation
sqrt(2) * pi</textarea>
                </div>
                <div class="cell-output" id="output-3"></div>
            </div>

            <button class="run-btn" style="margin: 8px 0;" onclick="addCell()">+ Add Cell</button>
        </div>
    </div>

    <!-- Inspector -->
    <div class="inspector">
        <div class="section">
            <h3>Object Inspector</h3>
            <div class="prop"><span class="prop-key">Type</span> <span class="prop-val">Matrix[3x3]</span></div>
            <div class="prop"><span class="prop-key">Field</span> <span class="prop-val">Real</span></div>
            <div class="prop"><span class="prop-key">Symmetric</span> <span class="prop-val">Yes</span></div>
            <div class="prop"><span class="prop-key">Pos. Definite</span> <span class="prop-val">Yes</span></div>
        </div>

        <div class="section">
            <h3>Spectral Data</h3>
            <div class="prop"><span class="prop-key">eigenvalues</span> <span class="prop-val">[1.20, 3.00, 4.80]</span></div>
            <div class="prop"><span class="prop-key">spectral radius</span> <span class="prop-val">4.80</span></div>
            <div class="prop"><span class="prop-key">condition #</span> <span class="prop-val">4.00</span></div>
        </div>

        <div class="section">
            <h3>Stability</h3>
            <span class="stability-badge stable">STABLE</span>
            <div class="prop" style="margin-top:8px"><span class="prop-key">precision</span> <span class="prop-val">float64</span></div>
            <div class="prop"><span class="prop-key">error bound</span> <span class="prop-val">&lt; 1e-14</span></div>
        </div>

        <div class="section">
            <h3>Visualization</h3>
            <div class="viz-canvas">Spectrum plot area</div>
        </div>

        <div class="section">
            <h3>Metadata</h3>
            <div class="prop"><span class="prop-key">Provenance</span> <span class="prop-val">Cell 1</span></div>
            <div class="prop"><span class="prop-key">Tags</span> <span class="prop-val">spectral, research</span></div>
        </div>
    </div>

    <!-- Console -->
    <div class="console">
        <div class="console-tabs">
            <div class="tab active">Console</div>
            <div class="tab">Problems</div>
            <div class="tab">Debug</div>
            <div class="tab">Profiler</div>
        </div>
        <div class="console-output" id="console-output">
            <div class="log-line info">[Grey Math] IDE initialized. Backend connected.</div>
            <div class="log-line">[Grey Math] Session created: workspace.gm</div>
            <div class="log-line info">[Grey Math] Plugins loaded: Functional Analysis, Diff Geometry, Category Theory</div>
        </div>
        <div class="console-input-row">
            <span class="prompt">gm&gt;</span>
            <input type="text" id="console-input" placeholder="Enter expression..." onkeydown="if(event.key==='Enter') consoleEval()">
        </div>
    </div>
</div>

<script>
const API_BASE = window.location.origin;
let sessionId = null;
let cellCount = 3;
let ws = null;

// Initialize session
async function init() {
    try {
        const resp = await fetch(API_BASE + '/api/session', { method: 'POST' });
        const data = await resp.json();
        sessionId = data.session_id;
        log('info', `Session started: ${sessionId}`);

        // Connect WebSocket
        const wsUrl = `ws://${window.location.host}/ws/${sessionId}`;
        ws = new WebSocket(wsUrl);
        ws.onmessage = (e) => {
            const msg = JSON.parse(e.data);
            if (msg.type === 'result') {
                log('info', JSON.stringify(msg.data));
            }
        };
        ws.onerror = () => log('warn', 'WebSocket connection failed — using REST API');
    } catch (e) {
        log('warn', 'Backend not available — running in demo mode');
    }
}

async function runCell(n) {
    const input = document.getElementById('input-' + n);
    const output = document.getElementById('output-' + n);
    const expr = input.value.trim();

    if (!expr || expr.startsWith('#')) {
        output.textContent = '';
        return;
    }

    // Filter out comments, get the actual math expression
    const lines = expr.split('\\n').filter(l => !l.trim().startsWith('#'));
    const mathExpr = lines.join('\\n').trim();

    if (!mathExpr) {
        output.textContent = '';
        return;
    }

    output.className = 'cell-output';

    try {
        const resp = await fetch(API_BASE + '/api/eval', {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({ expression: mathExpr, session_id: sessionId }),
        });
        const data = await resp.json();

        if (data.result) {
            const r = data.result;
            if (r.result) {
                output.textContent = formatResult(r.result);
            } else if (r.assigned) {
                output.textContent = `${r.assigned} = ${formatResult(r.value)}`;
            } else if (r.error) {
                output.className = 'cell-output error';
                output.textContent = r.error;
            }
        } else if (data.error) {
            output.className = 'cell-output error';
            output.textContent = data.error;
        }
    } catch (e) {
        // Demo mode fallback
        try {
            const result = eval(mathExpr.replace(/pi/g, 'Math.PI').replace(/sqrt/g, 'Math.sqrt').replace(/sin/g, 'Math.sin').replace(/cos/g, 'Math.cos').replace(/exp/g, 'Math.exp').replace(/log/g, 'Math.log'));
            output.textContent = String(result);
        } catch (e2) {
            output.className = 'cell-output error';
            output.textContent = 'Evaluation requires backend server. Start with: python -m greymath.ide.server';
        }
    }
}

function formatResult(r) {
    if (r === null || r === undefined) return 'null';
    if (typeof r === 'object') {
        if (r.type === 'array') return JSON.stringify(r.data);
        if (r.type === 'scalar') return String(r.value);
        if (r.type === 'complex') return `${r.real} + ${r.imag}i`;
        if (r.type === 'matrix') return JSON.stringify(r.data);
        return JSON.stringify(r);
    }
    return String(r);
}

function addCell() {
    cellCount++;
    const editor = document.getElementById('editor');
    const btn = editor.querySelector('.run-btn:last-child');
    const cell = document.createElement('div');
    cell.className = 'cell';
    cell.id = 'cell-' + cellCount;
    cell.innerHTML = `
        <div class="cell-header">
            <span class="cell-type">MATH</span>
            <span>Cell ${cellCount}</span>
            <button class="run-btn" onclick="runCell(${cellCount})">Run</button>
        </div>
        <div class="cell-input">
            <textarea id="input-${cellCount}" rows="2" placeholder="Enter expression..."></textarea>
        </div>
        <div class="cell-output" id="output-${cellCount}"></div>
    `;
    editor.insertBefore(cell, btn);
}

async function consoleEval() {
    const input = document.getElementById('console-input');
    const expr = input.value.trim();
    if (!expr) return;

    log('', `gm> ${expr}`);
    input.value = '';

    try {
        const resp = await fetch(API_BASE + '/api/eval', {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({ expression: expr, session_id: sessionId }),
        });
        const data = await resp.json();
        if (data.result && data.result.result) {
            log('info', formatResult(data.result.result));
        } else if (data.result && data.result.error) {
            log('error', data.result.error);
        } else if (data.error) {
            log('error', data.error);
        }
    } catch (e) {
        log('error', 'Backend not available');
    }
}

function log(level, msg) {
    const output = document.getElementById('console-output');
    const line = document.createElement('div');
    line.className = 'log-line' + (level ? ' ' + level : '');
    line.textContent = msg;
    output.appendChild(line);
    output.scrollTop = output.scrollHeight;
}

init();
</script>
</body>
</html>"""
