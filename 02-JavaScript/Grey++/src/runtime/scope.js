// ─── Grey++ Scope ────────────────────────────────────────────────────────────
// Lexical scope chain for variable isolation.  Each scope holds its own
// bindings and optionally delegates to a parent.  This prevents one plugin
// or function invocation from clobbering another's state.

export class Scope {
    /**
     * @param {Scope | null} [parent]
     */
    constructor(parent = null) {
        /** @type {Scope | null} */
        this.parent = parent;
        /** @type {Map<string, any>} */
        this.bindings = new Map();
    }

    /**
     * Define a variable in *this* scope (shadows parent).
     * @param {string} name
     * @param {any} value
     */
    define(name, value) {
        this.bindings.set(name, value);
    }

    /**
     * Look up a name, walking the scope chain.
     * @param {string} name
     * @returns {any}
     */
    get(name) {
        if (this.bindings.has(name)) return this.bindings.get(name);
        if (this.parent) return this.parent.get(name);
        return undefined;
    }

    /**
     * Check whether a name is resolvable anywhere in the chain.
     * @param {string} name
     * @returns {boolean}
     */
    has(name) {
        if (this.bindings.has(name)) return true;
        if (this.parent) return this.parent.has(name);
        return false;
    }

    /**
     * Create a child scope that inherits from this one.
     * @returns {Scope}
     */
    child() {
        return new Scope(this);
    }
}
