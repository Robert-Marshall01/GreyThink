// ─────────────────────────────────────────────────────────────
// Grey DB — Typed Query Builder
// Strongly typed TS query builder → SQL compilation
// Supports selects, joins, subqueries, CTEs, aggregates
// ─────────────────────────────────────────────────────────────

export type WhereOp = "=" | "!=" | ">" | "<" | ">=" | "<=" | "LIKE" | "ILIKE" | "IN" | "NOT IN" | "IS NULL" | "IS NOT NULL" | "BETWEEN" | "@>" | "<@";
export type JoinType = "INNER" | "LEFT" | "RIGHT" | "FULL" | "CROSS";
export type OrderDir = "ASC" | "DESC";
export type AggFn = "COUNT" | "SUM" | "AVG" | "MIN" | "MAX" | "ARRAY_AGG" | "JSON_AGG";

interface WhereClause {
    column: string;
    op: WhereOp;
    value?: any;
    or?: boolean;
}

interface JoinClause {
    type: JoinType;
    table: string;
    alias?: string;
    on: string;
}

interface OrderClause {
    column: string;
    dir: OrderDir;
    nulls?: "FIRST" | "LAST";
}

interface CTEDefinition {
    name: string;
    query: string;
    recursive?: boolean;
}

export class QueryBuilder {
    private _table: string = "";
    private _alias?: string;
    private _schema: string = "public";
    private _selectCols: string[] = [];
    private _whereClauses: WhereClause[] = [];
    private _joins: JoinClause[] = [];
    private _orderBy: OrderClause[] = [];
    private _groupBy: string[] = [];
    private _having: string[] = [];
    private _limit?: number;
    private _offset?: number;
    private _ctes: CTEDefinition[] = [];
    private _distinct: boolean = false;
    private _forUpdate: boolean = false;
    private _params: any[] = [];
    private _paramIndex: number = 1;
    private _tenantId?: string;
    private _rawWhere: Array<{ expr: string; values: any[]; or: boolean }> = [];

    // ── Builder methods ───────────────────────────────────────

    static from(table: string, schema = "public"): QueryBuilder {
        const qb = new QueryBuilder();
        qb._table = table;
        qb._schema = schema;
        return qb;
    }

    as(alias: string): QueryBuilder {
        this._alias = alias;
        return this;
    }

    select(...columns: string[]): QueryBuilder {
        this._selectCols.push(...columns);
        return this;
    }

    selectRaw(expr: string): QueryBuilder {
        this._selectCols.push(expr);
        return this;
    }

    selectAgg(fn: AggFn, column: string, alias?: string): QueryBuilder {
        const expr = `${fn}(${column})${alias ? ` AS "${alias}"` : ""}`;
        this._selectCols.push(expr);
        return this;
    }

    distinct(): QueryBuilder {
        this._distinct = true;
        return this;
    }

    where(column: string, op: WhereOp, value?: any): QueryBuilder {
        this._whereClauses.push({ column, op, value });
        return this;
    }

    orWhere(column: string, op: WhereOp, value?: any): QueryBuilder {
        this._whereClauses.push({ column, op, value, or: true });
        return this;
    }

    whereIn(column: string, values: any[]): QueryBuilder {
        this._whereClauses.push({ column, op: "IN", value: values });
        return this;
    }

    whereNull(column: string): QueryBuilder {
        this._whereClauses.push({ column, op: "IS NULL" });
        return this;
    }

    whereNotNull(column: string): QueryBuilder {
        this._whereClauses.push({ column, op: "IS NOT NULL" });
        return this;
    }

    whereBetween(column: string, low: any, high: any): QueryBuilder {
        this._whereClauses.push({ column, op: "BETWEEN", value: [low, high] });
        return this;
    }

    whereRaw(expression: string, ...values: any[]): QueryBuilder {
        this._rawWhere.push({ expr: expression, values, or: false });
        return this;
    }

    orWhereRaw(expression: string, ...values: any[]): QueryBuilder {
        this._rawWhere.push({ expr: expression, values, or: true });
        return this;
    }

    join(table: string, on: string, type: JoinType = "INNER", alias?: string): QueryBuilder {
        this._joins.push({ type, table, on, alias });
        return this;
    }

    leftJoin(table: string, on: string, alias?: string): QueryBuilder {
        return this.join(table, on, "LEFT", alias);
    }

    rightJoin(table: string, on: string, alias?: string): QueryBuilder {
        return this.join(table, on, "RIGHT", alias);
    }

    orderBy(column: string, dir: OrderDir = "ASC", nulls?: "FIRST" | "LAST"): QueryBuilder {
        this._orderBy.push({ column, dir, nulls });
        return this;
    }

    groupBy(...columns: string[]): QueryBuilder {
        this._groupBy.push(...columns);
        return this;
    }

    having(expr: string): QueryBuilder {
        this._having.push(expr);
        return this;
    }

    limit(n: number): QueryBuilder {
        this._limit = n;
        return this;
    }

    offset(n: number): QueryBuilder {
        this._offset = n;
        return this;
    }

    forUpdate(): QueryBuilder {
        this._forUpdate = true;
        return this;
    }

    withCTE(name: string, query: string, recursive = false): QueryBuilder {
        this._ctes.push({ name, query, recursive });
        return this;
    }

    forTenant(tenantId: string): QueryBuilder {
        this._tenantId = tenantId;
        return this;
    }

    // ── SQL compilation ───────────────────────────────────────

    toSQL(): { text: string; values: any[] } {
        this._params = [];
        this._paramIndex = 1;

        const parts: string[] = [];

        // CTEs
        if (this._ctes.length > 0) {
            const isRecursive = this._ctes.some((c) => c.recursive);
            const cteSQL = this._ctes.map((c) => `"${c.name}" AS (${c.query})`).join(",\n  ");
            parts.push(`WITH${isRecursive ? " RECURSIVE" : ""} ${cteSQL}`);
        }

        // SELECT
        const cols = this._selectCols.length > 0 ? this._selectCols.join(", ") : "*";
        parts.push(`SELECT${this._distinct ? " DISTINCT" : ""} ${cols}`);

        // FROM
        const tableRef = `"${this._schema}"."${this._table}"${this._alias ? ` AS "${this._alias}"` : ""}`;
        parts.push(`FROM ${tableRef}`);

        // JOINs
        for (const j of this._joins) {
            const alias = j.alias ? ` AS "${j.alias}"` : "";
            parts.push(`${j.type} JOIN "${j.table}"${alias} ON ${j.on}`);
        }

        // WHERE
        const whereParts = this.buildWhere();
        if (whereParts) parts.push(`WHERE ${whereParts}`);

        // GROUP BY
        if (this._groupBy.length > 0) {
            parts.push(`GROUP BY ${this._groupBy.join(", ")}`);
        }

        // HAVING
        if (this._having.length > 0) {
            parts.push(`HAVING ${this._having.join(" AND ")}`);
        }

        // ORDER BY
        if (this._orderBy.length > 0) {
            const orders = this._orderBy.map((o) => {
                let s = `${o.column} ${o.dir}`;
                if (o.nulls) s += ` NULLS ${o.nulls}`;
                return s;
            });
            parts.push(`ORDER BY ${orders.join(", ")}`);
        }

        // LIMIT / OFFSET
        if (this._limit !== undefined) parts.push(`LIMIT ${this._limit}`);
        if (this._offset !== undefined) parts.push(`OFFSET ${this._offset}`);

        // FOR UPDATE
        if (this._forUpdate) parts.push("FOR UPDATE");

        return { text: parts.join("\n"), values: this._params };
    }

    private buildWhere(): string {
        if (this._whereClauses.length === 0 && !this._tenantId && this._rawWhere.length === 0) return "";

        const conditions: string[] = [];

        if (this._tenantId) {
            conditions.push(`tenant_id = $${this._paramIndex++}`);
            this._params.push(this._tenantId);
        }

        for (const clause of this._whereClauses) {
            let cond: string;
            if (clause.op === "IS NULL" || clause.op === "IS NOT NULL") {
                cond = `${clause.column} ${clause.op}`;
            } else if (clause.op === "IN" || clause.op === "NOT IN") {
                const placeholders = (clause.value as any[]).map(() => `$${this._paramIndex++}`).join(", ");
                this._params.push(...clause.value);
                cond = `${clause.column} ${clause.op} (${placeholders})`;
            } else if (clause.op === "BETWEEN") {
                cond = `${clause.column} BETWEEN $${this._paramIndex++} AND $${this._paramIndex++}`;
                this._params.push(clause.value[0], clause.value[1]);
            } else {
                cond = `${clause.column} ${clause.op} $${this._paramIndex++}`;
                this._params.push(clause.value);
            }

            if (clause.or && conditions.length > 0) {
                conditions.push(`OR ${cond}`);
            } else {
                conditions.push(cond);
            }
        }

        // Handle raw WHERE expressions
        for (const raw of this._rawWhere) {
            let expr = raw.expr;
            for (const val of raw.values) {
                expr = expr.replace("?", `$${this._paramIndex++}`);
                this._params.push(val);
            }
            if (raw.or && conditions.length > 0) {
                conditions.push(`OR ${expr}`);
            } else {
                conditions.push(expr);
            }
        }

        return conditions.join(" AND ").replace(/ AND OR /g, " OR ");
    }
}

// ── Insert / Update / Delete builders ─────────────────────

export class InsertBuilder {
    private _table: string;
    private _schema: string;
    private _columns: string[] = [];
    private _values: any[][] = [];
    private _returning: string[] = [];
    private _onConflict?: { columns: string[]; action: "DO NOTHING" | "DO UPDATE"; updates?: string[] };

    constructor(table: string, schema = "public") {
        this._table = table;
        this._schema = schema;
    }

    static into(table: string, schema = "public"): InsertBuilder {
        return new InsertBuilder(table, schema);
    }

    columns(...cols: string[]): InsertBuilder {
        this._columns = cols;
        return this;
    }

    values(...rows: Record<string, any>[]): InsertBuilder {
        if (this._columns.length === 0 && rows.length > 0) {
            this._columns = Object.keys(rows[0]);
        }
        for (const row of rows) {
            this._values.push(this._columns.map((c) => row[c]));
        }
        return this;
    }

    onConflict(columns: string[], action: "DO NOTHING" | "DO UPDATE", updates?: string[]): InsertBuilder {
        this._onConflict = { columns, action, updates };
        return this;
    }

    returning(...cols: string[]): InsertBuilder {
        this._returning = cols;
        return this;
    }

    toSQL(): { text: string; values: any[] } {
        const params: any[] = [];
        let idx = 1;
        const cols = this._columns.map((c) => `"${c}"`).join(", ");
        const valueRows = this._values.map((row) => {
            const placeholders = row.map((v) => { params.push(v); return `$${idx++}`; }).join(", ");
            return `(${placeholders})`;
        }).join(",\n  ");

        let sql = `INSERT INTO "${this._schema}"."${this._table}" (${cols})\nVALUES ${valueRows}`;

        if (this._onConflict) {
            const conflictCols = this._onConflict.columns.map((c) => `"${c}"`).join(", ");
            sql += `\nON CONFLICT (${conflictCols})`;
            if (this._onConflict.action === "DO NOTHING") {
                sql += " DO NOTHING";
            } else {
                const updates = this._onConflict.updates?.join(", ") || this._columns.filter((c) => !this._onConflict!.columns.includes(c)).map((c) => `"${c}" = EXCLUDED."${c}"`).join(", ");
                sql += ` DO UPDATE SET ${updates}`;
            }
        }

        if (this._returning.length > 0) {
            sql += `\nRETURNING ${this._returning.join(", ")}`;
        }

        return { text: sql + ";", values: params };
    }
}

export class UpdateBuilder {
    private _table: string;
    private _schema: string;
    private _sets: Record<string, any> = {};
    private _whereClauses: WhereClause[] = [];
    private _returning: string[] = [];
    private _params: any[] = [];
    private _paramIndex: number = 1;

    constructor(table: string, schema = "public") {
        this._table = table;
        this._schema = schema;
    }

    static table(table: string, schema = "public"): UpdateBuilder {
        return new UpdateBuilder(table, schema);
    }

    set(column: string, value: any): UpdateBuilder {
        this._sets[column] = value;
        return this;
    }

    setAll(data: Record<string, any>): UpdateBuilder {
        Object.assign(this._sets, data);
        return this;
    }

    where(column: string, op: WhereOp, value?: any): UpdateBuilder {
        this._whereClauses.push({ column, op, value });
        return this;
    }

    returning(...cols: string[]): UpdateBuilder {
        this._returning = cols;
        return this;
    }

    toSQL(): { text: string; values: any[] } {
        this._params = [];
        this._paramIndex = 1;

        const sets = Object.entries(this._sets).map(([col, val]) => {
            this._params.push(val);
            return `"${col}" = $${this._paramIndex++}`;
        }).join(", ");

        let sql = `UPDATE "${this._schema}"."${this._table}" SET ${sets}`;

        if (this._whereClauses.length > 0) {
            const where = this._whereClauses.map((c) => {
                this._params.push(c.value);
                return `${c.column} ${c.op} $${this._paramIndex++}`;
            }).join(" AND ");
            sql += `\nWHERE ${where}`;
        }

        if (this._returning.length > 0) {
            sql += `\nRETURNING ${this._returning.join(", ")}`;
        }

        return { text: sql + ";", values: this._params };
    }
}

export class DeleteBuilder {
    private _table: string;
    private _schema: string;
    private _whereClauses: WhereClause[] = [];
    private _returning: string[] = [];
    private _params: any[] = [];
    private _paramIndex: number = 1;

    constructor(table: string, schema = "public") {
        this._table = table;
        this._schema = schema;
    }

    static from(table: string, schema = "public"): DeleteBuilder {
        return new DeleteBuilder(table, schema);
    }

    where(column: string, op: WhereOp, value?: any): DeleteBuilder {
        this._whereClauses.push({ column, op, value });
        return this;
    }

    returning(...cols: string[]): DeleteBuilder {
        this._returning = cols;
        return this;
    }

    toSQL(): { text: string; values: any[] } {
        this._params = [];
        this._paramIndex = 1;

        let sql = `DELETE FROM "${this._schema}"."${this._table}"`;

        if (this._whereClauses.length > 0) {
            const where = this._whereClauses.map((c) => {
                if (c.op === "IS NULL" || c.op === "IS NOT NULL") {
                    return `${c.column} ${c.op}`;
                }
                this._params.push(c.value);
                return `${c.column} ${c.op} $${this._paramIndex++}`;
            }).join(" AND ");
            sql += `\nWHERE ${where}`;
        }

        if (this._returning.length > 0) {
            sql += `\nRETURNING ${this._returning.join(", ")}`;
        }

        return { text: sql + ";", values: this._params };
    }
}
