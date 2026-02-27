// ─────────────────────────────────────────────────────────────
// Grey DB — Schema DSL
// Declarative, type-safe table definitions
// ─────────────────────────────────────────────────────────────

export type ColumnType =
  | "uuid"
  | "text"
  | "varchar"
  | "integer"
  | "bigint"
  | "serial"
  | "bigserial"
  | "boolean"
  | "timestamp"
  | "timestamptz"
  | "date"
  | "jsonb"
  | "json"
  | "float"
  | "double"
  | "numeric"
  | "bytea"
  | "vector";

export interface ColumnConstraints {
  primaryKey?: boolean;
  unique?: boolean;
  nullable?: boolean;
  indexed?: boolean;
  default?: string | number | boolean | null;
  defaultNow?: boolean;
  defaultUUID?: boolean;
  check?: string;
  references?: { table: string; column: string; onDelete?: string; onUpdate?: string };
  array?: boolean;
  vectorDimensions?: number;
}

export class ColumnBuilder {
  public readonly _type: ColumnType;
  public readonly _constraints: ColumnConstraints = {};
  public readonly _length?: number;
  public readonly _precision?: number;
  public readonly _scale?: number;

  constructor(type: ColumnType, length?: number, precision?: number, scale?: number) {
    this._type = type;
    this._length = length;
    this._precision = precision;
    this._scale = scale;
  }

  primaryKey(): ColumnBuilder {
    (this._constraints as any).primaryKey = true;
    return this;
  }

  unique(): ColumnBuilder {
    (this._constraints as any).unique = true;
    return this;
  }

  nullable(): ColumnBuilder {
    (this._constraints as any).nullable = true;
    return this;
  }

  notNull(): ColumnBuilder {
    (this._constraints as any).nullable = false;
    return this;
  }

  indexed(): ColumnBuilder {
    (this._constraints as any).indexed = true;
    return this;
  }

  default(value: string | number | boolean | null): ColumnBuilder {
    (this._constraints as any).default = value;
    return this;
  }

  defaultNow(): ColumnBuilder {
    (this._constraints as any).defaultNow = true;
    return this;
  }

  defaultUUID(): ColumnBuilder {
    (this._constraints as any).defaultUUID = true;
    return this;
  }

  check(expression: string): ColumnBuilder {
    (this._constraints as any).check = expression;
    return this;
  }

  references(table: string, column: string, opts?: { onDelete?: string; onUpdate?: string }): ColumnBuilder {
    (this._constraints as any).references = { table, column, ...opts };
    return this;
  }

  array(): ColumnBuilder {
    (this._constraints as any).array = true;
    return this;
  }
}

// ── Column factory functions ───────────────────────────────
export const uuid = () => new ColumnBuilder("uuid");
export const text = () => new ColumnBuilder("text");
export const varchar = (length: number) => new ColumnBuilder("varchar", length);
export const integer = () => new ColumnBuilder("integer");
export const bigint = () => new ColumnBuilder("bigint");
export const serial = () => new ColumnBuilder("serial");
export const bigserial = () => new ColumnBuilder("bigserial");
export const boolean = () => new ColumnBuilder("boolean");
export const timestamp = () => new ColumnBuilder("timestamp");
export const timestamptz = () => new ColumnBuilder("timestamptz");
export const date = () => new ColumnBuilder("date");
export const jsonb = () => new ColumnBuilder("jsonb");
export const json = () => new ColumnBuilder("json");
export const float = () => new ColumnBuilder("float");
export const double = () => new ColumnBuilder("double");
export const numeric = (precision?: number, scale?: number) => new ColumnBuilder("numeric", undefined, precision, scale);
export const bytea = () => new ColumnBuilder("bytea");
export const vector = (dimensions: number) => {
  const col = new ColumnBuilder("vector");
  (col._constraints as any).vectorDimensions = dimensions;
  return col;
};

// ── Table definition ───────────────────────────────────────
export interface IndexDefinition {
  name: string;
  columns: string[];
  unique?: boolean;
  method?: "btree" | "hash" | "gin" | "gist" | "brin" | "ivfflat" | "hnsw";
  where?: string;
  concurrently?: boolean;
}

export interface TableDefinition {
  name: string;
  schema: string;
  columns: Record<string, ColumnBuilder>;
  indexes: IndexDefinition[];
  enableRLS: boolean;
  tenantScoped: boolean;
  metadata: Record<string, any>;
}

export class Table {
  public readonly definition: TableDefinition;

  constructor(name: string, columns: Record<string, ColumnBuilder>, schema = "public") {
    this.definition = {
      name,
      schema,
      columns,
      indexes: [],
      enableRLS: false,
      tenantScoped: false,
      metadata: {},
    };
  }

  index(name: string, columns: string[], opts?: Partial<Omit<IndexDefinition, "name" | "columns">>): Table {
    this.definition.indexes.push({ name, columns, ...opts });
    return this;
  }

  withRLS(): Table {
    this.definition.enableRLS = true;
    return this;
  }

  withTenantScope(): Table {
    this.definition.tenantScoped = true;
    this.definition.enableRLS = true;
    return this;
  }

  meta(key: string, value: any): Table {
    this.definition.metadata[key] = value;
    return this;
  }
}

/**
 * Primary DSL entry point — define a Grey DB table.
 *
 * @example
 * const User = greyTable("users", {
 *   id: uuid().primaryKey(),
 *   email: text().unique().indexed(),
 *   createdAt: timestamptz().defaultNow(),
 * });
 */
export function greyTable(name: string, columns: Record<string, ColumnBuilder>, schema = "public"): Table {
  return new Table(name, columns, schema);
}
