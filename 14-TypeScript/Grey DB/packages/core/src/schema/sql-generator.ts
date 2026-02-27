// ─────────────────────────────────────────────────────────────
// Grey DB — SQL Generator
// Converts DSL table definitions → DDL SQL
// ─────────────────────────────────────────────────────────────

import { ColumnBuilder, ColumnType, IndexDefinition, Table } from "./dsl";

function pgType(col: ColumnBuilder): string {
  const t = col._type;
  const c = col._constraints;

  switch (t) {
    case "uuid":
      return "UUID";
    case "text":
      return "TEXT";
    case "varchar":
      return `VARCHAR(${col._length ?? 255})`;
    case "integer":
      return "INTEGER";
    case "bigint":
      return "BIGINT";
    case "serial":
      return "SERIAL";
    case "bigserial":
      return "BIGSERIAL";
    case "boolean":
      return "BOOLEAN";
    case "timestamp":
      return "TIMESTAMP";
    case "timestamptz":
      return "TIMESTAMPTZ";
    case "date":
      return "DATE";
    case "jsonb":
      return "JSONB";
    case "json":
      return "JSON";
    case "float":
      return "REAL";
    case "double":
      return "DOUBLE PRECISION";
    case "numeric": {
      if (col._precision && col._scale) return `NUMERIC(${col._precision}, ${col._scale})`;
      if (col._precision) return `NUMERIC(${col._precision})`;
      return "NUMERIC";
    }
    case "bytea":
      return "BYTEA";
    case "vector":
      return `vector(${c.vectorDimensions ?? 1536})`;
    default:
      return (t as string).toUpperCase();
  }
}

function columnDDL(name: string, col: ColumnBuilder): string {
  const c = col._constraints;
  const parts: string[] = [`"${name}"`, pgType(col)];

  if (c.array) parts[1] = parts[1] + "[]";
  if (c.primaryKey) parts.push("PRIMARY KEY");
  if (c.unique && !c.primaryKey) parts.push("UNIQUE");
  if (c.nullable === false || c.primaryKey) {
    if (!col._type.startsWith("serial") && !col._type.startsWith("bigserial"))
      parts.push("NOT NULL");
  }
  if (c.defaultNow) parts.push("DEFAULT NOW()");
  else if (c.defaultUUID) parts.push("DEFAULT gen_random_uuid()");
  else if (c.default !== undefined && c.default !== null) {
    const v = typeof c.default === "string" ? `'${c.default}'` : String(c.default);
    parts.push(`DEFAULT ${v}`);
  }
  if (c.check) parts.push(`CHECK (${c.check})`);
  if (c.references) {
    let ref = `REFERENCES "${c.references.table}"("${c.references.column}")`;
    if (c.references.onDelete) ref += ` ON DELETE ${c.references.onDelete}`;
    if (c.references.onUpdate) ref += ` ON UPDATE ${c.references.onUpdate}`;
    parts.push(ref);
  }
  return parts.join(" ");
}

function indexDDL(table: Table, idx: IndexDefinition): string {
  const u = idx.unique ? "UNIQUE " : "";
  const method = idx.method ? ` USING ${idx.method}` : "";
  const conc = idx.concurrently ? " CONCURRENTLY" : "";
  const cols = idx.columns.map((c) => `"${c}"`).join(", ");
  const where = idx.where ? ` WHERE ${idx.where}` : "";
  return `CREATE ${u}INDEX${conc} "${idx.name}" ON "${table.definition.schema}"."${table.definition.name}"${method} (${cols})${where};`;
}

export function generateCreateTable(table: Table): string {
  const d = table.definition;
  const lines: string[] = [];

  // Extensions we might need
  const needsVector = Object.values(d.columns).some((c) => c._type === "vector");
  if (needsVector) lines.push(`CREATE EXTENSION IF NOT EXISTS vector;`, "");

  lines.push(`CREATE TABLE IF NOT EXISTS "${d.schema}"."${d.name}" (`);

  const colDefs = Object.entries(d.columns).map(([name, col]) => `  ${columnDDL(name, col)}`);
  lines.push(colDefs.join(",\n"));
  lines.push(");");
  lines.push("");

  // Indexes (from explicit .index() calls + any auto from .indexed() constraint)
  for (const idx of d.indexes) {
    lines.push(indexDDL(table, idx));
  }
  // Auto‑indexes from column‑level .indexed()
  for (const [name, col] of Object.entries(d.columns)) {
    if (col._constraints.indexed && !d.indexes.some((i) => i.columns.length === 1 && i.columns[0] === name)) {
      lines.push(`CREATE INDEX "idx_${d.name}_${name}" ON "${d.schema}"."${d.name}" ("${name}");`);
    }
  }

  // RLS
  if (d.enableRLS) {
    lines.push("");
    lines.push(`ALTER TABLE "${d.schema}"."${d.name}" ENABLE ROW LEVEL SECURITY;`);
    if (d.tenantScoped) {
      lines.push(
        `CREATE POLICY tenant_isolation ON "${d.schema}"."${d.name}"`,
        `  USING (tenant_id = current_setting('grey.tenant_id')::uuid);`
      );
    }
  }

  return lines.join("\n");
}

export function generateDropTable(table: Table): string {
  return `DROP TABLE IF EXISTS "${table.definition.schema}"."${table.definition.name}" CASCADE;`;
}
