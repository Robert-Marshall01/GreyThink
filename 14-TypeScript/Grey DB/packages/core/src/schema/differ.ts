// ─────────────────────────────────────────────────────────────
// Grey DB — Schema Differ
// Compares two schema snapshots and generates safe migrations
// ─────────────────────────────────────────────────────────────

import { ColumnBuilder, Table } from "./dsl";

export type DiffAction =
  | { type: "CREATE_TABLE"; table: Table }
  | { type: "DROP_TABLE"; tableName: string; schema: string }
  | { type: "ADD_COLUMN"; tableName: string; schema: string; columnName: string; column: ColumnBuilder }
  | { type: "DROP_COLUMN"; tableName: string; schema: string; columnName: string }
  | { type: "ALTER_COLUMN"; tableName: string; schema: string; columnName: string; from: ColumnBuilder; to: ColumnBuilder }
  | { type: "ADD_INDEX"; tableName: string; schema: string; indexName: string; columns: string[]; unique?: boolean }
  | { type: "DROP_INDEX"; indexName: string }
  | { type: "ENABLE_RLS"; tableName: string; schema: string }
  | { type: "DISABLE_RLS"; tableName: string; schema: string };

export interface DiffWarning {
  severity: "info" | "warning" | "danger";
  message: string;
  action: DiffAction;
}

export interface SchemaDiffResult {
  actions: DiffAction[];
  warnings: DiffWarning[];
  safe: boolean;
  requiresConfirmation: boolean;
  destructiveActions: string[];
}

function columnsEqual(a: ColumnBuilder, b: ColumnBuilder): boolean {
  return (
    a._type === b._type &&
    a._length === b._length &&
    a._precision === b._precision &&
    a._scale === b._scale &&
    JSON.stringify(a._constraints) === JSON.stringify(b._constraints)
  );
}

/**
 * Diff two sets of table definitions and generate migration actions.
 * Detects dangerous operations and flags warnings.
 */
export function diffSchemas(current: Table[], desired: Table[]): SchemaDiffResult {
  const actions: DiffAction[] = [];
  const warnings: DiffWarning[] = [];

  const currentMap = new Map(current.map((t) => [`${t.definition.schema}.${t.definition.name}`, t]));
  const desiredMap = new Map(desired.map((t) => [`${t.definition.schema}.${t.definition.name}`, t]));

  // New tables
  for (const [key, table] of desiredMap) {
    if (!currentMap.has(key)) {
      const action: DiffAction = { type: "CREATE_TABLE", table };
      actions.push(action);
      warnings.push({ severity: "info", message: `New table: ${key}`, action });
    }
  }

  // Dropped tables
  for (const [key, table] of currentMap) {
    if (!desiredMap.has(key)) {
      const action: DiffAction = { type: "DROP_TABLE", tableName: table.definition.name, schema: table.definition.schema };
      actions.push(action);
      warnings.push({
        severity: "danger",
        message: `⚠ Table "${key}" will be DROPPED. All data will be lost.`,
        action,
      });
    }
  }

  // Modified tables
  for (const [key, desired] of desiredMap) {
    const cur = currentMap.get(key);
    if (!cur) continue;

    const d = desired.definition;
    const c = cur.definition;

    // Column additions
    for (const [colName, colDef] of Object.entries(d.columns)) {
      if (!(colName in c.columns)) {
        const action: DiffAction = { type: "ADD_COLUMN", tableName: d.name, schema: d.schema, columnName: colName, column: colDef };
        actions.push(action);
        warnings.push({ severity: "info", message: `Add column "${colName}" to "${key}"`, action });
      }
    }

    // Column drops
    for (const colName of Object.keys(c.columns)) {
      if (!(colName in d.columns)) {
        const action: DiffAction = { type: "DROP_COLUMN", tableName: d.name, schema: d.schema, columnName: colName };
        actions.push(action);
        warnings.push({
          severity: "danger",
          message: `⚠ Column "${colName}" in "${key}" will be DROPPED. Data will be lost.`,
          action,
        });
      }
    }

    // Column changes
    for (const [colName, colDef] of Object.entries(d.columns)) {
      if (colName in c.columns && !columnsEqual(c.columns[colName], colDef)) {
        const action: DiffAction = {
          type: "ALTER_COLUMN",
          tableName: d.name,
          schema: d.schema,
          columnName: colName,
          from: c.columns[colName],
          to: colDef,
        };
        actions.push(action);

        const fromType = c.columns[colName]._type;
        const toType = colDef._type;
        if (fromType !== toType) {
          warnings.push({
            severity: "danger",
            message: `⚠ Column "${colName}" in "${key}" type change: ${fromType} → ${toType}. This may fail or lose data.`,
            action,
          });
        } else {
          warnings.push({ severity: "warning", message: `Column "${colName}" in "${key}" constraints changed.`, action });
        }
      }
    }

    // Index changes
    const curIdxNames = new Set(c.indexes.map((i) => i.name));
    const desIdxNames = new Set(d.indexes.map((i) => i.name));

    for (const idx of d.indexes) {
      if (!curIdxNames.has(idx.name)) {
        actions.push({ type: "ADD_INDEX", tableName: d.name, schema: d.schema, indexName: idx.name, columns: idx.columns, unique: idx.unique });
      }
    }
    for (const idx of c.indexes) {
      if (!desIdxNames.has(idx.name)) {
        actions.push({ type: "DROP_INDEX", indexName: idx.name });
      }
    }

    // RLS changes
    if (d.enableRLS && !c.enableRLS) {
      actions.push({ type: "ENABLE_RLS", tableName: d.name, schema: d.schema });
    }
    if (!d.enableRLS && c.enableRLS) {
      actions.push({ type: "DISABLE_RLS", tableName: d.name, schema: d.schema });
    }
  }

  const safe = !warnings.some((w) => w.severity === "danger");
  const destructiveActions = warnings
    .filter((w) => w.severity === "danger")
    .map((w) => w.message);
  const requiresConfirmation = destructiveActions.length > 0;
  return { actions, warnings, safe, requiresConfirmation, destructiveActions };
}
