// ─────────────────────────────────────────────────────────────
// Grey DB — App Root
// ─────────────────────────────────────────────────────────────

import React, { useState } from "react";
import { Sidebar } from "./components/Sidebar";
import { SchemaPage } from "./pages/SchemaPage";
import { MigrationsPage } from "./pages/MigrationsPage";
import { TenantsPage } from "./pages/TenantsPage";
import { QueryPage } from "./pages/QueryPage";
import { AIPage } from "./pages/AIPage";
import { DashboardPage } from "./pages/DashboardPage";
import { IndexesPage } from "./pages/IndexesPage";

export type Page = "dashboard" | "schema" | "migrations" | "tenants" | "query" | "ai" | "indexes";

export function App() {
    const [page, setPage] = useState<Page>("dashboard");

    return (
        <div className="app-layout">
            <Sidebar currentPage={page} onNavigate={setPage} />
            <main className="main-content">
                {page === "dashboard" && <DashboardPage />}
                {page === "schema" && <SchemaPage />}
                {page === "migrations" && <MigrationsPage />}
                {page === "tenants" && <TenantsPage />}
                {page === "query" && <QueryPage />}
                {page === "ai" && <AIPage />}
                {page === "indexes" && <IndexesPage />}
            </main>
        </div>
    );
}
