import React from "react";
import { Page } from "../App";

const navSections = [
    {
        title: "Overview",
        items: [
            { id: "dashboard" as Page, label: "Dashboard", icon: "📊" },
        ],
    },
    {
        title: "Schema",
        items: [
            { id: "schema" as Page, label: "Schema Browser", icon: "🗂️" },
            { id: "migrations" as Page, label: "Migrations", icon: "🔄" },
            { id: "indexes" as Page, label: "Indexes", icon: "📇" },
        ],
    },
    {
        title: "Data",
        items: [
            { id: "query" as Page, label: "Query Console", icon: "⚡" },
            { id: "ai" as Page, label: "AI Assistant", icon: "🤖" },
        ],
    },
    {
        title: "Platform",
        items: [
            { id: "tenants" as Page, label: "Tenants", icon: "🏢" },
        ],
    },
];

interface SidebarProps {
    currentPage: Page;
    onNavigate: (page: Page) => void;
}

export function Sidebar({ currentPage, onNavigate }: SidebarProps) {
    return (
        <aside className="sidebar">
            <div className="sidebar-logo">
                <h1>
                    🗄️ <span>Grey</span> DB
                </h1>
                <p>Data Platform Console</p>
            </div>
            <nav className="sidebar-nav">
                {navSections.map((section) => (
                    <div key={section.title} className="nav-section">
                        <div className="nav-section-title">{section.title}</div>
                        {section.items.map((item) => (
                            <button
                                key={item.id}
                                className={`nav-item ${currentPage === item.id ? "active" : ""}`}
                                onClick={() => onNavigate(item.id)}
                            >
                                <span>{item.icon}</span>
                                {item.label}
                            </button>
                        ))}
                    </div>
                ))}
            </nav>
            <div style={{ padding: "12px 20px", borderTop: "1px solid var(--border)", fontSize: 12, color: "var(--text-dim)" }}>
                Grey DB v1.0.0
            </div>
        </aside>
    );
}
