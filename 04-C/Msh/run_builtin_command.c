/*
 * SPDX-License-Identifier: MIT
 * Copyright (C) 2025 Msh project contributors
 *
 * Part of the Msh project. See LICENSES.md for full license and attribution.
 */

/*
 * Implementation of run_builtin_command as a separate C file.
 * This mirrors the intended header implementation but keeps linkage
 * clear so we don't conflict with static helper functions in Msh.c.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <windows.h>
#include <fcntl.h>
#include <conio.h>
#include <io.h>
#include "jobs.h"
#include <time.h>
#include <sys/utime.h>
#include "regex_wrapper.h"

/* Lightweight FNV-1a 64-bit hash used for emulated password storage.
 * Produces a hex string representation to avoid external dependencies.
 */
static void fnv1a_hash_hex(const char *s, char *out_hex, size_t out_len) {
    unsigned long long h = 14695981039346656037ULL;
    while (*s) {
        h ^= (unsigned char)*s++;
        h *= 1099511628211ULL;
    }
    /* write as 16 hex chars (64-bit) */
    if (out_len >= 17) snprintf(out_hex, out_len, "%016llx", (unsigned long long)h);
}

/* File-scope globals and comparator for sort builtin */
static int g_sort_numeric = 0;
static int g_sort_fold = 0;
static int g_sort_reverse = 0;

static int sort_cmp_func(const void *a, const void *b) {
    const char *s1 = *(const char **)a;
    const char *s2 = *(const char **)b;
    int res = 0;
    if (g_sort_numeric) {
        char *end1, *end2;
        double v1 = strtod(s1, &end1);
        double v2 = strtod(s2, &end2);
        if (v1 < v2) res = -1;
        else if (v1 > v2) res = 1;
        else res = 0;
    } else if (g_sort_fold) {
        const unsigned char *p1 = (const unsigned char *)s1;
        const unsigned char *p2 = (const unsigned char *)s2;
        while (*p1 && *p2) {
            int c1 = tolower(*p1);
            int c2 = tolower(*p2);
            if (c1 < c2) { res = -1; break; }
            if (c1 > c2) { res = 1; break; }
            p1++; p2++;
        }
        if (res == 0) {
            if (*p1) res = 1;
            else if (*p2) res = -1;
            else res = 0;
        }
    } else {
        res = strcmp(s1, s2);
    }
    if (g_sort_reverse) res = -res;
    return res;
}

/* Definition of run_builtin_command starts here. The function body follows in this file. */
void run_builtin_command(const char *cmd, FILE *in, FILE *out) {
    if (!cmd) return;

    /* apt builtin: simulated package manager for testing inside Msh.
     * Repository stored at .msh_apt/repo.txt (one package name per line)
     * Installed packages recorded at .msh_apt/installed.txt (one package per line)
     * Supported subcommands: update, search <pattern>, install <pkg>, remove <pkg>, upgrade
     * This is intentionally simple and safe: it only modifies files under .msh_apt
     */
    if (strncmp(cmd, "apt", 3) == 0 && (cmd[3] == ' ' || cmd[3] == '\t' || cmd[3] == '\0')) {
        const char *p = cmd + 3; while (*p == ' ' || *p == '\t') p++;
        char sub[128] = {0}; int si = 0;
        while (*p && *p != ' ' && *p != '\t' && si < (int)sizeof(sub)-1) sub[si++] = *p++;
        sub[si] = '\0'; while (*p == ' ' || *p == '\t') p++;

        /* Ensure .msh_apt directory exists and repo file present */
        CreateDirectoryA(".msh_apt", NULL);
        const char *repo_path = ".msh_apt\\repo.txt";
        const char *installed_path = ".msh_apt\\installed.txt";

        if (sub[0] == '\0' || strcmp(sub, "help") == 0) {
            fprintf(out, "apt: supported: update, search <pattern>, install <pkg>, remove <pkg>, upgrade\n");
            fflush(out);
            return;
        }

        if (strcmp(sub, "update") == 0) {
            /* touch repo timestamp and create default repo if missing */
            FILE *rf = fopen(repo_path, "r");
            if (!rf) {
                rf = fopen(repo_path, "w");
                if (rf) {
                    fprintf(rf, "hello\ncoreutils\ngit\npython3\nlibssl\n");
                    fclose(rf);
                    fprintf(out, "Get:1 http://msh.local/ InRelease\nFetched 5 packages\n" );
                } else {
                    fprintf(out, "apt: failed to create repo file\n");
                }
            } else {
                fclose(rf);
                fprintf(out, "Hit: repository up-to-date\n");
            }
            fflush(out);
            return;
        }

        if (strcmp(sub, "search") == 0) {
            char pattern[256] = {0}; int pi = 0;
            if (*p == '"' || *p == '\'') { char q = *p++; while (*p && *p != q && pi < (int)sizeof(pattern)-1) pattern[pi++] = *p++; if (*p == q) p++; }
            else { while (*p && *p != ' ' && *p != '\t' && pi < (int)sizeof(pattern)-1) pattern[pi++] = *p++; }
            pattern[pi] = '\0';
            if (!pattern[0]) { fprintf(out, "Usage: apt search <pattern>\n"); fflush(out); return; }
            FILE *rf = fopen(repo_path, "r");
            if (!rf) { fprintf(out, "No packages available (run 'apt update').\n"); fflush(out); return; }
            char linebuf[512]; int found = 0;
            while (fgets(linebuf, sizeof(linebuf), rf)) {
                size_t l = strlen(linebuf); if (l && (linebuf[l-1] == '\n' || linebuf[l-1] == '\r')) linebuf[--l] = '\0';
                if (strstr(linebuf, pattern)) { fprintf(out, "%s\n", linebuf); found = 1; }
            }
            if (!found) fprintf(out, "No packages found matching '%s'\n", pattern);
            fclose(rf); fflush(out); return;
        }

        if (strcmp(sub, "install") == 0) {
            char pkg[256] = {0}; int pi = 0;
            if (*p == '"' || *p == '\'') { char q = *p++; while (*p && *p != q && pi < (int)sizeof(pkg)-1) pkg[pi++] = *p++; if (*p == q) p++; }
            else { while (*p && *p != ' ' && *p != '\t' && pi < (int)sizeof(pkg)-1) pkg[pi++] = *p++; }
            pkg[pi] = '\0'; if (!pkg[0]) { fprintf(out, "Usage: apt install <pkg>\n"); fflush(out); return; }
            /* Check repo for package */
            FILE *rf = fopen(repo_path, "r"); int available = 0;
            if (rf) {
                char lb[512]; while (fgets(lb, sizeof(lb), rf)) { size_t l = strlen(lb); if (l && (lb[l-1]=='\n' || lb[l-1]=='\r')) lb[--l]=0; if (strcmp(lb, pkg) == 0) { available = 1; break; } }
                fclose(rf);
            }
            if (!available) { fprintf(out, "apt: Package '%s' not found (run 'apt update' to refresh).\n", pkg); fflush(out); return; }
            /* add to installed list if not present */
            FILE *inf = fopen(installed_path, "r"); int already = 0; if (inf) { char lb[512]; while (fgets(lb, sizeof(lb), inf)) { size_t l = strlen(lb); if (l && (lb[l-1]=='\n' || lb[l-1]=='\r')) lb[--l]=0; if (strcmp(lb, pkg) == 0) { already = 1; break; } } fclose(inf); }
            if (already) { fprintf(out, "'%s' is already the newest version.\n", pkg); fflush(out); return; }
            FILE *wf = fopen(installed_path, "a"); if (!wf) { fprintf(out, "apt: failed to update installed database\n"); fflush(out); return; }
            fprintf(wf, "%s\n", pkg); fclose(wf);
            fprintf(out, "Selecting previously unselected package %s.\n", pkg);
            fprintf(out, "(Reading database ... )\n" );
            fprintf(out, "Preparing to unpack ...\n" );
            fprintf(out, "Unpacking %s (1.0) ...\n", pkg);
            fprintf(out, "Setting up %s (1.0) ...\n", pkg);
            fflush(out);
            return;
        }

        if (strcmp(sub, "remove") == 0 || strcmp(sub, "purge") == 0) {
            char pkg[256] = {0}; int pi = 0;
            if (*p == '"' || *p == '\'') { char q = *p++; while (*p && *p != q && pi < (int)sizeof(pkg)-1) pkg[pi++] = *p++; if (*p == q) p++; }
            else { while (*p && *p != ' ' && *p != '\t' && pi < (int)sizeof(pkg)-1) pkg[pi++] = *p++; }
            pkg[pi] = '\0'; if (!pkg[0]) { fprintf(out, "Usage: apt remove <pkg>\n"); fflush(out); return; }
            /* Read installed and write back without the package */
            FILE *inf = fopen(installed_path, "r"); if (!inf) { fprintf(out, "Package '%s' is not installed.\n", pkg); fflush(out); return; }
            char tmp[MAX_PATH]; if (!GetTempFileNameA(".", "mshapt", 0, tmp)) { fclose(inf); fprintf(out, "apt: temp file creation failed\n"); fflush(out); return; }
            FILE *tf = fopen(tmp, "w"); char lb[512]; int found = 0;
            while (fgets(lb, sizeof(lb), inf)) { size_t l = strlen(lb); if (l && (lb[l-1]=='\n' || lb[l-1]=='\r')) lb[--l]=0; if (strcmp(lb, pkg) == 0) { found = 1; continue; } fprintf(tf, "%s\n", lb); }
            fclose(inf); fclose(tf);
            if (!found) { remove(tmp); fprintf(out, "Package '%s' is not installed.\n", pkg); fflush(out); return; }
            remove(installed_path); rename(tmp, installed_path);
            fprintf(out, "Removing %s ...\n", pkg); fflush(out); return;
        }

        if (strcmp(sub, "upgrade") == 0) {
            /* Simulate upgrade: touch installed packages with a message */
            FILE *inf = fopen(installed_path, "r"); if (!inf) { fprintf(out, "apt: no packages installed\n"); fflush(out); return; }
            char lb[512]; while (fgets(lb, sizeof(lb), inf)) { size_t l = strlen(lb); if (l && (lb[l-1]=='\n' || lb[l-1]=='\r')) lb[--l]=0; if (lb[0]) fprintf(out, "Upgrading %s to newest version...\n", lb); }
            fclose(inf); fprintf(out, "All packages are up to date.\n"); fflush(out); return;
        }

        fprintf(out, "apt: unknown subcommand '%s'\n", sub); fflush(out); return;
    }

    /* sort builtin: supports basic options: -r (reverse), -n (numeric), -u (unique), -f (fold case)
     * Usage: sort [options] [file...]
     * If no files are given, reads from provided stream or stdin.
     */
    
    if (strncmp(cmd, "sort", 4) == 0 && (cmd[4] == ' ' || cmd[4] == '\t' || cmd[4] == '\0')) {
        const char *p = cmd + 4;
        int flag_r = 0, flag_n = 0, flag_u = 0, flag_f = 0;
        char filenames[32][512]; int fn = 0;

        while (*p) {
            while (*p == ' ' || *p == '\t') p++;
            if (!*p) break;
            if (*p == '-') {
                p++;
                while (*p && *p != ' ' && *p != '\t') {
                    if (*p == 'r') flag_r = 1;
                    else if (*p == 'n') flag_n = 1;
                    else if (*p == 'u') flag_u = 1;
                    else if (*p == 'f') flag_f = 1;
                    p++;
                }
                continue;
            }
            char buf[512]; int bi = 0;
            if (*p == '"' || *p == '\'') { char q = *p++; while (*p && *p != q && bi < (int)sizeof(buf)-1) buf[bi++] = *p++; if (*p == q) p++; }
            else { while (*p && *p != ' ' && *p != '\t' && bi < (int)sizeof(buf)-1) buf[bi++] = *p++; }
            buf[bi] = '\0';
            if (bi > 0 && fn < (int)(sizeof(filenames)/sizeof(filenames[0]))) {
                strncpy(filenames[fn], buf, sizeof(filenames[fn]) - 1);
                filenames[fn][sizeof(filenames[fn]) - 1] = '\0';
                fn++;
            }
        }

        /* read lines into dynamic array */
        char **lines = NULL; size_t lines_cap = 0; size_t lines_cnt = 0;

        if (fn == 0) {
            FILE *usein = (in != NULL && in != stdin) ? in : stdin;
            char bufline[8192];
            while (fgets(bufline, sizeof(bufline), usein)) {
                size_t len = strlen(bufline);
                while (len > 0 && (bufline[len-1] == '\n' || bufline[len-1] == '\r')) bufline[--len] = '\0';
                if (lines_cnt + 1 >= lines_cap) {
                    size_t nc = lines_cap ? lines_cap * 2 : 1024;
                    char **tmp = (char**)realloc(lines, nc * sizeof(char*));
                    if (!tmp) { fprintf(stderr, "sort: out of memory\n"); break; }
                    lines = tmp; lines_cap = nc;
                }
                lines[lines_cnt++] = _strdup(bufline);
            }
        } else {
            for (int i = 0; i < fn; ++i) {
                FILE *f = fopen(filenames[i], "r");
                if (!f) { fprintf(stderr, "sort: %s: ", filenames[i]); perror(""); continue; }
                char bufline[8192];
                while (fgets(bufline, sizeof(bufline), f)) {
                    size_t len = strlen(bufline);
                    while (len > 0 && (bufline[len-1] == '\n' || bufline[len-1] == '\r')) bufline[--len] = '\0';
                    if (lines_cnt + 1 >= lines_cap) {
                        size_t nc = lines_cap ? lines_cap * 2 : 1024;
                        char **tmp = (char**)realloc(lines, nc * sizeof(char*));
                        if (!tmp) { fprintf(stderr, "sort: out of memory\n"); break; }
                        lines = tmp; lines_cap = nc;
                    }
                    lines[lines_cnt++] = _strdup(bufline);
                }
                fclose(f);
            }
        }

        if (lines_cnt == 0) { fflush(out); return; }

        g_sort_numeric = flag_n; g_sort_fold = flag_f; g_sort_reverse = flag_r;
        qsort(lines, lines_cnt, sizeof(char*), sort_cmp_func);

        if (flag_u) {
            fputs(lines[0], out); fputc('\n', out);
            for (size_t i = 1; i < lines_cnt; ++i) {
                int eq = 0;
                if (flag_n) {
                    char *e1, *e2; double v1 = strtod(lines[i-1], &e1); double v2 = strtod(lines[i], &e2); eq = (v1 == v2);
                } else if (flag_f) {
                    const char *a = lines[i-1]; const char *b = lines[i];
                    while (*a && *b) { if (tolower((unsigned char)*a) != tolower((unsigned char)*b)) break; a++; b++; }
                    eq = (*a==0 && *b==0);
                } else {
                    eq = (strcmp(lines[i-1], lines[i]) == 0);
                }
                if (!eq) { fputs(lines[i], out); fputc('\n', out); }
            }
        } else {
            for (size_t i = 0; i < lines_cnt; ++i) { fputs(lines[i], out); fputc('\n', out); }
        }

        for (size_t i = 0; i < lines_cnt; ++i) free(lines[i]); free(lines);
        fflush(out); return;
    }

        /* awk builtin: lightweight support for simple actions like '{print $1}' or '{print $0}'
         * Additionally supports a pattern form: /regex/ { print ... }
         * Usage: awk '<program>' [file...]
         * This is intentionally minimal: regex uses POSIX regcomp/regexec (extended).
         */
        if (strncmp(cmd, "awk", 3) == 0 && (cmd[3] == ' ' || cmd[3] == '\t' || cmd[3] == '\0')) {
            const char *rest = cmd + 3; while (*rest == ' ' || *rest == '\t') rest++;
            if (!*rest) { fprintf(stderr, "Usage: awk '<program>' [file...]\n"); return; }
            char program[512] = {0}; int pi = 0;
            if (*rest == '"' || *rest == '\'') { char q = *rest++; while (*rest && *rest != q && pi < (int)sizeof(program)-1) program[pi++] = *rest++; if (*rest == q) rest++; }
            else { while (*rest && *rest != ' ' && *rest != '\t' && pi < (int)sizeof(program)-1) program[pi++] = *rest++; }
            program[pi] = '\0'; while (*rest == ' ' || *rest == '\t') rest++;

            int want_field = -2; /* as before */
            int pattern_mode = 0;
            char pattern[512] = {0};

            /* If program begins with a regex like /.../, extract it */
            if (program[0] == '/') {
                pattern_mode = 1;
                char delim = program[0]; const char *sp = program + 1; int pj = 0;
                while (*sp && *sp != delim && pj < (int)sizeof(pattern)-1) pattern[pj++] = *sp++;
                pattern[pj] = '\0';
                /* leave program as-is for parsing the action (print) below */
            }

            /* Parse program for print token and optional $N */
            const char *pr = strstr(program, "print");
            if (pr) {
                const char *dollar = strchr(pr, '$');
                if (!dollar) want_field = -1; else { dollar++; if (*dollar == '0') want_field = -1; else if (isdigit((unsigned char)*dollar)) { int val = 0; while (isdigit((unsigned char)*dollar)) { val = val*10 + (*dollar - '0'); dollar++; } if (val >= 1) want_field = val; else want_field = -1; } else want_field = -1; }
            } else {
                /* if pattern-only (e.g. /pat/), default action is print whole line */
                if (pattern_mode) want_field = -1;
            }

            /* Prepare list of files (if any) */
            char filenames[16][1024]; int fn = 0;
            while (*rest) { while (*rest == ' ' || *rest == '\t') rest++; if (!*rest) break; char fname[1024]; int fi = 0; if (*rest == '"' || *rest == '\'') { char q = *rest++; while (*rest && *rest != q && fi < (int)sizeof(fname)-1) fname[fi++] = *rest++; if (*rest == q) rest++; } else { while (*rest && *rest != ' ' && *rest != '\t' && fi < (int)sizeof(fname)-1) fname[fi++] = *rest++; } fname[fi] = '\0'; if (fi > 0 && fn < (int)(sizeof(filenames)/sizeof(filenames[0]))) { strncpy(filenames[fn], fname, sizeof(filenames[fn])-1); filenames[fn][sizeof(filenames[fn])-1] = '\0'; fn++; } }

            /* Compile/check regex if needed (use C++ std::regex wrapper) */
            int have_re = 0;
            if (pattern_mode) {
                if (pattern[0] != '\0') {
                    if (!c_regex_valid(pattern)) {
                        fprintf(stderr, "awk: invalid regex: %s\n", pattern);
                        pattern_mode = 0;
                    } else have_re = 1;
                } else pattern_mode = 0;
            }

            /* Process input streams */
            char linebuf[8192]; FILE *usef = NULL;
            if (fn == 0) {
                usef = (in != NULL && in != stdin) ? in : stdin;
                while (fgets(linebuf, sizeof(linebuf), usef)) {
                    size_t L = strlen(linebuf); if (L && linebuf[L-1] == '\n') linebuf[--L] = '\0'; if (L && linebuf[L-1] == '\r') linebuf[--L] = '\0';
                    int matched = 1;
                    if (pattern_mode && have_re) {
                        int m = c_regex_match(pattern, linebuf);
                        if (m == 1) matched = 1; else if (m == 0) matched = 0; else { /* invalid */ matched = 0; pattern_mode = 0; }
                    }
                    if (!matched) continue;
                    if (want_field == -1) { fprintf(out, "%s\n", linebuf); continue; }
                    int fld = 0; char *copy = _strdup(linebuf); char *tok = strtok(copy, " \t\r\n"); char *sel = NULL;
                    while (tok) { fld++; if (fld == want_field) { sel = tok; break; } tok = strtok(NULL, " \t\r\n"); }
                    if (sel) fprintf(out, "%s\n", sel); else fprintf(out, "\n"); free(copy);
                }
            } else {
                for (int i = 0; i < fn; ++i) {
                    FILE *f = fopen(filenames[i], "r"); if (!f) { fprintf(stderr, "awk: %s: ", filenames[i]); perror(""); continue; }
                    while (fgets(linebuf, sizeof(linebuf), f)) {
                        size_t L = strlen(linebuf); if (L && linebuf[L-1] == '\n') linebuf[--L] = '\0'; if (L && linebuf[L-1] == '\r') linebuf[--L] = '\0';
                        int matched = 1;
                        if (pattern_mode && have_re) { int m = c_regex_match(pattern, linebuf); if (m == 1) matched = 1; else if (m == 0) matched = 0; else { matched = 0; pattern_mode = 0; } }
                        if (!matched) continue;
                        if (want_field == -1) { fprintf(out, "%s\n", linebuf); continue; }
                        int fld = 0; char *copy = _strdup(linebuf); char *tok = strtok(copy, " \t\r\n"); char *sel = NULL;
                        while (tok) { fld++; if (fld == want_field) { sel = tok; break; } tok = strtok(NULL, " \t\r\n"); }
                        if (sel) fprintf(out, "%s\n", sel); else fprintf(out, "\n"); free(copy);
                    }
                    fclose(f);
                }
            }
            /* no regfree needed; wrapper handles resources */
            fflush(out); return;
        }

    /* jobs builtin: accept 'jobs' with optional trailing spaces */
    if (strncmp(cmd, "jobs", 4) == 0 && (cmd[4] == '\0' || cmd[4] == ' ' || cmd[4] == '\t')) {
        msh_list_jobs(out);
        return;
    }

    /* msh builtin: execute a script file line-by-line
     * Usage: msh <script-file>
     * For each non-empty, non-comment line in the script, this will invoke
     * the current Msh executable with --run-cmd "<line>" so that the line
     * is processed by the shell's existing command dispatch (builtins included).
     */
    if (strncmp(cmd, "msh", 3) == 0 && (cmd[3] == ' ' || cmd[3] == '\t')) {
        const char *p = cmd + 3; while (*p == ' ' || *p == '\t') p++;
        if (!*p) { fprintf(out, "msh: Usage: msh <script-file>\n"); fflush(out); return; }
        char filename[1024]; int fi = 0;
        if (*p == '"' || *p == '\'') { char q = *p++; while (*p && *p != q && fi < (int)sizeof(filename)-1) filename[fi++] = *p++; if (*p == q) p++; }
        else { while (*p && *p != ' ' && *p != '\t' && fi < (int)sizeof(filename)-1) filename[fi++] = *p++; }
        filename[fi] = '\0';
        FILE *sf = fopen(filename, "r");
        if (!sf) { fprintf(out, "msh: failed to open script '%s'\n", filename); fflush(out); return; }
        char linebuf[2048];
        char exePath[MAX_PATH]; GetModuleFileNameA(NULL, exePath, MAX_PATH);
        while (fgets(linebuf, sizeof(linebuf), sf)) {
            // strip newline and CR
            size_t l = strlen(linebuf); while (l > 0 && (linebuf[l-1] == '\n' || linebuf[l-1] == '\r')) linebuf[--l] = '\0';
            // skip empty and comment lines (lines starting with '#')
            char *s = linebuf; while (*s == ' ' || *s == '\t') s++;
            if (*s == '\0' || *s == '#') continue;
            // Build parameter string: --run-cmd <line>
            char params[4096];
            // Quote the command to pass as a single parameter
            // Use double quotes and escape any embedded double quotes
            char escaped[4096]; int ei = 0;
            escaped[ei++] = '\"';
            for (size_t i = 0; i < strlen(s) && ei < (int)sizeof(escaped)-2; ++i) {
                if (s[i] == '\"') { if (ei < (int)sizeof(escaped)-3) { escaped[ei++] = '\\'; escaped[ei++] = '\"'; } }
                else escaped[ei++] = s[i];
            }
            escaped[ei++] = '\"'; escaped[ei] = '\0';
            snprintf(params, sizeof(params), "--run-cmd %s", escaped);
            // Prepare ShellExecute to run elevated if needed; use CreateProcess to run synchronously
            char cmdline[8192]; snprintf(cmdline, sizeof(cmdline), "\"%s\" %s", exePath, params);
            STARTUPINFOA si; PROCESS_INFORMATION pi; memset(&si, 0, sizeof(si)); si.cb = sizeof(si);
            BOOL ok = CreateProcessA(NULL, cmdline, NULL, NULL, FALSE, 0, NULL, NULL, &si, &pi);
            if (ok) {
                WaitForSingleObject(pi.hProcess, INFINITE);
                CloseHandle(pi.hProcess); CloseHandle(pi.hThread);
            } else {
                fprintf(out, "msh: failed to execute subcommand\n");
            }
        }
        fclose(sf);
        fflush(out);
        return;
    }

    /* bg builtin: bg <jobid> */
    if (strncmp(cmd, "bg", 2) == 0 && (cmd[2] == ' ' || cmd[2] == '\t')) {
        const char *p = cmd + 2; while (*p == ' ' || *p == '\t') p++;
        if (!*p) { fprintf(stderr, "bg: Usage: bg <jobid>\n"); return; }
        int jid = atoi(p);
        if (jid <= 0) { fprintf(stderr, "bg: invalid job id\n"); return; }
        if (msh_bg_job(jid, out) != 0) fprintf(stderr, "bg: job not found\n");
        return;
    }

    /* fg builtin: fg <jobid> */
    if (strncmp(cmd, "fg", 2) == 0 && (cmd[2] == ' ' || cmd[2] == '\t')) {
        const char *p = cmd + 2; while (*p == ' ' || *p == '\t') p++;
        if (!*p) { fprintf(stderr, "fg: Usage: fg <jobid>\n"); return; }
        int jid = atoi(p);
        if (jid <= 0) { fprintf(stderr, "fg: invalid job id\n"); return; }
        if (msh_fg_job(jid, out) != 0) fprintf(stderr, "fg: job not found\n");
        return;
    }
    /*
     *   chmod -r file   -> clear read-only
     *   chmod 644 file   -> numeric: if any write bit present -> clear read-only else set read-only
     * This is intentionally limited (Windows attributes don't have execute bits).
     */
    if (strncmp(cmd, "chmod", 5) == 0 && (cmd[5] == ' ' || cmd[5] == '\t')) {
        const char *p = cmd + 5;
        while (*p == ' ' || *p == '\t') p++;
        char modebuf[64] = {0};
        int mi = 0;
        // parse mode token (handles quoted)
        if (*p == '"' || *p == '\'') {
            char q = *p++;
            while (*p && *p != q && mi < (int)sizeof(modebuf)-1) modebuf[mi++] = *p++;
            if (*p == q) p++;
        } else {
            while (*p && *p != ' ' && *p != '\t' && mi < (int)sizeof(modebuf)-1) modebuf[mi++] = *p++;
        }
        modebuf[mi] = '\0';
        while (*p == ' ' || *p == '\t') p++;
        char filebuf[1024] = {0}; int fi = 0;
        if (*p == '"' || *p == '\'') {
            char q = *p++;
            while (*p && *p != q && fi < (int)sizeof(filebuf)-1) filebuf[fi++] = *p++;
            if (*p == q) p++;
        } else {
            while (*p && *p != ' ' && *p != '\t' && fi < (int)sizeof(filebuf)-1) filebuf[fi++] = *p++;
        }
        filebuf[fi] = '\0';
        if (filebuf[0] == '\0' || modebuf[0] == '\0') {
            fprintf(stderr, "chmod: Usage: chmod MODE FILE\n");
            fflush(out);
            return;
        }

        DWORD attr = GetFileAttributesA(filebuf);
        if (attr == INVALID_FILE_ATTRIBUTES) { perror(filebuf); return; }
        DWORD newattr = attr;

        // symbolic forms
        if ((modebuf[0] == '+' || modebuf[0] == '-') && modebuf[1] != '\0') {
            char op = modebuf[0];
            for (int i = 1; modebuf[i]; ++i) {
                char m = modebuf[i];
                if (m == 'h') {
                    if (op == '+') newattr |= FILE_ATTRIBUTE_HIDDEN;
                    else newattr &= ~FILE_ATTRIBUTE_HIDDEN;
                } else if (m == 'r') {
                    if (op == '+') newattr |= FILE_ATTRIBUTE_READONLY;
                    else newattr &= ~FILE_ATTRIBUTE_READONLY;
                } else if (m == 'x') {
                    // no equivalent on Windows filesystem; warn
                    fprintf(stderr, "chmod: warning: 'x' has no equivalent Windows attribute - ignored\n");
                } else {
                    // ignore unknown
                }
            }
        } else {
            // numeric handling: if any of the 3 octal digits has write bit set, clear read-only; otherwise set read-only
            int is_numeric = 1;
            for (int i = 0; modebuf[i]; ++i) if (modebuf[i] < '0' || modebuf[i] > '7') is_numeric = 0;
            if (is_numeric && strlen(modebuf) >= 1) {
                int any_write = 0;
                int len = (int)strlen(modebuf);
                // check last three digits (owner,group,other)
                for (int k = 0; k < 3; ++k) {
                    int idx = len - 1 - k; if (idx < 0) break;
                    int val = modebuf[idx] - '0';
                    if (val & 2) any_write = 1;
                }
                if (any_write) newattr &= ~FILE_ATTRIBUTE_READONLY;
                else newattr |= FILE_ATTRIBUTE_READONLY;
            } else {
                fprintf(stderr, "chmod: unsupported mode: %s\n", modebuf);
                return;
            }
        }

        if (!SetFileAttributesA(filebuf, newattr)) {
            perror("chmod");
            return;
        }
        fflush(out);
        return;
    }

    /* touch builtin: create file if missing or update its timestamp
     * Usage: touch FILE...
     */
    if (strncmp(cmd, "touch", 5) == 0 && (cmd[5] == ' ' || cmd[5] == '\t')) {
        const char *p = cmd + 5; while (*p == ' ' || *p == '\t') p++;
        if (!*p) { fprintf(stderr, "touch: missing file operand\n"); return; }
        while (*p) {
            while (*p == ' ' || *p == '\t') p++; if (!*p) break;
            char filename[1024]; int fi = 0;
            if (*p == '"' || *p == '\'') { char q = *p++; while (*p && *p != q && fi < (int)sizeof(filename)-1) filename[fi++] = *p++; if (*p == q) p++; }
            else { while (*p && *p != ' ' && *p != '\t' && fi < (int)sizeof(filename)-1) filename[fi++] = *p++; }
            filename[fi] = '\0';
            if (fi == 0) continue;
            /* If file exists, update timestamp; else create empty file */
            struct _utimbuf times;
            if (access(filename, 0) == 0) {
                /* update to current time */
                times.actime = time(NULL);
                times.modtime = times.actime;
                _utime(filename, &times);
            } else {
                FILE *f = fopen(filename, "a"); if (f) fclose(f); else { fprintf(stderr, "touch: %s: ", filename); perror(""); }
            }
        }
        fflush(out); return;
    }

    /* ifconfig builtin: on Windows use ipconfig /all; prefer ipconfig but fall back to PowerShell if needed */
    if (strncmp(cmd, "ifconfig", 8) == 0 && (cmd[8] == '\0' || cmd[8] == ' ' || cmd[8] == '\t')) {
        const char *pcmd = "ipconfig /all";
        FILE *fp = _popen(pcmd, "r");
        int used_powershell = 0;
        if (!fp) {
            /* Try a PowerShell fallback that lists interfaces */
            pcmd = "powershell -Command \"Get-NetIPConfiguration | Format-List\"";
            fp = _popen(pcmd, "r");
            used_powershell = 1;
        }
        if (fp) {
            char buf[1024];
            while (fgets(buf, sizeof(buf), fp)) {
                size_t len = strlen(buf);
                if (len > 0 && buf[len-1] == '\n') buf[--len] = '\0';
                if (len > 0 && buf[len-1] == '\r') buf[--len] = '\0';
                // write original line
                fprintf(out, "%s\n", buf);

                // parse common IPv4/IPv6 tokens so we can emit 'inet'/'inet6' like Linux
                // ipconfig typically contains lines like "   IPv4 Address. . . . . . . . . . . : 192.168.1.5(Preferred)"
                // PowerShell fallback may contain "IPv4Address : {IPAddress}" or similar.
                char *p = NULL;
                if ((p = strstr(buf, "IPv4")) != NULL) {
                    // find ':' after this token
                    char *col = strchr(p, ':');
                    if (col) {
                        col++;
                        while (*col == ' ' || *col == '\t') col++;
                        // extract address up to space or '(' or '\0'
                        char addr[256] = {0}; int ai = 0;
                        while (*col && *col != ' ' && *col != '(' && *col != ',' && ai < (int)sizeof(addr)-1) addr[ai++] = *col++;
                        addr[ai] = '\0';
                        if (ai > 0) fprintf(out, "inet %s\n", addr);
                    }
                } else if ((p = strstr(buf, "IPv6")) != NULL || (p = strstr(buf, "IPv6 Address")) != NULL) {
                    char *col = strchr(buf, ':');
                    if (col) {
                        col++;
                        while (*col == ' ' || *col == '\t') col++;
                        char addr[256] = {0}; int ai = 0;
                        while (*col && *col != ' ' && *col != '(' && *col != ',' && ai < (int)sizeof(addr)-1) addr[ai++] = *col++;
                        addr[ai] = '\0';
                        if (ai > 0) fprintf(out, "inet6 %s\n", addr);
                    }
                } else if (used_powershell) {
                    // PowerShell formatted lines might be like "IPv4Address : 192.168.1.5"
                    char key[64]; int ki = 0; const char *q = buf;
                    while (*q && *q != ':' && ki < (int)sizeof(key)-1) { key[ki++] = *q++; }
                    key[ki] = '\0';
                    if (ki > 0 && (strstr(key, "IPv4") || strstr(key, "IPv6"))) {
                        if (*q == ':') q++;
                        while (*q == ' ' || *q == '\t') q++;
                        char addr[256]; int ai = 0; while (*q && *q != ' ' && *q != '\r' && *q != '\n' && ai < (int)sizeof(addr)-1) addr[ai++] = *q++; addr[ai]=0;
                        if (addr[0]) fprintf(out, "%s %s\n", strstr(key, "IPv6") ? "inet6" : "inet", addr);
                    }
                }
            }
            fflush(out);
            _pclose(fp);
        } else {
            fprintf(stderr, "ifconfig: failed to execute ipconfig/powershell\n");
        }
        return;
    }

    /* medit builtin: launch the GUI editor bundled with this project (Medit.exe)
     * Usage: medit FILE
     */
    if (strncmp(cmd, "medit", 5) == 0) {
        char *p = (char *)cmd + 5; /* allow either 'medit' or 'medit <file>' */
        while (*p == ' ' || *p == '\t') p++;
        if (!*p) { fprintf(stderr, "medit: No filename provided. Usage: medit <file>\n"); fflush(out); return; }
        char msh_path[MAX_PATH]; GetModuleFileNameA(NULL, msh_path, MAX_PATH);
        char *last_sep = strrchr(msh_path, '\\'); if (last_sep) *(last_sep+1) = '\0';
        char medit_path[MAX_PATH+64]; snprintf(medit_path, sizeof(medit_path), "%sMedit.exe", msh_path);
        FILE *mf = fopen(medit_path, "r"); if (!mf) { fprintf(stderr, "medit: Medit.exe not found at %s\n", medit_path); fflush(out); return; } fclose(mf);
        /* Use ShellExecuteA to open the editor (returns immediately) */
        HINSTANCE res = ShellExecuteA(NULL, "open", medit_path, p, NULL, SW_SHOWNORMAL);
        if ((INT_PTR)res <= 32) { fprintf(stderr, "medit: failed to launch Medit.exe (err %ld)\n", (long)(INT_PTR)res); }
        fflush(out); return;
    }

    /* sed builtin: lightweight support for simple substitution scripts
     * Usage examples supported:
     *   sed 's/old/new/g' file...
     *   sed -n 's/old/new/g' file...   (only print changed lines)
     *   sed -i 's/old/new/g' file...   (in-place edit)
     *   sed -e 's/old/new/g' ...       (multiple -e supported)
     * If no files provided, reads from provided stream or stdin.
     * This implementation does not support full regex; it performs
     * simple substring replacement. Delimiter other than '/' is supported.
     */
    if (strncmp(cmd, "sed", 3) == 0 && (cmd[3] == ' ' || cmd[3] == '\t')) {
        const char *p = cmd + 3; while (*p == ' ' || *p == '\t') p++;
        int flag_n = 0; int flag_i = 0;
        /* collect scripts and filenames */
        char scripts[16][1024]; int sc = 0;
        char filenames[32][1024]; int fn = 0;

        while (*p) {
            while (*p == ' ' || *p == '\t') p++;
            if (!*p) break;
            if (*p == '-' ) {
                p++;
                while (*p && *p != ' ' && *p != '\t') {
                    if (*p == 'n') flag_n = 1;
                    else if (*p == 'i') flag_i = 1;
                    p++;
                }
                continue;
            }
            /* token: either -e SCRIPT (handled above), or script token starting with s, or filename */
            char token[1024]; int ti = 0;
            if (*p == '"' || *p == '\'') {
                char q = *p++;
                while (*p && *p != q && ti < (int)sizeof(token)-1) token[ti++] = *p++;
                if (*p == q) p++;
            } else {
                while (*p && *p != ' ' && *p != '\t' && ti < (int)sizeof(token)-1) token[ti++] = *p++;
            }
            token[ti] = '\0';
            if (ti > 0) {
                if ((token[0] == 's' && token[1] != '\0') || (token[0] == '-' && token[1] == 'e')) {
                    /* treat as script */
                    if (sc < (int)(sizeof(scripts)/sizeof(scripts[0]))) {
                        /* if token starts with -e, the real script may be the rest after -e, try to extract */
                        if (token[0] == '-' && token[1] == 'e') {
                            /* empty -e or '-eSCRIPT' handled; here token may be just '-e' (not covered) */
                            /* ignore; real -e with separate arg not explicitly handled in this simple parser */
                        } else {
                            strncpy(scripts[sc], token, sizeof(scripts[sc])-1);
                            scripts[sc][sizeof(scripts[sc])-1] = '\0'; sc++;
                        }
                    }
                } else {
                    /* filename */
                    if (fn < (int)(sizeof(filenames)/sizeof(filenames[0]))) {
                        strncpy(filenames[fn], token, sizeof(filenames[fn])-1);
                        filenames[fn][sizeof(filenames[fn])-1] = '\0'; fn++;
                    }
                }
            }
        }

        if (sc == 0) { fprintf(stderr, "sed: missing script (e.g. 's/old/new/')\n"); return; }

        /* Helper: apply a single substitution script to a line (simple substring replacement)
         * script format expected: s<delim>old<delim>new<delim>[flags]
         */
        auto_apply_subst:
        ;
        for (int fileidx = 0; fileidx < (fn ? fn : 1); ++fileidx) {
            FILE *rf = NULL;
            const char *inname = NULL;
            char tmpname[MAX_PATH] = {0};
            FILE *wf = NULL;
            if (fn) {
                inname = filenames[fileidx];
                rf = fopen(inname, "r");
                if (!rf) { fprintf(stderr, "sed: %s: ", inname); perror(""); continue; }
                if (flag_i) {
                    if (!GetTempFileNameA(".", "msed", 0, tmpname)) { fprintf(stderr, "sed: failed to create temp file for in-place edit\n"); fclose(rf); continue; }
                    wf = fopen(tmpname, "w"); if (!wf) { fprintf(stderr, "sed: failed to open temp file %s for writing\n", tmpname); fclose(rf); continue; }
                }
            } else {
                rf = (in != NULL && in != stdin) ? in : stdin;
                wf = NULL;
            }

            char linebuf[8192];
            while (fgets(linebuf, sizeof(linebuf), rf)) {
                size_t L = strlen(linebuf); if (L && linebuf[L-1] == '\n') linebuf[--L] = '\0'; if (L && linebuf[L-1] == '\r') linebuf[--L] = '\0';
                char *curline = _strdup(linebuf);
                int any_changed = 0;
                for (int sidx = 0; sidx < sc; ++sidx) {
                    char *script = scripts[sidx];
                    if (!script || script[0] != 's') continue;
                    char delim = script[1]; if (delim == '\0') continue;
                    /* parse old */
                    const char *sp = script + 2;
                    char oldpat[1024] = {0}; int oi = 0;
                    while (*sp && *sp != delim && oi < (int)sizeof(oldpat)-1) oldpat[oi++] = *sp++;
                    if (*sp != delim) continue; sp++; oldpat[oi] = '\0';
                    /* parse new */
                    char newpat[1024] = {0}; int ni = 0;
                    while (*sp && *sp != delim && ni < (int)sizeof(newpat)-1) newpat[ni++] = *sp++;
                    if (*sp != delim) continue; sp++; newpat[ni] = '\0';
                    /* flags */
                    int flag_g = 0; int flag_p = 0;
                    while (*sp) { if (*sp == 'g') flag_g = 1; if (*sp == 'p') flag_p = 1; sp++; }

                    if (oldpat[0] == '\0') continue;
                    /* Try POSIX regex substitution first (extended regex). If it fails to compile,
                     * fall back to simple substring replacement.
                     */
                    int used_regex = 0;
                    if (c_regex_valid(oldpat)) used_regex = 1;

                    if (used_regex) {
                        int changed = 0;
                        char *replaced = c_regex_replace(oldpat, curline, newpat, flag_g, &changed);
                        if (replaced) {
                            if (changed) { free(curline); curline = replaced; any_changed = 1; }
                            else free(replaced);
                        } else {
                            /* replacement failed (invalid pattern or other); fall back to substring */
                            /* (fall through to substring implementation below) */
                        }
                    } else {
                        /* fallback: simple substring replacement (not regex). If g flag, replace all occurrences. */
                        char *outbuf = (char*)malloc(strlen(curline) + 1 + strlen(newpat) * 4 + 128);
                        outbuf[0] = '\0';
                        char *scan = curline; int changed = 0;
                        for (;;) {
                            char *pos = strstr(scan, oldpat);
                            if (!pos) {
                                strcat(outbuf, scan);
                                break;
                            }
                            /* copy up to pos */
                            size_t pref = pos - scan;
                            strncat(outbuf, scan, pref);
                            strcat(outbuf, newpat);
                            changed = 1;
                            scan = pos + strlen(oldpat);
                            if (!flag_g) { strcat(outbuf, scan); break; }
                        }
                        if (changed) {
                            free(curline); curline = _strdup(outbuf); any_changed = 1;
                        }
                        free(outbuf);
                    }
                }

                /* decide whether to print/write */
                if (flag_i) {
                    /* write to temp file (or stdout if wf is null) */
                    if (wf) { fprintf(wf, "%s\n", curline); }
                } else {
                    if (!flag_n || any_changed) fprintf(out, "%s\n", curline);
                }
                free(curline);
            }

            if (fn) { fclose(rf); if (wf) { fclose(wf); /* replace original */ remove(inname); if (rename(tmpname, inname) != 0) { fprintf(stderr, "sed: failed to move temp file over original %s\n", inname); } } }
        }
        fflush(out); return;
    }

    /* grep builtin: supports 'grep pattern' (reads from stdin) and 'grep pattern file' */
    if (strncmp(cmd, "grep", 4) == 0 && (cmd[4] == ' ' || cmd[4] == '\t')) {
        const char *rest = cmd + 4;
        while (*rest == ' ' || *rest == '\t') rest++;
        if (!*rest) { fprintf(stderr, "Usage: grep [pattern] [file]\n"); return; }
        char pattern[256] = {0}; int pi = 0;
        if (*rest == '"' || *rest == '\'') {
            char q = *rest++; while (*rest && *rest != q && pi < (int)sizeof(pattern)-1) pattern[pi++] = *rest++; if (*rest==q) rest++;
        } else { while (*rest && *rest != ' ' && *rest != '\t' && pi < (int)sizeof(pattern)-1) pattern[pi++] = *rest++; }
        pattern[pi] = '\0'; while (*rest == ' ' || *rest == '\t') rest++;
        FILE *src = NULL;
        if (*rest) {
            char filename[512] = {0}; int fi = 0;
            if (*rest == '"' || *rest == '\'') { char q=*rest++; while (*rest && *rest!=q && fi<(int)sizeof(filename)-1) filename[fi++]=*rest++; if(*rest==q) rest++; }
            else { while (*rest && *rest!=' ' && *rest!='\t' && fi<(int)sizeof(filename)-1) filename[fi++]=*rest++; }
            filename[fi] = '\0'; if (filename[0]) src = fopen(filename, "r");
        }
        if (!pattern[0]) { fprintf(stderr, "grep: Invalid pattern\n"); return; }
        char buf[1024];
        if (src) {
            while (fgets(buf, sizeof(buf), src)) {
                size_t len = strlen(buf);
                if (len > 0 && buf[len-1] == '\n') buf[--len] = '\0';
                if (len > 0 && buf[len-1] == '\r') buf[--len] = '\0';
                if (strstr(buf, pattern)) fprintf(out, "%s\n", buf);
            }
            fclose(src);
        } else {
            FILE *usein = (in != NULL && in != stdin) ? in : stdin;
            while (fgets(buf, sizeof(buf), usein)) {
                size_t len = strlen(buf);
                if (len > 0 && buf[len-1] == '\n') buf[--len] = '\0';
                if (len > 0 && buf[len-1] == '\r') buf[--len] = '\0';
                if (strstr(buf, pattern)) fprintf(out, "%s\n", buf);
            }
        }
        fflush(out); return;
    }

        /* useradd builtin: a limited implementation for convenience on Windows
         * Usage: useradd [-m] [-s SHELL] [-c "Full Name"] [-G group1,group2] USERNAME
         * On Windows this will map to `net user` where possible (creates local user).
         * This implementation is intentionally small and does not attempt to fully
         * emulate Linux useradd behaviour. For full control use WSL or native tools.
         */
    if (strncmp(cmd, "useradd", 7) == 0 && (cmd[7] == ' ' || cmd[7] == '\t' || cmd[7] == '\0')) {
            const char *p = cmd + 7;
            int flag_m = 0; char shell[256] = {0}; char gecos[256] = {0}; char groups[256] = {0};
            char username[256] = {0};
            while (*p == ' ' || *p == '\t') p++;
            while (*p && *p != '\n') {
                if (*p == '-') {
                    p++; if (*p == 'm') { flag_m = 1; p++; continue; }
                    if (*p == 's') { p++; while (*p == ' ' || *p == '\t') p++; int si = 0; if (*p=='\"'||*p=='\'') { char q=*p++; while (*p && *p!=q && si < (int)sizeof(shell)-1) shell[si++]=*p++; if (*p==q) p++; } else { while (*p && *p!=' ' && *p!='\t' && si < (int)sizeof(shell)-1) shell[si++]=*p++; } shell[si]=0; continue; }
                    if (*p == 'c') { p++; while (*p == ' ' || *p == '\t') p++; int ci = 0; if (*p=='\"'||*p=='\'') { char q=*p++; while (*p && *p!=q && ci < (int)sizeof(gecos)-1) gecos[ci++]=*p++; if (*p==q) p++; } else { while (*p && *p!=' ' && *p!='\t' && ci < (int)sizeof(gecos)-1) gecos[ci++]=*p++; } gecos[ci]=0; continue; }
                    if (*p == 'G') { p++; while (*p == ' ' || *p == '\t') p++; int gi=0; if (*p=='\"'||*p=='\'') { char q=*p++; while (*p && *p!=q && gi < (int)sizeof(groups)-1) groups[gi++]=*p++; if (*p==q) p++; } else { while (*p && *p!=' ' && *p!='\t' && gi < (int)sizeof(groups)-1) groups[gi++]=*p++; } groups[gi]=0; continue; }
                    // unknown flag: skip token
                    while (*p && *p != ' ' && *p != '\t') p++;
                    continue;
                }
                // parse username (last token)
                int ui = 0;
                if (*p == '"' || *p == '\'') { char q = *p++; while (*p && *p != q && ui < (int)sizeof(username)-1) username[ui++] = *p++; if (*p == q) p++; }
                else { while (*p && *p != ' ' && *p != '\t' && ui < (int)sizeof(username)-1) username[ui++] = *p++; }
                username[ui] = '\0';
                while (*p == ' ' || *p == '\t') p++;
            }
            if (username[0] == '\0') { fprintf(stderr, "useradd: missing username\n"); return; }

            // Basic validation: username must be alnum or limited punctuation
            for (int i = 0; username[i]; ++i) {
                char c = username[i]; if (!(isalnum((unsigned char)c) || c=='-' || c=='_' || c=='.')) { fprintf(stderr, "useradd: invalid username: %s\n", username); return; }
            }

    #ifdef _WIN32
            // Map to net user: net user username * /add
            char cmdline[1024];
            if (gecos[0]) {
                // net user doesn't accept comment easily; ignore gecos
            }
            // Build command. Use '*' to prompt for password interactively.
            snprintf(cmdline, sizeof(cmdline), "net user %s * /add", username);
            printf("[msh] creating Windows local user: %s\n", username);
            int rc = system(cmdline);
            if (rc != 0) {
                fprintf(stderr, "useradd: net user failed or insufficient privileges (rc=%d). Falling back to local emulation.\n", rc);
                /* Local fallback: record username in .msh_users in current directory */
                FILE *uf = fopen(".msh_users", "a");
                if (uf) {
                    fprintf(uf, "%s\n", username);
                    fclose(uf);
                    printf("useradd: emulated user '%s' recorded in .msh_users\n", username);
                } else {
                    fprintf(stderr, "useradd: failed to write local user database (.msh_users)\n");
                }
                /* If -m requested, try to create a home directory under C:\\Users\\USERNAME (best-effort) */
                if (flag_m) {
                    char homedir[512]; snprintf(homedir, sizeof(homedir), "C:\\Users\\%s", username);
                    if (CreateDirectoryA(homedir, NULL) || GetLastError() == ERROR_ALREADY_EXISTS) {
                        printf("useradd: created home directory: %s\n", homedir);
                    } else {
                        fprintf(stderr, "useradd: failed to create home directory %s (insufficient privileges?)\n", homedir);
                    }
                }
            } else {
                // Add to groups if requested using `net localgroup groupname username /add` for each comma-separated group
                if (groups[0]) {
                    char grpdup[256]; strncpy(grpdup, groups, sizeof(grpdup)-1); grpdup[sizeof(grpdup)-1]=0;
                    char *tok = strtok(grpdup, ",");
                    while (tok) {
                        // trim
                        char *s = tok; while (*s == ' ' || *s == '\t') s++; char *e = s + strlen(s)-1; while (e> s && (*e==' '||*e=='\t')) *e-- = '\0';
                        char gcmd[512]; snprintf(gcmd, sizeof(gcmd), "net localgroup \"%s\" %s /add", s, username);
                        system(gcmd);
                        tok = strtok(NULL, ",");
                    }
                }
            }
    #else
            // On non-Windows, try to invoke the system's useradd if it exists
            char cmdline[1024];
            if (shell[0]) {
                snprintf(cmdline, sizeof(cmdline), "useradd %s -s %s %s", flag_m?"-m":"", shell, username);
            } else {
                snprintf(cmdline, sizeof(cmdline), "useradd %s %s", flag_m?"-m":"", username);
            }
            if (groups[0]) {
                char opt[128]; snprintf(opt, sizeof(opt), " -G %s", groups);
                strncat(cmdline, opt, sizeof(cmdline)-strlen(cmdline)-1);
            }
            printf("[msh] invoking system useradd: %s\n", cmdline);
            int rc = system(cmdline);
            if (rc != 0) fprintf(stderr, "useradd: system useradd returned rc=%d\n", rc);
    #endif
            fflush(out);
            return;
        }

    /* cat builtin: supports reading from file(s) or from provided stream */
    if (strncmp(cmd, "cat", 3) == 0 && (cmd[3] == ' ' || cmd[3] == '\t' || cmd[3]=='\0')) {
        const char *p = cmd + 3; while (*p == ' ' || *p == '\t') p++; if (!*p) {
            FILE *usein = (in != NULL && in != stdin) ? in : stdin; char buf[4096]; while (fgets(buf, sizeof(buf), usein)) fputs(buf, out); fflush(out); return;
        }
        // one or more filenames (simple parse)
        while (*p) {
            while (*p == ' ' || *p == '\t') p++; if (!*p) break;
            char filename[1024]; int fi=0;
            if (*p == '"' || *p == '\'') { char q=*p++; while (*p && *p!=q && fi < (int)sizeof(filename)-1) filename[fi++]=*p++; if (*p==q) p++; }
            else { while (*p && *p!=' ' && *p!='\t' && fi < (int)sizeof(filename)-1) filename[fi++]=*p++; }
            filename[fi]='\0'; if (fi==0) break;
            FILE *f = fopen(filename, "r"); if (!f) { fprintf(stderr, "cat: %s: ", filename); perror(""); continue; }
            char buf[4096]; while (fgets(buf, sizeof(buf), f)) fputs(buf, out); fclose(f);
        }
        fflush(out); return;
    }

    /* head builtin: first 10 lines from file or stream */
    if (strncmp(cmd, "head", 4) == 0 && (cmd[4] == ' ' || cmd[4] == '\t' || cmd[4]=='\0')) {
        const char *p = cmd + 4; while (*p == ' ' || *p == '\t') p++; FILE *f = NULL; if (!*p) f = (in != NULL && in != stdin) ? in : stdin; else {
            char filename[1024]; int fi=0; if (*p=='"' || *p=='\'') { char q=*p++; while (*p&&*p!=q&&fi<(int)sizeof(filename)-1) filename[fi++]=*p++; if(*p==q) p++; } else { while (*p&&*p!=' '&&*p!='\t'&&fi<(int)sizeof(filename)-1) filename[fi++]=*p++; } filename[fi]='\0'; f = fopen(filename, "r"); if (!f) { fprintf(stderr, "head: %s: ", filename); perror(""); return; } }
        char buf[4096]; int count = 0; while (fgets(buf, sizeof(buf), f) && count < 10) { fputs(buf, out); count++; }
        if (f != NULL && f != stdin && f != (in != NULL ? in : stdin)) fclose(f);
        fflush(out); return;
    }

    /* tail builtin: last 10 lines (simple buffer) */
    if (strncmp(cmd, "tail", 4) == 0 && (cmd[4] == ' ' || cmd[4] == '\t' || cmd[4]=='\0')) {
        const char *p = cmd + 4; while (*p == ' ' || *p == '\t') p++; FILE *f = NULL; if (!*p) f = (in != NULL && in != stdin) ? in : stdin; else {
            char filename[1024]; int fi=0; if (*p=='"' || *p=='\'') { char q=*p++; while (*p&&*p!=q&&fi<(int)sizeof(filename)-1) filename[fi++]=*p++; if(*p==q) p++; } else { while (*p&&*p!=' '&&*p!='\t'&&fi<(int)sizeof(filename)-1) filename[fi++]=*p++; } filename[fi]='\0'; f = fopen(filename, "r"); if (!f) { fprintf(stderr, "tail: %s: ", filename); perror(""); return; } }
        char *lines[1024]; int cnt=0; char buf[4096]; while (fgets(buf, sizeof(buf), f)) { if (cnt < (int)(sizeof(lines)/sizeof(lines[0]))) lines[cnt++] = _strdup(buf); else { free(lines[0]); for (int j=1;j<(int)(sizeof(lines)/sizeof(lines[0]));++j) lines[j-1]=lines[j]; lines[(int)(sizeof(lines)/sizeof(lines[0]))-1] = _strdup(buf); } }
        int start = cnt > (int)(sizeof(lines)/sizeof(lines[0])) ? cnt - (int)(sizeof(lines)/sizeof(lines[0])) : 0; for (int i = start; i < cnt; ++i) { fputs(lines[i], out); free(lines[i]); }
        if (f != NULL && f != stdin && f != (in != NULL ? in : stdin)) fclose(f);
        fflush(out); return;
    }

    /* echo builtin */
    if (strncmp(cmd, "echo", 4) == 0 && (cmd[4] == ' ' || cmd[4] == '\t')) {
        const char *text = cmd + 4; while (*text == ' ') text++; fprintf(out, "%s\n", text); fflush(out); return;
    }

    /* pwd builtin */
    if (strcmp(cmd, "pwd") == 0) {
        char cwd[1024]; if (GetCurrentDirectoryA(sizeof(cwd), cwd)) fprintf(out, "%s\n", cwd); else perror("pwd"); fflush(out); return;
    }

    /* hostname builtin: prints the local computer name */
    if (strcmp(cmd, "hostname") == 0) {
        char namebuf[256];
        DWORD size = (DWORD)sizeof(namebuf);
        if (GetComputerNameA(namebuf, &size)) {
            fprintf(out, "%s\n", namebuf);
        } else {
            perror("hostname");
        }
        fflush(out);
        return;
    }

    /* userdel builtin: remove a user
     * Usage: userdel [-r] USERNAME
     * On Windows this maps to `net user USERNAME /delete` and if -r is passed
     * attempts to remove the user's profile directory under C:\Users\USERNAME
     * (best-effort, requires privileges).
     */
    if (strncmp(cmd, "userdel", 7) == 0 && (cmd[7] == ' ' || cmd[7] == '\t')) {
        const char *p = cmd + 7; while (*p == ' ' || *p == '\t') p++;
        int flag_r = 0; int flag_f = 0; char username[256] = {0};
        while (*p) {
            if (*p == '-') { p++; if (*p == 'r') { flag_r = 1; p++; } else if (*p == 'f') { flag_f = 1; p++; } else { while (*p && *p != ' ' && *p != '\t') p++; } continue; }
            int ui = 0; if (*p == '"' || *p == '\'') { char q=*p++; while (*p && *p != q && ui < (int)sizeof(username)-1) username[ui++] = *p++; if (*p==q) p++; } else { while (*p && *p!=' ' && *p!='\t' && ui < (int)sizeof(username)-1) username[ui++]=*p++; }
            username[ui]=0; while (*p == ' ' || *p == '\t') p++;
        }
        if (!username[0]) { fprintf(stderr, "userdel: missing username\n"); return; }
        for (int i=0; username[i]; ++i) { char c = username[i]; if (!(isalnum((unsigned char)c)||c=='-'||c=='_'||c=='.')) { fprintf(stderr, "userdel: invalid username: %s\n", username); return; } }

#ifdef _WIN32
    /* Use the PowerShell helper script which handles elevation/confirmation
     * and will fall back to removing the emulated .msh_users entry when
     * not running elevated. The script lives alongside the executable in
     * the project; we call it by relative path so it runs from the current
     * working directory where Msh is usually launched. */
    char cmdline[1024];
    char opts[64] = "";
    if (flag_r) strncat(opts, "-RemoveProfile ", sizeof(opts)-strlen(opts)-1);
    if (flag_f) strncat(opts, "-Force ", sizeof(opts)-strlen(opts)-1);
    else {
        /* If stdin is not a console (non-interactive), auto-add -Force so the
         * PowerShell helper won't require interactive confirmation and can
         * remove emulated users when called from scripts. */
        DWORD cm = 0; if (!GetConsoleMode(GetStdHandle(STD_INPUT_HANDLE), &cm)) {
            strncat(opts, "-Force ", sizeof(opts)-strlen(opts)-1);
        }
    }
    snprintf(cmdline, sizeof(cmdline), "powershell -NoProfile -ExecutionPolicy Bypass -File \"userdel_safe.ps1\" -UserName \"%s\" %s",
        username, opts);
    printf("[msh] invoking userdel helper: %s\n", cmdline);
    int rc = system(cmdline);
    if (rc != 0) fprintf(stderr, "userdel: userdel_safe.ps1 returned rc=%d\n", rc);
#else
        char cmdline[512]; if (flag_r) snprintf(cmdline, sizeof(cmdline), "userdel -r %s", username); else snprintf(cmdline, sizeof(cmdline), "userdel %s", username);
        printf("[msh] invoking system userdel: %s\n", cmdline);
        int rc = system(cmdline);
        if (rc != 0) fprintf(stderr, "userdel: system userdel returned rc=%d\n", rc);
#endif
        fflush(out); return;
    }

    /* adduser builtin: interactive wrapper around useradd on Unix/Windows
     * Usage: adduser [USERNAME]
     * If USERNAME is omitted, prompts interactively (very small prompt support).
     */
    if (strncmp(cmd, "adduser", 7) == 0 && (cmd[7] == ' ' || cmd[7] == '\t' || cmd[7] == '\0')) {
        const char *p = cmd + 7; while (*p == ' ' || *p == '\t') p++;
        char username[256] = {0};
        if (*p) {
            // parse provided username token
            int ui = 0; if (*p=='"' || *p=='\'') { char q=*p++; while (*p && *p!=q && ui < (int)sizeof(username)-1) username[ui++]=*p++; if(*p==q) p++; }
            else { while (*p && *p!=' ' && *p!='\t' && ui < (int)sizeof(username)-1) username[ui++]=*p++; }
            username[ui]=0;
            // call useradd with same username (simple)
            char call[512]; snprintf(call, sizeof(call), "useradd %s", username);
            run_builtin_command(call, in, out);
        } else {
            // prompt for username
            fprintf(out, "adduser: Enter new username: "); fflush(out);
            // read from stdin
            if (fgets(username, sizeof(username), stdin)) {
                // trim newline
                size_t l = strlen(username); if (l>0 && username[l-1]=='\n') username[--l]=0;
                if (l>0) {
                    char call[512]; snprintf(call, sizeof(call), "useradd %s", username);
                    run_builtin_command(call, in, out);
                } else fprintf(stderr, "adduser: no username entered\n");
            } else {
                fprintf(stderr, "adduser: failed to read username\n");
            }
        }
        return;
    }

    /* deluser builtin: interactive wrapper around userdel
     * Usage: deluser [--remove-home] USERNAME
     * If USERNAME is omitted, prompts for the username interactively.
     */
    if (strncmp(cmd, "deluser", 7) == 0 && (cmd[7] == ' ' || cmd[7] == '\t' || cmd[7] == '\0')) {
        const char *p = cmd + 7; while (*p == ' ' || *p == '\t') p++;
        int remove_home = 0; int flag_f = 0; char username[256] = {0};
        // parse tokens (support --remove-home and -f/--force)
        while (*p) {
            if (*p == '-') {
                if (strncmp(p, "--remove-home", 13) == 0) { remove_home = 1; p += 13; }
                else if (strncmp(p, "--force", 7) == 0) { flag_f = 1; p += 7; }
                else if (*(p+1) == 'f') { flag_f = 1; p += 2; }
                else { while (*p && *p != ' ' && *p != '\t') p++; }
                while (*p == ' ' || *p == '\t') p++;
                continue;
            }
            int ui = 0;
            if (*p=='"' || *p=='\'') { char q=*p++; while (*p && *p!=q && ui < (int)sizeof(username)-1) username[ui++]=*p++; if(*p==q) p++; }
            else { while (*p && *p!=' ' && *p!='\t' && ui < (int)sizeof(username)-1) username[ui++]=*p++; }
            username[ui]=0; while (*p == ' ' || *p == '\t') p++;
        }
        if (!username[0]) {
            fprintf(stdout, "deluser: Enter username to remove: "); fflush(stdout);
            if (fgets(username, sizeof(username), stdin)) {
                size_t l = strlen(username); if (l>0 && username[l-1]=='\n') username[--l]=0;
            }
        }
        if (!username[0]) { fprintf(stderr, "deluser: no username provided\n"); return; }
    /* Invoke the same userdel behavior directly to avoid recursion/context issues */
#ifdef _WIN32
    {
        char cmdline[1024];
        char opts[64] = "";
        if (remove_home) strncat(opts, "-RemoveProfile ", sizeof(opts)-strlen(opts)-1);
        /* Respect explicit -f/--force, otherwise auto-add -Force in non-interactive contexts */
        if (flag_f) {
            strncat(opts, "-Force ", sizeof(opts)-strlen(opts)-1);
        } else {
            DWORD cm = 0; if (!GetConsoleMode(GetStdHandle(STD_INPUT_HANDLE), &cm)) {
                strncat(opts, "-Force ", sizeof(opts)-strlen(opts)-1);
            }
        }
        snprintf(cmdline, sizeof(cmdline), "powershell -NoProfile -ExecutionPolicy Bypass -File \"userdel_safe.ps1\" -UserName \"%s\" %s", username, opts);
        printf("[msh] invoking userdel helper: %s\n", cmdline);
        int rc = system(cmdline);
        if (rc != 0) fprintf(stderr, "userdel: userdel_safe.ps1 returned rc=%d\n", rc);
    }
#else
    {
        char cmdline[512]; if (remove_home) snprintf(cmdline, sizeof(cmdline), "userdel -r %s", username); else snprintf(cmdline, sizeof(cmdline), "userdel %s", username);
        printf("[msh] invoking system userdel: %s\n", cmdline);
        int rc = system(cmdline);
        if (rc != 0) fprintf(stderr, "userdel: system userdel returned rc=%d\n", rc);
    }
#endif
        return;
    }

    /* groupadd builtin: create a group
     * Usage: groupadd [-g GID] [--system] GROUPNAME
     * On Windows maps to `net localgroup "GROUPNAME" /add` (GID and --system are ignored).
     */
    if (strncmp(cmd, "groupadd", 8) == 0 && (cmd[8] == ' ' || cmd[8] == '\t')) {
        const char *p = cmd + 8; while (*p == ' ' || *p == '\t') p++;
        char groupname[256] = {0}; char gid[64] = {0}; int is_system = 0;
        while (*p) {
            if (*p == '-') {
                if (strncmp(p, "--system", 8) == 0) { is_system = 1; p += 8; }
                else if (strncmp(p, "-g", 2) == 0) { p += 2; while (*p == ' ' || *p == '\t') p++; int gi = 0; if (*p=='"'||*p=='\'') { char q=*p++; while (*p && *p!=q && gi < (int)sizeof(gid)-1) gid[gi++]=*p++; if(*p==q) p++; } else { while (*p && *p!=' ' && *p!='\t' && gi < (int)sizeof(gid)-1) gid[gi++]=*p++; } gid[gi]=0; }
                else { while (*p && *p!=' ' && *p!='\t') p++; }
                while (*p == ' ' || *p == '\t') p++; continue;
            }
            int gi = 0; if (*p=='"'||*p=='\'') { char q=*p++; while (*p && *p!=q && gi < (int)sizeof(groupname)-1) groupname[gi++]=*p++; if(*p==q) p++; }
            else { while (*p && *p!=' ' && *p!='\t' && gi < (int)sizeof(groupname)-1) groupname[gi++]=*p++; }
            groupname[gi]=0; while (*p==' '||*p=='\t') p++;
        }
        if (!groupname[0]) { fprintf(stderr, "groupadd: missing group name\n"); return; }
        // basic validation
        for (int i=0; groupname[i]; ++i) { char c = groupname[i]; if (!(isalnum((unsigned char)c) || c=='_' || c=='-' || c=='.')) { fprintf(stderr, "groupadd: invalid group name: %s\n", groupname); return; } }

#ifdef _WIN32
        char cmdline[512]; snprintf(cmdline, sizeof(cmdline), "net localgroup \"%s\" /add", groupname);
        printf("[msh] creating Windows local group: %s\n", groupname);
        int rc = system(cmdline);
        if (rc != 0) {
            fprintf(stderr, "groupadd: failed to create group (net localgroup rc=%d). Falling back to local emulation.\n", rc);
            /* check for existing entry to avoid duplicates */
            int exists = 0;
            FILE *rf = fopen(".msh_groups", "r");
            if (rf) {
                char line[512];
                while (fgets(line, sizeof(line), rf)) {
                    size_t l = strlen(line); if (l>0 && (line[l-1]=='\n' || line[l-1]=='\r')) line[--l]=0;
                    if (strcmp(line, groupname) == 0) { exists = 1; break; }
                }
                fclose(rf);
            }
            if (exists) {
                printf("groupadd: group '%s' already present in .msh_groups (emulated)\n", groupname);
            } else {
                FILE *gf = fopen(".msh_groups", "a");
                if (gf) {
                    fprintf(gf, "%s\n", groupname);
                    fclose(gf);
                    printf("groupadd: emulated group '%s' recorded in .msh_groups\n", groupname);
                } else {
                    fprintf(stderr, "groupadd: failed to write local group database (.msh_groups)\n");
                }
            }
        }
#else
        char cmdline[512]; if (gid[0]) snprintf(cmdline, sizeof(cmdline), "groupadd -g %s %s", gid, groupname); else snprintf(cmdline, sizeof(cmdline), "groupadd %s", groupname);
        if (is_system) { /* many systems use -r for system groups */ }
        printf("[msh] invoking system groupadd: %s\n", cmdline);
        int rc = system(cmdline);
        if (rc != 0) fprintf(stderr, "groupadd: system groupadd returned rc=%d\n", rc);
#endif
        return;
    }

    /* groupdel builtin: remove a group
     * Usage: groupdel GROUPNAME
     */
    if (strncmp(cmd, "groupdel", 8) == 0 && (cmd[8] == ' ' || cmd[8] == '\t')) {
        const char *p = cmd + 8; while (*p == ' ' || *p == '\t') p++;
        char groupname[256] = {0}; int gi = 0;
        if (*p) {
            if (*p=='"' || *p=='\'') { char q=*p++; while (*p && *p!=q && gi < (int)sizeof(groupname)-1) groupname[gi++]=*p++; if(*p==q) p++; }
            else { while (*p && *p!=' ' && *p!='\t' && gi < (int)sizeof(groupname)-1) groupname[gi++]=*p++; }
            groupname[gi]=0;
        }
        if (!groupname[0]) { fprintf(stderr, "groupdel: missing group name\n"); return; }
        for (int i=0; groupname[i]; ++i) { char c = groupname[i]; if (!(isalnum((unsigned char)c)||c=='_'||c=='-'||c=='.')) { fprintf(stderr, "groupdel: invalid group name: %s\n", groupname); return; } }

#ifdef _WIN32
        char cmdline[512]; snprintf(cmdline, sizeof(cmdline), "net localgroup \"%s\" /delete", groupname);
        printf("[msh] deleting Windows local group: %s\n", groupname);
        int rc = system(cmdline);
        if (rc != 0) {
            fprintf(stderr, "groupdel: failed to delete group (net localgroup rc=%d). Falling back to local emulation.\n", rc);
            /* Remove from .msh_groups if present */
            FILE *gf = fopen(".msh_groups", "r");
            if (gf) {
                char tmpname[MAX_PATH]; char line[512]; int found = 0;
                if (GetTempPathA(MAX_PATH, tmpname) && GetTempFileNameA(tmpname, "mshg", 0, tmpname)) {
                    FILE *tf = fopen(tmpname, "w");
                    if (tf) {
                        while (fgets(line, sizeof(line), gf)) {
                            // trim newline
                            size_t l = strlen(line); if (l>0 && (line[l-1]=='\n' || line[l-1]=='\r')) line[--l]=0;
                            if (strcmp(line, groupname) == 0) { found = 1; continue; }
                            fprintf(tf, "%s\n", line);
                        }
                        fclose(tf);
                        fclose(gf);
                        if (found) {
                            if (remove(".msh_groups") == 0) {
                                /* move temp to original */
                                if (rename(tmpname, ".msh_groups") != 0) {
                                    /* try copy-by-stream as fallback */
                                    FILE *src = fopen(tmpname, "r"); FILE *dst = fopen(".msh_groups", "w"); if (src && dst) { while (fgets(line, sizeof(line), src)) fputs(line, dst); fclose(src); fclose(dst);} if (src) fclose(src); if (dst) fclose(dst);
                                }
                            } else {
                                /* if remove failed, try overwrite */
                                if (rename(tmpname, ".msh_groups") != 0) remove(tmpname);
                            }
                            printf("groupdel: emulated group '%s' removed from .msh_groups\n", groupname);
                        } else {
                            /* cleanup temp */
                            remove(tmpname);
                            printf("groupdel: emulated group '%s' not found in .msh_groups\n", groupname);
                        }
                    } else { fclose(gf); remove(tmpname); fprintf(stderr, "groupdel: failed to update .msh_groups (temp file create)\n"); }
                } else { fclose(gf); fprintf(stderr, "groupdel: failed to create temp file to update .msh_groups\n"); }
            } else {
                fprintf(stderr, "groupdel: .msh_groups not found for fallback deletion\n");
            }
        }
#else
        char cmdline[512]; snprintf(cmdline, sizeof(cmdline), "groupdel %s", groupname);
        printf("[msh] invoking system groupdel: %s\n", cmdline);
        int rc = system(cmdline);
        if (rc != 0) fprintf(stderr, "groupdel: system groupdel returned rc=%d\n", rc);
#endif
        return;
    }

    /* passwd builtin: change password for current user or another user
     * Usage: passwd [USERNAME]
     * On Windows attempts to invoke `net user USERNAME *` for interactive
     * password set when elevated. If that fails or when not possible the
     * command falls back to an emulated password file `.msh_passwd` in the
     * current directory where we store SHA256 hashes in the form:
     * username:hexsha256
     * This is intentionally limited and only for local testing of scripts.
     */
    if (strncmp(cmd, "passwd", 6) == 0 && (cmd[6] == ' ' || cmd[6] == '\t' || cmd[6] == '\0')) {
        const char *p = cmd + 6; while (*p == ' ' || *p == '\t') p++;
        char username[256] = {0};
        if (*p) {
            int ui = 0; if (*p == '"' || *p == '\'') { char q = *p++; while (*p && *p != q && ui < (int)sizeof(username)-1) username[ui++] = *p++; if (*p == q) p++; }
            else { while (*p && *p != ' ' && *p != '\t' && ui < (int)sizeof(username)-1) username[ui++] = *p++; }
            username[ui] = 0;
        } else {
            /* If username omitted, use current Windows user name */
#ifdef _WIN32
            DWORD size = sizeof(username);
            if (!GetUserNameA(username, &size)) username[0] = '\0';
#else
            /* Not expected in this codebase (Windows-focused), but support fallback */
            FILE *u = fopen("/etc/passwd", "r");
            if (u) {
                /* try to find current uid's name */
                char *envu = getenv("USER"); if (envu) strncpy(username, envu, sizeof(username)-1);
                fclose(u);
            } else {
                char *envu = getenv("USER"); if (envu) strncpy(username, envu, sizeof(username)-1);
            }
#endif
        }

        if (!username[0]) { fprintf(stderr, "passwd: cannot determine username\n"); return; }

        /* prompt for password twice without echo */
        char pass1[256] = {0}, pass2[256] = {0};
        fprintf(out, "Enter new password: "); fflush(out);
        /* simple no-echo input: read characters until newline */
        {
            int idx = 0; int c;
#ifdef _WIN32
            /* disable echo by changing console mode */
            HANDLE hin = GetStdHandle(STD_INPUT_HANDLE);
            DWORD mode = 0; GetConsoleMode(hin, &mode);
            DWORD newmode = mode & ~ENABLE_ECHO_INPUT;
            SetConsoleMode(hin, newmode);
            while ((c = getchar()) != '\n' && c != '\r' && c != EOF && idx < (int)sizeof(pass1)-1) { pass1[idx++] = (char)c; }
            /* consume possible CRLF pair */
            if (c == '\r') { int c2 = getchar(); if (c2 != '\n' && c2 != EOF) ungetc(c2, stdin); }
            SetConsoleMode(hin, mode);
#else
            while ((c = getchar()) != '\n' && c != EOF && idx < (int)sizeof(pass1)-1) { pass1[idx++] = (char)c; }
#endif
            pass1[idx] = '\0';
            fprintf(out, "\nRetype new password: "); fflush(out);
            idx = 0;
#ifdef _WIN32
            /* disable echo again */
            GetConsoleMode(hin, &mode);
            newmode = mode & ~ENABLE_ECHO_INPUT;
            SetConsoleMode(hin, newmode);
            while ((c = getchar()) != '\n' && c != '\r' && c != EOF && idx < (int)sizeof(pass2)-1) { pass2[idx++] = (char)c; }
            if (c == '\r') { int c2 = getchar(); if (c2 != '\n' && c2 != EOF) ungetc(c2, stdin); }
            SetConsoleMode(hin, mode);
#else
            while ((c = getchar()) != '\n' && c != EOF && idx < (int)sizeof(pass2)-1) { pass2[idx++] = (char)c; }
#endif
            pass2[idx] = '\0';
            fprintf(out, "\n"); fflush(out);
        }

        if (strcmp(pass1, pass2) != 0) { fprintf(stderr, "passwd: passwords do not match\n"); return; }

        /* On Windows try to call net user USERNAME * to set interactively when possible */
#ifdef _WIN32
        {
            char cmdline[1024]; snprintf(cmdline, sizeof(cmdline), "net user \"%s\" *", username);
            int rc = system(cmdline);
            if (rc == 0) { fflush(out); return; }
            /* else fall through to emulation */
        }
#endif

        /* Emulated password store: write/update .msh_passwd with username:sha256hex
         * Implementation: read existing file, update or append, write a temp file then rename.
         */
    char hex[32]; fnv1a_hash_hex(pass1, hex, sizeof(hex));

        FILE *rf = fopen(".msh_passwd", "r");
        char tmpname[MAX_PATH] = {0};
        int found = 0;
        if (rf) {
            /* create temp file */
            if (GetTempPathA(MAX_PATH, tmpname) && GetTempFileNameA(tmpname, "mshp", 0, tmpname)) {
                FILE *tf = fopen(tmpname, "w");
                if (tf) {
                    char line[1024];
                    while (fgets(line, sizeof(line), rf)) {
                        size_t l = strlen(line); if (l>0 && (line[l-1]=='\n' || line[l-1]=='\r')) line[--l]=0;
                        /* split at ':' */
                        char *col = strchr(line, ':');
                        if (col) { *col = '\0'; char *un = line; if (strcmp(un, username) == 0) { fprintf(tf, "%s:%s\n", username, hex); found = 1; } else { fprintf(tf, "%s:%s\n", un, col+1); } }
                        else { /* malformed line: copy through */ fprintf(tf, "%s\n", line); }
                    }
                    if (!found) fprintf(tf, "%s:%s\n", username, hex);
                    fclose(tf);
                    fclose(rf);
                    /* replace original file */
                    if (remove(".msh_passwd") == 0) { if (rename(tmpname, ".msh_passwd") != 0) {
                            /* fallback: copy */
                            FILE *src = fopen(tmpname, "r"), *dst = fopen(".msh_passwd", "w"); if (src && dst) { while (fgets(line, sizeof(line), src)) fputs(line, dst); fclose(src); fclose(dst); } if (src) fclose(src); if (dst) fclose(dst);
                        }
                    } else {
                        if (rename(tmpname, ".msh_passwd") != 0) remove(tmpname);
                    }
                    printf("passwd: updated emulated password for %s\n", username);
                    fflush(out);
                    return;
                } else { fclose(rf); fprintf(stderr, "passwd: failed to create temp file for .msh_passwd\n"); return; }
            } else { fclose(rf); fprintf(stderr, "passwd: failed to create temp file for .msh_passwd\n"); return; }
        } else {
            /* file doesn't exist: create and write */
            FILE *wf = fopen(".msh_passwd", "w");
            if (!wf) { fprintf(stderr, "passwd: failed to create .msh_passwd\n"); return; }
            fprintf(wf, "%s:%s\n", username, hex);
            fclose(wf);
            printf("passwd: created emulated password for %s\n", username);
            fflush(out);
            return;
        }
    }

    /* delgroup builtin: Debian-style wrapper that removes a group
     * Usage: delgroup [GROUPNAME]
     * If GROUPNAME is omitted, prompts interactively.
     */
    if (strncmp(cmd, "delgroup", 8) == 0 && (cmd[8] == ' ' || cmd[8] == '\t' || cmd[8] == '\0')) {
        const char *p = cmd + 8; while (*p == ' ' || *p == '\t') p++;
        char groupname[256] = {0};
        if (*p) {
            int gi = 0; if (*p=='"'||*p=='\'') { char q=*p++; while (*p && *p!=q && gi < (int)sizeof(groupname)-1) groupname[gi++]=*p++; if(*p==q) p++; } else { while (*p && *p!=' ' && *p!='\t' && gi < (int)sizeof(groupname)-1) groupname[gi++]=*p++; }
            groupname[gi]=0; char call[512]; snprintf(call, sizeof(call), "groupdel %s", groupname); run_builtin_command(call, in, out);
        } else {
            fprintf(stdout, "delgroup: Enter group name to remove: "); fflush(stdout);
            if (fgets(groupname, sizeof(groupname), stdin)) { size_t l = strlen(groupname); if (l>0 && groupname[l-1]=='\n') groupname[--l]=0; if (l>0) { char call[512]; snprintf(call, sizeof(call), "groupdel %s", groupname); run_builtin_command(call, in, out); } else fprintf(stderr, "delgroup: no group name entered\n"); }
        }
        return;
    }

    /* addgroup builtin: interactive wrapper around groupadd
     * Usage: addgroup [GROUPNAME]
     * If GROUPNAME omitted, prompts interactively.
     */
    if (strncmp(cmd, "addgroup", 8) == 0 && (cmd[8] == ' ' || cmd[8] == '\t' || cmd[8] == '\0')) {
        const char *p = cmd + 8; while (*p == ' ' || *p == '\t') p++;
        char groupname[256] = {0};
        if (*p) {
            int gi = 0; if (*p=='"'||*p=='\'') { char q=*p++; while (*p && *p!=q && gi < (int)sizeof(groupname)-1) groupname[gi++]=*p++; if(*p==q) p++; } else { while (*p && *p!=' ' && *p!='\t' && gi < (int)sizeof(groupname)-1) groupname[gi++]=*p++; }
            groupname[gi]=0; char call[512]; snprintf(call, sizeof(call), "groupadd %s", groupname); run_builtin_command(call, in, out);
        } else {
            fprintf(stdout, "addgroup: Enter new group name: "); fflush(stdout);
            if (fgets(groupname, sizeof(groupname), stdin)) { size_t l = strlen(groupname); if (l>0 && groupname[l-1]=='\n') groupname[--l]=0; if (l>0) { char call[512]; snprintf(call, sizeof(call), "groupadd %s", groupname); run_builtin_command(call, in, out); } else fprintf(stderr, "addgroup: no group name entered\n"); }
        }
        return;
    }

    /* chown builtin: change file owner
     * Usage: chown [-R] OWNER[:GROUP] FILE...
     * On Windows this maps to icacls FILE /setowner OWNER (recursively with /T)
     */
    if (strncmp(cmd, "chown", 5) == 0 && (cmd[5] == ' ' || cmd[5] == '\t')) {
        const char *p = cmd + 5; while (*p == ' ' || *p == '\t') p++;
        int flag_R = 0; char owner[256] = {0};
        while (*p == '-') {
            p++; if (*p == 'R') { flag_R = 1; p++; } else { while (*p && *p != ' ' && *p != '\t') p++; }
            while (*p == ' ' || *p == '\t') p++;
        }
        int oi = 0;
        if (*p == '"' || *p == '\'') { char q = *p++; while (*p && *p != q && oi < (int)sizeof(owner)-1) owner[oi++] = *p++; if (*p == q) p++; }
        else { while (*p && *p != ' ' && *p != '\t' && oi < (int)sizeof(owner)-1) owner[oi++] = *p++; }
        owner[oi] = '\0'; while (*p == ' ' || *p == '\t') p++;
        if (!owner[0]) { fprintf(stderr, "chown: missing owner\n"); return; }
        if (!*p) { fprintf(stderr, "chown: missing file operand\n"); return; }
        while (*p) {
            char file[1024]; int fi = 0;
            if (*p == '"' || *p == '\'') { char q = *p++; while (*p && *p != q && fi < (int)sizeof(file)-1) file[fi++] = *p++; if (*p == q) p++; }
            else { while (*p && *p != ' ' && *p != '\t' && fi < (int)sizeof(file)-1) file[fi++] = *p++; }
            file[fi] = '\0'; while (*p == ' ' || *p == '\t') p++;
            if (!file[0]) continue;
#ifdef _WIN32
            char cmdline[1400]; if (flag_R) snprintf(cmdline, sizeof(cmdline), "icacls \"%s\" /setowner %s /T", file, owner); else snprintf(cmdline, sizeof(cmdline), "icacls \"%s\" /setowner %s", file, owner);
            printf("[msh] setting owner: %s -> %s\n", file, owner);
            int rc = system(cmdline);
            if (rc != 0) fprintf(stderr, "chown: icacls returned rc=%d for %s\n", rc, file);
#else
            char cmdline[1400]; if (flag_R) snprintf(cmdline, sizeof(cmdline), "chown -R %s \"%s\"", owner, file); else snprintf(cmdline, sizeof(cmdline), "chown %s \"%s\"", owner, file);
            printf("[msh] invoking system chown: %s\n", cmdline);
            int rc = system(cmdline);
            if (rc != 0) fprintf(stderr, "chown: system chown returned rc=%d for %s\n", rc, file);
#endif
        }
        return;
    }

    /* chgrp builtin: change group of files
     * Usage: chgrp [-R] GROUP FILE...
     * On Windows this uses icacls to set group by granting the group modify rights (best-effort)
     */
    if (strncmp(cmd, "chgrp", 5) == 0 && (cmd[5] == ' ' || cmd[5] == '\t')) {
        const char *p = cmd + 5; while (*p == ' ' || *p == '\t') p++;
        int flag_R = 0; char group[256] = {0};
        while (*p == '-') {
            p++; if (*p == 'R') { flag_R = 1; p++; } else { while (*p && *p != ' ' && *p != '\t') p++; }
            while (*p == ' ' || *p == '\t') p++;
        }
        int gi = 0;
        if (*p == '"' || *p == '\'') { char q = *p++; while (*p && *p != q && gi < (int)sizeof(group)-1) group[gi++] = *p++; if (*p == q) p++; }
        else { while (*p && *p != ' ' && *p != '\t' && gi < (int)sizeof(group)-1) group[gi++] = *p++; }
        group[gi] = '\0'; while (*p == ' ' || *p == '\t') p++;
        if (!group[0]) { fprintf(stderr, "chgrp: missing group\n"); return; }
        if (!*p) { fprintf(stderr, "chgrp: missing file operand\n"); return; }
        while (*p) {
            char file[1024]; int fi = 0;
            if (*p == '"' || *p == '\'') { char q = *p++; while (*p && *p != q && fi < (int)sizeof(file)-1) file[fi++] = *p++; if (*p == q) p++; }
            else { while (*p && *p != ' ' && *p != '\t' && fi < (int)sizeof(file)-1) file[fi++] = *p++; }
            file[fi] = '\0'; while (*p == ' ' || *p == '\t') p++;
            if (!file[0]) continue;
#ifdef _WIN32
            /* Best-effort: use icacls to grant group Modify rights so group ownership-like behavior is approximated */
            char cmdline[1600];
            if (flag_R) snprintf(cmdline, sizeof(cmdline), "icacls \"%s\" /grant %s:(M) /T", file, group);
            else snprintf(cmdline, sizeof(cmdline), "icacls \"%s\" /grant %s:(M)", file, group);
            printf("[msh] setting group-like ACE: %s -> %s\n", file, group);
            int rc = system(cmdline);
            if (rc != 0) fprintf(stderr, "chgrp: icacls returned rc=%d for %s\n", rc, file);
#else
            char cmdline[1400]; if (flag_R) snprintf(cmdline, sizeof(cmdline), "chgrp -R %s \"%s\"", group, file); else snprintf(cmdline, sizeof(cmdline), "chgrp %s \"%s\"", group, file);
            printf("[msh] invoking system chgrp: %s\n", cmdline);
            int rc = system(cmdline);
            if (rc != 0) fprintf(stderr, "chgrp: system chgrp returned rc=%d for %s\n", rc, file);
#endif
        }
        return;
    }

    if (strncmp(cmd, "wc", 2) == 0) {
        const char *p = cmd + 2; while (*p==' '||*p=='\t') p++;
        int flags_l=0, flags_w=0, flags_c=0;
        char filenames[32][512]; int fn=0;

        while (*p) {
            while (*p==' '||*p=='\t') p++; if (!*p) break;
            if (*p=='-') { p++; while (*p && *p!=' ' && *p!='\t') { if (*p=='l') flags_l=1; else if (*p=='w') flags_w=1; else if (*p=='c') flags_c=1; p++; } continue; }
            char buf[512]; int bi=0;
            if (*p=='"' || *p=='\'') { char q=*p++; while (*p && *p!=q && bi < (int)sizeof(buf)-1) buf[bi++]=*p++; if (*p==q) p++; }
            else { while (*p && *p!=' ' && *p!='\t' && bi < (int)sizeof(buf)-1) buf[bi++]=*p++; }
            buf[bi]='\0'; if (bi>0 && fn < (int)(sizeof(filenames)/sizeof(filenames[0]))) { strncpy(filenames[fn], buf, sizeof(filenames[fn])-1); filenames[fn][sizeof(filenames[fn])-1]='\0'; fn++; }
        }

        if (!flags_l && !flags_w && !flags_c) flags_l = flags_w = flags_c = 1;

        /* Helper: count using buffered binary reads so byte counts match raw file bytes
         * Works correctly on Windows (avoids CRLF translation when stdin is in text mode)
         */
        auto_count_stream: ;
        if (fn == 0) {
            FILE *src = (in != NULL && in != stdin) ? in : stdin;
            /* If reading from real stdin on Windows, switch to binary mode while counting
             * so we count raw bytes rather than translated text-mode bytes.
             */
            int prev_mode = -1;
#ifdef _WIN32
            if (src == stdin) prev_mode = _setmode(_fileno(stdin), _O_BINARY);
#endif
            unsigned long long lines = 0, words = 0, bytes = 0;
            unsigned char buf[8192];
            size_t n;
            int inword = 0;
            while ((n = fread(buf, 1, sizeof(buf), src)) > 0) {
                bytes += n;
                for (size_t i = 0; i < n; ++i) {
                    unsigned char c = buf[i];
                    if (c == '\n') lines++;
                    if (c==' '||c=='\n'||c=='\t'||c=='\r'||c=='\v'||c=='\f') inword = 0;
                    else { if (!inword) { words++; inword = 1; } }
                }
            }
#ifdef _WIN32
            if (src == stdin && prev_mode != -1) _setmode(_fileno(stdin), prev_mode);
#endif
            if (flags_l) fprintf(out, "%10llu", lines); if (flags_w) fprintf(out, "%10llu", words); if (flags_c) fprintf(out, "%10llu", bytes); fprintf(out, "\n"); fflush(out); return;
        }

        unsigned long long total_lines = 0, total_words = 0, total_bytes = 0;
        for (int i = 0; i < fn; ++i) {
            FILE *f = fopen(filenames[i], "rb");
            if (!f) { fprintf(stderr, "wc: %s: ", filenames[i]); perror(""); continue; }
            unsigned long long lines = 0, words = 0, bytes = 0;
            unsigned char buf[8192]; size_t n; int inword = 0;
            while ((n = fread(buf, 1, sizeof(buf), f)) > 0) {
                bytes += n;
                for (size_t j = 0; j < n; ++j) {
                    unsigned char c = buf[j];
                    if (c == '\n') lines++;
                    if (c==' '||c=='\n'||c=='\t'||c=='\r'||c=='\v'||c=='\f') inword = 0;
                    else { if (!inword) { words++; inword = 1; } }
                }
            }
            fclose(f);
            total_lines += lines; total_words += words; total_bytes += bytes;
            if (flags_l) fprintf(out, "%10llu", lines);
            if (flags_w) fprintf(out, "%10llu", words);
            if (flags_c) fprintf(out, "%10llu", bytes);
            fprintf(out, " %s\n", filenames[i]);
        }
        if (fn > 1) {
            if (flags_l) fprintf(out, "%10llu", total_lines);
            if (flags_w) fprintf(out, "%10llu", total_words);
            if (flags_c) fprintf(out, "%10llu", total_bytes);
            fprintf(out, " total\n");
        }
        fflush(out);
        return;
    }


    /* shutdown/reboot builtins */
    if (strncmp(cmd, "shutdown", 8) == 0 && (cmd[8] == '\0' || cmd[8] == ' ' || cmd[8] == '\t')) {
        const char *p = cmd + 8;
        int reboot_flag = 0;
        int timeout = -1; /* -1 means default */
        while (*p == ' ' || *p == '\t') p++;
        // parse simple args: -r and -t N
        while (*p) {
            if (*p == '-') {
                p++;
                if (*p == 'r') { reboot_flag = 1; p++; }
                else if (*p == 't') {
                    p++;
                    while (*p == ' ' || *p == '\t') p++;
                    int v = 0; int seen = 0;
                    while (*p >= '0' && *p <= '9') { seen = 1; v = v*10 + (*p - '0'); p++; }
                    if (seen) timeout = v;
                } else {
                    // skip unknown flag
                    while (*p && *p != ' ' && *p != '\t') p++;
                }
            } else {
                // ignore positional args for now
                while (*p && *p != ' ' && *p != '\t') p++;
            }
            while (*p == ' ' || *p == '\t') p++;
        }

        // build command string for Windows 'shutdown'
        char exe[256];
        if (reboot_flag) snprintf(exe, sizeof(exe), "shutdown /r");
        else snprintf(exe, sizeof(exe), "shutdown /s");
        if (timeout >= 0) {
            // Windows shutdown expects seconds with /t
            char tbuf[64]; snprintf(tbuf, sizeof(tbuf), " /t %d", timeout);
            strncat(exe, tbuf, sizeof(exe) - strlen(exe) - 1);
        }

        FILE *fp = _popen(exe, "r");
        if (!fp) {
            fprintf(stderr, "%s: failed to execute shutdown command\n", "shutdown");
            return;
        }
        // read and forward any output
        char lbuf[512];
        while (fgets(lbuf, sizeof(lbuf), fp)) fputs(lbuf, out);
        _pclose(fp);
        fflush(out);
        return;
    }

    if (strncmp(cmd, "reboot", 6) == 0 && (cmd[6] == '\0' || cmd[6] == ' ' || cmd[6] == '\t')) {
        // alias to shutdown -r
        const char *p = cmd + 6; while (*p == ' ' || *p == '\t') p++;
        // support optional -t seconds
        int timeout = -1;
        while (*p) {
            if (*p == '-') {
                p++;
                if (*p == 't') {
                    p++; while (*p == ' ' || *p == '\t') p++;
                    int v=0, seen=0; while (*p>='0'&&*p<='9') { seen=1; v=v*10+(*p-'0'); p++; }
                    if (seen) timeout = v;
                } else { while (*p && *p!=' '&&*p!='\t') p++; }
            } else { while (*p && *p!=' '&&*p!='\t') p++; }
            while (*p == ' ' || *p == '\t') p++;
        }
        char exe[256]; snprintf(exe, sizeof(exe), "shutdown /r");
        if (timeout >= 0) { char tbuf[64]; snprintf(tbuf, sizeof(tbuf), " /t %d", timeout); strncat(exe, tbuf, sizeof(exe) - strlen(exe) - 1); }
        FILE *fp = _popen(exe, "r");
        if (!fp) { fprintf(stderr, "reboot: failed to execute shutdown command\n"); return; }
        char lbuf[512]; while (fgets(lbuf, sizeof(lbuf), fp)) fputs(lbuf, out);
        _pclose(fp); fflush(out); return;
    }

    /* more and less builtins: simple pager behavior
     * Usage:
     *   more [file...]
     *   less [file...]
     * If no files are given, read from provided stream or stdin.
     * If stdout is not a terminal, simply write all data through (no paging).
     * Implementation notes: when files (or stdin) are read we buffer lines into memory
     * so that 'less' can page backward. For non-seekable stdin we still buffer.
     */
    if ((strncmp(cmd, "more", 4) == 0 && (cmd[4] == ' ' || cmd[4] == '\t' || cmd[4] == '\0')) ||
        (strncmp(cmd, "less", 4) == 0 && (cmd[4] == ' ' || cmd[4] == '\t' || cmd[4] == '\0'))) {
        int is_less = (strncmp(cmd, "less", 4) == 0);
        const char *p = cmd + 4; while (*p == ' ' || *p == '\t') p++;
        char filenames[64][1024]; int fn = 0;

        while (*p) {
            while (*p == ' ' || *p == '\t') p++; if (!*p) break;
            char buf[1024]; int bi = 0;
            if (*p == '"' || *p == '\'') { char q = *p++; while (*p && *p != q && bi < (int)sizeof(buf)-1) buf[bi++] = *p++; if (*p == q) p++; }
            else { while (*p && *p != ' ' && *p != '\t' && bi < (int)sizeof(buf)-1) buf[bi++] = *p++; }
            buf[bi] = '\0'; if (bi > 0 && fn < (int)(sizeof(filenames)/sizeof(filenames[0]))) { strncpy(filenames[fn], buf, sizeof(filenames[fn]) - 1); filenames[fn][sizeof(filenames[fn]) - 1] = '\0'; fn++; }
        }

        /* If output is not a TTY, behave like cat: just write through */
        int out_is_tty = _isatty(_fileno(out));
        if (!out_is_tty) {
            /* simply cat files or stream */
            if (fn == 0) {
                FILE *usein = (in != NULL && in != stdin) ? in : stdin; char buf2[4096]; while (fgets(buf2, sizeof(buf2), usein)) fputs(buf2, out);
            } else {
                for (int i = 0; i < fn; ++i) {
                    FILE *f = fopen(filenames[i], "r"); if (!f) { fprintf(stderr, "%s: %s: ", is_less?"less":"more", filenames[i]); perror(""); continue; }
                    char buf2[4096]; while (fgets(buf2, sizeof(buf2), f)) fputs(buf2, out); fclose(f);
                }
            }
            fflush(out);
            return;
        }

        /* Buffer lines into memory so we can page and optionally go backwards */
        char **lines = NULL; size_t lines_cap = 0; size_t lines_cnt = 0;

        if (fn == 0) {
            FILE *usein = (in != NULL && in != stdin) ? in : stdin;
            char bufline[8192];
            while (fgets(bufline, sizeof(bufline), usein)) {
                size_t len = strlen(bufline);
                while (len > 0 && (bufline[len-1] == '\n' || bufline[len-1] == '\r')) bufline[--len] = '\0';
                if (lines_cnt + 1 >= lines_cap) {
                    size_t nc = lines_cap ? lines_cap * 2 : 1024;
                    char **tmp = (char**)realloc(lines, nc * sizeof(char*)); if (!tmp) { fprintf(stderr, "pager: out of memory\n"); break; }
                    lines = tmp; lines_cap = nc;
                }
                lines[lines_cnt++] = _strdup(bufline);
            }
        } else {
            for (int i = 0; i < fn; ++i) {
                FILE *f = fopen(filenames[i], "r"); if (!f) { fprintf(stderr, "%s: %s: ", is_less?"less":"more", filenames[i]); perror(""); continue; }
                char bufline[8192];
                while (fgets(bufline, sizeof(bufline), f)) {
                    size_t len = strlen(bufline);
                    while (len > 0 && (bufline[len-1] == '\n' || bufline[len-1] == '\r')) bufline[--len] = '\0';
                    if (lines_cnt + 1 >= lines_cap) {
                        size_t nc = lines_cap ? lines_cap * 2 : 1024;
                        char **tmp = (char**)realloc(lines, nc * sizeof(char*)); if (!tmp) { fprintf(stderr, "pager: out of memory\n"); break; }
                        lines = tmp; lines_cap = nc;
                    }
                    lines[lines_cnt++] = _strdup(bufline);
                }
                fclose(f);
            }
        }

        /* Nothing to show */
        if (lines_cnt == 0) { fflush(out); return; }

        /* determine console height */
        CONSOLE_SCREEN_BUFFER_INFO csbi; int rows = 24; if (GetConsoleScreenBufferInfo(GetStdHandle(STD_OUTPUT_HANDLE), &csbi)) { rows = csbi.srWindow.Bottom - csbi.srWindow.Top + 1; if (rows < 4) rows = 4; }
        int page_lines = rows - 1;

        size_t pos = 0;
        while (pos < lines_cnt) {
            size_t end = pos + page_lines; if (end > lines_cnt) end = lines_cnt;
            for (size_t i = pos; i < end; ++i) { fprintf(out, "%s\n", lines[i]); }
            fflush(out);

            if (end >= lines_cnt) break; /* finished */

            /* display prompt */
            fprintf(out, "--%s--", is_less?"Less":"More"); fflush(out);

            int c = _getch(); /* read single char */
            /* consume CRLF if necessary when user presses Enter (13) followed by 10 on some consoles - but _getch doesn't return CRLF pairs */
            fprintf(out, "\r\n"); fflush(out);

            if (c == 'q' || c == 'Q') break;
            else if (c == ' ' ) { pos = end; continue; } /* page */
            else if (c == '\r' || c == '\n' || c == 13) { pos = pos + 1; continue; } /* one line */
            else if (is_less && (c == 'b' || c == 'B')) { /* back a page */
                if (pos <= (size_t)page_lines) pos = 0; else pos = pos - page_lines; continue;
            } else if (is_less && (c == 'g')) { pos = 0; continue; }
            else if (is_less && (c == 'G')) { pos = lines_cnt > (size_t)page_lines ? lines_cnt - page_lines : 0; continue; }
            else {
                /* unknown key: treat as next page */
                pos = end; continue;
            }
        }

        for (size_t i = 0; i < lines_cnt; ++i) free(lines[i]); free(lines);
        fflush(out);
        return;
    }
}
