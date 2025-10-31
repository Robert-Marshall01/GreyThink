/*
 * SPDX-License-Identifier: MIT
 * See LICENSES.md and COPYING for license and attribution details.
 */

/*
 * A small standalone pager implementation for Windows.
 * Usage:
 *   less [file...]
 *   less            # reads from stdin
 *   less -h|--help  # prints this help and exits
 *
 * Behavior:
 * - If stdout is not a TTY, this program writes the file(s)/stdin to stdout (acts like cat).
 * - If stdout is a TTY, it paginates using the console height and accepts keys:
 *     Space: next page
 *     Enter: next line
 *     q/Q: quit
 *     b/B: back one page
 *     g: goto start
 *     G: goto end
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <windows.h>
#include <conio.h>
#include <io.h>

#ifndef MAX_PATH
#define MAX_PATH 260
#endif

static void print_usage(const char *prog) {
    fprintf(stderr, "Usage: %s [file ...]\n", prog);
    fprintf(stderr, "A small pager: space for next page, Enter for one line, q to quit.\n");
}

static int out_is_tty(void) {
    return _isatty(_fileno(stdout));
}

static void cat_files_or_stdin(int argc, char **argv) {
    if (argc <= 1) {
        char buf[4096]; while (fgets(buf, sizeof(buf), stdin)) fputs(buf, stdout);
        return;
    }
    for (int i = 1; i < argc; ++i) {
        const char *fname = argv[i];
        FILE *f = fopen(fname, "r");
        if (!f) { fprintf(stderr, "less: %s: ", fname); perror(""); continue; }
        char buf[4096]; while (fgets(buf, sizeof(buf), f)) fputs(buf, stdout);
        fclose(f);
    }
}

int main(int argc, char **argv) {
    if (argc > 1 && (strcmp(argv[1], "-h") == 0 || strcmp(argv[1], "--help") == 0)) {
        print_usage(argv[0]);
        return 0;
    }

    if (!out_is_tty()) {
        cat_files_or_stdin(argc, argv);
        return 0;
    }

    /* Buffer all lines into memory so we can page/backwards */
    char **lines = NULL; size_t cap = 0; size_t cnt = 0;

    if (argc <= 1) {
        char buf[8192]; while (fgets(buf, sizeof(buf), stdin)) {
            size_t len = strlen(buf);
            while (len > 0 && (buf[len-1] == '\n' || buf[len-1] == '\r')) buf[--len] = '\0';
            if (cnt + 1 >= cap) {
                size_t nc = cap ? cap * 2 : 1024;
                char **tmp = (char**)realloc(lines, nc * sizeof(char*)); if (!tmp) { fprintf(stderr, "less: out of memory\n"); break; }
                lines = tmp; cap = nc;
            }
            lines[cnt++] = _strdup(buf);
        }
    } else {
        for (int i = 1; i < argc; ++i) {
            FILE *f = fopen(argv[i], "r"); if (!f) { fprintf(stderr, "less: %s: ", argv[i]); perror(""); continue; }
            char buf[8192]; while (fgets(buf, sizeof(buf), f)) {
                size_t len = strlen(buf);
                while (len > 0 && (buf[len-1] == '\n' || buf[len-1] == '\r')) buf[--len] = '\0';
                if (cnt + 1 >= cap) {
                    size_t nc = cap ? cap * 2 : 1024;
                    char **tmp = (char**)realloc(lines, nc * sizeof(char*)); if (!tmp) { fprintf(stderr, "less: out of memory\n"); break; }
                    lines = tmp; cap = nc;
                }
                lines[cnt++] = _strdup(buf);
            }
            fclose(f);
        }
    }

    if (cnt == 0) { free(lines); return 0; }

    /* Determine console height */
    CONSOLE_SCREEN_BUFFER_INFO csbi; int rows = 24;
    if (GetConsoleScreenBufferInfo(GetStdHandle(STD_OUTPUT_HANDLE), &csbi)) {
        rows = csbi.srWindow.Bottom - csbi.srWindow.Top + 1;
        if (rows < 4) rows = 4;
    }
    int pagelines = rows - 1;

    size_t pos = 0;
    for (;;) {
        size_t end = pos + pagelines; if (end > cnt) end = cnt;
        for (size_t i = pos; i < end; ++i) { printf("%s\n", lines[i]); }
        fflush(stdout);
        if (end >= cnt) break;

        printf("--Less--"); fflush(stdout);
        int c = _getch();
        printf("\r\n"); fflush(stdout);
        if (c == 'q' || c == 'Q') break;
        else if (c == ' ') { pos = end; continue; }
        else if (c == '\r' || c == '\n') { pos = pos + 1; if (pos > cnt) pos = cnt; continue; }
        else if (c == 'b' || c == 'B') { if (pos <= (size_t)pagelines) pos = 0; else pos = pos - pagelines; continue; }
        else if (c == 'g') { pos = 0; continue; }
        else if (c == 'G') { pos = cnt > (size_t)pagelines ? cnt - pagelines : 0; continue; }
        else { pos = end; continue; }
    }

    for (size_t i = 0; i < cnt; ++i) free(lines[i]); free(lines);
    return 0;
}
