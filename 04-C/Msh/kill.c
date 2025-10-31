#/* SPDX-License-Identifier: MIT */
/* See LICENSES.md and COPYING for license and attribution details. */
#include <windows.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static void usage(const char *prog) {
    fprintf(stderr, "Usage: %s [-9] PID [PID ...]\n", prog);
    fprintf(stderr, "  -9         : force kill (SIGKILL semantics)\n");
}

int main(int argc, char **argv) {
    if (argc < 2) {
        usage(argv[0]);
        return 1;
    }
    int force = 0;
    int any_error = 0;
    for (int i = 1; i < argc; ++i) {
        if (strcmp(argv[i], "-9") == 0) { force = 1; continue; }
        if (strcmp(argv[i], "-SIGKILL") == 0) { force = 1; continue; }
        // parse PID
        char *endptr = NULL;
        long pid = strtol(argv[i], &endptr, 10);
        if (endptr == argv[i] || pid <= 0) {
            fprintf(stderr, "kill: invalid PID: %s\n", argv[i]);
            any_error = 1;
            continue;
        }
        HANDLE h = OpenProcess(PROCESS_TERMINATE | PROCESS_QUERY_LIMITED_INFORMATION, FALSE, (DWORD)pid);
        if (!h) {
            DWORD err = GetLastError();
            fprintf(stderr, "kill: failed to open process %ld (error code: %lu)\n", pid, err);
            any_error = 1;
            continue;
        }
        BOOL ok = TerminateProcess(h, force ? 9 : 0);
        if (!ok) {
            DWORD err = GetLastError();
            fprintf(stderr, "kill: failed to terminate process %ld (error code: %lu)\n", pid, err);
            CloseHandle(h);
            any_error = 1;
            continue;
        }
        // wait briefly for cleanup
        WaitForSingleObject(h, 200);
        CloseHandle(h);
    }
    return any_error ? 2 : 0;
}
