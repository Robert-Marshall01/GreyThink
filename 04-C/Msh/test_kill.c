/*
 * SPDX-License-Identifier: MIT
 * See LICENSES.md and COPYING for license and attribution details.
 */

#include <windows.h>
#include <stdio.h>
#include "run_builtin_command.h"

// Stubs to satisfy linker when building this small test. The real
// implementations live in Msh.c in the project, but for this standalone
// test we provide minimal versions which won't be called by the kill
// implementation.
char *extract_pattern_arg(const char *cmd, int skiplen) { (void)cmd; (void)skiplen; return NULL; }
char *extract_filename_arg(const char *cmd, int skiplen) { (void)cmd; (void)skiplen; return NULL; }

int main(void) {
    STARTUPINFOA si = {0};
    PROCESS_INFORMATION pi = {0};
    si.cb = sizeof(si);
    // Launch notepad
    if (!CreateProcessA(NULL, "notepad.exe", NULL, NULL, FALSE, 0, NULL, NULL, &si, &pi)) {
        DWORD err = GetLastError();
        fprintf(stderr, "Failed to start notepad (error %lu)\n", err);
        return 1;
    }
    printf("Launched notepad PID=%lu\n", (unsigned long)pi.dwProcessId);
    // Give it a moment
    Sleep(500);
    char cmd[128];
    snprintf(cmd, sizeof(cmd), "kill -9 %lu", (unsigned long)pi.dwProcessId);
    printf("Running builtin: %s\n", cmd);
    run_builtin_command(cmd, stdin, stdout);
    // Wait up to 3 seconds for process to exit
    DWORD wait = WaitForSingleObject(pi.hProcess, 3000);
    if (wait == WAIT_OBJECT_0) {
        DWORD code = 0;
        if (GetExitCodeProcess(pi.hProcess, &code)) {
            printf("Process exited with code %lu\n", code);
        } else {
            printf("Process exited (exit code unknown)\n");
        }
    } else if (wait == WAIT_TIMEOUT) {
        printf("Process still running after kill attempt.\n");
    } else {
        printf("Wait failed (err=%lu)\n", GetLastError());
    }
    CloseHandle(pi.hProcess);
    CloseHandle(pi.hThread);
    return 0;
}
