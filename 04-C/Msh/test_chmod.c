#/*
 * SPDX-License-Identifier: MIT
 * See LICENSES.md and COPYING for license and attribution details.
 */

#include <stdio.h>
#include <windows.h>
#include <string.h>

// Include the builtin implementation header
#include "run_builtin_command.h"

int main(void) {
    const char *fname = "test_chmod.tmp";
    FILE *f = fopen(fname, "w");
    if (!f) {
        perror("creating test file");
        return 1;
    }
    fprintf(f, "hello\n");
    fclose(f);

    printf("Initial attributes:\n");
    DWORD a = GetFileAttributesA(fname);
    if (a == INVALID_FILE_ATTRIBUTES) { perror("GetFileAttributes"); return 1; }
    printf("  raw: 0x%08lx\n", (unsigned long)a);

    printf("Calling: chmod +h %s\n", fname);
    run_builtin_command("chmod +h test_chmod.tmp", NULL, stdout);
    a = GetFileAttributesA(fname);
    printf("  after +h raw: 0x%08lx\n", (unsigned long)a);

    printf("Calling: chmod -h %s\n", fname);
    run_builtin_command("chmod -h test_chmod.tmp", NULL, stdout);
    a = GetFileAttributesA(fname);
    printf("  after -h raw: 0x%08lx\n", (unsigned long)a);

    printf("Calling: chmod +r %s\n", fname);
    run_builtin_command("chmod +r test_chmod.tmp", NULL, stdout);
    a = GetFileAttributesA(fname);
    printf("  after +r raw: 0x%08lx\n", (unsigned long)a);

    printf("Calling: chmod -r %s\n", fname);
    run_builtin_command("chmod -r test_chmod.tmp", NULL, stdout);
    a = GetFileAttributesA(fname);
    printf("  after -r raw: 0x%08lx\n", (unsigned long)a);

    printf("Calling: chmod +x %s (should warn and not change attributes)\n", fname);
    run_builtin_command("chmod +x test_chmod.tmp", NULL, stdout);
    a = GetFileAttributesA(fname);
    printf("  after +x raw: 0x%08lx\n", (unsigned long)a);

    // cleanup
    RemoveFile:
    if (!DeleteFileA(fname)) perror("DeleteFile");
    return 0;
}

// Stubs to satisfy header references when compiling this small test program.
char *extract_pattern_arg(const char *cmd, int skiplen) { (void)cmd; (void)skiplen; return NULL; }
char *extract_filename_arg(const char *cmd, int skiplen) { (void)cmd; (void)skiplen; return NULL; }
