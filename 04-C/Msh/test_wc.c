#/*
 * SPDX-License-Identifier: MIT
 * See LICENSES.md and COPYING for license and attribution details.
 */

#include <stdio.h>

void run_builtin_command(const char *cmd, FILE *in, FILE *out);

int main(void) {
    run_builtin_command("wc TEST\\nicole.txt", NULL, stdout);
    return 0;
}
