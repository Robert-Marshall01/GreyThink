/*
 * SPDX-License-Identifier: MIT
 * Copyright (C) 2025 Msh project contributors
 *
 * Lightweight header: only declarations and include guard. Implementation
 * lives in run_builtin_command.c to avoid duplicating code into Msh.c.
 */

#ifndef RUN_BUILTIN_COMMAND_H
#define RUN_BUILTIN_COMMAND_H

#include <stdio.h>

/* Helpers implemented in Msh.c */
char *extract_pattern_arg(const char *cmd, int skiplen);
char *extract_filename_arg(const char *cmd, int skiplen);

/* run_builtin_command implemented in run_builtin_command.c */
void run_builtin_command(const char *cmd, FILE *in, FILE *out);

#endif /* RUN_BUILTIN_COMMAND_H */