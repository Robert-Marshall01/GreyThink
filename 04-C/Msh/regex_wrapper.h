/*
 * SPDX-License-Identifier: MIT
 * Copyright (C) 2025 Msh project contributors
 */

#ifndef REGEX_WRAPPER_H
#define REGEX_WRAPPER_H

#ifdef __cplusplus
extern "C" {
#endif

/* Return 1 if pattern compiles, 0 otherwise */
int c_regex_valid(const char *pattern);

/* Return 1 if pattern matches text, 0 if not, -1 on invalid pattern */
int c_regex_match(const char *pattern, const char *text);

/* Perform replacement. Returns a malloc()-allocated string when successful (caller must free).
 * If pattern is invalid or some error occurs, returns NULL.
 * If changed_out is provided, it is set to 1 if any replacement happened, 0 otherwise.
 */
char *c_regex_replace(const char *pattern, const char *text, const char *repl, int global, int *changed_out);

#ifdef __cplusplus
}
#endif

#endif /* REGEX_WRAPPER_H */