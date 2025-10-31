/*
 * SPDX-License-Identifier: MIT
 * Copyright (C) 2025 Msh project contributors
 *
 * Basic job control API for Msh (lightweight, Windows-based)
 * Jobs are processes launched with trailing '&' and tracked here.
 */

#ifndef MSH_JOBS_H
#define MSH_JOBS_H

#include <stdio.h>
#include <windows.h>

#define MSH_MAX_JOBS 128

int msh_add_job(HANDLE hProcess, HANDLE hThread, DWORD pid, const char *cmdline);
void msh_reap_jobs(void);
void msh_list_jobs(FILE *out);
int msh_bg_job(int jid, FILE *out);
int msh_fg_job(int jid, FILE *out);

#endif /* MSH_JOBS_H */
