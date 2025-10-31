/*
 * SPDX-License-Identifier: MIT
 * Copyright (C) 2025 Msh project contributors
 *
 * Job control helpers for Msh. See LICENSES.md for licensing and attribution details.
 */

#include "jobs.h"
#include <string.h>
#include <stdlib.h>

typedef enum { JSTATE_RUNNING, JSTATE_STOPPED, JSTATE_DONE } job_state;

typedef struct {
    int used;
    int id;
    HANDLE hProcess;
    HANDLE hThread;
    DWORD pid;
    job_state state;
    char *cmdline;
} msh_job;

static msh_job jobs[MSH_MAX_JOBS];
static int next_jid = 1;

int msh_add_job(HANDLE hProcess, HANDLE hThread, DWORD pid, const char *cmdline) {
    for (int i = 0; i < MSH_MAX_JOBS; ++i) {
        if (!jobs[i].used) {
            jobs[i].used = 1;
            jobs[i].id = next_jid++;
            jobs[i].hProcess = hProcess;
            jobs[i].hThread = hThread;
            jobs[i].pid = pid;
            jobs[i].state = JSTATE_RUNNING;
            jobs[i].cmdline = _strdup(cmdline);
            return jobs[i].id;
        }
    }
    return -1;
}

/* Check for finished processes and update state */
void msh_reap_jobs(void) {
    for (int i = 0; i < MSH_MAX_JOBS; ++i) {
        if (!jobs[i].used) continue;
        DWORD status = WaitForSingleObject(jobs[i].hProcess, 0);
        if (status == WAIT_OBJECT_0) {
            jobs[i].state = JSTATE_DONE;
        }
    }
}

void msh_list_jobs(FILE *out) {
    msh_reap_jobs();
    for (int i = 0; i < MSH_MAX_JOBS; ++i) {
        if (!jobs[i].used) continue;
        const char *s = "?";
        if (jobs[i].state == JSTATE_RUNNING) s = "Running";
        else if (jobs[i].state == JSTATE_STOPPED) s = "Stopped";
        else if (jobs[i].state == JSTATE_DONE) s = "Done";
        fprintf(out, "[%d] %s %lu %s\n", jobs[i].id, s, (unsigned long)jobs[i].pid, jobs[i].cmdline ? jobs[i].cmdline : "");
    }
    fflush(out);
}

static msh_job *find_job_by_jid(int jid) {
    for (int i = 0; i < MSH_MAX_JOBS; ++i) {
        if (jobs[i].used && jobs[i].id == jid) return &jobs[i];
    }
    return NULL;
}

int msh_bg_job(int jid, FILE *out) {
    msh_job *j = find_job_by_jid(jid);
    if (!j) return -1;
    // On Windows, resume thread if it was suspended. Send a 'continue' by resuming the thread.
    if (j->state == JSTATE_STOPPED && j->hThread) {
        ResumeThread(j->hThread);
    }
    j->state = JSTATE_RUNNING;
    fprintf(out, "[%d] %lu\n", j->id, (unsigned long)j->pid);
    fflush(out);
    return 0;
}

int msh_fg_job(int jid, FILE *out) {
    msh_job *j = find_job_by_jid(jid);
    if (!j) return -1;
    // Bring job to foreground: wait for process to finish
    if (j->state == JSTATE_STOPPED && j->hThread) ResumeThread(j->hThread);
    j->state = JSTATE_RUNNING;
    // Wait for process to finish (blocking)
    WaitForSingleObject(j->hProcess, INFINITE);
    DWORD exitcode = 0; GetExitCodeProcess(j->hProcess, &exitcode);
    fprintf(out, "[%d] Done %lu (exit=%lu)\n", j->id, (unsigned long)j->pid, (unsigned long)exitcode);
    // free job
    CloseHandle(j->hProcess);
    if (j->hThread) CloseHandle(j->hThread);
    if (j->cmdline) free(j->cmdline);
    j->used = 0;
    fflush(out);
    return 0;
}
