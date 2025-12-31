/*
 * SPDX-License-Identifier: MIT
 * Copyright (C) 2025 Msh project contributors
 *
 * Windows implementation of cross-platform abstractions
 */

#include "platform.h"

#if MSH_PLATFORM_WINDOWS

/* ============================================================================
 * String Utilities
 * ============================================================================ */

char *msh_strdup(const char *s) {
    if (!s) return NULL;
    return _strdup(s);
}

char *msh_fullpath(const char *path, char *resolved, size_t size) {
    if (resolved) {
        if (_fullpath(resolved, path, (unsigned int)size)) {
            return resolved;
        }
        return NULL;
    } else {
        return _fullpath(NULL, path, 0);
    }
}

/* ============================================================================
 * File System Operations
 * ============================================================================ */

int msh_mkdir(const char *path) {
    return CreateDirectoryA(path, NULL) ? 1 : 0;
}

int msh_rmdir(const char *path) {
    return RemoveDirectoryA(path) ? 1 : 0;
}

int msh_chdir(const char *path) {
    return SetCurrentDirectoryA(path) ? 1 : 0;
}

char *msh_getcwd(char *buf, size_t size) {
    DWORD result = GetCurrentDirectoryA((DWORD)size, buf);
    if (result > 0 && result < size) {
        return buf;
    }
    return NULL;
}

int msh_remove(const char *path) {
    DWORD attrs = GetFileAttributesA(path);
    if (attrs == INVALID_FILE_ATTRIBUTES) return 0;
    
    if (attrs & FILE_ATTRIBUTE_DIRECTORY) {
        return RemoveDirectoryA(path) ? 1 : 0;
    } else {
        return DeleteFileA(path) ? 1 : 0;
    }
}

int msh_rename(const char *oldpath, const char *newpath) {
    return MoveFileA(oldpath, newpath) ? 1 : 0;
}

int msh_copy_file(const char *src, const char *dst) {
    return CopyFileA(src, dst, FALSE) ? 1 : 0;
}

int msh_create_hardlink(const char *linkpath, const char *targetpath) {
    return CreateHardLinkA(linkpath, targetpath, NULL) ? 1 : 0;
}

/* ============================================================================
 * File Attributes
 * ============================================================================ */

uint32_t msh_get_file_attributes(const char *path) {
    DWORD attrs = GetFileAttributesA(path);
    if (attrs == INVALID_FILE_ATTRIBUTES) return 0;
    
    uint32_t result = 0;
    
    if (attrs & FILE_ATTRIBUTE_DIRECTORY) result |= MSH_ATTR_DIRECTORY;
    if (attrs & FILE_ATTRIBUTE_READONLY) result |= MSH_ATTR_READONLY;
    if (attrs & FILE_ATTRIBUTE_HIDDEN) result |= MSH_ATTR_HIDDEN;
    if (attrs & FILE_ATTRIBUTE_SYSTEM) result |= MSH_ATTR_SYSTEM;
    if (attrs & FILE_ATTRIBUTE_REPARSE_POINT) result |= MSH_ATTR_SYMLINK;
    
    /* Check if file is executable by extension */
    const char *ext = strrchr(path, '.');
    if (ext) {
        if (_stricmp(ext, ".exe") == 0 ||
            _stricmp(ext, ".cmd") == 0 ||
            _stricmp(ext, ".bat") == 0 ||
            _stricmp(ext, ".com") == 0) {
            result |= MSH_ATTR_EXECUTABLE;
        }
    }
    
    return result;
}

int msh_set_file_attributes(const char *path, uint32_t attrs) {
    DWORD win_attrs = 0;
    
    if (attrs & MSH_ATTR_READONLY) win_attrs |= FILE_ATTRIBUTE_READONLY;
    if (attrs & MSH_ATTR_HIDDEN) win_attrs |= FILE_ATTRIBUTE_HIDDEN;
    if (attrs & MSH_ATTR_SYSTEM) win_attrs |= FILE_ATTRIBUTE_SYSTEM;
    
    if (win_attrs == 0) win_attrs = FILE_ATTRIBUTE_NORMAL;
    
    return SetFileAttributesA(path, win_attrs) ? 1 : 0;
}

int msh_file_exists(const char *path) {
    DWORD attrs = GetFileAttributesA(path);
    return (attrs != INVALID_FILE_ATTRIBUTES) ? 1 : 0;
}

int msh_is_directory(const char *path) {
    DWORD attrs = GetFileAttributesA(path);
    if (attrs == INVALID_FILE_ATTRIBUTES) return 0;
    return (attrs & FILE_ATTRIBUTE_DIRECTORY) ? 1 : 0;
}

/* ============================================================================
 * Directory Enumeration
 * ============================================================================ */

msh_dir_handle_t *msh_opendir(const char *path) {
    char search_path[MSH_MAX_PATH + 3];
    snprintf(search_path, sizeof(search_path), "%s\\*", path);
    
    msh_dir_handle_t *dh = (msh_dir_handle_t *)malloc(sizeof(msh_dir_handle_t));
    if (!dh) return NULL;
    
    dh->handle = FindFirstFileA(search_path, &dh->find_data);
    if (dh->handle == INVALID_HANDLE_VALUE) {
        free(dh);
        return NULL;
    }
    
    MSH_SAFE_STRCPY(dh->base_path, path, MSH_MAX_PATH);
    dh->first = 1;
    
    return dh;
}

int msh_readdir(msh_dir_handle_t *dh, msh_dir_entry_t *entry) {
    if (!dh || !entry || dh->handle == INVALID_HANDLE_VALUE) return 0;
    
    if (dh->first) {
        dh->first = 0;
    } else {
        if (!FindNextFileA(dh->handle, &dh->find_data)) {
            return 0;
        }
    }
    
    MSH_SAFE_STRCPY(entry->name, dh->find_data.cFileName, MSH_MAX_PATH);
    
    /* Convert FILETIME to time_t */
    ULARGE_INTEGER ull;
    ull.LowPart = dh->find_data.ftLastWriteTime.dwLowDateTime;
    ull.HighPart = dh->find_data.ftLastWriteTime.dwHighDateTime;
    entry->mtime = (time_t)((ull.QuadPart / 10000000ULL) - 11644473600ULL);
    
    /* Calculate size */
    entry->size = ((uint64_t)dh->find_data.nFileSizeHigh << 32) | dh->find_data.nFileSizeLow;
    entry->inode = 0;  /* Windows doesn't have inodes */
    
    /* Convert attributes */
    entry->attributes = 0;
    DWORD attrs = dh->find_data.dwFileAttributes;
    if (attrs & FILE_ATTRIBUTE_DIRECTORY) entry->attributes |= MSH_ATTR_DIRECTORY;
    if (attrs & FILE_ATTRIBUTE_READONLY) entry->attributes |= MSH_ATTR_READONLY;
    if (attrs & FILE_ATTRIBUTE_HIDDEN) entry->attributes |= MSH_ATTR_HIDDEN;
    if (attrs & FILE_ATTRIBUTE_SYSTEM) entry->attributes |= MSH_ATTR_SYSTEM;
    if (attrs & FILE_ATTRIBUTE_REPARSE_POINT) entry->attributes |= MSH_ATTR_SYMLINK;
    
    /* Check executable by extension */
    const char *ext = strrchr(entry->name, '.');
    if (ext) {
        if (_stricmp(ext, ".exe") == 0 ||
            _stricmp(ext, ".cmd") == 0 ||
            _stricmp(ext, ".bat") == 0 ||
            _stricmp(ext, ".com") == 0) {
            entry->attributes |= MSH_ATTR_EXECUTABLE;
        }
    }
    
    return 1;
}

void msh_closedir(msh_dir_handle_t *dh) {
    if (dh) {
        if (dh->handle != INVALID_HANDLE_VALUE) {
            FindClose(dh->handle);
        }
        free(dh);
    }
}

/* ============================================================================
 * Temporary Files
 * ============================================================================ */

int msh_get_temp_path(char *buf, size_t size) {
    DWORD result = GetTempPathA((DWORD)size, buf);
    return (result > 0 && result < size) ? 1 : 0;
}

int msh_get_temp_filename(const char *dir, const char *prefix, char *buf, size_t size) {
    char temp_dir[MSH_MAX_PATH];
    
    if (!dir || !dir[0]) {
        if (!msh_get_temp_path(temp_dir, sizeof(temp_dir))) {
            return 0;
        }
        dir = temp_dir;
    }
    
    char temp_file[MSH_MAX_PATH];
    UINT result = GetTempFileNameA(dir, prefix ? prefix : "msh", 0, temp_file);
    if (result == 0) return 0;
    
    MSH_SAFE_STRCPY(buf, temp_file, size);
    return 1;
}

/* ============================================================================
 * Process Management
 * ============================================================================ */

int msh_spawn_process(const char *cmdline, msh_process_t *proc, int background) {
    if (!cmdline || !proc) return 0;
    
    memset(proc, 0, sizeof(msh_process_t));
    
    STARTUPINFOA si;
    PROCESS_INFORMATION pi;
    
    ZeroMemory(&si, sizeof(si));
    si.cb = sizeof(si);
    ZeroMemory(&pi, sizeof(pi));
    
    /* Make a mutable copy of command line (CreateProcessA may modify it) */
    char *cmd_copy = _strdup(cmdline);
    if (!cmd_copy) return 0;
    
    DWORD flags = background ? CREATE_NEW_CONSOLE : 0;
    
    BOOL success = CreateProcessA(
        NULL,           /* Application name */
        cmd_copy,       /* Command line */
        NULL,           /* Process security attributes */
        NULL,           /* Thread security attributes */
        FALSE,          /* Inherit handles */
        flags,          /* Creation flags */
        NULL,           /* Environment */
        NULL,           /* Current directory */
        &si,            /* Startup info */
        &pi             /* Process information */
    );
    
    free(cmd_copy);
    
    if (!success) return 0;
    
    proc->handle = pi.hProcess;
    proc->thread_handle = pi.hThread;
    proc->pid = pi.dwProcessId;
    proc->running = 1;
    
    if (!background) {
        /* Wait for foreground process to complete */
        WaitForSingleObject(pi.hProcess, INFINITE);
        proc->running = 0;
    }
    
    return 1;
}

int msh_wait_process(msh_process_t *proc, int timeout_ms) {
    if (!proc || proc->handle == MSH_INVALID_HANDLE) return 0;
    
    DWORD timeout = (timeout_ms < 0) ? INFINITE : (DWORD)timeout_ms;
    DWORD result = WaitForSingleObject(proc->handle, timeout);
    
    if (result == WAIT_OBJECT_0) {
        proc->running = 0;
        return 1;  /* Process exited */
    } else if (result == WAIT_TIMEOUT) {
        return 0;  /* Timeout */
    }
    
    return -1;  /* Error */
}

int msh_get_exit_code(msh_process_t *proc, int *exit_code) {
    if (!proc || !exit_code || proc->handle == MSH_INVALID_HANDLE) return 0;
    
    DWORD code;
    if (GetExitCodeProcess(proc->handle, &code)) {
        if (code == STILL_ACTIVE) {
            return 0;  /* Still running */
        }
        *exit_code = (int)code;
        return 1;
    }
    return 0;
}

int msh_terminate_process(msh_process_t *proc, int signal_num) {
    (void)signal_num;  /* Windows doesn't use signals for termination */
    
    if (!proc || proc->handle == MSH_INVALID_HANDLE) return 0;
    return TerminateProcess(proc->handle, 1) ? 1 : 0;
}

int msh_resume_process(msh_process_t *proc) {
    if (!proc || proc->thread_handle == MSH_INVALID_HANDLE) return 0;
    return ResumeThread(proc->thread_handle) != (DWORD)-1 ? 1 : 0;
}

void msh_close_process(msh_process_t *proc) {
    if (proc) {
        if (proc->handle != MSH_INVALID_HANDLE) {
            CloseHandle(proc->handle);
            proc->handle = MSH_INVALID_HANDLE;
        }
        if (proc->thread_handle != MSH_INVALID_HANDLE) {
            CloseHandle(proc->thread_handle);
            proc->thread_handle = MSH_INVALID_HANDLE;
        }
        proc->pid = 0;
    }
}

msh_pid_t msh_getpid(void) {
    return GetCurrentProcessId();
}

int msh_is_process_running(msh_process_t *proc) {
    if (!proc || proc->handle == MSH_INVALID_HANDLE) return 0;
    
    DWORD result = WaitForSingleObject(proc->handle, 0);
    if (result == WAIT_TIMEOUT) {
        return 1;  /* Still running */
    }
    
    proc->running = 0;
    return 0;
}

/* ============================================================================
 * System Information
 * ============================================================================ */

int msh_get_username(char *buf, size_t size) {
    DWORD len = (DWORD)size;
    return GetUserNameA(buf, &len) ? 1 : 0;
}

int msh_get_hostname(char *buf, size_t size) {
    DWORD len = (DWORD)size;
    return GetComputerNameA(buf, &len) ? 1 : 0;
}

int msh_is_admin(void) {
    BOOL is_admin = FALSE;
    HANDLE token = NULL;
    
    if (OpenProcessToken(GetCurrentProcess(), TOKEN_QUERY, &token)) {
        TOKEN_ELEVATION elevation;
        DWORD size = sizeof(elevation);
        
        if (GetTokenInformation(token, TokenElevation, &elevation, sizeof(elevation), &size)) {
            is_admin = elevation.TokenIsElevated;
        }
        CloseHandle(token);
    }
    
    return is_admin ? 1 : 0;
}

int msh_get_home_dir(char *buf, size_t size) {
    const char *userprofile = getenv("USERPROFILE");
    if (userprofile) {
        MSH_SAFE_STRCPY(buf, userprofile, size);
        return 1;
    }
    
    const char *homedrive = getenv("HOMEDRIVE");
    const char *homepath = getenv("HOMEPATH");
    if (homedrive && homepath) {
        snprintf(buf, size, "%s%s", homedrive, homepath);
        return 1;
    }
    
    return 0;
}

/* ============================================================================
 * Console/Terminal
 * ============================================================================ */

int msh_is_interactive(void) {
    DWORD mode;
    return GetConsoleMode(GetStdHandle(STD_INPUT_HANDLE), &mode) ? 1 : 0;
}

void msh_clear_screen(void) {
    /* Use ANSI escape on Windows 10+ or cls command */
    system("cls");
}

/* ============================================================================
 * Shell Execution
 * ============================================================================ */

int msh_system(const char *cmd) {
    return system(cmd);
}

int msh_shell_execute(const char *cmd, const char *args, int elevated) {
    const char *verb = elevated ? "runas" : "open";
    
    SHELLEXECUTEINFOA sei;
    ZeroMemory(&sei, sizeof(sei));
    sei.cbSize = sizeof(sei);
    sei.fMask = SEE_MASK_NOCLOSEPROCESS;
    sei.lpVerb = verb;
    sei.lpFile = cmd;
    sei.lpParameters = args;
    sei.nShow = SW_SHOWNORMAL;
    
    if (ShellExecuteExA(&sei)) {
        if (sei.hProcess) {
            WaitForSingleObject(sei.hProcess, INFINITE);
            CloseHandle(sei.hProcess);
        }
        return 0;
    }
    
    return -1;
}

/* ============================================================================
 * Wildcard Matching
 * ============================================================================ */

/* Simple wildcard match implementation for Windows */
static int match_helper(const char *pattern, const char *str) {
    while (*pattern && *str) {
        if (*pattern == '*') {
            pattern++;
            if (!*pattern) return 1;  /* Trailing * matches all */
            while (*str) {
                if (match_helper(pattern, str)) return 1;
                str++;
            }
            return 0;
        } else if (*pattern == '?' || tolower(*pattern) == tolower(*str)) {
            pattern++;
            str++;
        } else {
            return 0;
        }
    }
    
    /* Handle trailing wildcards */
    while (*pattern == '*') pattern++;
    
    return (!*pattern && !*str) ? 1 : 0;
}

int msh_wildcard_match(const char *pattern, const char *str) {
    return match_helper(pattern, str);
}

/* ============================================================================
 * Module/Executable Path
 * ============================================================================ */

int msh_get_executable_path(char *buf, size_t size) {
    DWORD result = GetModuleFileNameA(NULL, buf, (DWORD)size);
    return (result > 0 && result < size) ? 1 : 0;
}

#endif /* MSH_PLATFORM_WINDOWS */
