/*
 * SPDX-License-Identifier: MIT
 * Copyright (C) 2025 Msh project contributors
 *
 * Cross-platform abstraction layer for Msh
 * Provides unified API for Windows and POSIX systems
 */

#ifndef MSH_PLATFORM_H
#define MSH_PLATFORM_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

/* ============================================================================
 * Platform Detection
 * ============================================================================ */

#if defined(_WIN32) || defined(_WIN64) || defined(__CYGWIN__)
    #define MSH_PLATFORM_WINDOWS 1
    #define MSH_PLATFORM_POSIX   0
#elif defined(__linux__) || defined(__unix__) || defined(__APPLE__)
    #define MSH_PLATFORM_WINDOWS 0
    #define MSH_PLATFORM_POSIX   1
#else
    #error "Unsupported platform"
#endif

/* ============================================================================
 * Platform-Specific Includes
 * ============================================================================ */

#if MSH_PLATFORM_WINDOWS
    #ifndef WIN32_LEAN_AND_MEAN
        #define WIN32_LEAN_AND_MEAN
    #endif
    #include <windows.h>
    #include <io.h>
    #include <direct.h>
    #include <process.h>
#else
    #include <unistd.h>
    #include <sys/types.h>
    #include <sys/stat.h>
    #include <sys/wait.h>
    #include <dirent.h>
    #include <fcntl.h>
    #include <signal.h>
    #include <pwd.h>
    #include <grp.h>
    #include <errno.h>
    #include <time.h>
    #include <fnmatch.h>
    #include <limits.h>
    #include <pthread.h>
#endif

/* ============================================================================
 * Cross-Platform Type Definitions
 * ============================================================================ */

#if MSH_PLATFORM_WINDOWS
    typedef HANDLE          msh_handle_t;
    typedef DWORD           msh_pid_t;
    typedef DWORD           msh_error_t;
    typedef BOOL            msh_bool_t;
    #define MSH_INVALID_HANDLE  INVALID_HANDLE_VALUE
    #define MSH_TRUE            TRUE
    #define MSH_FALSE           FALSE
#else
    typedef int             msh_handle_t;
    typedef pid_t           msh_pid_t;
    typedef int             msh_error_t;
    typedef int             msh_bool_t;
    #define MSH_INVALID_HANDLE  (-1)
    #define MSH_TRUE            1
    #define MSH_FALSE           0
#endif

/* Maximum path length */
#ifndef MSH_MAX_PATH
    #if MSH_PLATFORM_WINDOWS
        #define MSH_MAX_PATH MAX_PATH
    #else
        #define MSH_MAX_PATH PATH_MAX
    #endif
#endif

/* Path separator */
#if MSH_PLATFORM_WINDOWS
    #define MSH_PATH_SEP        '\\'
    #define MSH_PATH_SEP_STR    "\\"
    #define MSH_PATH_SEP_ALT    '/'
#else
    #define MSH_PATH_SEP        '/'
    #define MSH_PATH_SEP_STR    "/"
    #define MSH_PATH_SEP_ALT    '/'
#endif

/* ============================================================================
 * File Attribute Flags (cross-platform)
 * ============================================================================ */

#define MSH_ATTR_DIRECTORY  0x0001
#define MSH_ATTR_READONLY   0x0002
#define MSH_ATTR_HIDDEN     0x0004
#define MSH_ATTR_SYSTEM     0x0008
#define MSH_ATTR_ARCHIVE    0x0010
#define MSH_ATTR_SYMLINK    0x0020
#define MSH_ATTR_EXECUTABLE 0x0040

/* ============================================================================
 * File/Directory Entry Structure
 * ============================================================================ */

typedef struct msh_dir_entry {
    char name[MSH_MAX_PATH];
    uint64_t size;
    uint32_t attributes;
    time_t mtime;
    uint64_t inode;  /* File index / inode number */
} msh_dir_entry_t;

/* Directory handle for iteration */
typedef struct msh_dir_handle {
#if MSH_PLATFORM_WINDOWS
    HANDLE handle;
    WIN32_FIND_DATAA find_data;
    int first;  /* Flag for first read */
    char base_path[MSH_MAX_PATH];
#else
    DIR *dir;
    char base_path[MSH_MAX_PATH];
#endif
} msh_dir_handle_t;

/* ============================================================================
 * Process Structure
 * ============================================================================ */

typedef struct msh_process {
    msh_handle_t handle;
    msh_handle_t thread_handle;  /* Windows only, unused on POSIX */
    msh_pid_t pid;
    int running;
} msh_process_t;

/* ============================================================================
 * Function Prototypes - String Utilities
 * ============================================================================ */

/* strdup wrapper (POSIX uses strdup, Windows uses _strdup) */
char *msh_strdup(const char *s);

/* fullpath wrapper */
char *msh_fullpath(const char *path, char *resolved, size_t size);

/* ============================================================================
 * Function Prototypes - File System Operations
 * ============================================================================ */

/* Directory operations */
int msh_mkdir(const char *path);
int msh_rmdir(const char *path);
int msh_chdir(const char *path);
char *msh_getcwd(char *buf, size_t size);

/* File operations */
int msh_remove(const char *path);
int msh_rename(const char *oldpath, const char *newpath);
int msh_copy_file(const char *src, const char *dst);
int msh_create_hardlink(const char *linkpath, const char *targetpath);

/* File attributes */
uint32_t msh_get_file_attributes(const char *path);
int msh_set_file_attributes(const char *path, uint32_t attrs);
int msh_file_exists(const char *path);
int msh_is_directory(const char *path);

/* Directory enumeration */
msh_dir_handle_t *msh_opendir(const char *path);
int msh_readdir(msh_dir_handle_t *dh, msh_dir_entry_t *entry);
void msh_closedir(msh_dir_handle_t *dh);

/* Temporary files */
int msh_get_temp_path(char *buf, size_t size);
int msh_get_temp_filename(const char *dir, const char *prefix, char *buf, size_t size);

/* ============================================================================
 * Function Prototypes - Process Management
 * ============================================================================ */

/* Process creation and control */
int msh_spawn_process(const char *cmdline, msh_process_t *proc, int background);
int msh_wait_process(msh_process_t *proc, int timeout_ms);  /* -1 = infinite */
int msh_get_exit_code(msh_process_t *proc, int *exit_code);
int msh_terminate_process(msh_process_t *proc, int signal_num);
int msh_resume_process(msh_process_t *proc);
void msh_close_process(msh_process_t *proc);

/* Process info */
msh_pid_t msh_getpid(void);
int msh_is_process_running(msh_process_t *proc);

/* ============================================================================
 * Function Prototypes - System Information
 * ============================================================================ */

int msh_get_username(char *buf, size_t size);
int msh_get_hostname(char *buf, size_t size);
int msh_is_admin(void);
int msh_get_home_dir(char *buf, size_t size);

/* ============================================================================
 * Function Prototypes - Console/Terminal
 * ============================================================================ */

int msh_is_interactive(void);
void msh_clear_screen(void);

/* ============================================================================
 * Function Prototypes - Shell Execution
 * ============================================================================ */

int msh_system(const char *cmd);
int msh_shell_execute(const char *cmd, const char *args, int elevated);

/* ============================================================================
 * Function Prototypes - Wildcard Matching
 * ============================================================================ */

int msh_wildcard_match(const char *pattern, const char *str);

/* ============================================================================
 * Function Prototypes - Module/Executable Path
 * ============================================================================ */

int msh_get_executable_path(char *buf, size_t size);

/* ============================================================================
 * Utility Macros
 * ============================================================================ */

/* Safe string copy */
#define MSH_SAFE_STRCPY(dst, src, size) \
    do { strncpy((dst), (src), (size) - 1); (dst)[(size) - 1] = '\0'; } while(0)

/* Min/Max */
#ifndef MSH_MIN
    #define MSH_MIN(a, b) (((a) < (b)) ? (a) : (b))
#endif
#ifndef MSH_MAX
    #define MSH_MAX(a, b) (((a) > (b)) ? (a) : (b))
#endif

#endif /* MSH_PLATFORM_H */
