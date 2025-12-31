/*
 * SPDX-License-Identifier: MIT
 * Copyright (C) 2025 Msh project contributors
 *
 * POSIX/Linux implementation of cross-platform abstractions
 */

#include "platform.h"

#if MSH_PLATFORM_POSIX

#include <sys/ioctl.h>
#include <termios.h>
#include <glob.h>

/* ============================================================================
 * String Utilities
 * ============================================================================ */

char *msh_strdup(const char *s) {
    if (!s) return NULL;
    return strdup(s);
}

char *msh_fullpath(const char *path, char *resolved, size_t size) {
    char *result = realpath(path, NULL);
    if (result && resolved) {
        MSH_SAFE_STRCPY(resolved, result, size);
        free(result);
        return resolved;
    }
    return result;
}

/* ============================================================================
 * File System Operations
 * ============================================================================ */

int msh_mkdir(const char *path) {
    return mkdir(path, 0755) == 0 ? 1 : 0;
}

int msh_rmdir(const char *path) {
    return rmdir(path) == 0 ? 1 : 0;
}

int msh_chdir(const char *path) {
    return chdir(path) == 0 ? 1 : 0;
}

char *msh_getcwd(char *buf, size_t size) {
    return getcwd(buf, size);
}

int msh_remove(const char *path) {
    struct stat st;
    if (stat(path, &st) != 0) return 0;
    
    if (S_ISDIR(st.st_mode)) {
        return rmdir(path) == 0 ? 1 : 0;
    } else {
        return unlink(path) == 0 ? 1 : 0;
    }
}

int msh_rename(const char *oldpath, const char *newpath) {
    return rename(oldpath, newpath) == 0 ? 1 : 0;
}

int msh_copy_file(const char *src, const char *dst) {
    FILE *fsrc = fopen(src, "rb");
    if (!fsrc) return 0;
    
    FILE *fdst = fopen(dst, "wb");
    if (!fdst) {
        fclose(fsrc);
        return 0;
    }
    
    char buf[8192];
    size_t n;
    while ((n = fread(buf, 1, sizeof(buf), fsrc)) > 0) {
        if (fwrite(buf, 1, n, fdst) != n) {
            fclose(fsrc);
            fclose(fdst);
            return 0;
        }
    }
    
    /* Preserve permissions */
    struct stat st;
    if (fstat(fileno(fsrc), &st) == 0) {
        fchmod(fileno(fdst), st.st_mode);
    }
    
    fclose(fsrc);
    fclose(fdst);
    return 1;
}

int msh_create_hardlink(const char *linkpath, const char *targetpath) {
    return link(targetpath, linkpath) == 0 ? 1 : 0;
}

/* ============================================================================
 * File Attributes
 * ============================================================================ */

uint32_t msh_get_file_attributes(const char *path) {
    struct stat st;
    if (lstat(path, &st) != 0) return 0;
    
    uint32_t attrs = 0;
    
    if (S_ISDIR(st.st_mode)) attrs |= MSH_ATTR_DIRECTORY;
    if (S_ISLNK(st.st_mode)) attrs |= MSH_ATTR_SYMLINK;
    if (!(st.st_mode & S_IWUSR)) attrs |= MSH_ATTR_READONLY;
    if (st.st_mode & S_IXUSR) attrs |= MSH_ATTR_EXECUTABLE;
    
    /* Hidden files in POSIX start with '.' */
    const char *basename = strrchr(path, '/');
    if (basename) basename++;
    else basename = path;
    if (basename[0] == '.') attrs |= MSH_ATTR_HIDDEN;
    
    return attrs;
}

int msh_set_file_attributes(const char *path, uint32_t attrs) {
    struct stat st;
    if (stat(path, &st) != 0) return 0;
    
    mode_t mode = st.st_mode;
    
    if (attrs & MSH_ATTR_READONLY) {
        mode &= ~(S_IWUSR | S_IWGRP | S_IWOTH);
    } else {
        mode |= S_IWUSR;
    }
    
    if (attrs & MSH_ATTR_EXECUTABLE) {
        mode |= S_IXUSR;
    } else {
        mode &= ~S_IXUSR;
    }
    
    return chmod(path, mode) == 0 ? 1 : 0;
}

int msh_file_exists(const char *path) {
    struct stat st;
    return stat(path, &st) == 0 ? 1 : 0;
}

int msh_is_directory(const char *path) {
    struct stat st;
    if (stat(path, &st) != 0) return 0;
    return S_ISDIR(st.st_mode) ? 1 : 0;
}

/* ============================================================================
 * Directory Enumeration
 * ============================================================================ */

msh_dir_handle_t *msh_opendir(const char *path) {
    DIR *dir = opendir(path);
    if (!dir) return NULL;
    
    msh_dir_handle_t *dh = (msh_dir_handle_t *)malloc(sizeof(msh_dir_handle_t));
    if (!dh) {
        closedir(dir);
        return NULL;
    }
    
    dh->dir = dir;
    MSH_SAFE_STRCPY(dh->base_path, path, MSH_MAX_PATH);
    
    return dh;
}

int msh_readdir(msh_dir_handle_t *dh, msh_dir_entry_t *entry) {
    if (!dh || !dh->dir || !entry) return 0;
    
    struct dirent *de = readdir(dh->dir);
    if (!de) return 0;
    
    MSH_SAFE_STRCPY(entry->name, de->d_name, MSH_MAX_PATH);
    
    /* Get full path for stat */
    char fullpath[MSH_MAX_PATH * 2];
    snprintf(fullpath, sizeof(fullpath), "%s/%s", dh->base_path, de->d_name);
    
    struct stat st;
    if (stat(fullpath, &st) == 0) {
        entry->size = st.st_size;
        entry->mtime = st.st_mtime;
        entry->inode = st.st_ino;
        
        entry->attributes = 0;
        if (S_ISDIR(st.st_mode)) entry->attributes |= MSH_ATTR_DIRECTORY;
        if (S_ISLNK(st.st_mode)) entry->attributes |= MSH_ATTR_SYMLINK;
        if (!(st.st_mode & S_IWUSR)) entry->attributes |= MSH_ATTR_READONLY;
        if (st.st_mode & S_IXUSR) entry->attributes |= MSH_ATTR_EXECUTABLE;
        if (de->d_name[0] == '.') entry->attributes |= MSH_ATTR_HIDDEN;
    } else {
        entry->size = 0;
        entry->mtime = 0;
        entry->inode = 0;
        entry->attributes = 0;
    }
    
    return 1;
}

void msh_closedir(msh_dir_handle_t *dh) {
    if (dh) {
        if (dh->dir) closedir(dh->dir);
        free(dh);
    }
}

/* ============================================================================
 * Temporary Files
 * ============================================================================ */

int msh_get_temp_path(char *buf, size_t size) {
    const char *tmpdir = getenv("TMPDIR");
    if (!tmpdir) tmpdir = getenv("TMP");
    if (!tmpdir) tmpdir = getenv("TEMP");
    if (!tmpdir) tmpdir = "/tmp";
    
    MSH_SAFE_STRCPY(buf, tmpdir, size);
    return 1;
}

int msh_get_temp_filename(const char *dir, const char *prefix, char *buf, size_t size) {
    char template[MSH_MAX_PATH];
    snprintf(template, sizeof(template), "%s/%sXXXXXX", 
             dir ? dir : "/tmp", 
             prefix ? prefix : "msh");
    
    int fd = mkstemp(template);
    if (fd < 0) return 0;
    close(fd);
    
    MSH_SAFE_STRCPY(buf, template, size);
    return 1;
}

/* ============================================================================
 * Process Management
 * ============================================================================ */

int msh_spawn_process(const char *cmdline, msh_process_t *proc, int background) {
    if (!cmdline || !proc) return 0;
    
    memset(proc, 0, sizeof(msh_process_t));
    
    pid_t pid = fork();
    
    if (pid < 0) {
        /* Fork failed */
        return 0;
    } else if (pid == 0) {
        /* Child process */
        if (background) {
            /* Detach from controlling terminal for background process */
            setsid();
        }
        
        /* Execute via shell */
        execl("/bin/sh", "sh", "-c", cmdline, (char *)NULL);
        _exit(127);  /* exec failed */
    } else {
        /* Parent process */
        proc->pid = pid;
        proc->handle = pid;
        proc->thread_handle = -1;
        proc->running = 1;
        
        if (!background) {
            /* Wait for foreground process */
            int status;
            waitpid(pid, &status, 0);
            proc->running = 0;
        }
        
        return 1;
    }
}

int msh_wait_process(msh_process_t *proc, int timeout_ms) {
    if (!proc) return 0;
    
    if (timeout_ms < 0) {
        /* Infinite wait */
        int status;
        pid_t result = waitpid(proc->pid, &status, 0);
        if (result == proc->pid) {
            proc->running = 0;
            return 1;
        }
        return 0;
    } else if (timeout_ms == 0) {
        /* Non-blocking check */
        int status;
        pid_t result = waitpid(proc->pid, &status, WNOHANG);
        if (result == proc->pid) {
            proc->running = 0;
            return 1;
        }
        return (result == 0) ? 0 : -1;
    } else {
        /* Timeout wait - poll with small intervals */
        int elapsed = 0;
        while (elapsed < timeout_ms) {
            int status;
            pid_t result = waitpid(proc->pid, &status, WNOHANG);
            if (result == proc->pid) {
                proc->running = 0;
                return 1;
            } else if (result < 0) {
                return -1;
            }
            usleep(10000);  /* 10ms */
            elapsed += 10;
        }
        return 0;  /* Timeout */
    }
}

int msh_get_exit_code(msh_process_t *proc, int *exit_code) {
    if (!proc || !exit_code) return 0;
    
    int status;
    pid_t result = waitpid(proc->pid, &status, WNOHANG);
    
    if (result == proc->pid) {
        if (WIFEXITED(status)) {
            *exit_code = WEXITSTATUS(status);
            return 1;
        } else if (WIFSIGNALED(status)) {
            *exit_code = 128 + WTERMSIG(status);
            return 1;
        }
    }
    
    return 0;
}

int msh_terminate_process(msh_process_t *proc, int signal_num) {
    if (!proc) return 0;
    
    if (signal_num == 0) signal_num = SIGTERM;
    
    if (kill(proc->pid, signal_num) == 0) {
        return 1;
    }
    return 0;
}

int msh_resume_process(msh_process_t *proc) {
    if (!proc) return 0;
    return kill(proc->pid, SIGCONT) == 0 ? 1 : 0;
}

void msh_close_process(msh_process_t *proc) {
    if (proc) {
        /* Reap zombie if still around */
        int status;
        waitpid(proc->pid, &status, WNOHANG);
        proc->handle = MSH_INVALID_HANDLE;
        proc->pid = 0;
    }
}

msh_pid_t msh_getpid(void) {
    return getpid();
}

int msh_is_process_running(msh_process_t *proc) {
    if (!proc) return 0;
    
    int status;
    pid_t result = waitpid(proc->pid, &status, WNOHANG);
    
    if (result == 0) {
        return 1;  /* Still running */
    } else if (result == proc->pid) {
        proc->running = 0;
        return 0;  /* Finished */
    }
    
    return 0;  /* Error or not found */
}

/* ============================================================================
 * System Information
 * ============================================================================ */

int msh_get_username(char *buf, size_t size) {
    struct passwd *pw = getpwuid(getuid());
    if (pw && pw->pw_name) {
        MSH_SAFE_STRCPY(buf, pw->pw_name, size);
        return 1;
    }
    
    /* Fallback to environment */
    const char *user = getenv("USER");
    if (user) {
        MSH_SAFE_STRCPY(buf, user, size);
        return 1;
    }
    
    return 0;
}

int msh_get_hostname(char *buf, size_t size) {
    if (gethostname(buf, size) == 0) {
        buf[size - 1] = '\0';
        return 1;
    }
    return 0;
}

int msh_is_admin(void) {
    return getuid() == 0 ? 1 : 0;
}

int msh_get_home_dir(char *buf, size_t size) {
    const char *home = getenv("HOME");
    if (home) {
        MSH_SAFE_STRCPY(buf, home, size);
        return 1;
    }
    
    struct passwd *pw = getpwuid(getuid());
    if (pw && pw->pw_dir) {
        MSH_SAFE_STRCPY(buf, pw->pw_dir, size);
        return 1;
    }
    
    return 0;
}

/* ============================================================================
 * Console/Terminal
 * ============================================================================ */

int msh_is_interactive(void) {
    return isatty(STDIN_FILENO) ? 1 : 0;
}

void msh_clear_screen(void) {
    /* Use ANSI escape sequence */
    printf("\033[2J\033[H");
    fflush(stdout);
}

/* ============================================================================
 * Shell Execution
 * ============================================================================ */

int msh_system(const char *cmd) {
    return system(cmd);
}

int msh_shell_execute(const char *cmd, const char *args, int elevated) {
    char cmdline[4096];
    
    if (elevated && getuid() != 0) {
        /* Try to use sudo for elevation */
        if (args && args[0]) {
            snprintf(cmdline, sizeof(cmdline), "sudo %s %s", cmd, args);
        } else {
            snprintf(cmdline, sizeof(cmdline), "sudo %s", cmd);
        }
    } else {
        if (args && args[0]) {
            snprintf(cmdline, sizeof(cmdline), "%s %s", cmd, args);
        } else {
            snprintf(cmdline, sizeof(cmdline), "%s", cmd);
        }
    }
    
    return system(cmdline);
}

/* ============================================================================
 * Wildcard Matching
 * ============================================================================ */

int msh_wildcard_match(const char *pattern, const char *str) {
    return fnmatch(pattern, str, FNM_NOESCAPE) == 0 ? 1 : 0;
}

/* ============================================================================
 * Module/Executable Path
 * ============================================================================ */

int msh_get_executable_path(char *buf, size_t size) {
#ifdef __linux__
    ssize_t len = readlink("/proc/self/exe", buf, size - 1);
    if (len > 0) {
        buf[len] = '\0';
        return 1;
    }
#elif defined(__APPLE__)
    uint32_t bufsize = (uint32_t)size;
    if (_NSGetExecutablePath(buf, &bufsize) == 0) {
        return 1;
    }
#endif
    
    /* Fallback: try to find in PATH or use argv[0] */
    return 0;
}

#endif /* MSH_PLATFORM_POSIX */
