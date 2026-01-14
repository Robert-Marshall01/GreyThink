/*
 * Grey Optimizer - Common Header
 *
 * Shared definitions for all C enforcement modules.
 * These modules implement low-level Linux system calls
 * that cannot be efficiently done in Python.
 *
 * Safety: All functions validate inputs and return
 * error codes rather than crashing on failure.
 */

#ifndef GREY_OPTIMIZER_COMMON_H
#define GREY_OPTIMIZER_COMMON_H

#define _GNU_SOURCE

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>

/* Error codes */
#define GREY_OK              0
#define GREY_ERR_INVALID    -1
#define GREY_ERR_PERM       -2
#define GREY_ERR_NOTFOUND   -3
#define GREY_ERR_IO         -4
#define GREY_ERR_NOMEM      -5
#define GREY_ERR_BUSY       -6

/* Buffer sizes */
#define GREY_PATH_MAX       4096
#define GREY_SMALL_BUF      256

/* Logging macros */
#ifdef GREY_DEBUG
    #define GREY_LOG(fmt, ...) \
        fprintf(stderr, "[grey] " fmt "\n", ##__VA_ARGS__)
#else
    #define GREY_LOG(fmt, ...) do {} while(0)
#endif

#define GREY_ERR(fmt, ...) \
    fprintf(stderr, "[grey ERROR] " fmt "\n", ##__VA_ARGS__)

/* Utility functions */

/**
 * Safely write a string to a file.
 *
 * @param path  Path to the file
 * @param value String to write
 * @return GREY_OK on success, error code on failure
 */
static inline int grey_write_file(const char *path, const char *value) {
    int fd = open(path, O_WRONLY | O_TRUNC);
    if (fd < 0) {
        GREY_ERR("Cannot open %s: %s", path, strerror(errno));
        return errno == EACCES ? GREY_ERR_PERM : GREY_ERR_IO;
    }

    ssize_t len = strlen(value);
    ssize_t written = write(fd, value, len);
    close(fd);

    if (written != len) {
        GREY_ERR("Write to %s failed: %s", path, strerror(errno));
        return GREY_ERR_IO;
    }

    return GREY_OK;
}

/**
 * Safely read a string from a file.
 *
 * @param path   Path to the file
 * @param buf    Buffer to store the content
 * @param buflen Size of the buffer
 * @return GREY_OK on success, error code on failure
 */
static inline int grey_read_file(const char *path, char *buf, size_t buflen) {
    int fd = open(path, O_RDONLY);
    if (fd < 0) {
        GREY_ERR("Cannot open %s: %s", path, strerror(errno));
        return errno == ENOENT ? GREY_ERR_NOTFOUND : GREY_ERR_IO;
    }

    ssize_t n = read(fd, buf, buflen - 1);
    close(fd);

    if (n < 0) {
        GREY_ERR("Read from %s failed: %s", path, strerror(errno));
        return GREY_ERR_IO;
    }

    buf[n] = '\0';
    /* Strip trailing newline */
    if (n > 0 && buf[n-1] == '\n') {
        buf[n-1] = '\0';
    }

    return GREY_OK;
}

/**
 * Check if a path exists.
 *
 * @param path Path to check
 * @return 1 if exists, 0 otherwise
 */
static inline int grey_path_exists(const char *path) {
    struct stat st;
    return stat(path, &st) == 0;
}

/**
 * Check if running as root.
 *
 * @return 1 if root, 0 otherwise
 */
static inline int grey_is_root(void) {
    return geteuid() == 0;
}

#endif /* GREY_OPTIMIZER_COMMON_H */
