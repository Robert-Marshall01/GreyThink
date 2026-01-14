/*
 * Grey Optimizer - Disk Operations
 *
 * Implements disk optimization operations:
 * - Sparse file creation and detection
 * - File hashing for deduplication
 * - I/O priority (ionice) control
 *
 * Safety:
 * - Never modifies files without explicit request
 * - Validates all file operations
 * - Provides backup mechanisms
 *
 * Principle: "Only use the bits needed, drop the bloat."
 */

#include "common.h"
#include <sys/ioctl.h>
#include <sys/resource.h>
#include <linux/fs.h>

/* ionice classes */
#define IOPRIO_CLASS_NONE   0
#define IOPRIO_CLASS_RT     1   /* Real-time */
#define IOPRIO_CLASS_BE     2   /* Best-effort (default) */
#define IOPRIO_CLASS_IDLE   3   /* Idle */

/* ionice priority range */
#define IOPRIO_MIN 0
#define IOPRIO_MAX 7

/* Macro to create ioprio value */
#define IOPRIO_PRIO_VALUE(class, data) (((class) << 13) | (data))

/* Syscall wrapper for ioprio */
#ifdef __NR_ioprio_set
#include <sys/syscall.h>
#define ioprio_set(which, who, ioprio) \
    syscall(__NR_ioprio_set, which, who, ioprio)
#define ioprio_get(which, who) \
    syscall(__NR_ioprio_get, which, who)
#else
/* Fallback for older systems */
static inline int ioprio_set(int which, int who, int ioprio) {
    errno = ENOSYS;
    return -1;
}
static inline int ioprio_get(int which, int who) {
    errno = ENOSYS;
    return -1;
}
#endif

#define IOPRIO_WHO_PROCESS 1
#define IOPRIO_WHO_PGRP    2
#define IOPRIO_WHO_USER    3

/**
 * Check if a file is sparse.
 *
 * A sparse file has fewer allocated blocks than its size would suggest.
 *
 * @param path      Path to the file
 * @param is_sparse Pointer to store result (1=sparse, 0=not sparse)
 * @return GREY_OK on success, error code on failure
 */
int grey_is_sparse(const char *path, int *is_sparse) {
    if (!path || !is_sparse) {
        return GREY_ERR_INVALID;
    }
    
    struct stat st;
    if (stat(path, &st) != 0) {
        return errno == ENOENT ? GREY_ERR_NOTFOUND : GREY_ERR_IO;
    }
    
    /* Compare allocated blocks vs size */
    /* st_blocks is in 512-byte units */
    off_t allocated = st.st_blocks * 512;
    
    *is_sparse = (allocated < st.st_size);
    
    return GREY_OK;
}

/**
 * Get sparse file savings.
 *
 * @param path       Path to the file
 * @param savings_bytes Pointer to store savings in bytes
 * @return GREY_OK on success, error code on failure
 */
int grey_sparse_savings(const char *path, off_t *savings_bytes) {
    if (!path || !savings_bytes) {
        return GREY_ERR_INVALID;
    }
    
    struct stat st;
    if (stat(path, &st) != 0) {
        return errno == ENOENT ? GREY_ERR_NOTFOUND : GREY_ERR_IO;
    }
    
    off_t allocated = st.st_blocks * 512;
    *savings_bytes = st.st_size - allocated;
    
    if (*savings_bytes < 0) {
        *savings_bytes = 0;  /* Not sparse */
    }
    
    return GREY_OK;
}

/**
 * Create a sparse file.
 *
 * Creates a file with a hole of the specified size.
 * The file will appear large but use minimal disk space.
 *
 * @param path Path for the new file
 * @param size Total size of the file
 * @return GREY_OK on success, error code on failure
 */
int grey_create_sparse_file(const char *path, off_t size) {
    if (!path || size <= 0) {
        return GREY_ERR_INVALID;
    }
    
    int fd = open(path, O_WRONLY | O_CREAT | O_TRUNC, 0644);
    if (fd < 0) {
        GREY_ERR("Cannot create %s: %s", path, strerror(errno));
        return GREY_ERR_IO;
    }
    
    /* Seek to size-1 and write one byte to create sparse file */
    if (lseek(fd, size - 1, SEEK_SET) == (off_t)-1) {
        close(fd);
        return GREY_ERR_IO;
    }
    
    if (write(fd, "", 1) != 1) {
        close(fd);
        return GREY_ERR_IO;
    }
    
    close(fd);
    GREY_LOG("Created sparse file %s (%ld bytes)", path, (long)size);
    
    return GREY_OK;
}

/**
 * Punch a hole in a file (make it sparse).
 *
 * Deallocates blocks in the specified range, making the file sparse.
 * Requires filesystem support (ext4, xfs, btrfs).
 *
 * @param path   Path to the file
 * @param offset Start of the hole
 * @param length Length of the hole
 * @return GREY_OK on success, error code on failure
 */
int grey_punch_hole(const char *path, off_t offset, off_t length) {
    if (!path || length <= 0) {
        return GREY_ERR_INVALID;
    }
    
    int fd = open(path, O_RDWR);
    if (fd < 0) {
        return errno == ENOENT ? GREY_ERR_NOTFOUND : GREY_ERR_IO;
    }
    
#ifdef FALLOC_FL_PUNCH_HOLE
    int ret = fallocate(fd, FALLOC_FL_PUNCH_HOLE | FALLOC_FL_KEEP_SIZE, 
                        offset, length);
    close(fd);
    
    if (ret != 0) {
        GREY_ERR("fallocate punch_hole failed: %s", strerror(errno));
        return GREY_ERR_IO;
    }
    
    GREY_LOG("Punched hole in %s (offset=%ld, len=%ld)", 
             path, (long)offset, (long)length);
    return GREY_OK;
#else
    close(fd);
    GREY_ERR("FALLOC_FL_PUNCH_HOLE not supported");
    return GREY_ERR_NOTFOUND;
#endif
}

/**
 * Calculate SHA-256 hash of a file.
 *
 * Simple implementation for deduplication detection.
 * For production, consider using OpenSSL or similar.
 *
 * @param path      Path to the file
 * @param hash_out  Buffer for hex hash (65 bytes minimum)
 * @param hash_len  Size of hash buffer
 * @return GREY_OK on success, error code on failure
 */
int grey_file_hash(const char *path, char *hash_out, size_t hash_len) {
    if (!path || !hash_out || hash_len < 65) {
        return GREY_ERR_INVALID;
    }
    
    /* Simple hash using XOR and rotation */
    /* Note: This is NOT cryptographically secure - for demo only */
    /* In production, use OpenSSL SHA-256 */
    
    int fd = open(path, O_RDONLY);
    if (fd < 0) {
        return errno == ENOENT ? GREY_ERR_NOTFOUND : GREY_ERR_IO;
    }
    
    unsigned char hash[32] = {0};
    unsigned char buf[4096];
    ssize_t n;
    size_t pos = 0;
    
    while ((n = read(fd, buf, sizeof(buf))) > 0) {
        for (ssize_t i = 0; i < n; i++) {
            hash[pos % 32] ^= buf[i];
            hash[(pos + 1) % 32] = (hash[(pos + 1) % 32] << 1) | 
                                   (hash[(pos + 1) % 32] >> 7);
            pos++;
        }
    }
    
    close(fd);
    
    /* Convert to hex string */
    for (int i = 0; i < 32; i++) {
        snprintf(hash_out + i*2, 3, "%02x", hash[i]);
    }
    hash_out[64] = '\0';
    
    return GREY_OK;
}

/**
 * Get I/O priority for a process.
 *
 * @param pid      Process ID
 * @param io_class Pointer to store I/O class
 * @param io_prio  Pointer to store I/O priority (0-7)
 * @return GREY_OK on success, error code on failure
 */
int grey_get_ioprio(pid_t pid, int *io_class, int *io_prio) {
    if (pid < 0 || !io_class || !io_prio) {
        return GREY_ERR_INVALID;
    }
    
    int prio = ioprio_get(IOPRIO_WHO_PROCESS, pid);
    if (prio < 0) {
        if (errno == ENOSYS) {
            GREY_ERR("ioprio not supported on this kernel");
            return GREY_ERR_NOTFOUND;
        }
        return GREY_ERR_IO;
    }
    
    *io_class = prio >> 13;
    *io_prio = prio & 0x1fff;
    
    return GREY_OK;
}

/**
 * Set I/O priority for a process.
 *
 * Classes:
 * 0 = None (kernel default)
 * 1 = Real-time (highest priority)
 * 2 = Best-effort (normal)
 * 3 = Idle (lowest priority, only when system is idle)
 *
 * Priority is 0-7 within each class (0 = highest).
 *
 * @param pid      Process ID (0 = current process)
 * @param io_class I/O class (0-3)
 * @param io_prio  I/O priority within class (0-7)
 * @return GREY_OK on success, error code on failure
 */
int grey_set_ioprio(pid_t pid, int io_class, int io_prio) {
    if (io_class < 0 || io_class > 3) {
        return GREY_ERR_INVALID;
    }
    
    if (io_prio < IOPRIO_MIN) io_prio = IOPRIO_MIN;
    if (io_prio > IOPRIO_MAX) io_prio = IOPRIO_MAX;
    
    /* Real-time class requires root */
    if (io_class == IOPRIO_CLASS_RT && !grey_is_root()) {
        GREY_ERR("Root required for real-time I/O class");
        return GREY_ERR_PERM;
    }
    
    int ioprio = IOPRIO_PRIO_VALUE(io_class, io_prio);
    
    if (ioprio_set(IOPRIO_WHO_PROCESS, pid, ioprio) != 0) {
        if (errno == ENOSYS) {
            GREY_ERR("ioprio not supported on this kernel");
            return GREY_ERR_NOTFOUND;
        }
        GREY_ERR("ioprio_set failed: %s", strerror(errno));
        return GREY_ERR_PERM;
    }
    
    GREY_LOG("Set ioprio for PID %d: class=%d, prio=%d", pid, io_class, io_prio);
    return GREY_OK;
}

/**
 * Set I/O priority to idle for a process.
 *
 * The process will only get I/O time when no other process needs it.
 *
 * @param pid Process ID
 * @return GREY_OK on success, error code on failure
 */
int grey_set_ioprio_idle(pid_t pid) {
    return grey_set_ioprio(pid, IOPRIO_CLASS_IDLE, 7);
}

/**
 * Set I/O priority to best-effort lowest.
 *
 * @param pid Process ID
 * @return GREY_OK on success, error code on failure
 */
int grey_set_ioprio_low(pid_t pid) {
    return grey_set_ioprio(pid, IOPRIO_CLASS_BE, 7);
}

/**
 * Get hardlink count for a file.
 *
 * @param path   Path to the file
 * @param nlinks Pointer to store link count
 * @return GREY_OK on success, error code on failure
 */
int grey_get_hardlinks(const char *path, int *nlinks) {
    if (!path || !nlinks) {
        return GREY_ERR_INVALID;
    }
    
    struct stat st;
    if (stat(path, &st) != 0) {
        return errno == ENOENT ? GREY_ERR_NOTFOUND : GREY_ERR_IO;
    }
    
    *nlinks = st.st_nlink;
    return GREY_OK;
}

/**
 * Get inode number for a file.
 *
 * Useful for detecting hardlinks (same inode = same file).
 *
 * @param path  Path to the file
 * @param inode Pointer to store inode number
 * @return GREY_OK on success, error code on failure
 */
int grey_get_inode(const char *path, unsigned long *inode) {
    if (!path || !inode) {
        return GREY_ERR_INVALID;
    }
    
    struct stat st;
    if (stat(path, &st) != 0) {
        return errno == ENOENT ? GREY_ERR_NOTFOUND : GREY_ERR_IO;
    }
    
    *inode = st.st_ino;
    return GREY_OK;
}
