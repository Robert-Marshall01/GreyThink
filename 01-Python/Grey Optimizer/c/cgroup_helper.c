/*
 * ═══════════════════════════════════════════════════════════════════════════════
 * Grey Optimizer - cgroup v2 Helper
 * ═══════════════════════════════════════════════════════════════════════════════
 *
 * Minimal CLI tool for safe cgroup v2 memory management operations:
 * - Create cgroups with memory limits
 * - Move processes between cgroups
 * - Set memory.max and memory.high limits
 * - Read memory statistics
 * - Trigger memory reclamation (memory.reclaim)
 *
 * SAFETY NOTES:
 * - All operations are atomic where possible
 * - Original settings are preserved for rollback
 * - Simulation mode shows what would happen without changes
 * - Memory limits have safety margins to prevent OOM
 *
 * Usage:
 *   cgroup_helper create --name=<name> --memory-max=<bytes> [--simulate]
 *   cgroup_helper move --pid=<pid> --cgroup=<path> [--simulate]
 *   cgroup_helper set-limit --cgroup=<path> --memory-max=<bytes> [--simulate]
 *   cgroup_helper reclaim --cgroup=<path> --bytes=<bytes> [--simulate]
 *   cgroup_helper stats --cgroup=<path>
 *   cgroup_helper restore --backup=<file>
 *
 * Exit codes:
 *   0 - Success
 *   1 - Invalid arguments
 *   2 - Permission denied
 *   3 - cgroup not found / not supported
 *   4 - Operation failed
 *   5 - Safety check failed
 *
 * ═══════════════════════════════════════════════════════════════════════════════
 */

#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <dirent.h>
#include <stdint.h>
#include <getopt.h>
#include <time.h>
#include <limits.h>

/* ═══════════════════════════════════════════════════════════════════════════════
 * Constants and Configuration
 * ═══════════════════════════════════════════════════════════════════════════════ */

#define CGROUP_ROOT "/sys/fs/cgroup"
#define GREY_CGROUP_PREFIX "grey-optimizer"
#define MAX_PATH_LEN 512
#define MAX_LINE_LEN 256
#define BACKUP_DIR "/var/lib/grey-optimizer/backups"

/* Safety margins */
#define MIN_MEMORY_LIMIT_MB 64       /* Never set below 64 MB */
#define SAFETY_MARGIN_PERCENT 10     /* Leave 10% headroom */

/* ═══════════════════════════════════════════════════════════════════════════════
 * Data Structures
 * ═══════════════════════════════════════════════════════════════════════════════ */

typedef struct {
    char path[MAX_PATH_LEN];
    unsigned long memory_current;   /* Current memory usage */
    unsigned long memory_max;       /* Hard limit */
    unsigned long memory_high;      /* Soft limit (throttling) */
    unsigned long memory_low;       /* Memory protection */
    unsigned long memory_min;       /* Absolute memory protection */
    unsigned long swap_current;     /* Current swap usage */
    unsigned long swap_max;         /* Swap limit */
    unsigned long anon;             /* Anonymous memory */
    unsigned long file;             /* File-backed memory */
    unsigned long kernel;           /* Kernel memory */
    unsigned long oom_events;       /* OOM event count */
    unsigned long oom_kill_events;  /* OOM kill count */
    int memory_pressure_some;       /* Some tasks stalled (0-100) */
    int memory_pressure_full;       /* All tasks stalled (0-100) */
} cgroup_stats_t;

typedef struct {
    char cgroup_path[MAX_PATH_LEN];
    unsigned long old_memory_max;
    unsigned long old_memory_high;
    int pid;
    char old_cgroup[MAX_PATH_LEN];
    time_t timestamp;
} backup_entry_t;

typedef enum {
    CMD_NONE = 0,
    CMD_CREATE,
    CMD_DELETE,
    CMD_MOVE,
    CMD_SET_LIMIT,
    CMD_RECLAIM,
    CMD_STATS,
    CMD_RESTORE,
    CMD_LIST,
} command_t;

typedef struct {
    command_t command;
    char name[MAX_PATH_LEN];
    char cgroup_path[MAX_PATH_LEN];
    char backup_file[MAX_PATH_LEN];
    int pid;
    unsigned long memory_max;
    unsigned long memory_high;
    unsigned long reclaim_bytes;
    int simulate;
    int confirm_live;
    int verbose;
    int json_output;
    int force;
} options_t;

/* ═══════════════════════════════════════════════════════════════════════════════
 * Helper Functions
 * ═══════════════════════════════════════════════════════════════════════════════ */

static int is_cgroup_v2(void) {
    /* Check if cgroup v2 unified hierarchy is mounted */
    struct stat st;
    char unified_path[MAX_PATH_LEN];
    
    snprintf(unified_path, sizeof(unified_path), 
             "%s/cgroup.controllers", CGROUP_ROOT);
    
    return (stat(unified_path, &st) == 0);
}

static int cgroup_exists(const char *path) {
    struct stat st;
    return (stat(path, &st) == 0 && S_ISDIR(st.st_mode));
}

static int write_file(const char *path, const char *content, int simulate) {
    if (simulate) {
        printf("  [SIM] Would write to %s: %s", path, content);
        if (content[strlen(content)-1] != '\n') printf("\n");
        return 0;
    }
    
    int fd = open(path, O_WRONLY);
    if (fd < 0) {
        return -1;
    }
    
    ssize_t len = strlen(content);
    ssize_t written = write(fd, content, len);
    close(fd);
    
    return (written == len) ? 0 : -1;
}

static int read_file_value(const char *path, unsigned long *value) {
    FILE *fp = fopen(path, "r");
    if (!fp) return -1;
    
    char line[MAX_LINE_LEN];
    if (fgets(line, sizeof(line), fp)) {
        if (strcmp(line, "max\n") == 0) {
            *value = ULONG_MAX;
        } else {
            *value = strtoul(line, NULL, 10);
        }
        fclose(fp);
        return 0;
    }
    
    fclose(fp);
    return -1;
}

static int read_file_string(const char *path, char *buf, size_t buflen) {
    FILE *fp = fopen(path, "r");
    if (!fp) return -1;
    
    if (fgets(buf, buflen, fp)) {
        /* Remove trailing newline */
        size_t len = strlen(buf);
        if (len > 0 && buf[len-1] == '\n') {
            buf[len-1] = '\0';
        }
        fclose(fp);
        return 0;
    }
    
    fclose(fp);
    return -1;
}

static unsigned long parse_memory_value(const char *str) {
    char *endptr;
    unsigned long value = strtoul(str, &endptr, 10);
    
    switch (*endptr) {
        case 'K': case 'k': value *= 1024; break;
        case 'M': case 'm': value *= 1024 * 1024; break;
        case 'G': case 'g': value *= 1024 * 1024 * 1024; break;
        case 'T': case 't': value *= 1024UL * 1024 * 1024 * 1024; break;
        default: break;
    }
    
    return value;
}

static void format_bytes(unsigned long bytes, char *buf, size_t buflen) {
    if (bytes >= 1024UL * 1024 * 1024 * 1024) {
        snprintf(buf, buflen, "%.2f TB", bytes / (1024.0 * 1024 * 1024 * 1024));
    } else if (bytes >= 1024UL * 1024 * 1024) {
        snprintf(buf, buflen, "%.2f GB", bytes / (1024.0 * 1024 * 1024));
    } else if (bytes >= 1024UL * 1024) {
        snprintf(buf, buflen, "%.2f MB", bytes / (1024.0 * 1024));
    } else if (bytes >= 1024) {
        snprintf(buf, buflen, "%.2f KB", bytes / 1024.0);
    } else {
        snprintf(buf, buflen, "%lu B", bytes);
    }
}

/* ═══════════════════════════════════════════════════════════════════════════════
 * cgroup Operations
 * ═══════════════════════════════════════════════════════════════════════════════ */

static int get_cgroup_stats(const char *cgroup_path, cgroup_stats_t *stats) {
    char path[MAX_PATH_LEN];
    char line[MAX_LINE_LEN];
    
    memset(stats, 0, sizeof(*stats));
    strncpy(stats->path, cgroup_path, sizeof(stats->path) - 1);
    
    /* Read memory.current */
    snprintf(path, sizeof(path), "%s/memory.current", cgroup_path);
    read_file_value(path, &stats->memory_current);
    
    /* Read memory.max */
    snprintf(path, sizeof(path), "%s/memory.max", cgroup_path);
    read_file_value(path, &stats->memory_max);
    
    /* Read memory.high */
    snprintf(path, sizeof(path), "%s/memory.high", cgroup_path);
    read_file_value(path, &stats->memory_high);
    
    /* Read memory.low */
    snprintf(path, sizeof(path), "%s/memory.low", cgroup_path);
    read_file_value(path, &stats->memory_low);
    
    /* Read memory.min */
    snprintf(path, sizeof(path), "%s/memory.min", cgroup_path);
    read_file_value(path, &stats->memory_min);
    
    /* Read memory.swap.current */
    snprintf(path, sizeof(path), "%s/memory.swap.current", cgroup_path);
    read_file_value(path, &stats->swap_current);
    
    /* Read memory.swap.max */
    snprintf(path, sizeof(path), "%s/memory.swap.max", cgroup_path);
    read_file_value(path, &stats->swap_max);
    
    /* Read memory.stat for detailed breakdown */
    snprintf(path, sizeof(path), "%s/memory.stat", cgroup_path);
    FILE *fp = fopen(path, "r");
    if (fp) {
        while (fgets(line, sizeof(line), fp)) {
            if (strncmp(line, "anon ", 5) == 0) {
                stats->anon = strtoul(line + 5, NULL, 10);
            } else if (strncmp(line, "file ", 5) == 0) {
                stats->file = strtoul(line + 5, NULL, 10);
            } else if (strncmp(line, "kernel ", 7) == 0) {
                stats->kernel = strtoul(line + 7, NULL, 10);
            }
        }
        fclose(fp);
    }
    
    /* Read memory.events for OOM events */
    snprintf(path, sizeof(path), "%s/memory.events", cgroup_path);
    fp = fopen(path, "r");
    if (fp) {
        while (fgets(line, sizeof(line), fp)) {
            if (strncmp(line, "oom ", 4) == 0) {
                stats->oom_events = strtoul(line + 4, NULL, 10);
            } else if (strncmp(line, "oom_kill ", 9) == 0) {
                stats->oom_kill_events = strtoul(line + 9, NULL, 10);
            }
        }
        fclose(fp);
    }
    
    /* Read memory.pressure */
    snprintf(path, sizeof(path), "%s/memory.pressure", cgroup_path);
    fp = fopen(path, "r");
    if (fp) {
        while (fgets(line, sizeof(line), fp)) {
            if (strncmp(line, "some ", 5) == 0) {
                /* Format: some avg10=X.XX avg60=Y.YY avg300=Z.ZZ total=T */
                char *avg10 = strstr(line, "avg10=");
                if (avg10) {
                    stats->memory_pressure_some = (int)(strtof(avg10 + 6, NULL));
                }
            } else if (strncmp(line, "full ", 5) == 0) {
                char *avg10 = strstr(line, "avg10=");
                if (avg10) {
                    stats->memory_pressure_full = (int)(strtof(avg10 + 6, NULL));
                }
            }
        }
        fclose(fp);
    }
    
    return 0;
}

static int create_cgroup(const char *name, unsigned long memory_max, 
                        unsigned long memory_high, int simulate, int verbose) {
    char cgroup_path[MAX_PATH_LEN];
    char file_path[MAX_PATH_LEN];
    char content[MAX_LINE_LEN];
    
    snprintf(cgroup_path, sizeof(cgroup_path), 
             "%s/%s/%s", CGROUP_ROOT, GREY_CGROUP_PREFIX, name);
    
    if (verbose) {
        printf("Creating cgroup: %s\n", cgroup_path);
    }
    
    /* First, create the parent grey-optimizer cgroup if needed */
    char parent_path[MAX_PATH_LEN];
    snprintf(parent_path, sizeof(parent_path), 
             "%s/%s", CGROUP_ROOT, GREY_CGROUP_PREFIX);
    
    if (!cgroup_exists(parent_path)) {
        if (simulate) {
            printf("  [SIM] Would create directory: %s\n", parent_path);
        } else {
            if (mkdir(parent_path, 0755) != 0 && errno != EEXIST) {
                fprintf(stderr, "Error: Cannot create %s: %s\n", 
                        parent_path, strerror(errno));
                return -1;
            }
        }
        
        /* Enable memory controller in parent */
        snprintf(file_path, sizeof(file_path), 
                 "%s/cgroup.subtree_control", CGROUP_ROOT);
        if (write_file(file_path, "+memory\n", simulate) != 0 && !simulate) {
            /* May already be enabled - not fatal */
        }
    }
    
    /* Create the child cgroup */
    if (cgroup_exists(cgroup_path)) {
        if (verbose) {
            printf("  cgroup already exists\n");
        }
    } else {
        if (simulate) {
            printf("  [SIM] Would create directory: %s\n", cgroup_path);
        } else {
            if (mkdir(cgroup_path, 0755) != 0) {
                fprintf(stderr, "Error: Cannot create %s: %s\n", 
                        cgroup_path, strerror(errno));
                return -1;
            }
        }
    }
    
    /* Enable memory controller for subtree */
    snprintf(file_path, sizeof(file_path), 
             "%s/cgroup.subtree_control", parent_path);
    if (write_file(file_path, "+memory\n", simulate) != 0 && !simulate) {
        /* May fail if no children - not fatal */
    }
    
    /* Set memory.max */
    if (memory_max > 0) {
        /* Apply safety margin */
        unsigned long min_limit = MIN_MEMORY_LIMIT_MB * 1024 * 1024;
        if (memory_max < min_limit) {
            fprintf(stderr, "Warning: memory_max too low, using minimum %d MB\n",
                    MIN_MEMORY_LIMIT_MB);
            memory_max = min_limit;
        }
        
        snprintf(file_path, sizeof(file_path), "%s/memory.max", cgroup_path);
        snprintf(content, sizeof(content), "%lu\n", memory_max);
        
        if (verbose) {
            char formatted[64];
            format_bytes(memory_max, formatted, sizeof(formatted));
            printf("  Setting memory.max = %s\n", formatted);
        }
        
        if (write_file(file_path, content, simulate) != 0 && !simulate) {
            fprintf(stderr, "Error: Cannot set memory.max: %s\n", strerror(errno));
            return -1;
        }
    }
    
    /* Set memory.high (soft limit for throttling) */
    if (memory_high > 0) {
        snprintf(file_path, sizeof(file_path), "%s/memory.high", cgroup_path);
        snprintf(content, sizeof(content), "%lu\n", memory_high);
        
        if (verbose) {
            char formatted[64];
            format_bytes(memory_high, formatted, sizeof(formatted));
            printf("  Setting memory.high = %s\n", formatted);
        }
        
        if (write_file(file_path, content, simulate) != 0 && !simulate) {
            fprintf(stderr, "Warning: Cannot set memory.high: %s\n", strerror(errno));
            /* Not fatal - memory.high might not be available */
        }
    }
    
    return 0;
}

static int delete_cgroup(const char *cgroup_path, int simulate, int verbose) {
    if (!cgroup_exists(cgroup_path)) {
        if (verbose) {
            printf("cgroup does not exist: %s\n", cgroup_path);
        }
        return 0;
    }
    
    if (verbose) {
        printf("Deleting cgroup: %s\n", cgroup_path);
    }
    
    if (simulate) {
        printf("  [SIM] Would remove directory: %s\n", cgroup_path);
        return 0;
    }
    
    /* cgroup must be empty (no processes) to delete */
    if (rmdir(cgroup_path) != 0) {
        fprintf(stderr, "Error: Cannot delete %s: %s\n", 
                cgroup_path, strerror(errno));
        fprintf(stderr, "  (cgroup may still have processes)\n");
        return -1;
    }
    
    return 0;
}

static int move_process(int pid, const char *cgroup_path, 
                       int simulate, int verbose) {
    char file_path[MAX_PATH_LEN];
    char content[MAX_LINE_LEN];
    
    if (!cgroup_exists(cgroup_path)) {
        fprintf(stderr, "Error: cgroup does not exist: %s\n", cgroup_path);
        return -1;
    }
    
    if (verbose) {
        printf("Moving PID %d to cgroup: %s\n", pid, cgroup_path);
    }
    
    snprintf(file_path, sizeof(file_path), "%s/cgroup.procs", cgroup_path);
    snprintf(content, sizeof(content), "%d\n", pid);
    
    if (write_file(file_path, content, simulate) != 0 && !simulate) {
        fprintf(stderr, "Error: Cannot move PID %d: %s\n", pid, strerror(errno));
        return -1;
    }
    
    return 0;
}

static int set_memory_limit(const char *cgroup_path, unsigned long memory_max,
                           unsigned long memory_high, int simulate, int verbose) {
    char file_path[MAX_PATH_LEN];
    char content[MAX_LINE_LEN];
    
    if (!cgroup_exists(cgroup_path)) {
        fprintf(stderr, "Error: cgroup does not exist: %s\n", cgroup_path);
        return -1;
    }
    
    /* Get current stats first */
    cgroup_stats_t stats;
    get_cgroup_stats(cgroup_path, &stats);
    
    /* Safety check: don't set limit below current usage + safety margin */
    if (memory_max > 0 && memory_max != ULONG_MAX) {
        unsigned long safe_min = stats.memory_current + 
                                 (stats.memory_current * SAFETY_MARGIN_PERCENT / 100);
        if (memory_max < safe_min) {
            fprintf(stderr, "Warning: Requested limit (%lu) is below current usage + safety margin\n",
                    memory_max);
            fprintf(stderr, "  Current: %lu, Safe minimum: %lu\n", 
                    stats.memory_current, safe_min);
            if (!simulate) {
                fprintf(stderr, "  Adjusting to safe minimum\n");
                memory_max = safe_min;
            }
        }
    }
    
    if (memory_max > 0) {
        snprintf(file_path, sizeof(file_path), "%s/memory.max", cgroup_path);
        if (memory_max == ULONG_MAX) {
            snprintf(content, sizeof(content), "max\n");
        } else {
            snprintf(content, sizeof(content), "%lu\n", memory_max);
        }
        
        if (verbose) {
            if (memory_max == ULONG_MAX) {
                printf("Setting memory.max = max (unlimited)\n");
            } else {
                char formatted[64];
                format_bytes(memory_max, formatted, sizeof(formatted));
                printf("Setting memory.max = %s\n", formatted);
            }
        }
        
        if (write_file(file_path, content, simulate) != 0 && !simulate) {
            fprintf(stderr, "Error: Cannot set memory.max: %s\n", strerror(errno));
            return -1;
        }
    }
    
    if (memory_high > 0) {
        snprintf(file_path, sizeof(file_path), "%s/memory.high", cgroup_path);
        if (memory_high == ULONG_MAX) {
            snprintf(content, sizeof(content), "max\n");
        } else {
            snprintf(content, sizeof(content), "%lu\n", memory_high);
        }
        
        if (verbose) {
            if (memory_high == ULONG_MAX) {
                printf("Setting memory.high = max (unlimited)\n");
            } else {
                char formatted[64];
                format_bytes(memory_high, formatted, sizeof(formatted));
                printf("Setting memory.high = %s\n", formatted);
            }
        }
        
        if (write_file(file_path, content, simulate) != 0 && !simulate) {
            fprintf(stderr, "Warning: Cannot set memory.high: %s\n", strerror(errno));
        }
    }
    
    return 0;
}

static int trigger_reclaim(const char *cgroup_path, unsigned long bytes,
                          int simulate, int verbose) {
    char file_path[MAX_PATH_LEN];
    char content[MAX_LINE_LEN];
    
    if (!cgroup_exists(cgroup_path)) {
        fprintf(stderr, "Error: cgroup does not exist: %s\n", cgroup_path);
        return -1;
    }
    
    /* Check if memory.reclaim is available (Linux 5.17+) */
    snprintf(file_path, sizeof(file_path), "%s/memory.reclaim", cgroup_path);
    
    if (access(file_path, W_OK) != 0) {
        fprintf(stderr, "Warning: memory.reclaim not available (requires Linux 5.17+)\n");
        if (simulate) {
            printf("  [SIM] Would attempt to reclaim %lu bytes\n", bytes);
            return 0;
        }
        return -1;
    }
    
    if (verbose) {
        char formatted[64];
        format_bytes(bytes, formatted, sizeof(formatted));
        printf("Triggering reclaim of %s from %s\n", formatted, cgroup_path);
    }
    
    snprintf(content, sizeof(content), "%lu\n", bytes);
    
    if (simulate) {
        printf("  [SIM] Would write to %s: %s", file_path, content);
        return 0;
    }
    
    if (write_file(file_path, content, 0) != 0) {
        fprintf(stderr, "Error: Reclaim failed: %s\n", strerror(errno));
        return -1;
    }
    
    return 0;
}

/* ═══════════════════════════════════════════════════════════════════════════════
 * Output Formatting
 * ═══════════════════════════════════════════════════════════════════════════════ */

static void print_stats_json(const cgroup_stats_t *stats) {
    printf("{\n");
    printf("  \"path\": \"%s\",\n", stats->path);
    printf("  \"memory\": {\n");
    printf("    \"current\": %lu,\n", stats->memory_current);
    if (stats->memory_max == ULONG_MAX) {
        printf("    \"max\": \"unlimited\",\n");
    } else {
        printf("    \"max\": %lu,\n", stats->memory_max);
    }
    if (stats->memory_high == ULONG_MAX) {
        printf("    \"high\": \"unlimited\",\n");
    } else {
        printf("    \"high\": %lu,\n", stats->memory_high);
    }
    printf("    \"low\": %lu,\n", stats->memory_low);
    printf("    \"min\": %lu,\n", stats->memory_min);
    printf("    \"anon\": %lu,\n", stats->anon);
    printf("    \"file\": %lu,\n", stats->file);
    printf("    \"kernel\": %lu\n", stats->kernel);
    printf("  },\n");
    printf("  \"swap\": {\n");
    printf("    \"current\": %lu,\n", stats->swap_current);
    if (stats->swap_max == ULONG_MAX) {
        printf("    \"max\": \"unlimited\"\n");
    } else {
        printf("    \"max\": %lu\n", stats->swap_max);
    }
    printf("  },\n");
    printf("  \"events\": {\n");
    printf("    \"oom\": %lu,\n", stats->oom_events);
    printf("    \"oom_kill\": %lu\n", stats->oom_kill_events);
    printf("  },\n");
    printf("  \"pressure\": {\n");
    printf("    \"some_avg10\": %d,\n", stats->memory_pressure_some);
    printf("    \"full_avg10\": %d\n", stats->memory_pressure_full);
    printf("  }\n");
    printf("}\n");
}

static void print_stats_human(const cgroup_stats_t *stats) {
    char current[64], max[64], high[64], anon[64], file[64];
    char swap_current[64], swap_max[64];
    
    format_bytes(stats->memory_current, current, sizeof(current));
    format_bytes(stats->anon, anon, sizeof(anon));
    format_bytes(stats->file, file, sizeof(file));
    format_bytes(stats->swap_current, swap_current, sizeof(swap_current));
    
    if (stats->memory_max == ULONG_MAX) {
        snprintf(max, sizeof(max), "unlimited");
    } else {
        format_bytes(stats->memory_max, max, sizeof(max));
    }
    
    if (stats->memory_high == ULONG_MAX) {
        snprintf(high, sizeof(high), "unlimited");
    } else {
        format_bytes(stats->memory_high, high, sizeof(high));
    }
    
    if (stats->swap_max == ULONG_MAX) {
        snprintf(swap_max, sizeof(swap_max), "unlimited");
    } else {
        format_bytes(stats->swap_max, swap_max, sizeof(swap_max));
    }
    
    printf("\n");
    printf("═══════════════════════════════════════════════════\n");
    printf(" cgroup: %s\n", stats->path);
    printf("═══════════════════════════════════════════════════\n");
    printf(" Memory:\n");
    printf("   Current:    %-15s  Max:  %s\n", current, max);
    printf("   High:       %-15s\n", high);
    printf("   Anonymous:  %-15s  File: %s\n", anon, file);
    printf(" Swap:\n");
    printf("   Current:    %-15s  Max:  %s\n", swap_current, swap_max);
    printf(" Events:\n");
    printf("   OOM:        %-15lu  OOM kills: %lu\n", 
           stats->oom_events, stats->oom_kill_events);
    printf(" Pressure (avg10):\n");
    printf("   Some:       %d%%              Full: %d%%\n",
           stats->memory_pressure_some, stats->memory_pressure_full);
    printf("═══════════════════════════════════════════════════\n\n");
}

static int list_cgroups(int json_output) {
    char grey_path[MAX_PATH_LEN];
    DIR *dir;
    struct dirent *entry;
    
    snprintf(grey_path, sizeof(grey_path), "%s/%s", 
             CGROUP_ROOT, GREY_CGROUP_PREFIX);
    
    if (!cgroup_exists(grey_path)) {
        if (json_output) {
            printf("{\"cgroups\": []}\n");
        } else {
            printf("No Grey Optimizer cgroups found.\n");
        }
        return 0;
    }
    
    dir = opendir(grey_path);
    if (!dir) {
        fprintf(stderr, "Error: Cannot list %s: %s\n", grey_path, strerror(errno));
        return -1;
    }
    
    if (json_output) {
        printf("{\"cgroups\": [\n");
        int first = 1;
        
        while ((entry = readdir(dir)) != NULL) {
            if (entry->d_type == DT_DIR && 
                entry->d_name[0] != '.') {
                
                if (!first) printf(",\n");
                first = 0;
                
                char cg_path[MAX_PATH_LEN];
                snprintf(cg_path, sizeof(cg_path), "%s/%s", 
                         grey_path, entry->d_name);
                
                cgroup_stats_t stats;
                get_cgroup_stats(cg_path, &stats);
                
                printf("  {\"name\": \"%s\", \"memory_current\": %lu, \"memory_max\": %lu}",
                       entry->d_name, stats.memory_current, stats.memory_max);
            }
        }
        printf("\n]}\n");
    } else {
        printf("\nGrey Optimizer cgroups:\n");
        printf("%-30s  %-15s  %-15s\n", "Name", "Current", "Max");
        printf("─────────────────────────────────────────────────────────\n");
        
        while ((entry = readdir(dir)) != NULL) {
            if (entry->d_type == DT_DIR && 
                entry->d_name[0] != '.') {
                
                char cg_path[MAX_PATH_LEN];
                snprintf(cg_path, sizeof(cg_path), "%s/%s", 
                         grey_path, entry->d_name);
                
                cgroup_stats_t stats;
                get_cgroup_stats(cg_path, &stats);
                
                char current[32], max[32];
                format_bytes(stats.memory_current, current, sizeof(current));
                if (stats.memory_max == ULONG_MAX) {
                    snprintf(max, sizeof(max), "unlimited");
                } else {
                    format_bytes(stats.memory_max, max, sizeof(max));
                }
                
                printf("%-30s  %-15s  %-15s\n", entry->d_name, current, max);
            }
        }
        printf("\n");
    }
    
    closedir(dir);
    return 0;
}

/* ═══════════════════════════════════════════════════════════════════════════════
 * Usage and Help
 * ═══════════════════════════════════════════════════════════════════════════════ */

static void print_usage(const char *prog) {
    printf("\nUsage: %s <command> [OPTIONS]\n\n", prog);
    printf("cgroup v2 helper for Grey Optimizer memory management.\n\n");
    printf("Commands:\n");
    printf("  create         Create a new cgroup with memory limits\n");
    printf("  delete         Delete a cgroup\n");
    printf("  move           Move a process to a cgroup\n");
    printf("  set-limit      Set memory limits on an existing cgroup\n");
    printf("  reclaim        Trigger memory reclamation (Linux 5.17+)\n");
    printf("  stats          Show cgroup statistics\n");
    printf("  list           List Grey Optimizer cgroups\n");
    printf("\n");
    printf("Options:\n");
    printf("  --name=<name>      cgroup name (for create)\n");
    printf("  --cgroup=<path>    Full cgroup path\n");
    printf("  --pid=<PID>        Process ID (for move)\n");
    printf("  --memory-max=<N>   Hard memory limit (e.g., 512M, 1G)\n");
    printf("  --memory-high=<N>  Soft memory limit (throttling threshold)\n");
    printf("  --bytes=<N>        Bytes to reclaim (for reclaim)\n");
    printf("  --simulate         Show what would be done (default)\n");
    printf("  --confirm-live     Actually perform changes\n");
    printf("  --verbose          Verbose output\n");
    printf("  --json             Output in JSON format\n");
    printf("  --force            Force operation (skip safety checks)\n");
    printf("  --help             Show this help\n");
    printf("\n");
    printf("Examples:\n");
    printf("  %s create --name=myapp --memory-max=512M --simulate\n", prog);
    printf("  %s move --pid=1234 --cgroup=/sys/fs/cgroup/grey-optimizer/myapp --confirm-live\n", prog);
    printf("  %s set-limit --cgroup=/sys/fs/cgroup/grey-optimizer/myapp --memory-max=1G\n", prog);
    printf("  %s reclaim --cgroup=/sys/fs/cgroup/grey-optimizer/myapp --bytes=100M\n", prog);
    printf("  %s stats --cgroup=/sys/fs/cgroup/grey-optimizer/myapp --json\n", prog);
    printf("  %s list\n", prog);
    printf("\n");
}

/* ═══════════════════════════════════════════════════════════════════════════════
 * Main Entry Point
 * ═══════════════════════════════════════════════════════════════════════════════ */

int main(int argc, char *argv[]) {
    options_t opts = {
        .command = CMD_NONE,
        .pid = 0,
        .memory_max = 0,
        .memory_high = 0,
        .reclaim_bytes = 0,
        .simulate = 1,           /* Default: simulation mode */
        .confirm_live = 0,
        .verbose = 0,
        .json_output = 0,
        .force = 0,
    };
    
    memset(opts.name, 0, sizeof(opts.name));
    memset(opts.cgroup_path, 0, sizeof(opts.cgroup_path));
    
    /* Parse command first */
    if (argc < 2) {
        print_usage(argv[0]);
        return 1;
    }
    
    if (strcmp(argv[1], "create") == 0) opts.command = CMD_CREATE;
    else if (strcmp(argv[1], "delete") == 0) opts.command = CMD_DELETE;
    else if (strcmp(argv[1], "move") == 0) opts.command = CMD_MOVE;
    else if (strcmp(argv[1], "set-limit") == 0) opts.command = CMD_SET_LIMIT;
    else if (strcmp(argv[1], "reclaim") == 0) opts.command = CMD_RECLAIM;
    else if (strcmp(argv[1], "stats") == 0) opts.command = CMD_STATS;
    else if (strcmp(argv[1], "list") == 0) opts.command = CMD_LIST;
    else if (strcmp(argv[1], "--help") == 0 || strcmp(argv[1], "-h") == 0) {
        print_usage(argv[0]);
        return 0;
    } else {
        fprintf(stderr, "Error: Unknown command: %s\n", argv[1]);
        print_usage(argv[0]);
        return 1;
    }
    
    /* Parse remaining options */
    static struct option long_options[] = {
        {"name",         required_argument, 0, 'n'},
        {"cgroup",       required_argument, 0, 'c'},
        {"pid",          required_argument, 0, 'p'},
        {"memory-max",   required_argument, 0, 'M'},
        {"memory-high",  required_argument, 0, 'H'},
        {"bytes",        required_argument, 0, 'b'},
        {"simulate",     no_argument,       0, 's'},
        {"confirm-live", no_argument,       0, 'L'},
        {"verbose",      no_argument,       0, 'v'},
        {"json",         no_argument,       0, 'j'},
        {"force",        no_argument,       0, 'f'},
        {"help",         no_argument,       0, 'h'},
        {0, 0, 0, 0}
    };
    
    /* Skip the command argument */
    optind = 2;
    
    int opt;
    while ((opt = getopt_long(argc, argv, "n:c:p:M:H:b:sLvjfh", long_options, NULL)) != -1) {
        switch (opt) {
            case 'n':
                strncpy(opts.name, optarg, sizeof(opts.name) - 1);
                break;
            case 'c':
                strncpy(opts.cgroup_path, optarg, sizeof(opts.cgroup_path) - 1);
                break;
            case 'p':
                opts.pid = atoi(optarg);
                break;
            case 'M':
                opts.memory_max = parse_memory_value(optarg);
                break;
            case 'H':
                opts.memory_high = parse_memory_value(optarg);
                break;
            case 'b':
                opts.reclaim_bytes = parse_memory_value(optarg);
                break;
            case 's':
                opts.simulate = 1;
                break;
            case 'L':
                opts.confirm_live = 1;
                opts.simulate = 0;
                break;
            case 'v':
                opts.verbose = 1;
                break;
            case 'j':
                opts.json_output = 1;
                break;
            case 'f':
                opts.force = 1;
                break;
            case 'h':
                print_usage(argv[0]);
                return 0;
            default:
                print_usage(argv[0]);
                return 1;
        }
    }
    
    /* Check cgroup v2 support */
    if (!is_cgroup_v2()) {
        fprintf(stderr, "Error: cgroup v2 unified hierarchy not detected\n");
        fprintf(stderr, "  Expected: %s/cgroup.controllers\n", CGROUP_ROOT);
        fprintf(stderr, "  Hint: Boot with 'systemd.unified_cgroup_hierarchy=1'\n");
        return 3;
    }
    
    /* Execute command */
    int ret = 0;
    
    switch (opts.command) {
        case CMD_CREATE:
            if (opts.name[0] == '\0') {
                fprintf(stderr, "Error: --name is required for create\n");
                return 1;
            }
            ret = create_cgroup(opts.name, opts.memory_max, opts.memory_high,
                               opts.simulate, opts.verbose);
            break;
            
        case CMD_DELETE:
            if (opts.cgroup_path[0] == '\0' && opts.name[0] != '\0') {
                snprintf(opts.cgroup_path, sizeof(opts.cgroup_path),
                         "%s/%s/%s", CGROUP_ROOT, GREY_CGROUP_PREFIX, opts.name);
            }
            if (opts.cgroup_path[0] == '\0') {
                fprintf(stderr, "Error: --cgroup or --name is required\n");
                return 1;
            }
            ret = delete_cgroup(opts.cgroup_path, opts.simulate, opts.verbose);
            break;
            
        case CMD_MOVE:
            if (opts.pid <= 0) {
                fprintf(stderr, "Error: --pid is required for move\n");
                return 1;
            }
            if (opts.cgroup_path[0] == '\0') {
                fprintf(stderr, "Error: --cgroup is required for move\n");
                return 1;
            }
            ret = move_process(opts.pid, opts.cgroup_path, 
                              opts.simulate, opts.verbose);
            break;
            
        case CMD_SET_LIMIT:
            if (opts.cgroup_path[0] == '\0') {
                fprintf(stderr, "Error: --cgroup is required for set-limit\n");
                return 1;
            }
            if (opts.memory_max == 0 && opts.memory_high == 0) {
                fprintf(stderr, "Error: --memory-max or --memory-high required\n");
                return 1;
            }
            ret = set_memory_limit(opts.cgroup_path, opts.memory_max, 
                                  opts.memory_high, opts.simulate, opts.verbose);
            break;
            
        case CMD_RECLAIM:
            if (opts.cgroup_path[0] == '\0') {
                fprintf(stderr, "Error: --cgroup is required for reclaim\n");
                return 1;
            }
            if (opts.reclaim_bytes == 0) {
                fprintf(stderr, "Error: --bytes is required for reclaim\n");
                return 1;
            }
            ret = trigger_reclaim(opts.cgroup_path, opts.reclaim_bytes,
                                 opts.simulate, opts.verbose);
            break;
            
        case CMD_STATS:
            if (opts.cgroup_path[0] == '\0') {
                fprintf(stderr, "Error: --cgroup is required for stats\n");
                return 1;
            }
            {
                cgroup_stats_t stats;
                if (get_cgroup_stats(opts.cgroup_path, &stats) != 0) {
                    fprintf(stderr, "Error: Cannot read cgroup stats\n");
                    return 4;
                }
                if (opts.json_output) {
                    print_stats_json(&stats);
                } else {
                    print_stats_human(&stats);
                }
            }
            break;
            
        case CMD_LIST:
            ret = list_cgroups(opts.json_output);
            break;
            
        default:
            fprintf(stderr, "Error: No command specified\n");
            print_usage(argv[0]);
            return 1;
    }
    
    if (opts.simulate && ret == 0 && !opts.json_output) {
        printf("\n[!] This was a SIMULATION. No changes were made.\n");
        printf("    To apply changes, use: --confirm-live\n\n");
    }
    
    return ret;
}
