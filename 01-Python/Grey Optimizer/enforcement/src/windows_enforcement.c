/**
 * Grey Optimizer - Windows Enforcement Module
 * 
 * This module provides Windows-specific enforcement operations.
 * 
 * WHAT WE CAN DO:
 * - Job Objects: Create job objects to limit CPU and memory for process groups
 * - Priority Class: SetPriorityClass to lower process priority
 * - Affinity: SetProcessAffinityMask to limit CPU cores
 * - NTFS Hardlinks: CreateHardLink for file deduplication
 * - Sparse Files: DeviceIoControl with FSCTL_SET_SPARSE
 * 
 * LIMITATIONS:
 * - No kernel-level memory merging (no KSM equivalent)
 * - Cache dropping requires third-party tools or specific APIs
 * - Job Objects require careful handling to avoid orphaning processes
 * 
 * Safety: All operations check simulation mode before executing.
 */

#include "platform.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef GREY_PLATFORM_WINDOWS

#include <psapi.h>
#include <winioctl.h>
#pragma comment(lib, "psapi.lib")

/* ═══════════════════════════════════════════════════════════════════════════
 * JOB OBJECTS (Process Resource Limits)
 * ═══════════════════════════════════════════════════════════════════════════ */

/**
 * Create a job object with CPU and memory limits.
 * 
 * Job objects are Windows' mechanism for grouping processes and
 * applying resource limits to the group.
 * 
 * @param job_name Name for the job object (should be unique)
 * @param cpu_rate_percent CPU rate limit (1-100, as percentage of CPU time)
 * @param memory_limit_mb Memory limit in MB (0 for no limit)
 * @param out_handle Output: Handle to the created job object
 * @return Result structure
 * 
 * Safety: Job objects must be properly closed when done.
 * Reversible: Close the job object to remove limits.
 */
GREY_API grey_result_t grey_windows_create_job(
    const char* job_name,
    int cpu_rate_percent,
    size_t memory_limit_mb,
    HANDLE* out_handle
) {
    if (!job_name || !out_handle) {
        return grey_result_error(GREY_ERROR_INVALID_PARAM, "Parameters are NULL");
    }
    
    if (grey_simulation_mode) {
        printf("[SIMULATION] Would create job '%s' with CPU=%d%%, Memory=%zuMB\n",
               job_name, cpu_rate_percent, memory_limit_mb);
        *out_handle = NULL;
        return grey_result_ok(true);
    }
    
    /* Create named job object */
    wchar_t wname[256];
    MultiByteToWideChar(CP_UTF8, 0, job_name, -1, wname, 256);
    
    HANDLE job = CreateJobObjectW(NULL, wname);
    if (!job) {
        char buf[256];
        snprintf(buf, sizeof(buf), "CreateJobObject failed: %lu", GetLastError());
        return grey_result_error(GREY_ERROR_SYSTEM, buf);
    }
    
    /* Set CPU rate limit (Windows 8+) */
    if (cpu_rate_percent > 0 && cpu_rate_percent < 100) {
        JOBOBJECT_CPU_RATE_CONTROL_INFORMATION cpu_info = {0};
        cpu_info.ControlFlags = JOB_OBJECT_CPU_RATE_CONTROL_ENABLE | 
                                JOB_OBJECT_CPU_RATE_CONTROL_HARD_CAP;
        cpu_info.CpuRate = cpu_rate_percent * 100; /* In 100ths of a percent */
        
        if (!SetInformationJobObject(job, JobObjectCpuRateControlInformation,
                                      &cpu_info, sizeof(cpu_info))) {
            /* CPU rate control might not be available on older Windows */
            printf("[WARNING] CPU rate control not available: %lu\n", GetLastError());
        }
    }
    
    /* Set memory limit */
    if (memory_limit_mb > 0) {
        JOBOBJECT_EXTENDED_LIMIT_INFORMATION limit_info = {0};
        limit_info.BasicLimitInformation.LimitFlags = 
            JOB_OBJECT_LIMIT_JOB_MEMORY | JOB_OBJECT_LIMIT_PROCESS_MEMORY;
        limit_info.JobMemoryLimit = memory_limit_mb * 1024 * 1024;
        limit_info.ProcessMemoryLimit = memory_limit_mb * 1024 * 1024;
        
        if (!SetInformationJobObject(job, JobObjectExtendedLimitInformation,
                                      &limit_info, sizeof(limit_info))) {
            DWORD err = GetLastError();
            CloseHandle(job);
            char buf[256];
            snprintf(buf, sizeof(buf), "SetInformationJobObject failed: %lu", err);
            return grey_result_error(GREY_ERROR_SYSTEM, buf);
        }
    }
    
    *out_handle = job;
    printf("[LIVE] Created job '%s' with CPU=%d%%, Memory=%zuMB\n",
           job_name, cpu_rate_percent, memory_limit_mb);
    return grey_result_ok(false);
}

/**
 * Assign a process to a job object.
 * 
 * @param job Job object handle
 * @param pid Process ID to assign
 * @return Result structure
 */
GREY_API grey_result_t grey_windows_assign_to_job(HANDLE job, DWORD pid) {
    if (!job) {
        return grey_result_error(GREY_ERROR_INVALID_PARAM, "Job handle is NULL");
    }
    
    if (grey_simulation_mode) {
        printf("[SIMULATION] Would assign PID %lu to job\n", pid);
        return grey_result_ok(true);
    }
    
    HANDLE process = OpenProcess(PROCESS_SET_QUOTA | PROCESS_TERMINATE, FALSE, pid);
    if (!process) {
        char buf[256];
        snprintf(buf, sizeof(buf), "OpenProcess failed: %lu", GetLastError());
        return grey_result_error(GREY_ERROR_PERMISSION, buf);
    }
    
    if (!AssignProcessToJobObject(job, process)) {
        DWORD err = GetLastError();
        CloseHandle(process);
        char buf[256];
        snprintf(buf, sizeof(buf), "AssignProcessToJobObject failed: %lu", err);
        return grey_result_error(GREY_ERROR_SYSTEM, buf);
    }
    
    CloseHandle(process);
    printf("[LIVE] Assigned PID %lu to job\n", pid);
    return grey_result_ok(false);
}

/**
 * Close a job object and release all limits.
 * 
 * @param job Job object handle
 * @return Result structure
 */
GREY_API grey_result_t grey_windows_close_job(HANDLE job) {
    if (!job) {
        return grey_result_error(GREY_ERROR_INVALID_PARAM, "Job handle is NULL");
    }
    
    CloseHandle(job);
    printf("[LIVE] Closed job object\n");
    return grey_result_ok(false);
}

/* ═══════════════════════════════════════════════════════════════════════════
 * PROCESS PRIORITY
 * ═══════════════════════════════════════════════════════════════════════════ */

/**
 * Set the priority class of a process.
 * 
 * @param pid Process ID
 * @param priority_class One of: IDLE_PRIORITY_CLASS, BELOW_NORMAL_PRIORITY_CLASS,
 *                       NORMAL_PRIORITY_CLASS, ABOVE_NORMAL_PRIORITY_CLASS,
 *                       HIGH_PRIORITY_CLASS, REALTIME_PRIORITY_CLASS
 * @return Result structure
 * 
 * Safety: Lower priority doesn't harm the process, just schedules it less.
 * Reversible: Can be restored by setting to original priority class.
 */
GREY_API grey_result_t grey_windows_set_priority(DWORD pid, DWORD priority_class) {
    if (grey_simulation_mode) {
        printf("[SIMULATION] Would set PID %lu priority class to 0x%lx\n", 
               pid, priority_class);
        return grey_result_ok(true);
    }
    
    HANDLE process = OpenProcess(PROCESS_SET_INFORMATION, FALSE, pid);
    if (!process) {
        char buf[256];
        snprintf(buf, sizeof(buf), "OpenProcess failed: %lu", GetLastError());
        return grey_result_error(GREY_ERROR_PERMISSION, buf);
    }
    
    if (!SetPriorityClass(process, priority_class)) {
        DWORD err = GetLastError();
        CloseHandle(process);
        char buf[256];
        snprintf(buf, sizeof(buf), "SetPriorityClass failed: %lu", err);
        return grey_result_error(GREY_ERROR_SYSTEM, buf);
    }
    
    CloseHandle(process);
    printf("[LIVE] Set PID %lu priority class to 0x%lx\n", pid, priority_class);
    return grey_result_ok(false);
}

/**
 * Set the CPU affinity mask of a process.
 * 
 * @param pid Process ID
 * @param affinity_mask Bitmask of allowed CPUs (1 = CPU0, 2 = CPU1, etc.)
 * @return Result structure
 * 
 * Safety: Limiting CPUs doesn't harm the process, just constrains scheduling.
 * Reversible: Can be restored by setting to system affinity mask.
 */
GREY_API grey_result_t grey_windows_set_affinity(DWORD pid, DWORD_PTR affinity_mask) {
    if (grey_simulation_mode) {
        printf("[SIMULATION] Would set PID %lu affinity to 0x%llx\n", 
               pid, (unsigned long long)affinity_mask);
        return grey_result_ok(true);
    }
    
    HANDLE process = OpenProcess(PROCESS_SET_INFORMATION | PROCESS_QUERY_INFORMATION, 
                                  FALSE, pid);
    if (!process) {
        char buf[256];
        snprintf(buf, sizeof(buf), "OpenProcess failed: %lu", GetLastError());
        return grey_result_error(GREY_ERROR_PERMISSION, buf);
    }
    
    if (!SetProcessAffinityMask(process, affinity_mask)) {
        DWORD err = GetLastError();
        CloseHandle(process);
        char buf[256];
        snprintf(buf, sizeof(buf), "SetProcessAffinityMask failed: %lu", err);
        return grey_result_error(GREY_ERROR_SYSTEM, buf);
    }
    
    CloseHandle(process);
    printf("[LIVE] Set PID %lu affinity to 0x%llx\n", pid, (unsigned long long)affinity_mask);
    return grey_result_ok(false);
}

/* ═══════════════════════════════════════════════════════════════════════════
 * FILE OPERATIONS
 * ═══════════════════════════════════════════════════════════════════════════ */

/**
 * Create an NTFS hardlink for file deduplication.
 * 
 * @param target The existing file to link to
 * @param linkpath The path for the new hardlink
 * @return Result structure
 * 
 * Safety: Original file is preserved. Only works on same volume.
 */
GREY_API grey_result_t grey_windows_create_hardlink(
    const char* target,
    const char* linkpath
) {
    if (!target || !linkpath) {
        return grey_result_error(GREY_ERROR_INVALID_PARAM, "Paths are NULL");
    }
    
    if (grey_simulation_mode) {
        printf("[SIMULATION] Would create hardlink: %s -> %s\n", linkpath, target);
        return grey_result_ok(true);
    }
    
    wchar_t wtarget[MAX_PATH], wlink[MAX_PATH];
    MultiByteToWideChar(CP_UTF8, 0, target, -1, wtarget, MAX_PATH);
    MultiByteToWideChar(CP_UTF8, 0, linkpath, -1, wlink, MAX_PATH);
    
    if (!CreateHardLinkW(wlink, wtarget, NULL)) {
        char buf[256];
        snprintf(buf, sizeof(buf), "CreateHardLink failed: %lu", GetLastError());
        return grey_result_error(GREY_ERROR_IO, buf);
    }
    
    printf("[LIVE] Created hardlink: %s -> %s\n", linkpath, target);
    return grey_result_ok(false);
}

/**
 * Set a file as sparse and zero a range.
 * 
 * @param path Path to the file
 * @param offset Offset to start zeroing
 * @param length Length of the range to zero
 * @return Result structure
 * 
 * Safety: Only works on NTFS. Backup recommended.
 */
GREY_API grey_result_t grey_windows_set_sparse(
    const char* path,
    LONGLONG offset,
    LONGLONG length
) {
    if (!path) {
        return grey_result_error(GREY_ERROR_INVALID_PARAM, "Path is NULL");
    }
    
    if (grey_simulation_mode) {
        printf("[SIMULATION] Would set sparse: %s, offset=%lld, length=%lld\n",
               path, offset, length);
        return grey_result_ok(true);
    }
    
    wchar_t wpath[MAX_PATH];
    MultiByteToWideChar(CP_UTF8, 0, path, -1, wpath, MAX_PATH);
    
    HANDLE file = CreateFileW(wpath, GENERIC_WRITE, 0, NULL, OPEN_EXISTING,
                               FILE_ATTRIBUTE_NORMAL, NULL);
    if (file == INVALID_HANDLE_VALUE) {
        char buf[256];
        snprintf(buf, sizeof(buf), "CreateFile failed: %lu", GetLastError());
        return grey_result_error(GREY_ERROR_IO, buf);
    }
    
    /* Set file as sparse */
    DWORD bytes_returned;
    if (!DeviceIoControl(file, FSCTL_SET_SPARSE, NULL, 0, NULL, 0, 
                          &bytes_returned, NULL)) {
        DWORD err = GetLastError();
        CloseHandle(file);
        char buf[256];
        snprintf(buf, sizeof(buf), "FSCTL_SET_SPARSE failed: %lu", err);
        return grey_result_error(GREY_ERROR_IO, buf);
    }
    
    /* Zero the data range */
    FILE_ZERO_DATA_INFORMATION zero_info;
    zero_info.FileOffset.QuadPart = offset;
    zero_info.BeyondFinalZero.QuadPart = offset + length;
    
    if (!DeviceIoControl(file, FSCTL_SET_ZERO_DATA, &zero_info, sizeof(zero_info),
                          NULL, 0, &bytes_returned, NULL)) {
        DWORD err = GetLastError();
        CloseHandle(file);
        char buf[256];
        snprintf(buf, sizeof(buf), "FSCTL_SET_ZERO_DATA failed: %lu", err);
        return grey_result_error(GREY_ERROR_IO, buf);
    }
    
    CloseHandle(file);
    printf("[LIVE] Set sparse: %s, zeroed %lld bytes at offset %lld\n",
           path, length, offset);
    return grey_result_ok(false);
}

/* ═══════════════════════════════════════════════════════════════════════════
 * MEMORY INFORMATION
 * ═══════════════════════════════════════════════════════════════════════════ */

/**
 * Get system memory statistics.
 * 
 * @param out_total_mb Output: total physical memory in MB
 * @param out_available_mb Output: available memory in MB
 * @return Result structure
 */
GREY_API grey_result_t grey_windows_get_memory_stats(
    DWORDLONG* out_total_mb,
    DWORDLONG* out_available_mb
) {
    if (!out_total_mb || !out_available_mb) {
        return grey_result_error(GREY_ERROR_INVALID_PARAM, "Output parameters are NULL");
    }
    
    MEMORYSTATUSEX mem_status;
    mem_status.dwLength = sizeof(mem_status);
    
    if (!GlobalMemoryStatusEx(&mem_status)) {
        char buf[256];
        snprintf(buf, sizeof(buf), "GlobalMemoryStatusEx failed: %lu", GetLastError());
        return grey_result_error(GREY_ERROR_SYSTEM, buf);
    }
    
    *out_total_mb = mem_status.ullTotalPhys / (1024 * 1024);
    *out_available_mb = mem_status.ullAvailPhys / (1024 * 1024);
    
    return grey_result_ok(false);
}

#endif /* GREY_PLATFORM_WINDOWS */
