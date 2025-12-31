# Cross-Platform Migration Guide for Msh

This guide documents how to migrate Msh source files from Windows-only code to cross-platform code using the platform abstraction layer.

## Overview

The platform abstraction layer consists of:
- **platform.h** - Cross-platform header with types, macros, and function prototypes
- **platform_posix.c** - POSIX/Linux/macOS implementations
- **platform_win32.c** - Windows implementations

## Step 1: Update Includes

**Before:**
```c
#include <windows.h>
#include <io.h>
#include <direct.h>
```

**After:**
```c
#include "platform.h"
```

## Step 2: Type Replacements

| Windows Type | Cross-Platform Type |
|--------------|---------------------|
| `HANDLE` | `msh_handle_t` |
| `DWORD` (for PIDs) | `msh_pid_t` |
| `BOOL` | `msh_bool_t` or `int` |
| `TRUE/FALSE` | `MSH_TRUE/MSH_FALSE` or `1/0` |
| `MAX_PATH` | `MSH_MAX_PATH` |
| `INVALID_HANDLE_VALUE` | `MSH_INVALID_HANDLE` |

## Step 3: Function Replacements

### String Utilities

| Windows Function | Cross-Platform Function |
|------------------|-------------------------|
| `_strdup()` | `msh_strdup()` |
| `_fullpath()` | `msh_fullpath()` |

### File System Operations

| Windows Function | Cross-Platform Function |
|------------------|-------------------------|
| `CreateDirectoryA()` | `msh_mkdir()` |
| `RemoveDirectoryA()` | `msh_rmdir()` |
| `SetCurrentDirectoryA()` | `msh_chdir()` |
| `GetCurrentDirectoryA()` | `msh_getcwd()` |
| `DeleteFileA()` | `msh_remove()` |
| `MoveFileA()` | `msh_rename()` |
| `CopyFileA()` | `msh_copy_file()` |
| `CreateHardLinkA()` | `msh_create_hardlink()` |

### File Attributes

| Windows Function | Cross-Platform Function |
|------------------|-------------------------|
| `GetFileAttributesA()` | `msh_get_file_attributes()` |
| `SetFileAttributesA()` | `msh_set_file_attributes()` |
| `FILE_ATTRIBUTE_DIRECTORY` | `MSH_ATTR_DIRECTORY` |
| `FILE_ATTRIBUTE_READONLY` | `MSH_ATTR_READONLY` |
| `FILE_ATTRIBUTE_HIDDEN` | `MSH_ATTR_HIDDEN` |

### Directory Enumeration

**Before:**
```c
WIN32_FIND_DATAA findData;
HANDLE hFind = FindFirstFileA("*", &findData);
if (hFind != INVALID_HANDLE_VALUE) {
    do {
        printf("%s\n", findData.cFileName);
    } while (FindNextFileA(hFind, &findData));
    FindClose(hFind);
}
```

**After:**
```c
msh_dir_handle_t *dh = msh_opendir(".");
if (dh) {
    msh_dir_entry_t entry;
    while (msh_readdir(dh, &entry)) {
        printf("%s\n", entry.name);
    }
    msh_closedir(dh);
}
```

### Process Management

| Windows Function | Cross-Platform Function |
|------------------|-------------------------|
| `CreateProcessA()` | `msh_spawn_process()` |
| `WaitForSingleObject()` | `msh_wait_process()` |
| `GetExitCodeProcess()` | `msh_get_exit_code()` |
| `TerminateProcess()` | `msh_terminate_process()` |
| `ResumeThread()` | `msh_resume_process()` |
| `CloseHandle()` | `msh_close_process()` |
| `GetCurrentProcessId()` | `msh_getpid()` |

**Before:**
```c
STARTUPINFOA si;
PROCESS_INFORMATION pi;
ZeroMemory(&si, sizeof(si));
si.cb = sizeof(si);
ZeroMemory(&pi, sizeof(pi));

if (CreateProcessA(NULL, cmdline, NULL, NULL, FALSE, 0, NULL, NULL, &si, &pi)) {
    WaitForSingleObject(pi.hProcess, INFINITE);
    CloseHandle(pi.hProcess);
    CloseHandle(pi.hThread);
}
```

**After:**
```c
msh_process_t proc;
if (msh_spawn_process(cmdline, &proc, 0)) {
    msh_wait_process(&proc, -1);
    msh_close_process(&proc);
}
```

### System Information

| Windows Function | Cross-Platform Function |
|------------------|-------------------------|
| `GetUserNameA()` | `msh_get_username()` |
| `GetComputerNameA()` | `msh_get_hostname()` |
| `GetTempPathA()` | `msh_get_temp_path()` |
| `GetTempFileNameA()` | `msh_get_temp_filename()` |
| Check for admin | `msh_is_admin()` |

### Console Operations

| Windows Function | Cross-Platform Function |
|------------------|-------------------------|
| `GetConsoleMode()` check | `msh_is_interactive()` |
| `system("cls")` | `msh_clear_screen()` |

### Shell Execution

| Windows Function | Cross-Platform Function |
|------------------|-------------------------|
| `system()` | `msh_system()` |
| `ShellExecuteA()` | `msh_shell_execute()` |

## Step 4: Path Separator Handling

Use `MSH_PATH_SEP` and `MSH_PATH_SEP_STR` for path operations:

**Before:**
```c
char path[MAX_PATH];
sprintf(path, "%s\\%s", dir, file);
```

**After:**
```c
char path[MSH_MAX_PATH];
sprintf(path, "%s%s%s", dir, MSH_PATH_SEP_STR, file);
```

Or for checking separators:
```c
if (path[i] == MSH_PATH_SEP || path[i] == MSH_PATH_SEP_ALT) {
    // Found separator
}
```

## Step 5: Conditional Platform Code

When platform-specific code is unavoidable:

```c
#if MSH_PLATFORM_WINDOWS
    // Windows-specific code
#elif MSH_PLATFORM_POSIX
    // POSIX-specific code
#endif
```

## Step 6: Error Handling

All platform functions return:
- `1` for success
- `0` for failure
- `-1` for error (in some cases)

Check return values consistently:

```c
if (msh_mkdir(path)) {
    printf("Directory created\n");
} else {
    printf("Failed to create directory\n");
}
```

## Building

### Linux/macOS
```bash
./build.sh
# or
make
# or for debug build
make debug
```

### Windows (MinGW/MSYS2)
```bash
make
# or
./build.ps1
```

### Docker (Linux container)
```bash
docker build -t msh .
docker run --rm -it msh
```

## Testing Cross-Platform Build

After migration, verify the build works on both platforms:

1. **Linux/macOS:**
   ```bash
   make clean && make
   ./Msh --version
   ```

2. **Windows:**
   ```bash
   make clean && make
   Msh.exe --version
   ```

3. **Docker:**
   ```bash
   docker build -t msh . && docker run --rm msh --version
   ```
