# kill (standalone)

This repository includes a small Windows-native `kill` program that mimics the builtin `kill` in the `Msh` shell.

Build

Use your preferred Windows C toolchain. Example with GCC/TDM or MinGW in PowerShell:

```powershell
gcc .\kill.c -o .\kill.exe
```

Usage

Run from PowerShell or cmd:

```powershell
# kill single process by PID
kill.exe 12345

# force kill (SIGKILL semantics)
kill.exe -9 12345

# kill multiple PIDs
kill.exe -9 12345 67890
```

Notes

- On Windows this program uses OpenProcess + TerminateProcess which forcibly stops the target process. It does not attempt graceful shutdown of GUI or console apps.
- You can put `kill.exe` on your PATH to call it from any shell.