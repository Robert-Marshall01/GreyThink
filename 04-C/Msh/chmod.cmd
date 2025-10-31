@echo off
rem Simple Windows-friendly chmod wrapper (supports +h/-h, +r/-r and 3-digit octal for read-only)
if "%~1"=="" goto usage
nset MODE=%~1
if "%~2"=="" goto usage
nset FILE=%~2

n:: symbolic quick handlers
nif "%MODE%"=="+h" (attrib +h "%FILE%" & exit /b 0)
if "%MODE%"=="-h" (attrib -h "%FILE%" & exit /b 0)
if "%MODE%"=="+r" (attrib +r "%FILE%" & exit /b 0)
if "%MODE%"=="-r" (attrib -r "%FILE%" & exit /b 0)

n:: numeric mode: accept 3-digit octal like 644 or 755. We'll treat write-bit (2) presence as writable => clear readonly.
necho %MODE% | findstr /R "^[0-7][0-7][0-7]$" >nul
nif errorlevel 1 goto unknown
nset d0=%MODE:~0,1%
set d1=%MODE:~1,1%
set d2=%MODE:~2,1%
set /a anywrite=( (d0 & 2) | (d1 & 2) | (d2 & 2) )
nif %anywrite% NEQ 0 (
    attrib -r "%FILE%"
) else (
    attrib +r "%FILE%"
)
ngoto :eof

n:usage
necho Usage: chmod (+h|-h|+r|-r|MODE) FILE
necho Examples:
necho   chmod +h file.txt    - add Hidden
necho   chmod -h file.txt    - remove Hidden
necho   chmod +r file.txt    - set Read-Only
necho   chmod -r file.txt    - clear Read-Only
necho   chmod 644 file.txt    - numeric: clear Read-Only if any write bits set (owner/group/other)
ngoto :eof
n
n:unknown
necho Unsupported mode: %MODE%
necho Supported: +h -h +r -r or 3-digit octal like 644
nexit /b 1