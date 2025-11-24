<#
NOTE: Portions of this script (build instructions and helper logic) were generated
by an AI assistant and then reviewed/modified by the repository owner. The
generated instructions may be incomplete or incorrect for your environment.
Always review commands before running them.
#>

<#
Simple build helper for local development.
It tries NASM first, then falls back to MASM (ml/link) if available.
Usage: Open PowerShell in the project root and run `\.\build.ps1`.
#>

$root = Split-Path -Path $MyInvocation.MyCommand.Definition -Parent
Push-Location $root

if (Test-Path .\clock_asm.asm -PathType Leaf) {
    Write-Host "Found clock_asm.asm"
} else {
    Write-Error "clock_asm.asm not found in project root."
    Pop-Location
    exit 1
}

if (Get-Command nasm -ErrorAction SilentlyContinue) {
    Write-Host "Using NASM to assemble..."
    try {
        nasm -f bin clock_asm.asm -o clock_asm.bin
        Write-Host "Built: clock_asm.bin"
    } catch {
        Write-Warning "NASM build failed. You may need to adjust the format flags for your environment."
    }
} elseif (Get-Command ml -ErrorAction SilentlyContinue) {
    Write-Host "Using MASM (ml) to assemble..."
    try {
        ml /c /coff clock_asm.asm
        if (Test-Path .\clock_asm.obj) {
            link /SUBSYSTEM:CONSOLE clock_asm.obj
            Write-Host "Linked: clock_asm.exe"
        } else {
            Write-Warning "MASM produced no object file. Check source and directives."
        }
    } catch {
        Write-Warning "MASM build failed."
    }
} else {
    Write-Error "No supported assembler found (nasm or ml). Install NASM or MASM and re-run this script."
    Pop-Location
    exit 2
}

Pop-Location
