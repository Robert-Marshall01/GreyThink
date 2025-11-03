<#
Safe publish script (PowerShell).
This script only prints the commands it will run. Remove -WhatIf to execute.
#>

param(
    [string]$remote = 'origin',
    [string]$branch = 'main',
    [string]$version = 'v0.1.0'
)

Write-Host "=== Publish script (dry-run) ==="
Write-Host "Remote: $remote, Branch: $branch, Tag: $version"

Write-Host "1) Ensure working tree is clean"
Write-Host '   git status --porcelain'

Write-Host "2) Push branch to remote (dry run):"
Write-Host "   git push $remote $branch --follow-tags --no-verify --dry-run"

Write-Host "3) Create and push tag (dry run):"
Write-Host "   git tag -a $version -m 'Release $version'"
Write-Host "   git push $remote $version --dry-run"

Write-Host "When you're ready, run the commands above without --dry-run or remove the dry-run flags."
