# Recursively count lines of code in all files (excluding .git folder)
$files = Get-ChildItem -Recurse -File | Where-Object { $_.FullName -notmatch '\\.git\\' }
$total = 0
foreach ($file in $files) {
    $lines = (Get-Content $file.FullName | Measure-Object -Line).Lines
    $total += $lines
}
Write-Output "Total lines of code: $total"
