Write-Host "=== Maven Wrapper Filing Probe (PowerShell) ==="

# 1. Shadow system Maven
$oldPath = $env:PATH
$env:PATH = ($env:PATH -split ';' | Where-Object { $_ -notmatch 'apache-maven' }) -join ';'
Write-Host "System Maven shadowed. PATH adjusted."

# 2. Purge Maven Wrapper cache
Remove-Item -Recurse -Force "$env:USERPROFILE\.m2\wrapper\dists\*" -ErrorAction SilentlyContinue
Write-Host "Wrapper cache cleared."

# 3. Run Maven Wrapper
Write-Host "Running .\mvnw.cmd -v ..."
.\mvnw.cmd -v

# 4. Inspect distribution folder
$dist = "$env:USERPROFILE\.m2\wrapper\dists\apache-maven-*"
if (Test-Path $dist) {
    Get-ChildItem $dist -Recurse | Select-Object FullName
} else {
    Write-Host "No distributions found!"
}

# 5. Optionally purge again
Remove-Item -Recurse -Force "$env:USERPROFILE\.m2\wrapper\dists\*" -ErrorAction SilentlyContinue
Write-Host "Final purge done."

# Restore PATH
$env:PATH = $oldPath
Write-Host "PATH restored."
