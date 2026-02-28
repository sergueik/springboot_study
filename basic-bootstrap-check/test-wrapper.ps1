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
# NOTE the call
# .\mvnw.cmd -v > NUL 2> NUL
# or
# cmd %%- .\mvnw.cmd -v > NUL 2> NUL
# leads to error:
# out-file : FileStream was asked to open a device that was not a file. For
# support for devices like 'com1:' or 'lpt1:', 
# call CreateFile, then use the
# FileStream constructors that take an OS handle as an IntPtr.
# the following syntax sugar is required
.\mvnw.cmd -v *> $null

if ($LASTEXITCODE -ne 0) {
    Write-Error "Maven Wrapper failed to bootstrap"
    exit 1
}

# 4. Inspect distribution folder
$dist = "$env:USERPROFILE\.m2\wrapper\dists\apache-maven-*"
if (Test-Path $dist) {
    Get-ChildItem $dist -Recurse | Select-Object FullName
} else {
    Write-Host "No distributions found!"
}

# 5. Optionally purge again
# NOTE |out-null is not sufficient
# NOTE *> $null also does not work
Remove-Item -Recurse -Force "$env:USERPROFILE\.m2\wrapper\dists\*" -ErrorAction SilentlyContinue *> $null
# Root cause:
# What’s happening here is subtle:
#
# Multiple streams – PowerShell has 6+ output streams (Success, Error, Warning, Verbose, Debug, Information). 
# Redirecting one or two (Out-Null or *> $null) often isn’t enough.

Host auto-forma
$null = Remove-Item -Recurse -Force "$env:USERPROFILE\.m2\wrapper\dists\*" -ErrorAction SilentlyContinue
Write-Host "Final purge done."

# Restore PATH
$env:PATH = $oldPath
Write-Host "PATH restored."
