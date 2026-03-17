# -----------------------------
# Minimal Maven plugin presence check (PowerShell)
# -----------------------------

$repo = Join-Path $env:TEMP "maven-empty-repo"

# 1. Cleanup any previous repo (silent)
Remove-Item -Recurse -Force -ErrorAction SilentlyContinue $repo

# 2. Create fresh repo
New-Item -ItemType Directory -Force -Path $repo | Out-Null

# 3. Run Maven (IMPORTANT: use --% to stop PowerShell parsing)
mvn --% -Dmaven.repo.local="$repo" help:help -ntp -B *> $null

# 4. Check presence
$pluginPath = Join-Path $repo "org\apache\maven\plugins\maven-help-plugin"

if (Test-Path $pluginPath) {
    Write-Output "[OK] Maven Help Plugin appears to be downloaded."
    exit 0
} else {
    Write-Output "[FAIL] Maven Help Plugin is missing."
    exit 1
}
