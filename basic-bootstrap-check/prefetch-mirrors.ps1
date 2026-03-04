<#
.SYNOPSIS
Pre-fetch Maven dependencies from mirrors in a settings.xml to local repository.

.PARAMETER SettingsXml
Path to Maven settings.xml. Defaults to C:\Users\$env:USERNAME\.m2\settings.xml

.PARAMETER LocalRepo
Path to Maven local repository. Defaults to C:\Users\$env:USERNAME\.m2\repository
#>

param(
    [string]$SettingsXml = "C:\Users\$env:USERNAME\.m2\settings.xml",
    [string]$LocalRepo   = "C:\Users\$env:USERNAME\.m2\repository"
)

if (-not (Test-Path $SettingsXml)) {
    Write-Error "settings.xml not found at $SettingsXml"
    exit 1
}

if (-not (Test-Path $LocalRepo)) {
    Write-Host "Creating local repository at $LocalRepo"
    New-Item -ItemType Directory -Force -Path $LocalRepo | Out-Null
}

# Load settings.xml safely (handles comments)
[xml]$doc = Get-Content $SettingsXml

# Extract all mirror URLs:q
$mirrors = @()
foreach ($mirror in $doc.settings.mirrors.mirror) {
    $url = $mirror.url.'#text'
    if ($url) { $mirrors += $url.Trim() }
}

if ($mirrors.Count -eq 0) {
    Write-Warning "No mirrors found in $SettingsXml"
} else {
    Write-Host "Found mirrors:"
    $mirrors | ForEach-Object { Write-Host " - $_" }
}

# Example fixed dependency list (can later parse pom.xml)
$dependencies = @(
    "org.apache.commons:commons-lang3:3.12.0",
    "junit:junit:4.13.2"
)

foreach ($mirror in $mirrors) {
    Write-Host "Using mirror: $mirror"
    foreach ($dep in $dependencies) {
        Write-Host "Fetching $dep ..."
        $args = @(
            "dependency:get",
            "-Dartifact=$dep",
            "-Dmaven.repo.local=$LocalRepo",
            "-s", $SettingsXml,
            "-DremoteRepositories=$mirror"
        )
        $proc = Start-Process mvn -ArgumentList $args -NoNewWindow -Wait -PassThru
        if ($proc.ExitCode -ne 0) {
            Write-Warning "Failed to fetch $dep from $mirror"
        }
    }
}

Write-Host "Prefetch complete."
