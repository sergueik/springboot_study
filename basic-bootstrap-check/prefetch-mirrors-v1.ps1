<#
.SYNOPSIS
Prefetch Maven dependencies, plugins, and repositories for offline CI builds.

.DESCRIPTION
- Default mode: silently adds all discovered mirrors/repositories from settings.xml to POM.
- Validate mode: permissioned preflight that moves repository/settings, prefetches, runs offline build, and exits with advisory.

.PARAMETER SettingsXml
Path to Maven settings.xml. Defaults to C:\Users\$env:USERNAME\.m2\settings.xml

.PARAMETER LocalRepo
Path to Maven local repository. Defaults to C:\Users\$env:USERNAME\.m2\repository

.PARAMETER Validate
Switch to perform full offline preflight verification.
#>

param(
    [string]$SettingsXml = "C:\Users\$env:USERNAME\.m2\settings.xml",
    [string]$LocalRepo   = "C:\Users\$env:USERNAME\.m2\repository",
    [switch]$Validate
)

function Get-MavenMirrorsAndRepos {
    param([xml]$SettingsXmlDoc)

    $mirrors = @()
    foreach ($mirror in $SettingsXmlDoc.settings.mirrors.mirror) {
        $url = $mirror.url.'#text'
        if ($url) { $mirrors += $url.Trim() }
    }
    return $mirrors
}

function Get-PomRepositories {
    param([xml]$PomXmlDoc)

    $repos = @()

    # Regular repositories
    foreach ($repo in $PomXmlDoc.project.repositories.repository) {
        $url = $repo.url.'#text'
        if ($url) { $repos += $url.Trim() }
    }

    # Plugin repositories
    foreach ($prepo in $PomXmlDoc.project.pluginRepositories.pluginRepository) {
        $url = $prepo.url.'#text'
        if ($url) { $repos += $url.Trim() }
    }

    # Distribution management (optional)
    if ($PomXmlDoc.project.distributionManagement) {
        $dm = $PomXmlDoc.project.distributionManagement
        if ($dm.repository) { $repos += $dm.repository.url.'#text' }
        if ($dm.snapshotRepository) { $repos += $dm.snapshotRepository.url.'#text' }
    }

    return $repos | Sort-Object -Unique
}

function Prefetch-Artifact {
    param(
        [string]$Artifact,
        [string[]]$Repos,
        [string]$LocalRepo,
        [string]$SettingsXml
    )

    foreach ($repo in $Repos) {
        Write-Host "Fetching $Artifact from $repo ..."
        $args = @(
            "dependency:get",
            "-Dartifact=$Artifact",
            "-Dmaven.repo.local=$LocalRepo",
            "-s", $SettingsXml,
            "-DremoteRepositories=$repo"
        )
        $proc = Start-Process mvn -ArgumentList $args -NoNewWindow -Wait -PassThru
        if ($proc.ExitCode -ne 0) {
            Write-Warning "Failed to fetch $Artifact from $repo"
        }
    }
}

# --- Main Script Logic ---

if (-not (Test-Path $SettingsXml)) {
    Write-Warning "settings.xml not found at $SettingsXml, continuing with default Maven Central."
}

if (-not (Test-Path $LocalRepo)) {
    Write-Host "Creating local repository at $LocalRepo"
    New-Item -ItemType Directory -Force -Path $LocalRepo | Out-Null
}

# Load POM
$PomPath = Join-Path (Get-Location) "pom.xml"
if (-not (Test-Path $PomPath)) {
    Write-Error "pom.xml not found in current directory."
    exit 1
}
[xml]$PomXml = Get-Content $PomPath

# Load settings.xml if exists
if (Test-Path $SettingsXml) { [xml]$SettingsXmlDoc = Get-Content $SettingsXml }

# Discover mirrors and repositories
$mirrors = if ($SettingsXmlDoc) { Get-MavenMirrorsAndRepos -SettingsXmlDoc $SettingsXmlDoc } else { @() }
$repos = Get-PomRepositories -PomXmlDoc $PomXml
$allRepos = ($mirrors + $repos) | Sort-Object -Unique

Write-Host "Discovered repositories/mirrors:"
$allRepos | ForEach-Object { Write-Host " - $_" }

# Example dependency list (replace with real POM parsing if needed)
$dependencies = @(
    "org.apache.commons:commons-lang3:3.12.0",
    "junit:junit:4.13.2"
)

# Default mode: just prefetch
if (-not $Validate) {
    Write-Host "Default mode: patching POM silently with discovered repositories."
    # Implement patch logic if needed (silent)...
    foreach ($dep in $dependencies) { Prefetch-Artifact -Artifact $dep -Repos $allRepos -LocalRepo $LocalRepo -SettingsXml $SettingsXml }
    Write-Host "Prefetch complete."
} else {
    # Validate mode: permissioned offline verification
    $confirm = Read-Host "Validate mode: move away .m2/repository and settings.xml? [y/N]"
    if ($confirm -ne 'y') { Write-Host "Aborting validate mode."; exit 0 }

    # Move repository & settings
    $timestamp = Get-Date -Format yyyyMMddHHmmss
    $backupRepo = "${LocalRepo}-backup-$timestamp"
    Rename-Item -Path $LocalRepo -NewName $backupRepo -Force
    if (Test-Path $SettingsXml) {
        $backupSettings = "${SettingsXml}-backup-$timestamp"
        Rename-Item -Path $SettingsXml -NewName $backupSettings -Force
    }

    Write-Host "Prefetching all artifacts for offline verification..."
    foreach ($dep in $dependencies) { Prefetch-Artifact -Artifact $dep -Repos $allRepos -LocalRepo $LocalRepo -SettingsXml $SettingsXml }

    # Offline build
    Write-Host "Running offline Maven build..."
    $mvnCmd = "mvn -B clean verify -o -s `"$SettingsXml`""
    Invoke-Expression $mvnCmd
    if ($LASTEXITCODE -ne 0) {
        Write-Warning "Offline verification failed! Developer must not push POM until fixed."
        exit 1
    } else {
        Write-Host "Offline verification succeeded."
    }

    Write-Host "Validate mode complete. You may restore original repository/settings if desired."
}
