$ErrorActionPreference = 'Stop'

##############################################################################
# CONFIGURATION
##############################################################################

$WorkDir = $env:WORKDIR
if (-not $WorkDir) {
  $WorkDir = Join-Path (Get-Location) 'build'
}

$MvnRepo = Join-Path $WorkDir 'm2'
$JavaRequiredMajor = 11

##############################################################################
# TOOLS CHECK
##############################################################################

foreach ($cmd in @('git','mvn','java')) {
  if (-not (Get-Command $cmd -ErrorAction SilentlyContinue)) {
    throw "$cmd not found"
  }
}
<#
$javaVersion = & java -version 2>&1 | Select-String 'version'
$javaMajor = ($javaVersion -split '[\." ]')[1]
if ([int]$javaMajor -lt $JavaRequiredMajor) {
  throw "Java $JavaRequiredMajor+ required"
}
#>
##############################################################################
# CLEAN START
##############################################################################

Write-Host "==> Cleaning work directory"
Remove-Item -Recurse -Force $WorkDir -ErrorAction SilentlyContinue
New-Item -ItemType Directory -Path $WorkDir, $MvnRepo | Out-Null

$env:MVN_OPTS = "-DskipTests -Dmaven.repo.local=$MvnRepo"

##############################################################################
# FUNCTION: build component
##############################################################################

function Build-Component {
  param(
    [string]$Name,
    [string]$Repo,
    [string]$Commit,
    [string]$OverridePom,
    [string]$AvoidFile
  )

  Write-Host ""
  Write-Host "==> Building $Name"
  Set-Location $WorkDir

  git clone $Repo $Name
  Set-Location $Name
  git checkout $Commit

  Copy-Item (Join-Path $WorkDir $OverridePom) pom.xml -Force

  if (Test-Path (Join-Path $WorkDir $AvoidFile)) {
    Copy-Item (Join-Path $WorkDir $AvoidFile) avoid_deps.txt -Force
  }

  mvn -f pom.xml $env:MVN_OPTS clean -DskipTests install

}

##############################################################################
# COPY OVERRIDDEN FILES
##############################################################################

Copy-Item `
  pom.cb2xml.xml,
  pom.jrecord.xml,
  pom.cobol2json.xml,
  avoid_cb2xml_deps.txt,
  avoid_jrecord_deps.txt `
  $WorkDir

##############################################################################
# 1) cb2xml
##############################################################################

Build-Component `
  -Name cb2xml `
  -Repo https://github.com/bmTas/cb2xml `
  -Commit 97f8dc8 `
  -OverridePom pom.cb2xml.xml `
  -AvoidFile avoid_cb2xml_deps.txt

##############################################################################
# 2) JRecord
##############################################################################

Build-Component `
  -Name jrecord `
  -Repo https://github.com/bmTas/JRecord `
  -Commit f50ece71 `
  -OverridePom pom.jrecord.xml `
  -AvoidFile avoid_jrecord_deps.txt

##############################################################################
# 3) CobolToJson
##############################################################################

Write-Host ""
Write-Host "==> Building CobolToJson"

Set-Location $WorkDir
git clone https://github.com/bmTas/CobolToJson cobol2json
Set-Location cobol2json
git checkout 99b0aa2

Copy-Item (Join-Path $WorkDir 'pom.cobol2json.xml') pom.xml -Force
mvn clean -DskipTests package

$AppJar = Get-ChildItem target -Filter '*-shaded.jar' -ErrorAction SilentlyContinue |
  Select-Object -First 1

if (-not $AppJar) {
  $AppJar = Get-ChildItem target -Filter '*.jar' |
    Where-Object { $_.Name -notlike 'original-*' } |
    Select-Object -First 1
}

if (-not $AppJar) {
  throw "Application jar not found"
}

##############################################################################
# 4) Inspect shaded JAR (no unzip)
##############################################################################

Write-Host ""
Write-Host "==> Inspecting shaded JAR"

Add-Type -AssemblyName System.IO.Compression.FileSystem
$zip = [System.IO.Compression.ZipFile]::OpenRead($AppJar.FullName)

try {
  $manifest = $zip.Entries | Where-Object { $_.FullName -eq 'META-INF/MANIFEST.MF' }
  if (-not $manifest) {
    throw "MANIFEST.MF not found"
  }

  $reader = New-Object IO.StreamReader ($manifest.Open())
  $manifestText = $reader.ReadToEnd()
  $reader.Close()

  Write-Output $manifestText
<#
Manifest-Version: 1.0
Archiver-Version: Plexus Archiver
Created-By: Apache Maven 3.6.1
Built-By: kouzm
Build-Jdk: 11.0.12
Main-Class: net.sf.cobolToJson.Data2Json
#>
<#
  if ($manifestText -notmatch '^Main-Class:' ) {
    throw "Main-Class missing in manifest"
  }
  #>
  if ( ( $manifestText -split '\r?\n' | where-object {$_ -match 'Main-Class:' } ).count -eq 0) {
    throw "Main-Class missing in manifest"
  }
  if ($zip.Entries | Where-Object { $_.FullName -like '*.jar' }) {
    throw "Nested jars found (shade misconfigured)"
  }

  Write-Host "OK: shaded jar structure valid"
}
finally {
  $zip.Dispose()
}

##############################################################################
# 5) Validate Maven repo cleanliness
##############################################################################

Write-Host ""
Write-Host "==> Validating Maven repo isolation"

$Forbidden = @(
  'com/github',
  'org/springframework',
  'org/apache/httpcomponents',
  'ch/qos/logback'
)

foreach ($p in $Forbidden) {
  if (Get-ChildItem -Recurse $MvnRepo -Directory |
      Where-Object { $_.FullName -like "*$p*" }) {
    throw "Forbidden dependency leaked into repo: $p"
  }
}

##############################################################################
# DONE
##############################################################################

Write-Host ""
Write-Host "==> BUILD COMPLETE"
Write-Host "    Shaded JAR: $($AppJar.FullName)"
Write-Host "    Maven repo: $MvnRepo"

