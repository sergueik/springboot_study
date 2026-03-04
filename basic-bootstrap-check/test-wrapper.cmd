@echo off
setlocal EnableDelayedExpansion

echo === Maven Wrapper Filing Probe (CMD) ===

:: Save original PATH
set "OLD_PATH=%PATH%"

:: Build new PATH without apache-maven entries
set "NEW_PATH="

for %%i in ("%OLD_PATH:;=" "%") do (
    echo %%~i | findstr /i "apache-maven" >nul
    if errorlevel 1 (
        if defined NEW_PATH (
            set "NEW_PATH=!NEW_PATH!;%%~i"
        ) else (
            set "NEW_PATH=%%~i"
        )
    )
)

set "PATH=%NEW_PATH%"
echo System Maven shadowed. PATH adjusted.

:: Verify mvn is gone
where mvn >nul 2>&1
if not errorlevel 1 (
    echo WARNING: mvn still found in PATH
) else (
    echo OK: mvn not found in PATH
)

:: Purge wrapper cache
if exist "%USERPROFILE%\.m2\wrapper\dists" (
    rmdir /s /q "%USERPROFILE%\.m2\wrapper\dists"
)
echo Wrapper cache cleared.

:: Run wrapper
echo Running mvnw.cmd -v ...
call mvnw.cmd -v


:: Examine wrapper version, compare with distributionUrl 
for /f "tokens=2 delims==" %%A in ('findstr distributionUrl .mvn\wrapper\maven-wrapper.properties') do set URL=%%A
for %%B in (%URL%) do set DIST=%%~nB
set DIST=%DIST:apache-maven-=%
set DIST=%DIST:-bin=%

for /f "tokens=3" %%V in ('mvnw.cmd -v ^| findstr "Apache Maven"') do set MVNVER=%%V

if "%DIST%"=="%MVNVER%" (
  echo OK: Maven Wrapper version matches %MVNVER%
  REM exit /b 0
) else (
  echo Mismatch: wrapper=%DIST% runtime=%MVNVER%
  exit /b 1
)

:: Inspect distribution folder
if exist "%USERPROFILE%\.m2\wrapper\dists\apache-maven-*" (
    dir "%USERPROFILE%\.m2\wrapper\dists\apache-maven-*"
) else (
    echo No distributions found!
)

:: Optional final purge
if exist "%USERPROFILE%\.m2\wrapper\dists" (
    rmdir /s /q "%USERPROFILE%\.m2\wrapper\dists"
)
echo Final purge done.

:: Restore PATH
set "PATH=%OLD_PATH%"
echo PATH restored.

endlocal
