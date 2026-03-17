@echo off
REM -----------------------------
REM Minimal Maven plugin presence check (Windows CMD)
REM -----------------------------

REM 1. Create a fresh local Maven repository
@set "LOCAL_REPO=%TEMP%\maven-empty-repo"
@ 1> NUL 2> NUL mkdir "%LOCAL_REPO%"
 
REM 2. Trigger Maven Help Plugin download
@call mvn -Dmaven.repo.local="%LOCAL_REPO%" help:help -ntp -B 1> NUL 2> NUL

REM 3. Check if plugin jar folder exists
if exist "%LOCAL_REPO%\org\apache\maven\plugins\maven-help-plugin" (
    echo [OK] Maven Help Plugin appears to be downloaded.
) else (
    echo [FAIL] Maven Help Plugin is missing.i 
    exit /b 1
)
goto :EOF 
REM 4. Optional: pause for inspection
pause
