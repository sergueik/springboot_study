@echo off
set SCRIPT_NAME=%~dp0rebuilder.ps1
powershell.exe -ExecutionPolicy Bypass -File %SCRIPT_NAME% %*
exit /b %errorlevel%
goto :EOF
