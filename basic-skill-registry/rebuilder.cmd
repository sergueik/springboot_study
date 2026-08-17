@echo off
set SCRIPT_NAME=%~dp0rebuider.ps1
powershell.exe -ExecutionPolicy Bypass -File %SCRIPT_NAME% %*
exit /b %errorlevel%
goto :EOF
echo rebuider.cmd sergueik springboot_study basic-karate-example3 master
