@echo OFF 
setlocal ENABLEDELAYEDEXPANSION ENABLEEXTENSIONS
subst E: /d > nul 2>& 1
set FOLDER=%1
if /i "%FOLDER%" EQU "" set "FOLDER=dist"
if NOT exist "%CD%\%FOLDER%"  (
  echo Directory does not exist
  exit /b 1
)
for /F "tokens=*" %%_ in ('cygpath.exe -maC OEM %FOLDER%') do @set URL=%%_

cd %FOLDER%
subst E: %CD%
"C:\Program Files\Google\Chrome\Application\chrome.exe" --user-data-dir=C:\temp\chrome-file-test --allow-file-access-from-files file:///E:/index.html
subst E: /d > nul 2>& 1
exit /b

