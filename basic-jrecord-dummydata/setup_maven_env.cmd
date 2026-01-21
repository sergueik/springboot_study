@echo off
REM Step 1: Set Maven env for a private repository
set MAVEN_LOCAL_REPO=%USERPROFILE%\.m2\private-repo
if not exist "%MAVEN_LOCAL_REPO%" mkdir "%MAVEN_LOCAL_REPO%"

set MAVEN_OPTS=-Dmaven.repo.local=%MAVEN_LOCAL_REPO%
echo Maven local repo set to %MAVEN_LOCAL_REPO%

