@echo off
setlocal enabledelayedexpansion

REM ============================================================================
REM CONFIGURATION
REM ============================================================================

set WORKDIR=%CD%\build
set MVN_REPO=%WORKDIR%\m2
set JAVA_REQUIRED_MAJOR=11

REM ============================================================================
REM TOOL CHECKS
REM ============================================================================

where git >nul 2>&1 || (
  echo ERROR: git not found
  exit /b 1
)

where mvn >nul 2>&1 || (
  echo ERROR: maven not found
  exit /b 1
)

where java >nul 2>&1 || (
  echo ERROR: java not found
  exit /b 1
)
goto :SKIP_JAVA_VERSION_CHECK
for /f "tokens=2 delims==." %%v in ('java -version 2^>^&1 ^| findstr "version"') do (
  set JAVA_MAJOR=%%v
)

if %JAVA_MAJOR% LSS %JAVA_REQUIRED_MAJOR% (REM
  echo ERROR: Java %JAVA_REQUIRED_MAJOR%+ required
  exit /b 1
)
:SKIP_JAVA_VERSION_CHECK

REM TODO: check
REM for /F "tokens=*" %%. in ('ping -n 1 8.8.8.8 ^| findstr -i reply')  do @echo %%.
REM Reply from 192.168.12.178: Destination host unreachable.

REM ============================================================================
REM CLEAN START
REM ============================================================================

echo ==> Cleaning work directory
rmdir /s /q "%WORKDIR%" 2>nul
mkdir "%WORKDIR%"
mkdir "%MVN_REPO%"

set MVN_OPTS=-Dmaven.repo.local=%MVN_REPO% -DskipTests

REM ============================================================================
REM COPY OVERRIDDEN FILES
REM ============================================================================

copy pom.cb2xml.xml "%WORKDIR%" >nul
copy pom.jrecord.xml "%WORKDIR%" >nul
copy pom.cobol2json.xml "%WORKDIR%" >nul
copy avoid_cb2xml_deps.txt "%WORKDIR%" >nul
copy avoid_jrecord_deps.txt "%WORKDIR%" >nul

REM ============================================================================
REM FUNCTION-LIKE BLOCK: BUILD COMPONENT
REM ============================================================================

call :build_component cb2xml https://github.com/bmTas/cb2xml 97f8dc8 pom.cb2xml.xml avoid_cb2xml_deps.txt
call :build_component jrecord https://github.com/bmTas/JRecord f50ece71 pom.jrecord.xml avoid_jrecord_deps.txt

REM ============================================================================
REM BUILD APPLICATION
REM ============================================================================

echo.
echo ==> Building CobolToJson

cd /d "%WORKDIR%"
git clone https://github.com/bmTas/CobolToJson cobol2json || exit /b 1
cd cobol2json
git checkout 99b0aa2 || exit /b 1

copy "%WORKDIR%\pom.cobol2json.xml" pom.xml >nul

call mvn %MVN_OPTS% -DskipTests clean package || exit /b 1

REM ============================================================================
REM LOCATE APPLICATION JAR
REM ============================================================================

set APP_JAR=

for %%f in (target\*-shaded.jar) do (
  set APP_JAR=%%f
)

if not defined APP_JAR (
  for %%f in (target\*.jar) do (
    echo %%~nxf | findstr /v "^original-" >nul && set APP_JAR=%%f
  )
)

if not defined APP_JAR (
  echo ERROR: application jar not found
  exit /b 1
)

echo Built %APP_JAR%

REM ============================================================================
REM INSPECT JAR (CMD-ONLY)
REM ============================================================================

echo.
echo ==> Inspecting shaded JAR

jar tf "%APP_JAR%" > "%WORKDIR%\jar.lst" || exit /b 1

findstr /c:"META-INF/MANIFEST.MF" "%WORKDIR%\jar.lst" >nul || (
  echo ERROR: MANIFEST.MF missing
  exit /b 1
)

jar xf "%APP_JAR%" META-INF\MANIFEST.MF || exit /b 1

findstr /b "Main-Class:" META-INF\MANIFEST.MF >nul || (
  echo ERROR: Main-Class missing in manifest
  exit /b 1
)

for /f %%j in ('findstr /r "\.jar$" "%WORKDIR%\jar.lst"') do (
  echo ERROR: nested jar detected: %%j
  exit /b 1
)

rmdir /s /q META-INF

echo OK: shaded jar structure valid

REM ============================================================================
REM VALIDATE MAVEN REPO HYGIENE
REM ============================================================================

echo.
echo ==> Validating Maven repo isolation

for %%p in (
  "\com\github\"
  "\org\springframework\"
  "\org\apache\httpcomponents\"
  "\ch\qos\logback\"
) do (
  dir /s /b "%MVN_REPO%\*%%p*" >nul 2>&1 && (
    echo ERROR: forbidden dependency leaked: %%p
    exit /b 1
  )
)

REM ============================================================================
REM DONE
REM ============================================================================

echo.
echo ==> BUILD COMPLETE
echo     Shaded JAR: %APP_JAR%
echo     Maven repo: %MVN_REPO%
exit /b 0

REM ============================================================================
REM SUBROUTINES
REM ============================================================================

:build_component
set NAME=%1
set REPO=%2
set COMMIT=%3
set POM=%4
set AVOID=%5

echo.
echo ==> Building %NAME%

cd /d "%WORKDIR%"
git clone %REPO% %NAME% || exit /b 1
cd %NAME%
git checkout %COMMIT% || exit /b 1

copy "%WORKDIR%\%POM%" pom.xml >nul
if exist "%WORKDIR%\%AVOID%" copy "%WORKDIR%\%AVOID%" avoid_deps.txt >nul

call mvn %MVN_OPTS% -f pom.xml -DskipTests clean install || exit /b 1
exit /b 0

