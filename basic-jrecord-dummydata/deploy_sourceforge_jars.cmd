@echo off
REM -----------------------------
REM Use environment variable for user profile
REM -----------------------------
set "USER_HOME=%USERPROFILE%"
set "MAVEN_LOCAL_REPO=%USER_HOME%\.m2\private-repo"

REM Create directory if it doesn't exist
if not exist "%MAVEN_LOCAL_REPO%" mkdir "%MAVEN_LOCAL_REPO%"

REM Deploy all three JRecord jars to custom Maven repo
mvn deploy:deploy-file -DgroupId=net.sf.jrecord -DartifactId=JRecord -Dversion=0.93.2 -Dpackaging=jar -Dfile=JRecord.jar -Durl=file:///%MAVEN_LOCAL_REPO% -DrepositoryId=local & ^
mvn deploy:deploy-file -DgroupId=net.sf.jrecord -DartifactId=JRecordCodeGen -Dversion=0.93.2 -Dpackaging=jar -Dfile=JRecordCodeGen.jar -Durl=file:///%MAVEN_LOCAL_REPO% -DrepositoryId=local & ^
mvn deploy:deploy-file -DgroupId=net.sf.jrecord -DartifactId=cb2xml -Dversion=0.93.2 -Dpackaging=jar -Dfile=cb2xml.jar -Durl=file:///%MAVEN_LOCAL_REPO% -DrepositoryId=local

echo ✅ All JRecord jars deployed to %MAVEN_LOCAL_REPO%
