### Info
__Standalone JRecord based COBOL 2 JSON Package__

### Install
```sh
VERSION=0.93.1

curl -skLo ~/Downloads/CobolToJson.zip https://master.dl.sourceforge.net/project/coboltojson/Version_$VERSION/CobolToJson_$VERSION.zip
```
```sh
unzip -l ~/Downloads/CobolToJson.zip
```

> NOTE expand full archive to `$TEMP`, remove some files before submitting 
```sh
unzip -d "${TEMP}/CobolToJson" -x ~/Downloads/CobolToJson.zip
```
```sh
unzip -x ~/Downloads/CobolToJson.zip
```
```sh
find . -iname __MACOSX -exec rm  {} \;
find . -iname '*zip' -exec rm  {} \;
find . -iname '*.jar' -exec rm  {} \;
```
pull dependencies and disconnect

```sh
rm -fr Source
git clone https://github.com/bmTas/CobolToJson Source
rm -fr Source/.git
rm -fr Source/.git
```
```sh
sed 's|0.93.4|0.90.2|g' Source/pom.xml
```
```sh
curl -skLo jrecord-0.93.3.jar https://github.com/bmTas/JRecord/releases/download/0.93.3/jrecord-0.93.3.jar
unzip -l jrecord-0.93.3.jar
mkdir -p ~/.m2/repository/net/sf/jrecord/Jrecord/0.93.3
cp jrecord-0.93.3.jar  ~/.m2/repository/net/sf/jrecord/Jrecord/0.93.3
```

```sh
pushd Source
mvn -ntp dependency:go-offline
```
```text
INFO] -------------------------< net.sf:cobolToJson >-------------------------
[INFO] Building CobolToJson 0.90.2
[INFO] --------------------------------[ jar ]---------------------------------
[INFO]
[INFO] >>> maven-dependency-plugin:2.8:go-offline (default-cli) > :resolve-plugins @ cobolToJson >>>
[INFO]
[INFO] --- maven-dependency-plugin:2.8:resolve-plugins (resolve-plugins) @ cobolToJson ---
[INFO] Plugin Resolved: maven-install-plugin-2.4.jar
[INFO]     Plugin Dependency Resolved: maven-plugin-api-2.0.6.jar
[INFO]     Plugin Dependency Resolved: maven-project-2.0.6.jar
[INFO]     Plugin Dependency Resolved: maven-model-2.0.6.jar
[INFO]     Plugin Dependency Resolved: maven-artifact-manager-2.0.6.jar
[INFO]     Plugin Dependency Resolved: maven-artifact-2.0.6.jar
[INFO]     Plugin Dependency Resolved: plexus-utils-3.0.5.jar
[INFO]     Plugin Dependency Resolved: plexus-digest-1.0.jar
[INFO] Plugin Resolved: maven-deploy-plugin-2.7.jar
[INFO]     Plugin Dependency Resolved: maven-plugin-api-2.0.6.jar
[INFO]     Plugin Dependency Resolved: maven-project-2.0.6.jar
[INFO]     Plugin Dependency Resolved: maven-model-2.0.6.jar
[INFO]     Plugin Dependency Resolved: maven-artifact-2.0.6.jar
[INFO]     Plugin Dependency Resolved: plexus-utils-1.5.6.jar
[INFO] Plugin Resolved: maven-jar-plugin-2.4.jar
[INFO]     Plugin Dependency Resolved: maven-plugin-api-2.0.6.jar
[INFO]     Plugin Dependency Resolved: maven-project-2.0.6.jar
[INFO]     Plugin Dependency Resolved: maven-model-2.0.6.jar
[INFO]     Plugin Dependency Resolved: maven-artifact-2.0.6.jar
[INFO]     Plugin Dependency Resolved: maven-archiver-2.5.jar
[INFO]     Plugin Dependency Resolved: plexus-archiver-2.1.jar
[INFO]     Plugin Dependency Resolved: commons-lang-2.1.jar
[INFO]     Plugin Dependency Resolved: plexus-utils-3.0.jar
[INFO] Plugin Resolved: maven-surefire-plugin-3.5.2.jar
[INFO]     Plugin Dependency Resolved: surefire-api-3.5.2.jar
[INFO]     Plugin Dependency Resolved: surefire-extensions-api-3.5.2.jar
[INFO]     Plugin Dependency Resolved: maven-surefire-common-3.5.2.jar
[INFO] Plugin Resolved: maven-compiler-plugin-3.1.jar
[INFO]     Plugin Dependency Resolved: maven-plugin-api-2.0.9.jar
[INFO]     Plugin Dependency Resolved: maven-artifact-2.0.9.jar
[INFO]     Plugin Dependency Resolved: maven-core-2.0.9.jar
[INFO]     Plugin Dependency Resolved: maven-toolchain-1.0.jar
[INFO]     Plugin Dependency Resolved: maven-shared-utils-0.1.jar
[INFO]     Plugin Dependency Resolved: maven-shared-incremental-1.1.jar
[INFO]     Plugin Dependency Resolved: plexus-compiler-api-2.2.jar
[INFO]     Plugin Dependency Resolved: plexus-compiler-manager-2.2.jar
[INFO]     Plugin Dependency Resolved: plexus-compiler-javac-2.2.jar
[INFO]     Plugin Dependency Resolved: plexus-container-default-1.5.5.jar
[INFO] Plugin Resolved: maven-site-plugin-3.3.jar
[INFO]     Plugin Dependency Resolved: maven-reporting-exec-1.1.jar
[INFO]     Plugin Dependency Resolved: maven-core-3.0.jar
[INFO]     Plugin Dependency Resolved: maven-model-3.0.jar
[INFO]     Plugin Dependency Resolved: maven-plugin-api-3.0.jar
[INFO]     Plugin Dependency Resolved: maven-settings-3.0.jar
[INFO]     Plugin Dependency Resolved: maven-settings-builder-3.0.jar
[INFO]     Plugin Dependency Resolved: maven-archiver-2.4.2.jar
[INFO]     Plugin Dependency Resolved: doxia-sink-api-1.4.jar
[INFO]     Plugin Dependency Resolved: doxia-logging-api-1.4.jar
[INFO]     Plugin Dependency Resolved: doxia-core-1.4.jar
[INFO]     Plugin Dependency Resolved: doxia-module-xhtml-1.4.jar
[INFO]     Plugin Dependency Resolved: doxia-module-apt-1.4.jar
[INFO]     Plugin Dependency Resolved: doxia-module-xdoc-1.4.jar
[INFO]     Plugin Dependency Resolved: doxia-module-fml-1.4.jar
[INFO]     Plugin Dependency Resolved: doxia-module-markdown-1.4.jar
[INFO]     Plugin Dependency Resolved: servlet-api-2.5.jar
[INFO]     Plugin Dependency Resolved: doxia-decoration-model-1.4.jar
[INFO]     Plugin Dependency Resolved: doxia-site-renderer-1.4.jar
[INFO]     Plugin Dependency Resolved: doxia-integration-tools-1.5.jar
[INFO]     Plugin Dependency Resolved: wagon-provider-api-1.0.jar
[INFO]     Plugin Dependency Resolved: plexus-archiver-1.0.jar
[INFO]     Plugin Dependency Resolved: plexus-i18n-1.0-beta-7.jar
[INFO]     Plugin Dependency Resolved: velocity-1.5.jar
[INFO]     Plugin Dependency Resolved: plexus-velocity-1.1.8.jar
[INFO]     Plugin Dependency Resolved: plexus-utils-1.5.10.jar
[INFO]     Plugin Dependency Resolved: jetty-6.1.25.jar
[INFO]     Plugin Dependency Resolved: jetty-util-6.1.25.jar
[INFO]     Plugin Dependency Resolved: commons-lang-2.5.jar
[INFO]     Plugin Dependency Resolved: commons-io-1.4.jar
[INFO] Plugin Resolved: maven-clean-plugin-2.5.jar
[INFO]     Plugin Dependency Resolved: maven-plugin-api-2.0.6.jar
[INFO]     Plugin Dependency Resolved: plexus-utils-3.0.jar
[INFO] Plugin Resolved: maven-resources-plugin-2.6.jar
[INFO]     Plugin Dependency Resolved: maven-plugin-api-2.0.6.jar
[INFO]     Plugin Dependency Resolved: maven-project-2.0.6.jar
[INFO]     Plugin Dependency Resolved: maven-core-2.0.6.jar
[INFO]     Plugin Dependency Resolved: maven-artifact-2.0.6.jar
[INFO]     Plugin Dependency Resolved: maven-settings-2.0.6.jar
[INFO]     Plugin Dependency Resolved: maven-model-2.0.6.jar
[INFO]     Plugin Dependency Resolved: maven-monitor-2.0.6.jar
[INFO]     Plugin Dependency Resolved: plexus-container-default-1.0-alpha-9-stable-1.jar
[INFO]     Plugin Dependency Resolved: plexus-utils-2.0.5.jar
[INFO]     Plugin Dependency Resolved: maven-filtering-1.1.jar
[INFO]     Plugin Dependency Resolved: plexus-interpolation-1.13.jar
[INFO]
[INFO] <<< maven-dependency-plugin:2.8:go-offline (default-cli) < :resolve-plugins @ cobolToJson <<<
[INFO]
[WARNING] Could not validate integrity of download from https://nexus.pentaho.org/content/groups/omni/net/sf/jrecord/JRecord/0.90.2/JRecord-0.90.2.pom: Checksum validation failed, expected <!DOCTYPE but is fd3e9f53c8fb633026897117d2ac76ef7016abec
[WARNING] The POM for net.sf.jrecord:JRecord:jar:0.90.2 is invalid, transitive dependencies (if any) will not be available, enable debug logging for more details
[WARNING] Could not validate integrity of download from https://nexus.pentaho.org/content/groups/omni/net/sf/jrecord/JRecord/0.90.2/JRecord-0.90.2.jar: Checksum validation failed, expected <!DOCTYPE but is df8fe1482784949857e7a2270ae0132cb193a1aa

```

```sh
mvn -DskiprTests package
```
> NOTE: if you do not skip tests, the build will take a while and fail in test goal:
```text
03:16 PM
[INFO] Scanning for projects...
[INFO]
[INFO] -------------------------< net.sf:cobolToJson >-------------------------
[INFO] Building CobolToJson 0.90.2
[INFO] --------------------------------[ jar ]---------------------------------
[WARNING] The POM for net.sf.jrecord:JRecord:jar:0.93.3 is invalid, transitive dependencies (if any) will not be available, enable debug logging for more details
[INFO]
[INFO] --- maven-resources-plugin:2.6:resources (default-resources) @ cobolToJson ---
[WARNING] Using platform encoding (Cp1252 actually) to copy filtered resources, i.e. build is platform dependent!
[INFO] skip non existing resourceDirectory C:\developer\sergueik\springboot_study\basic-cobol2json\Source\src\main\resources
[INFO]
[INFO] --- maven-compiler-plugin:3.1:compile (default-compile) @ cobolToJson ---
[INFO] Nothing to compile - all classes are up to date
[INFO]
[INFO] --- maven-resources-plugin:2.6:testResources (default-testResources) @ cobolToJson ---
[WARNING] Using platform encoding (Cp1252 actually) to copy filtered resources, i.e. build is platform dependent!
[INFO] Copying 73 resources
[INFO]
[INFO] --- maven-compiler-plugin:3.1:testCompile (default-testCompile) @ cobolToJson ---
[INFO] Changes detected - recompiling the module!
[WARNING] File encoding has not been set, using platform encoding Cp1252, i.e. build is platform dependent!
[INFO] Compiling 68 source files to C:\developer\sergueik\springboot_study\basic-cobol2json\Source\target\test-classes
[WARNING] /C:/developer/sergueik/springboot_study/basic-cobol2json/Source/src/test/java/net/sf/cobolToJson/zTest/TstReadJSon.java: C:\developer\sergueik\springboot_study\basic-cobol2json\Source\src\test\java\net\sf\cobolToJson\zTest\TstReadJSon.java uses or overrides a deprecated API.
[WARNING] /C:/developer/sergueik/springboot_study/basic-cobol2json/Source/src/test/java/net/sf/cobolToJson/zTest/TstReadJSon.java: Recompile with -Xlint:deprecation for details.
[INFO]
[INFO] --- maven-surefire-plugin:3.2.5:test (default-test) @ cobolToJson ---
[INFO] Using auto detected provider org.apache.maven.surefire.junit4.JUnit4Provider
[INFO]
[INFO] -------------------------------------------------------
[INFO]  T E S T S
[INFO] -------------------------------------------------------
[INFO] Running net.sf.cobolToJson.zTest.cbl2json.attr.TstSetAttributes01
...19,000 lines of logs omitted
[INFO]
[ERROR] Tests run: 82, Failures: 45, Errors: 0, Skipped: 0
```

```cmd
cd /d ..\lib
copy /y ..\source\coboltojson\target\cobolToJson-0.90.2.jar Cobol2Json.jar
call Cobol2Json.bat -cobol "..\Example\cobol\DTAR020a.cbl" -fileOrganisation FixedWidth -font cp037 -input "..\Example\in\DTAR020.bin" -output DTAR020.json
```
```cmd

```
```text
no main manifest attribute, in C:\developer\sergueik\springboot_study\basic-cobol2json\lib\/Cobol2Json.jar
```
```text
Archive:  target\cobolToJson-0.90.2.jar
  Length      Date    Time    Name
---------  ---------- -----   ----
        0  2026-01-15 15:14   META-INF/
      129  2026-01-15 15:14   META-INF/MANIFEST.MF
        0  2026-01-15 15:07   net/
        0  2026-01-15 15:07   net/sf/
        0  2026-01-15 15:07   net/sf/cobolToJson/
        0  2026-01-15 15:07   net/sf/cobolToJson/def/
        0  2026-01-15 15:07   net/sf/cobolToJson/impl/
        0  2026-01-15 15:07   net/sf/cobolToJson/impl/jsonSchema/
        0  2026-01-15 15:07   net/sf/cobolToJson/impl/jsonWriter/
        0  2026-01-15 15:07   net/sf/cobolToJson/impl/readJson/
        0  2026-01-15 15:07   net/sf/cobolToJson/impl/updateSchema/
        0  2026-01-15 15:07   net/sf/cobolToJson/util/
        0  2026-01-15 15:07   net/sf/cobolToJson/util/errorLog/
     6266  2026-01-15 15:07   net/sf/cobolToJson/Cobol2Json.class
     2083  2026-01-15 15:07   net/sf/cobolToJson/CobolJsonConversion.class
     1078  2026-01-15 15:07   net/sf/cobolToJson/Data2Json.class
     4443  2026-01-15 15:07   net/sf/cobolToJson/def/Icb2xml2Json.class
     7962  2026-01-15 15:07   net/sf/cobolToJson/def/ICobol2Json.class
     4510  2026-01-15 15:07   net/sf/cobolToJson/def/ICobolJsonConversion.class
      677  2026-01-15 15:07   net/sf/cobolToJson/def/ICobolMultipleRecordsToArrayIn.class
      457  2026-01-15 15:07   net/sf/cobolToJson/def/ICobolMultipleRecordsToArrayOut.class
      593  2026-01-15 15:07   net/sf/cobolToJson/def/ICobolRecordToJsonObjectIn.class
      438  2026-01-15 15:07   net/sf/cobolToJson/def/ICobolRecordToJsonObjectOut.class
      472  2026-01-15 15:07   net/sf/cobolToJson/def/IJsonArrayToMultipleCobolRecordsIn.class
      459  2026-01-15 15:07   net/sf/cobolToJson/def/IJsonArrayToMultipleCobolRecordsOut.class
      417  2026-01-15 15:07   net/sf/cobolToJson/def/IJsonObjectToCobolRecordIn.class
      383  2026-01-15 15:07   net/sf/cobolToJson/def/IJsonObjectToCobolRecordOut.class
      235  2026-01-15 15:07   net/sf/cobolToJson/impl/Cobol2JsonImp$1.class
     1483  2026-01-15 15:07   net/sf/cobolToJson/impl/Cobol2JsonImp$IntStack.class
     1480  2026-01-15 15:07   net/sf/cobolToJson/impl/Cobol2JsonImp$ReadManager.class
    41749  2026-01-15 15:07   net/sf/cobolToJson/impl/Cobol2JsonImp.class
      244  2026-01-15 15:07   net/sf/cobolToJson/impl/Cobol2JsonSchema$1.class
     1510  2026-01-15 15:07   net/sf/cobolToJson/impl/Cobol2JsonSchema$IntStack.class
    14354  2026-01-15 15:07   net/sf/cobolToJson/impl/Cobol2JsonSchema.class
     4244  2026-01-15 15:07   net/sf/cobolToJson/impl/CobolMultipleRecordsToJsonArray.class
     3714  2026-01-15 15:07   net/sf/cobolToJson/impl/CobolRecordToJsonObject.class
      238  2026-01-15 15:07   net/sf/cobolToJson/impl/ConvertOptions$1.class
     1381  2026-01-15 15:07   net/sf/cobolToJson/impl/ConvertOptions$Opts.class
    10453  2026-01-15 15:07   net/sf/cobolToJson/impl/ConvertOptions.class
     2762  2026-01-15 15:07   net/sf/cobolToJson/impl/JsonArrayToMultipleCobolRecords.class
     2629  2026-01-15 15:07   net/sf/cobolToJson/impl/JsonObjectToCobolRecord.class
     2137  2026-01-15 15:07   net/sf/cobolToJson/impl/jsonSchema/JsonItem.class
     1960  2026-01-15 15:07   net/sf/cobolToJson/impl/jsonSchema/JsonType.class
      804  2026-01-15 15:07   net/sf/cobolToJson/impl/jsonSchema/WriteJsonSchema$1.class
     3816  2026-01-15 15:07   net/sf/cobolToJson/impl/jsonSchema/WriteJsonSchema.class
      991  2026-01-15 15:07   net/sf/cobolToJson/impl/jsonWriter/IJsonWriter.class
     4175  2026-01-15 15:07   net/sf/cobolToJson/impl/jsonWriter/JsonSchemaCreator.class
     3862  2026-01-15 15:07   net/sf/cobolToJson/impl/jsonWriter/JsonWriter.class
     1032  2026-01-15 15:07   net/sf/cobolToJson/impl/readJson/AFieldsToJRecLine.class
      201  2026-01-15 15:07   net/sf/cobolToJson/impl/readJson/IArrayDtls.class
      386  2026-01-15 15:07   net/sf/cobolToJson/impl/readJson/IProcessFields.class
     1253  2026-01-15 15:07   net/sf/cobolToJson/impl/readJson/JsonToCobol$1.class
     2886  2026-01-15 15:07   net/sf/cobolToJson/impl/readJson/JsonToCobol$ArrayMgr.class
      709  2026-01-15 15:07   net/sf/cobolToJson/impl/readJson/JsonToCobol$IndexDtls.class
     1676  2026-01-15 15:07   net/sf/cobolToJson/impl/readJson/JsonToCobol$ParserMgr.class
     3756  2026-01-15 15:07   net/sf/cobolToJson/impl/readJson/JsonToCobol.class
     2067  2026-01-15 15:07   net/sf/cobolToJson/impl/readJson/ToJRecordFile.class
      988  2026-01-15 15:07   net/sf/cobolToJson/impl/readJson/ToJRecordLine.class
      920  2026-01-15 15:07   net/sf/cobolToJson/impl/RecordParent.class
     1078  2026-01-15 15:07   net/sf/cobolToJson/impl/RecordSelect.class
     1394  2026-01-15 15:07   net/sf/cobolToJson/impl/updateSchema/BlankLine.class
     1361  2026-01-15 15:07   net/sf/cobolToJson/impl/updateSchema/BlankLineReader.class
     1950  2026-01-15 15:07   net/sf/cobolToJson/impl/updateSchema/BlankLineReaderAllOptions.class
      239  2026-01-15 15:07   net/sf/cobolToJson/impl/updateSchema/ExitManager$IExit.class
     2285  2026-01-15 15:07   net/sf/cobolToJson/impl/updateSchema/ExitManager$RedefinesSelection.class
     1503  2026-01-15 15:07   net/sf/cobolToJson/impl/updateSchema/ExitManager$UpdateRecorder.class
     1637  2026-01-15 15:07   net/sf/cobolToJson/impl/updateSchema/ExitManager$WriteCheck.class
     1959  2026-01-15 15:07   net/sf/cobolToJson/impl/updateSchema/ExitManager.class
      223  2026-01-15 15:07   net/sf/cobolToJson/impl/updateSchema/IIndexList.class
     1892  2026-01-15 15:07   net/sf/cobolToJson/impl/updateSchema/ItemUpdateDetails.class
      267  2026-01-15 15:07   net/sf/cobolToJson/impl/updateSchema/RecordIndexList$1.class
     1448  2026-01-15 15:07   net/sf/cobolToJson/impl/updateSchema/RecordIndexList$StandardIndexList.class
     1960  2026-01-15 15:07   net/sf/cobolToJson/impl/updateSchema/RecordIndexList$TreeIndexList.class
     1140  2026-01-15 15:07   net/sf/cobolToJson/impl/updateSchema/RecordIndexList.class
     1865  2026-01-15 15:07   net/sf/cobolToJson/impl/updateSchema/UpdateDetailsForSampleRecords.class
      640  2026-01-15 15:07   net/sf/cobolToJson/Json2Data.class
     3379  2026-01-15 15:07   net/sf/cobolToJson/util/errorLog/BasicErrorLog.class
      312  2026-01-15 15:07   net/sf/cobolToJson/util/errorLog/ILogLinesInError.class
        0  2026-01-15 15:14   META-INF/maven/
        0  2026-01-15 15:14   META-INF/maven/net.sf/
        0  2026-01-15 15:14   META-INF/maven/net.sf/cobolToJson/
     4759  2026-01-15 15:12   META-INF/maven/net.sf/cobolToJson/pom.xml
      108  2026-01-15 15:14   META-INF/maven/net.sf/cobolToJson/pom.properties
---------                     -------
   181861                     83 files

```
```text
Archive:  Cobol2Json.jar
  Length      Date    Time    Name
---------  ---------- -----   ----
      199  2023-06-07 17:25   META-INF/MANIFEST.MF
     3367  2023-05-24 22:26   net/sf/JRecord/util/errorLog/BasicErrorLog.class
      308  2023-05-24 22:26   net/sf/JRecord/util/errorLog/ILogLinesInError.class
     6267  2023-06-07 17:25   net/sf/cobolToJson/Cobol2Json.class
     1078  2023-05-24 22:26   net/sf/cobolToJson/Data2Json.class
      640  2023-05-24 22:26   net/sf/cobolToJson/Json2Data.class
     7608  2023-06-07 17:25   net/sf/cobolToJson/def/ICobol2Json.class
     4149  2023-06-07 17:25   net/sf/cobolToJson/def/Icb2xml2Json.class
      235  2023-06-07 17:25   net/sf/cobolToJson/impl/Cobol2JsonImp$1.class
     1483  2023-06-07 17:25   net/sf/cobolToJson/impl/Cobol2JsonImp$IntStack.class
     1512  2023-06-07 17:25   net/sf/cobolToJson/impl/Cobol2JsonImp$ReadManager.class
    32871  2023-06-07 17:25   net/sf/cobolToJson/impl/Cobol2JsonImp.class
      244  2023-05-24 22:26   net/sf/cobolToJson/impl/Cobol2JsonSchema$1.class
     1510  2023-05-24 22:26   net/sf/cobolToJson/impl/Cobol2JsonSchema$IntStack.class
    14359  2023-05-24 22:26   net/sf/cobolToJson/impl/Cobol2JsonSchema.class
      238  2023-05-24 22:26   net/sf/cobolToJson/impl/ConvertOptions$1.class
     1381  2023-05-24 22:26   net/sf/cobolToJson/impl/ConvertOptions$Opts.class
    10505  2023-05-24 22:26   net/sf/cobolToJson/impl/ConvertOptions.class
      920  2023-05-24 22:26   net/sf/cobolToJson/impl/RecordParent.class
     1078  2023-05-24 22:26   net/sf/cobolToJson/impl/RecordSelect.class
     1032  2023-05-24 22:26   net/sf/cobolToJson/impl/readJson/AFieldsToJRecLine.class
      201  2023-05-24 22:26   net/sf/cobolToJson/impl/readJson/IArrayDtls.class
      385  2023-05-24 22:26   net/sf/cobolToJson/impl/readJson/IProcessFields.class
     1253  2023-05-24 22:26   net/sf/cobolToJson/impl/readJson/JsonToCobol$1.class
     1904  2023-05-24 22:26   net/sf/cobolToJson/impl/readJson/JsonToCobol$ArrayMgr.class
     1676  2023-05-24 22:26   net/sf/cobolToJson/impl/readJson/JsonToCobol$ParserMgr.class
     3694  2023-05-24 22:26   net/sf/cobolToJson/impl/readJson/JsonToCobol.class
     2043  2023-05-24 22:26   net/sf/cobolToJson/impl/readJson/ToJRecordFile.class
      961  2023-05-24 22:26   net/sf/cobolToJson/impl/readJson/ToJRecordLine.class
---------                     -------
   103101                     29 files

```have in `META-INF\MANIFEST.MF`

```
Manifest-Version: 1.0
Archiver-Version: Plexus Archiver
Created-By: Apache Maven 3.6.1
Built-By: kouzm
Build-Jdk: 11.0.12


```
need to  have in `META-INF\MANIFEST.MF`
```text
Ant-Vesion: Apache Ant 1.10.12
Created-By: 17.0.6+9-LTS-190 (Oracle Corporation)
Main-Class: net.sf.cobolToJson.Data2Json
Class-Path: cb2xml.jar JRecord.jar Jackson.jar

```

Verdict

You successfully built a JAR, but Maven built it as a library JAR, not an executable JAR
```tetx

mkdir lib

copy %USERPROFILE%\\.m2\repository\net\sf\jrecord\JRecord\0.93.3\JRecord-0.93.3.jar .\lib
copy %USERPROFILE%\\.m2\repository\com\github\\bmTas\cb2xml\1.01.08\cb2xml-1.01.08.jar lib
copy %USERPROFILE%\\.m2\repository\com\fasterxml\jackson\core\jackson-core\2.17.2\jackson-core-2.17.2.jar lib
```
```text
[ERROR] error reading C:\Users\kouzm\.m2\repository\net\sf\jrecord\JRecord\0.93.3\JRecord-0.93.3.jar; zip END header not found
```
```cmd
call Cobol2Json.bat -cobol "..\Example\cobol\DTAR020a.cbl" -fileOrganisation FixedWidth -font cp037 -input "..\Example\in\DTAR020.bin" -output DTAR020.json
```
```text
type DTAR020.json | c:\tools\jq ".sss[0:3]"
```
```JSON
[
  {
    "DTAR020-KCODE-STORE-KEY": {
      "DTAR020-KEYCODE-NO": "69684558",
      "DTAR020-STORE-NO": 20
    },
    "DTAR020-DATE": 40118,
    "DTAR020-DEPT-NO": 280,
    "DTAR020-QTY-SOLD": 1,
    "DTAR020-SALE-PRICE": 19.00
  },
  {
    "DTAR020-KCODE-STORE-KEY": {
      "DTAR020-KEYCODE-NO": "69684558",
      "DTAR020-STORE-NO": 20
    },
    "DTAR020-DATE": 40118,
    "DTAR020-DEPT-NO": 280,
    "DTAR020-QTY-SOLD": -1,
    "DTAR020-SALE-PRICE": -19.00
  },
  {
    "DTAR020-KCODE-STORE-KEY": {
      "DTAR020-KEYCODE-NO": "69684558",
      "DTAR020-STORE-NO": 20
    },
    "DTAR020-DATE": 40118,
    "DTAR020-DEPT-NO": 280,
    "DTAR020-QTY-SOLD": 1,
    "DTAR020-SALE-PRICE": 5.01
  }
]

```
```sh


 grep -ir mvn ../* --include 'README.md' | grep -vE '(test)'
```
### See Also

 * [JRecord](https://github.com/bmTas/JRecord) - core COBOL I/O & utilities
 * [cb2xml](https://github.com/bmTas/cb2xml) - COBOL Copybook → XML / Java model parser - internal representation in __JRecord__
 * https://github.com/bmTas/CobolToJson
