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
pushd Source
mvn -ntp dependency:go-offline
```
```text
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
[INFO]
[INFO] <<< maven-dependency-plugin:2.8:go-offline (default-cli) < :resolve-plugins @ cobolToJson <<<
[INFO]
[WARNING] The POM for net.sf:cb2xml_src:jar:0.95.8 is missing, no dependency information available
[WARNING] The POM for net.sf:jrecord-base:jar:0.93.1 is missing, no dependency information available
```
```sh
 grep -ir mvn ../* --include 'README.md' | grep -vE '(test)'
```
