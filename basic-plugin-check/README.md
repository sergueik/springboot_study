### Usage
* set it up
```
mkdir -p $TEMP/maven-empty-repo
```
* this will act as Maven’s local repository for this test.

* Run Maven pointing to the empty repo overriding Maven’s default ~/.m2/repository using the `maven.repo.local` property:
```sh
mvn -Dmaven.repo.local=/tmp/maven-empty-repo help:help -ntp -B
```

NOTE: provide the CICD style options to reduce console logs
Alternatively may enable logging and scan for the line

```text
Downloading from maven-central-plugins: https://repo.maven.apache.org/maven2/org/apache/maven/plugins/maven-help-plugin/3.5.1/maven-help-plugin-3.5.1.pom
```
Call
```
run.cmd
```
```text
[OK] Maven Help Plugin appears to be downloaded.
```

when it fails it print
```text
[FAIL] Maven Help Plugin is missing.
```
alternatively run it by hand step by step
```cmd
mkdir -p %TEMP%\maven-empty-repo
mvn -Dmaven.repo.local=%TEMP%\maven-empty-repo help:help -ntp -B
```

```cmd
if exist "%temp%\maven-empty-repo\org"  echo 1
```
```text
1
```
```sh
rm -rf /tmp/maven-empty-repo/*
mvn -Dmaven.repo.local=/tmp/maven-empty-repo help:help -ntp -B
```
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
