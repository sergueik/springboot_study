### Usage
* pull

* build
```
IMAGE=basic-gradle-side
docker build -t $IMAGE -f Dockerfile .
```
* run
```sh
docker container run -u root -it $IMAGE sh
```
this will print to console

```text
Welcome to Gradle 5.4.1!

Here are the highlights of this release:
 - Run builds with JDK12
 - New API for Incremental Tasks
 - Updates to native projects, including Swift 5 support

For more details see https://docs.gradle.org/5.4.1/release-notes.html

Starting a Gradle Daemon (subsequent builds will be faster)
> Task :definePath
> Task :testbuild
> Task :basic-karate-collector:clean UP-TO-DATE
> Task :basic-karate-collector:compileJava NO-SOURCE
> Task :basic-karate-collector:processResources NO-SOURCE
> Task :basic-karate-collector:classes UP-TO-DATE
> Task :basic-karate-collector:compileTestJava
> Task :basic-karate-collector:processTestResources
> Task :basic-karate-collector:testClasses
> Task :basic-karate-collector:test

> Task :testclean
deleting: [/work/karate/build/classes, /work/karate/build/karate-reports, /work/
karate/build/test-results, /work/karate/build/resources, /work/karate/build/repo
rts, /work/karate/build/tmp, /work/karate/build/generated]
done.

BUILD SUCCESSFUL in 4m 9s
3 actionable tasks: 3 executed
```
* debug

```sh
docker container run -u root -it $IMAGE sh
```
```sh
cd launcher
gradle testbuild
```


it will print

```sh
> Task :definePath
> Task :basic-karate-collector:clean
> Task :basic-karate-collector:compileJava NO-SOURCE
> Task :basic-karate-collector:processResources NO-SOURC
> Task :basic-karate-collector:classes UP-TO-DATE
> Task :basic-karate-collector:compileTestJava
> Task :basic-karate-collector:processTestResources
> Task :basic-karate-collector:testClasses
> Task :testbuild
> Task :basic-karate-collector:test

BUILD SUCCESSFUL in 42s
2 actionable tasks: 2 executed
```
verify built artifacts are present:
```sh
ls ./karate/build/karate-reports/
```
```text
example.feature.Test.html             karate-summary-json.txt
example.feature.Test.karate-json.txt  karate-summary.html
favicon.ico                           karate-tags.html
karate-labs-logo-ring.svg             karate-timeline.html
karate-logo.png                       res
karate-logo.svg
```
### Cleanup

```sh
docker container prune -f 
docker image rm $IMAGE
```
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
