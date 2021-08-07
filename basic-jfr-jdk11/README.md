### Info

This directory contains 
https://mkyong.com/java/java-11-java-flight-recorder/
a basi Selenium compiled as standalone app to practice running Selenium tests on Java 11 runtime -
* compile and package jar on JDK11/Maven "builder" container and copy the jar into a searate Docker image with JDK 11 and jfr:


```sh
DOCKER_IMAGE=alpine-jdk11-maven
docker build -t $DOCKER_IMAGE -f Dockerfile.$DOCKER_IMAGE .
```
followed by
```sh
DOCKER_IMAGE=alpine-jdk11-jfr
docker build -t $DOCKER_IMAGE -f Dockerfile.$DOCKER_IMAGE .
docker run -it $DOCKER_IMAGE
```
which will log
```sh
Started recording 1. The result will be written to:

/application/leak.jfr
Exception in thread "Producer Thread" java.lang.OutOfMemoryError: Java heap space
  at example.App.lambda$main$0(App.java:23)
  at example.App$$Lambda$70/0x00000001000e8440.run(Unknown Source)
  at java.base/java.lang.Thread.run(Thread.java:829)
```
* copy recorder artifact to the host
```sh
docker cp $(docker container ls | grep $DOCKER_IMAGE | head -1 | awk '{print #1}'):/application/leak.jfr .
```
Note:
The JMC 8 runs fine on Windows or Bionic and later releases. 

On Xenial Ubuntu Linux __16.04__/ JDK 8 the jmc (which is just an eclipse) 
```sh
~/Downloads/jmc-8.0.1_linux-x64/JDK\ Mission\ Control/jmc 
```
has SWT having some trouble with GTK configuration 
 
```sh
 **WARNING: GTK+ version too old (micro mismatch)
***WARNING: SWT requires GTK 3.20.0
***WARNING: Detected: 3.18.9
```
making file dialog opening files fail to display, so temporarily copy the recorder file `leak.jfr` to a Windows or Bionic machine and launch from there
The other alternative to try is to use `jmc` that is part of JDK 8:
```sh
/opt/jdk1.8.0_161/bin/jmc
```
but that fails since the default `jfr` format produced by __JDK 11__ JFR is not supported by JMC __5.5__.

The other option is to replace
```sh
~/Downloads/jmc-8.0.1_linux-x64/JDK Mission Control/plugins/org.eclipse.swt.gtk.linux.x86_64_3.114.100.v20210108-0932.jar
```
with the latest of 

```sh
find  /opt/jdk1.8.0_161 -iname 'org.eclipse.swt.gtk.linux.x86_64_*'
find  /opt/eclipse/ -iname 'org.eclipse.swt.gtk.linux.x86_64_*'
```

this is failing because some classes were not provided in that jar:
```sh
java.lang.NoClassDefFoundError: org/eclipse/swt/SWTError
1628277319957.log:Caused by: java.lang.ClassNotFoundException: org.eclipse.swt.SWTError cannot be found by org.eclipse.ui.workbench_3.119.0.v20200521-1247
```

### See Also
 * https://groups.google.com/g/smartsvn/c/Z4lJ6sGdo7Q?pli=1
 
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
