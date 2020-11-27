### Info

This directory contains a springboot wrapped version of [SWET JavaFx project](https://github.com/sergueik/selenium_java/tree/master/javafx_example)
based on [Skeleton SpringBoot + JavaFx](https://github.com/ruslanys/sample-spring-boot-javafx).

The SWET JavaFx in turn is the JavaFx port of [SWT based Selenum recorder](https://github.com/sergueik/SWET).

![icon](https://github.com/sergueik/springboot_study/tree/master/javafx_swet/screenshots/swet_javafx.png)

### Run application

Compile and start appplication
```sh
mvn clean spring-boot:run
```

#### Note for Linux Platform
On Linux to run the javaFx 2.x application one needs to [install javafx-java](https://stackoverflow.com/questions/9294646/javafx-2-as-a-maven-dependency) system-wide
```sh
sudo apt-get install -qqy libopenjfx-java
```
To prevent the
```sh
cannot find package javafx.scene
```
build error on Linux one has to [completely remove openjdk](https://github.com/narrowtux/SmartModInserter/issues/4):

```sh
sudo apt-get remove -qqy openjdk-8-jre openjdk-8-jdk openjdk-8-jre-headless
sudo apt-get -qqy autoremove
sudo apt-get -qqy autoclean
```
then download Maven standalone from [Apache Maven project](https://maven.apache.org/download.cgi) and install and add to alternatives:

```sh
tar xzvf ~/Downloads/apache-maven-3.5.3-bin.tar.gz
sudo mv apache-maven-3.5.3 /usr/lib/jvm/
update-alternatives --list
sudo update-alternatives --install /usr/bin/javac javac /usr/lib/jvm/jdk1.8.0_161/bin/javac 0
sudo update-alternatives --install /usr/bin/java java /usr/lib/jvm/jdk1.8.0_161/bin/java 0
sudo update-alternatives --install /usr/bin/mvn mvn /usr/lib/jvm/apache-maven-3.5.3/bin/mvn 0
```
- on ubuntu or centos maven is bound to depend on `opendjk-jre`.
The Oracle java can be downloaded from [Oracle Technology Network](http://www.oracle.com/technetwork/java/javase/downloads/index.html).

### See also

  * Integration of JavaFX and Spring technologies on the client and server [demo roject](https://github.com/steveonjava/javafx-spring)
  * [Javafx2 with spring](http://koenserneels.blogspot.com/2012/11/javafx-2-with-spring.html)
  *  [SpringBoot + JavaFx publication](https://habrahabr.ru/post/265511) (in russian)
  * [JavaFX and classic Spring](https://habr.com/ru/post/203960) (in Russian) 
  * [JavaFX and classic Spring](https://habr.com/ru/post/348850) (in Russian) 
  * [JavaFx + Spring Boot + Gradle: Project Setup guide and test](https://better-coding.com/javafx-spring-boot-gradle-project-setup-guide-and-test/)


### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
