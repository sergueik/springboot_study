### Info

this directory contains Jenkins installed on JDK 8 on alpine python base image `python:3.8.2-alpine` using the instrucions borrowed from
[HearstAT/docker-alpinejenkins](https://github.com/HearstAT/docker-alpinejenkins) but installing JDK explicitly

### Usage
* build the image

```sh
IMAGE=basic-python-jenkins
docker build -f Dockerfile -t $IMAGE .
```
* followed by
```sh
NAME=$IMAGE
docker run --name $NAME -p 8080:8080 -d $IMAGE
```
* wait for logs
```sh
docker logs -f $NAME
```
to show the
```text
*************************************************************
*************************************************************
*************************************************************

Jenkins initial setup is required. An admin user has been created and a password generated.
Please use the following password to proceed to installation:

f73bffb1dfb5424f8f61d38d44aa02e7

This may also be found at: /var/lib/jenkins/secrets/initialAdminPassword

*************************************************************
*************************************************************
*************************************************************
```

the password will vary with the instance. Run as advised to not rely on the message to stay onscreen

```sh
docker exec -it $NAME cat /var/lib/jenkins/secrets/initialAdminPassword
```

* open and do initial setup through the browser

![setup](https://github.com/sergueik/springboot_study/blob/master/basic-python-jenkins/screenshots/capture-jenkins-start.png)
### Plugins

to import plugins the way the vendor image does, clone it

```sh
export JENKINS_VERSION=2.346.2
docker pull jenkins/jenkins:${JENKINS_VERSION}-alpine
```

* copy the script file
```sh
docker run -it jenkins:${JENKINS_VERSION}-alpine sh
```

```sh
export JENKINS_PLUGIN_MANAGER_VERSION=2.12.8
curl -fL https://github.com/jenkinsci/plugin-installation-manager-tool/releases/download/${JENKINS_PLUGIN_MANAGER_VERSION}/jenkins-plugin-manager-${JENKINS_PLUGIN_MANAGER_VERSION}.jar -o jenkins-plugin-manager.jar
```
* copy jar into the container
```sh
docker cp jenkins-plugin-manager.jar $NAME:/var/lib/jenkins
```
* run install commands
```sh
docker  exec -it $NAME java -jar /var/lib/jenkins/jenkins-plugin-manager.jar --war /usr/share/jenkins/jenkins.war --plugins naginator-plugin
```

apparently will need to upgrade to JDK 11:
```text
Exception in thread "main" java.lang.UnsupportedClassVersionError: 
io/jenkins/tools/pluginmanager/cli/Main has been compiled by a more recent version
of the Java Runtime (class file version 55.0), 
this version of the Java Runtime only recognizes 
class file versions up to 52.0
```

Alternatively use Jenkins version __2.154__

### See Also

  * another [docker Jenkins alpine](https://github.com/liatrio/alpine-jenkins), where notably along with core war, installs plugins like `naginator`
  * How to create a custom Docker image with JDK8, Maven and Gradle [blog](https://medium.com/@migueldoctor/how-to-create-a-custom-docker-image-with-jdk8-maven-and-gradle-ddc90f41cee4)
  * [jenkinsci/plugin-installation-manager-tool](https://github.com/jenkinsci/plugin-installation-manager-tool)
  * Plugin Installation Manager CLI Tool / Library  [documentation](https://www.jenkins.io/projects/gsoc/2019/plugin-installation-manager-tool-cli/)
  * Managing Plugins [documentation](https://www.jenkins.io/doc/book/managing/plugins/)


### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
