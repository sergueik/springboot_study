
### Info

This directory contains examples to use Docker command line to run and log processes one time in a later discarded Docker container,
e.g. for  exercising java 11 or later

### Usage
* compile and run the `EnvironmentPrinter.java` source file residing in the current directory. Note the quotes
```sh
CLASS=EnvironmentPrinter
NAME=basic-command
IMAGE=openjdk:8-jdk-alpine3.9
WORKDIR=/tmp/app
LOGFILE=run.log
SCRIPT="(javac $CLASS.java|tee $LOGFILE; java $CLASS 2>&1|tee -a $LOGFILE)"
docker run --name $NAME -w $WORKDIR -d -v $(pwd):$WORKDIR $IMAGE /bin/sh -c "$SCRIPT"
```
then look in `$LOGFILE`
* alternatively

```sh
SCRIPT="javac $CLASS.java  2>&1|tee $LOGFILE; java $CLASS 2>&1|tee -a $LOGFILE"
echo "$SCRIPT" | docker run --name $NAME -w $WORKDIR -i -v $(pwd):$WORKDIR $IMAGE
```
Note, without `-i` will see no output, but based on absence of the `$LOGFILE`, execution failed and tricky to debug
### Cleanup

```sh
ID=$(docker container ls -a | grep $NAME|cut -d' ' -f1); test $ID && docker container rm $ID
rm -f $LOGFILE $CLASS.class
```
note, `$LOGFILE` and `$CLASS.class` in the current directory will be owned by the root user.

### See Also
  * https://devconnected.com/docker-exec-command-with-examples/
  * [java 11 specific commands Dockerfile examples](https://stackoverflow.com/questions/53669151/java-11-application-as-lightweight-docker-image)

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)

