### Info

Loose Java class Docker example extracted from the article  [Java-приложения и Docker (in Russian)](https://habr.com/company/billing/blog/350138/).
The original git repository of that article can be found on  [alexff91/Java-meetup-2018](https://github.com/alexff91/Java-meetup-2018).

### Testing

* temporarily pull jdk base container
```sh
TAG='8-jdk-alpine3.9'
docker pull openjdk:8-jdk-alpine3.9
```
* buld container . Note this involves compiling the Java code inside the container which may benot the best practice, if good at all (is there such a term as ultrabad practice ?)

```sh
IMAGE='basic-loose'
docker build -f Dockerfile -t $IMAGE .
```
run
```sh
docker run --rm -e "X=Y" -it $IMAGE
```
which will print a warmup `Hello` message and system environment to the console

```sh
Hello Java!
PATH:
        "/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/lib/jvm/java-1.8-openjdk/jre/bin:/usr/lib/jvm/java-1.8-openjdk/bin"
HOSTNAME:
        "6f261a142a46"
JAVA_ALPINE_VERSION:
        "8.212.04-r0"
LD_LIBRARY_PATH:
        "/usr/lib/jvm/java-1.8-openjdk/jre/lib/amd64/server:/usr/lib/jvm/java-1.8-openjdk/jre/lib/amd64:/usr/lib/jvm/java-1.8-openjdk/jre/../lib/amd64"
JAVA_HOME:
        "/usr/lib/jvm/java-1.8-openjdk"
X:
        "Y"
TERM:
        "xterm"
JAVA_VERSION:
        "8u212"
LANG:
        "C.UTF-8"
HOME:
        "/root"
```
the entry `X:"Y"` above has been set on the command line
### Cleanup

```sh
docker container prune -f
docker image prune -f
docker image rm $IMAGE
docker image ls | grep $TAG | awk '{print $3}' | xargs -IX docker image rm X
docker system prune -f
```

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
