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
the entry `X:"Y"` above has been set on the command line:
filtered output shows that:
```sh
docker run --rm -e "X=Y" -it $IMAGE  2>&1  | grep 'X = '
```
```sh
X = Y
```
### Cleanup

```sh
docker container prune -f
docker image prune -f
docker image rm $IMAGE
docker image ls | grep $TAG | awk '{print $3}' | xargs -IX docker image rm X
docker system prune -f
```
### Setup on Xenial

It appears the only quick way of installing Docker on 16.04 is through direct downloads from https://download.docker.com:

```sh
sudo apt-get install libcurl3-gnutls=7.47.0-1ubuntu2
sudo apt-get update
sudo apt-get install apt-transport-https ca-certificates curl software-properties-common
```
The below versions are pinned:
```sh
curl -O https://download.docker.com/linux/ubuntu/dists/xenial/pool/edge/amd64/containerd.io_1.2.6-3_amd64.deb
curl -O https://download.docker.com/linux/ubuntu/dists/xenial/pool/edge/amd64/docker-ce-cli_19.03.8~3-0~ubuntu-xenial_amd64.deb
curl -O https://download.docker.com/linux/ubuntu/dists/xenial/pool/edge/amd64/docker-ce_19.03.8~3-0~ubuntu-xenial_amd64.deb

sudo dpkg -i containerd.io_1.2.6-3_amd64.deb
sudo dpkg -i docker-ce-cli_19.03.8~3-0~ubuntu-xenial_amd64.deb
sudo dpkg -i docker-ce_19.03.8~3-0~ubuntu-xenial_amd64.deb
sudo curl -L https://github.com/docker/compose/releases/download/1.25.0/docker-compose-`uname -s`-`uname -m` -o /usr/local/bin/docker-compose
sudo chmod +x /usr/local/bin/docker-compose
```

### See also

  * https://www.vultr.com/docs/installing-docker-ce-on-ubuntu-16-04
  * https://askubuntu.com/questions/908567/need-help-installing-curl-on-xubuntu-16-04
  * https://askubuntu.com/questions/1180060/getting-error-while-installing-docker-docker-ce-depends-containerd-io-1
  * https://download.docker.com/linux/ubuntu/dists/xenial/pool/edge/amd64/

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
