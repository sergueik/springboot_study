### Info

Original multi-stage Dockerfile split into build and run pieces and 
working directory on the host used to transfer the artifacts.


### Usage

```sh
export IMAGE=basic-go-build
docker image rm  $IMAGE
docker build -t $IMAGE -f Dockerfile.build .
export NAME=basic-go-build
docker container rm $NAME
docker run -d --name=$NAME $IMAGE
docker cp $NAME:/app/example1 .
docker cp $NAME:/app/example2 .
```
```sh
IMAGE=basic-go-run
docker build -t $IMAGE -f Dockerfile.run  .
docker run -v /etc:/tmp/etc -it $IMAGE /usr/bin/tail -f /tmp/etc/lsb-release
```
```sh
DISTRIB_ID=Ubuntu
DISTRIB_RELEASE=18.04
DISTRIB_CODENAME=bionic
DISTRIB_DESCRIPTION="Ubuntu 18.04.5 LTS"
```

```sh
docker run -v /etc:/tmp/etc -it $IMAGE /example1 /tmp/etc/lsb-release
```
```sh
2021/01/24 21:53:13 Reading /tmp/etc/lsb-release
2021/01/24 21:53:13 scanner exit
2021/01/24 21:53:13 Waiting for command to finish...
2021/01/24 21:53:13 cmd.Wait error: exit status 1
```

```sh
docker run -v /etc:/tmp/etc -it $IMAGE /example2 /tmp/etc/lsb-release
```
```sh
```
```sh
docker image rm -f $NAME
```
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)

irq/29-DELL087E
