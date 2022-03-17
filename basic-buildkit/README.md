### Info

Example using [Docker Buildkit](https://hub.docker.com/r/docker/dockerfile-upstream) syntax


### Usage

```sh
export DOCKER_BUILDKIT=0
IMAGE=xvfb-fluxbox
docker build -t $IMAGE -f Dockerfile.vanilla .
```
```sh
docker image history $IMAGE
```
this will show
```text
IMAGE          CREATED              CREATED BY                                      SIZE      COMMENT
90f7393ce041   About a minute ago   /bin/sh -c #(nop)  CMD ["/opt/bootstrap.sh"]    0B
55773334dc2f   About a minute ago   /bin/sh -c #(nop) WORKDIR /home/user            0B
0fdec3be627b   About a minute ago   /bin/sh -c #(nop)  ENV HOME=/home/user          0B
8ee27d114f8f   About a minute ago   /bin/sh -c #(nop)  USER user                    0B
d7ddf328ea7a   About a minute ago   /bin/sh -c #(nop) COPY file:9e723a23c51c3e27…   1.65kB
77006b360583   About a minute ago   /bin/sh -c sed -r -i "s/\[exec\] \(xterm\) \…   1.41kB
dd96f7295048   2 minutes ago        /bin/sh -c apk add xvfb x11vnc fluxbox xdpyi…   62.8MB
0d778d87f452   6 
months ago         /bin/sh -c test -d /opt || mkdir -p /opt        0B
af1b11d590be   6 months ago         /bin/sh -c sh -c 'test -d /opt || mkdir -p /…   0B
ab2c04e60410   6 months ago         /bin/sh -c mkdir -p /home/user  && chown use…   0B
a4452f67c5c2   6 months ago         /bin/sh -c adduser -D -g users user             4.81kB
d2a0db2e7b97   6 months ago         /bin/sh -c apk update                           1.31MB
78a2ce922f86   23 months ago        /bin/sh -c #(nop)  CMD ["/bin/sh"]              0B
<missing>      23 months ago        /bin/sh -c #(nop) ADD file:a0afd0b0db7f9ee94…   5.55MB

```
* repeat with modified `Dockerfile` run on `buuildkit` plugin:
```sh
export DOCKER_BUILDKIT=1
IMAGE=xvfb-fluxbox-buildkit
docker build -t $IMAGE -f Dockerfile.buildkit .
```
```sh
docker image history $IMAGE
```

```text
IMAGE          CREATED              CREATED BY                          SIZE      COMMENT
92ee2a4f57cc   About a minute ago   CMD ["/opt/bootstrap.sh"]           0B        buildkit.dockerfile.v0
<missing>      About a minute ago   WORKDIR /home/user                  0B        buildkit.dockerfile.v0
<missing>      About a minute ago   ENV HOME=/home/user                 0B        buildkit.dockerfile.v0
<missing>      About a minute ago   USER user                           0B        buildkit.dockerfile.v0
<missing>      About a minute ago   COPY bootstrap.sh /opt # buildkit   1.65kB    buildkit.dockerfile.v0
<missing>      About a minute ago   RUN /bin/sh -c apk update

adduser -D -g use…   62.8MB          buildkit.dockerfile.v0
<missing>            23 months ago   /bin/sh -c #(nop)  CMD ["/bin/sh"]              0B
<missing>            23 months ago   /bin/sh -c #(nop) ADD file:a0afd0b0db7f9ee94…   5.55MB
```
* image size check
```sh
docker image ls | grep -E '(xvfb-fluxbox-buildkit|xvfb-fluxbox)'
```
```text
xvfb-fluxbox-buildkit   latest       92ee2a4f57cc   3 minutes ago   68.4MB
xvfb-fluxbox            latest       90f7393ce041   7 minutes ago   69.7MB
```
### Testing

* run container
```sh
docker run -d $IMAGE
```

```sh
ID=$(docker container ls | grep $IMAGE |awk '{print $1}')
docker inspect $ID | jq '.[]|.NetworkSettings.Networks.bridge.IPAddress'
```
### Cleanup

```sh
docker container stop $ID
docker container rm $ID
docker image rm xvfb-fluxbox-buildkit xvfb-fluxbox
docker image prune -f
```
### See Also

  * https://www.docker.com/blog/introduction-to-heredocs-in-dockerfiles/	

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
