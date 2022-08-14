### Info

This directory contains a close replica of the

[rakibtg/docker-web-gui](https://github.com/rakibtg/docker-web-gui) app -  node.js alpine docker hosted web based GUI for managing Docker containers and images

### Modifications to the Project

Switched from `node:alpine` to `python:3.8.2-alpine` due to Python dependency and install docker on container to allow reading the host inventory through socket which is exported volume mounted to the container during the run
      
### Usage

* build the image:
```sh
NAME=docker-web-gui
docker build . -t $NAME 
```
* run the image (note it will package it too):
```sh
docker run -p 3230:3230 -v /var/run/docker.sock:/var/run/docker.sock $NAME
```
NOTE: the original project was providing an invalid command which atempts to map both the socket and the binary. We install client locally in the contianer and only mount volume for socket

### Result

* images
![images](https://github.com/sergueik/springboot_study/blob/master/docker-web-gui/screenshots/capture-images.png)

* containers
![containers](https://github.com/sergueik/springboot_study/blob/master/docker-web-gui/screenshots/capture-containers.png)


### Original Documentations

  * [Backend API](https://github.com/rakibtg/docker-web-gui/tree/master/backend)
  * [Client](https://github.com/rakibtg/docker-web-gui/tree/master/client)

### Note

the application fails to discover any containers or images becasue it expects the `docker` executable to be woking in the container, but it is not installed there
### See Also

  * [top 6 GUI tools for managing Docker environments](https://www.upnxtblog.com/index.php/2018/01/17/top-6-gui-tools-for-managing-docker-environments/) - note the piblication is from 2018
  * [WPF UI to docker running on Windows](https://github.com/sonujose/docker-soul) - not very funcional 
  * __PR__ [45](https://github.com/rakibtg/docker-web-gui/pull/45/commits/a245814b2cd3ac30925b092a09f368d471e9d22b) to the original project soling the same problem again but instead of switching to python image, install python via `apk`.

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
