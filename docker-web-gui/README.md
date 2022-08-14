### Info

This directory contains a close replica of the node alpine docker hosted 

[docker dashboard](https://github.com/rakibtg/docker-web-gui) app - web based GUI for managing Docker containers and images. Switched from node:alpine to due to Python dependency (the application is unable to find any images or containers probably because it looks for inventory on the host where it runs from, which is a docker container

### Usage

* build the image:
```sh
docker build . -t docker-web-gu
```
* run the image (note it will package it too):
```sh
docker run -p 3230:3230 -v /usr/bin/docker:/usr/local/bin/docker -v /var/run/docker.sock:/var/run/docker.sock docker-web-gui
```
### Original Documentations
- [Backend API](https://github.com/rakibtg/docker-web-gui/tree/master/backend)
- [Client](https://github.com/rakibtg/docker-web-gui/tree/master/client)

### Note

the application fails to discover any containers or images becasue it expects the `docker` executable to be woking in the container, but it is not installed there

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
