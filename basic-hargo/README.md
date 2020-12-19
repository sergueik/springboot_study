### Info


This directory contains subset of the [Hargo](https://github.com/mrichman/hargo)
with original multi-stage Dokcerfile split into build and run pieces and working directory on the host used to transfer the artifacts.


### Usage

* pull the project source
```sh
mkdir -p tmp
git clone https://github.com/mrichman/hargo tmp
rm -fr tmp/.git
```
* pull the basic images, build and keep the container with application
```sh
docker pull golang:alpine3.9
IMAGE_NAME='basic-hargo.build'
CONTAINER_NAME='build'
docker build -t $IMAGE_NAME -f Dockerfile.build tmp
docker run --name $CONTAINER_NAME -d $IMAGE_NAME
```
* copy artifacts locally
```sh
docker cp $CONTAINER_NAME:/go/src/hargo/hargo .
docker cp $CONTAINER_NAME:/go/src/hargo/hargo.exe .
docker cp $CONTAINER_NAME:/etc/ssl/certs/ca-certificates.crt .
```
```sh
IMAGE_NAME='basic-hargo'
CONTAINER_NAME='run-hargo'
docker build -t $IMAGE_NAME -f Dockerfile.run .
docker run --rm --name $CONTAINER_NAME -it $IMAGE_NAME hargo --help
docker run --rm --name $CONTAINER_NAME -it -v $(pwd)/tmp/test:/test basic-hargo hargo run /test/golang.org.har
```
this wil produce the console report:
```sh
INFO[0000] run .har file: /test/golang.org.har
[GET,200] URL: https://golang.org/
[GET,200] URL: https://fonts.googleapis.com/css?family=Work+Sans:600|Roboto:400,700
[GET,200] URL: https://golang.org/lib/godoc/style.css
[GET,200] URL: https://golang.org/lib/godoc/jquery.js
[GET,200] URL: https://fonts.googleapis.com/css?family=Product+Sans&text=Supported%20by%20Google&display=swap
...
[GET,200] URL: https://golang.org/favicon.ico
[POST,200] URL: https://www.youtube.com/youtubei/v1/log_event?alt=json&key=AIzaSyAO_FJ2SlqU8Q4STEHLGCilw_Y9_11qcW8
```
Alternatively can copy Windows executable into Windows VM and run the test fro there:
```cmd
hargo.exe run  test\golang.org.har
```
### Cleanup

```sh
docker container rm $CONTAINER_NAME
docker image prune -f
IMAGE_NAME='basic-hargo.build'
docker image rm $IMAGE_NAME
rm -fr tmp
```
### See Also

  * [cross-compile Go programs for Windows, macOS, and Linux](https://freshman.tech/snippets/go/cross-compile-go-programs/)
  * [multi stage](https://docs.docker.com/develop/develop-images/multistage-build/) `Dockerfile`

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
