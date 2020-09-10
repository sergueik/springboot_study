### Info

This directory contains a `Dockerfile` derived from [](https://github.com/Zenika/alpine-chrome/blob/master/Dockerfile) but using maven/jdk8 apline base [image] ( https://hub.docker.com/r/zenika/alpine-maven/tags)
by the same author
Note: the `chomium` binary still has some X dependencies but is runnable in headless mode.

### Usage

* build the `chromium` and `chromium-driver` into the image
```sh
IMAGE='basic-maven-chromium'
docker build -t $IMAGE -f Dockerfile .
```
* run some ultra basic regular Selenium test 
```sh
docker run -it -v "$PWD/demo.selenium":/demo -w /demo $IMAGE mvn clean test
```
and CDP test (e.g. copy from [sergueik/cdp4j_tests](https://github.com/sergueik/cdp4j_tests)
```sh
git clone https://github.com/sergueik/cdp4j_tests demo.cdp
docker run -it -v "$PWD/demo.cdp":/demo -w /demo -e USE_CHROMIUM=true $IMAGE mvn clean test
```
from mounted host directory
* alternatively connect into the container
```sh
docker run -v demo -it $IMAGE sh
```
```sh
docker container prune -f
```
### Note

* in the current layout the `target` directory in `demo` project gets owned by root account.


