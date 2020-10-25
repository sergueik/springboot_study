### Info

This directory contains a `Dockerfile` from [zenika/alpine-chrome](https://github.com/Zenika/alpine-chrome/blob/master/Dockerfile) but switched to __maven/jdk8__ apline base [image]( https://hub.docker.com/r/zenika/alpine-maven/tags) with maven and chromium to run the test suites. Note: the `chromium` binary installs some X libraries but is runnable in headless mode in console mode.

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
which returns
```sh
Results :
Tests run: 1, Failures: 0, Errors: 0, Skipped: 0
```
and basic CDP test (e.g. copy from [sergueik/cdp4j_tests](https://github.com/sergueik/cdp4j_tests)
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

* in the current layout the `target` directory in `demo` project becomes owned by root account.


### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
