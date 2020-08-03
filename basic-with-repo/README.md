### Info

Vanilla Docker basic project based on [Dockerising a Perl application](https://robn.io/docker-perl/) note converted to run on alpine openjdk jre base image, exploring the latest committed hash to trigger git pull
### Test
* Build and run container 2 times
```sh
docker build -f Dockerfile -t basic-example . 
docker run -p 8080:8080 basic-example
```
the second build will all be using caches:
```sh

 Step 1/9 : FROM openjdk:8-jre-alpine3.9
 ---> f7a292bbb70c
Step 2/9 : ARG apps_dir="/opt/apps"
 ---> Using cache
 ---> a6a0519601b6
Step 3/9 : ARG project="springboot_study"
 ---> Using cache
 ---> 1084fd822cfb
Step 4/9 : EXPOSE 8085
 ---> Using cache
 ---> fd04c2fe8791
Step 5/9 : RUN mkdir ${apps_dir}
 ---> Using cache
 ---> cf71f71e4f0d
Step 6/9 : RUN git --version &>/dev/null; if [ $? -ne 0 ] ; then apk update && apk upgrade && apk add --no-cache git ; fi
 ---> Using cache
 ---> 2111184d509d
Step 7/9 : RUN if [ -d ${apps_dir}/${project}.git ] ; then echo $(git --git-dir= ${apps_dir}/${project}.git rev-parse --short HEAD); else echo 'No workspace' ;fi
 ---> Using cache
 ---> 75bf09ee470b
Step 8/9 : RUN cd ${apps_dir} && git clone https://github.com/sergueik/${project}
 ---> Using cache
 ---> e7d6e247dc33
Step 9/9 : CMD tail -f /dev/null
 ---> Using cache
 ---> c85ea0d82c14
Successfully built c85ea0d82c14
Successfully tagged basic-example:latest
```
To make example work use environment

```sh
export PROJECT=springboot_study
export APPS_DIR=/$HOME/src
export LATEST_HASH=$(git --git-dir=${APPS_DIR}/${PROJECT}/.git rev-parse --short HEAD)
docker build -f Dockerfile --build-arg "LATEST_HASH=$(git --git-dir=${APPS_DIR}/${PROJECT}/.git rev-parse --short HEAD)" -t basic-example .
_
```
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
