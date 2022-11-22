#!/bin/sh
if [ -z $GID ] ; then 
  export GID=$(id -u )
fi
IMAGE=sample-java17-app-build
docker build -f Dockerfile.build \
             -t $IMAGE .
NAME=sample-java17-app
docker run --rm --name $NAME --user app --volume $(pwd):/app $IMAGE mvn clean package
IMAGE=sample-java17-app
docker build -f Dockerfile.app -t $IMAGE .
#  rebuild for alpine
IMAGE=sample-java17-app-alpine
docker build -f Dockerfile.app-alpine -t $IMAGE .
