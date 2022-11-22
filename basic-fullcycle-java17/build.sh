#!/bin/sh
# NOTE: ubuntu bionic - UID is set but GID is not
if [ -z $GID ] ; then 
  export GID=$(id -u )
fi
IMAGE=sample.app-app
docker build -f Dockerfile.build \
             -t $IMAGE .
echo step 1 done
NAME=sample.app-app
docker run --rm \
           --name $NAME \
           --user app \
           --volume $(pwd):/app \
           $IMAGE \
           mvn clean install
docker image rm $IMAGE
docker build -f Dockerfile.app \
             -t $IMAGE .
