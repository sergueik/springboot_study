### Info

This directory contains a replica of the basic persistence docker volume [example](https://github.com/rjwvandenberg/docker-volume-example)


### Usage

`script.py` in Docker  container reads  and increments a number from a file prints it to standard output and saves the next number back to the file

### Usage
```sh
IMAGE_NAME='basic-counter-image'
CONTAINER_NAME='basic-counter'
docker build -t $IMAGE_NAME -f Dockerfile . 
docker run --name $CONTAINER_NAME -v startup-counter-vol:/app  -t $IMAGE_NAME 
docker logs $CONTAINER_NAME
```

```sh
CONTAINER_NAME='basic-counter'
docker inspect $CONTAINER_NAME| jq '.' |  tee /tmp/a.json
VOLUME_PATH=$(jq -cr '.[]|.Mounts|.[]|.Source' /tmp/a.json)
sudo cat $VOLUME_PATH/number
```
```sh
docker rm $CONTAINER_NAME
docker container rm $IMAGE_NAME
```
