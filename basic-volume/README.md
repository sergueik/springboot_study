### Info

This directory contains a replica of the basic persistence Docker volume [example](https://github.com/rjwvandenberg/docker-volume-example)


### Usage

`script.py` in Docker container reads and increments a number from a file prints it to standard output and saves the next number back to the file

### Usage
```sh
IMAGE_NAME='basic-counter-image'
CONTAINER_NAME='basic-counter'
docker build -t $IMAGE_NAME -f Dockerfile . 
docker volume create --name startup-counter-vol
docker run --name $CONTAINER_NAME -v startup-counter-vol:/app -t $IMAGE_NAME 
docker logs $CONTAINER_NAME
```
it will print:
```text
0
```
```sh
CONTAINER_NAME='basic-counter'
docker inspect $CONTAINER_NAME| jq '.' | tee /tmp/a.json
VOLUME_PATH=$(jq -cr '.[]|.Mounts|.[]|.Source' /tmp/a.json)
sudo cat $VOLUME_PATH/number
```
it will print 
```text
1
```
* remove container and run again
```sh
docker rm $CONTAINER_NAME
docker run --name $CONTAINER_NAME -v startup-counter-vol:/app -t $IMAGE_NAME 
docker logs $CONTAINER_NAME
```
it will print:
```text
1
```
* print file contents again
```sh
docker inspect $CONTAINER_NAME| jq '.' | tee /tmp/a.json
VOLUME_PATH=$(jq -cr '.[]|.Mounts|.[]|.Source' /tmp/a.json)
sudo cat $VOLUME_PATH/numberdocker container rm $IMAGE_NAME
```
it will print:
```text
2
```
### Repeat with mounted local directory

* 
```sh
docker run --name $CONTAINER_NAME --rm -v $(pwd)/app:/app -t $IMAGE_NAME 
docker logs $CONTAINER_NAME
```
it will print:
```text
0
```

the counter will be in the file owned by root user:
```sh
ls -l app/
total 12
-rw-r--r-- 1 root     root        1 May 30 22:43 number
```
```sh
cat app/number
```
```text
1
```
update the counter and rerun
```sh
echo 42 | sudo tee app/number
```

```sh
docker run --name $CONTAINER_NAME --rm -v $(pwd)/app:/app -t $IMAGE_NAME 
```
it will print
```text
42
```
### NOTE

if not provided full path to local dir 
```sh
docker run --name $CONTAINER_NAME --rm -v app:/app -t $IMAGE_NAME

```
it will not be created in the current dir:
```sh
ls -l app/  
```

```text
ls: cannot access 'app/': No such file or directory
```
but will be interpreted as a name of the volume
```sh
docker volume ls
```
```text
DRIVER    VOLUME NAME
local     app

```
### Cleanup

```sh
docker container prune -f
docker volume rm startup-counter-vol
docker image prune -f
```

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
