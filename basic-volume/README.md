### Info

This directory contains a replica of the basic persistence Docker volume [example](https://github.com/rjwvandenberg/docker-volume-example)


### Usage

`script.py` in Docker container reads and increments a number from a file prints it to standard output and saves the next number back to the file

### Usage
```sh
IMAGE_NAME='basic-counter-image'
CONTAINER_NAME='basic-counter'
docker build -t $IMAGE_NAME -f Dockerfile . 
VOLUME_NAME=counter-vol
docker volume create --name $VOLUME_NAME
docker run --name $CONTAINER_NAME -v $VOLUME_NAME:/app -t $IMAGE_NAME 
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
docker run --name $CONTAINER_NAME -v $VOLUME_NAME:/app -t $IMAGE_NAME 
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
sudo cat $VOLUME_PATH/number
docker rm $CONTAINER_NAME
```
it will print:
```text
2
```
* list the volume contents

```sh
VOLUME_PATH=$(docker volume inspect $VOLUME_NAME| jq -cr '.[].Mountpoint')
echo $VOLUME_PATH
```
```text
/var/lib/docker/volumes/startup-counter-vol/_data
```
```sh
sudo ls /var/lib/docker/volumes/startup-counter-vol/_data
```
```text
number
```
```sh
sudo cat /var/lib/docker/volumes/startup-counter-vol/_data/number
```

```text
1
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
### NOTE

* Directory mapping does not work right in __Docker Toolbox__ with Windows directories:
```cmd
docker run --name $CONTAINER_NAME --rm -v ./app:/app:rw -t $IMAGE_NAME
```
```text
C:\Program Files\Docker Toolbox\docker.exe: 
Error response from daemon: create ./app: "./app" includes invalid characters for a local volume name, 
only "[a-zA-Z0-9][a-zA-Z0-9_.-]" are allowed. 
If you intended to pass a host directory, use absolute path.
```
the only way to pass a directory argument is to have it on __Docker Toolbox__ machine - no workaround atm.

* to inspect the volume will need to also ssh to __Docker Toolbox__ machine:
```sh
docker-machine ssh
```
```text
   ( '>')
  /) TC (\   Core is distributed with ABSOLUTELY NO WARRANTY.
 (/-_--_-\)           www.tinycorelinux.net
```
inspect the volumes either from Windows or from Debian side (NOTE, `jq` is likely to not be available on either):
```sh
VOLUME_NAME=startup-counter-vol
docker volume inspect $VOLUME_NAME
```
```json
[
    {
        "CreatedAt": "2023-05-31T13:05:48Z",
        "Driver": "local",
        "Labels": {},
        "Mountpoint": "/mnt/sda1/var/lib/docker/volumes/startup-counter-vol/_data",
        "Name": "startup-counter-vol",
        "Options": {},
        "Scope": "local"
    }
]
```

```sh
sudo ls /mnt/sda1/var/lib/docker/volumes/startup-counter-vol/_data
```

```text
number
```
### Cleanup

```sh
docker container prune -f
docker volume rm $VOLUME_NAME
docker image prune -f
sudo  rm -fr app/
```

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
