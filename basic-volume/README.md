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
### Scanning the Files in Volume


`reader.py` in Docker container scans the volume and prints files to standard output

* build 
```
IMAGE_NAME='basic-reader-image'
CONTAINER_NAME='basic-reader'
docker build -t $IMAGE_NAME -f Dockerfile.reader . 
```
*  run
```sh
docker run --name $CONTAINER_NAME -v $(pwd)/app:/app -t $IMAGE_NAME 
```
create a few files
```
sudo touch ./app/file1.txt ./app/file2.txt ./app/file3.txt
```
observe in docker console the files shown:
```text
[]
['file2.txt', 'file1.txt', 'file3.txt']
```

similarly  through `docker-compose`:
```sh
docker-compose -f docker-compose-reader.yml up --build
```
```sh
[+] Building 2.4s (8/8) FINISHED                                                
 => [internal] load build definition from Dockerfile.reader                0.1s
 => => transferring dockerfile: 38B                                        0.0s
 => [internal] load .dockerignore                                          0.1s
 => => transferring context: 2B                                            0.0s
 => [internal] load metadata for docker.io/library/python:3.8.2-alpine     0.0s
 => [1/3] FROM docker.io/library/python:3.8.2-alpine                       0.2s
 => [internal] load build context                                          0.0s
 => => transferring context: 31B                                           0.0s
 => [2/3] WORKDIR /tmp                                                     0.2s
 => [3/3] COPY reader.py ./                                                1.5s
 => exporting to image                                                     0.3s
 => => exporting layers                                                    0.2s
 => => writing image sha256:48da6582d7fc93b2190b7cbc7e53201509c9972217ab7  0.0s
 => => naming to docker.io/library/python:3.8.2-alpine                     0.0s
[+] Running 1/1
 ⠿ Container basic-volume-reader-1  R...                                  11.4s
Attaching to basic-volume-reader-1
basic-volume-reader-1  | ['file11.txt', 'file13.txt', 'file12.txt']
basic-volume-reader-1  | ['file23.txt', 'file11.txt', 'file13.txt', 'file22.txt', 'file12.txt', 'file21.txt']
basic-volume-reader-1  | ['file23.txt', 'file11.txt', 'file13.txt', 'file22.txt', 'file12.txt', 'file21.txt']
```

### Node.js Exercise
```sh
IMAGE_NAME='basic-reader-image-node'
CONTAINER_NAME='basic-reader-node'
docker build -t $IMAGE_NAME -f Dockerfile.reader-node .
```

```sh
docker run --name $CONTAINER_NAME -v $(pwd)/app:/app -p 3000:3000 -e PORT=3000 -t $IMAGE_NAME
```
```sh
sudo touch ./app/file1.txt ./app/file2.txt ./app/file3.txt
```

```sh
curl -X POST  http://localhost:3000/app
```

```JSON
["file21.txt","file22.txt","file23.txt","file31.txt","file32.txt","file33.txt"]
```
```sh
 curl -X POST  http://localhost:3000/app
```

```JSON
["file11.txt","file12.txt","file13.txt","file31.txt","file32.txt","file33.txt"]
```

```sh
sudo touch ./app/file11.txt ./app/file12.txt ./app/file13.txt
sudo rm ./app/file21.txt ./app/file22.txt ./app/file23.txt
```

```sh
docker container stop  $CONTAINER_NAME
docker container rm $CONTAINER_NAME
```

### Cleanup

```sh
docker-compose -f docker-compose-reader.yml  stop
docker-compose -f docker-compose-reader.yml  rm -f

docker stop $CONTAINER_NAME
docker container rm $CONTAINER_NAME
docker container prune -f
docker volume rm $VOLUME_NAME
docker image prune -f
sudo  rm -fr app/
```

### See Also
  * https://stackoverflow.com/questions/3207219/how-do-i-list-all-files-of-a-directory
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
