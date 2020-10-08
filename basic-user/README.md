### Info

This directory contains standard Docker example of creating the container with
specific non-root user cloning the invoking user UID, mount that user home directory as  writable volume mapped to project directory and launch the editor in docker

### Usage


#### Note
Use the (readonly) environment `UID` from the host user login session to update the `Dockerfile` - modifying the `UID` though run argument way will not have intended effect
```sh
DOCKER_USER=docker_user
TAG=basic_user
sed -i "s|UID=.*|UID=$UID|g" Dockerfile
docker build -t $TAG -f Dockerfile .
docker run -it -v $(pwd):/home/$DOCKER_USER:rw $TAG
```
save the file in the editor e.g. as `test.txt`
find the local file with the expected contents in the project directory:
```sh
cat test.txt
```
```sh
this is test
```
Note the group of the file:
```sh
ls -l test.txt
```
```sh
-rw-r--r-- 1 sergueik systemd-journal 13 Oct  8 02:54 test.txt
```
### Troubleshooting
in a sibling shell inspect the home directory of the default user:
```sh
docker exec -it $(docker container ls | grep "$TAG" | cut -f 1 -d' ') sh
```

### Cleanup
```sh
docker container rm $(docker container ls -a | grep "$TAG" | cut -f 1 -d' ')
docker container prune -f
docker image prune -f
```
### See Also

 * original "coding interview"-grade [discussion](https://www.cyberforum.ru/shell/thread2707382.html) (in Russian)
 * https://stackoverflow.com/questions/34031397/running-docker-on-ubuntu-mounted-host-volume-is-not-writable-from-container

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
