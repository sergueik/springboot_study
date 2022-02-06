### Info

This directory contains standard Docker example of creating the container with
specific non-root user `docker_user`, cloning the invoking user UID, mount that container user home directory as  writable volume mapped to project directory and launch the editor in docker
er build -t $TAG -f Dockerfile .


#### Note

Use the (readonly) environment `UID` from the host user login session to update the `Dockerfile` - modifying the `UID` though run argument way will not have intended effect
```sh
DOCKER_USER=docker_user
TAG=basic_user
sed -i "s|UID=[0-9][0-9]*|UID=$UID|g" Dockerfile
docker build -t $TAG -f Dockerfile .
docker run -u $UID -it -v $(pwd):/home/$DOCKER_USER:rw $TAG
```
alternatively provide `UID` arguments when run the container:
```sh
docker run -it -e UID -e GID=$(echo $GROUPS | cut -f 1 -d ' ') -v $(pwd):/home/$DOCKER_USER:rw $TAG
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
-rw-r--r-- 1 sergueik systemd-journal 5 Feb  6 16:03 test.txt
```
### Troubleshooting
in a sibling shell inspect the home directory of the default user:
```sh
docker exec -it $(docker container ls | grep "$TAG" | cut -f 1 -d' ') sh
```
```sh
export COMPOSE_HTTP_TIMEOUT=600
docker-compose build
```
```sh
docker-compose run user-nano
```
### Cleanup
```sh
docker container rm $(docker container ls -a | grep "$TAG" | cut -f 1 -d' ')
docker container prune -f
docker image rm $TAG
docker image prune -f
```
### NOTE 
 * observed errors in `docker-compose` on repeateed rebuilds:
```sh
addgroup: group 'docker_user' in use
```
and occasionaly in `docker` too:

```sh
/bin/sh: can't create /etc/apk/repositories: Permission denied
```
the only way to cure is to purging the containers and images, including `alpine;3.9.5`:

```sh
docker image rm $TAG
docker image rm alpine:3.9.5
```
### See Also

  * original "coding interview"-grade [discussion](https://www.cyberforum.ru/shell/thread2707382.html) (in Russian)
  * https://stackoverflow.com/questions/34031397/running-docker-on-ubuntu-mounted-host-volume-is-not-writable-from-container
  * [Docker user contfiguration](https://habr.com/ru/post/448480/) (in  Russian)
  * [bash environment variables](https://www.shell-tips.com/bash/environment-variables/)
  * https://unix.stackexchange.com/questions/397232/adduser-addgroup-group-in-use
 
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
