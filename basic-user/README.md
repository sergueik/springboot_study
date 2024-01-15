### Info

This directory contains standard Docker example of creating the container with
specific non-root user `docker_user`, cloning the invoking user UID, mount that container user home directory as writable volume mapped to project directory and launch the editor in docker

#### Usage

* Write some text into `test.txt` on the host, e.g.

```text
this file was copied from the host
```

the file will be copied into container during docker build

* Use the (readonly) environment `UID` from the host user login session to update the `Dockerfile` - modifying the `UID` though run argument way will not have intended effect
```sh
DOCKER_USER=docker_user
IMAGE=basic_user
sed -i "s|UID=[0-9][0-9]*|UID=$UID|g" Dockerfile
docker build -t $IMAGE -f Dockerfile .
```
```sh
IMAGE=basic_user
docker run -u $DOCKER_USER -it -v $(pwd):/home/$DOCKER_USER:rw $IMAGE
```
alternatively provide arguments
```sh
docker run -it -e UID -e GID=$(id -G| cut -f 1 -d ' ') -u $DOCKER_USER -v $(pwd):/home/$DOCKER_USER:rw $IMAGE
```
this will
open `test.txt` in the `nano` editor running in the container  (it is currently configured to use `nano` as entrypoint)

![Docker Nano](https://github.com/sergueik/springboot_study/blob/master/basic-user/screenshots/capture-nano.png)
* add some text and save the file in the editor e.g. under same name, as `test.txt`. Provide file name and confirm to overwrit when prompted

find the local file with the expected contents in the project directory:
```sh
cat test.txt
```
```sh
this is test
```
Note the `systemd-journal` group of the file:
```sh
ls -l test.txt
```
```sh
-rw-r--r-- 1 sergueik systemd-journal 5 Feb  6 16:03 test.txt
```
### Troubleshooting
in a sibling shell inspect the home directory of the default user:
```sh
IMAGE=basic_user
docker exec -it $(docker container ls | grep "$IMAGE" | cut -f 1 -d' ') sh
```
in the shell, run
```sh
ls -l test.txt
```
this will print
```text
-rw-r--r--    1 docker_u 1000            23 Nov 18 00:10 test.txt
```
alternatively change the entrypoint and run a second container:
```sh
docker run -it -e UID -e GID=$(id -G| cut -f 1 -d ' ') -u $DOCKER_USER -v $(pwd):/home/$DOCKER_USER:rw --entrypoint sh $IMAGE
```
#### Produce files owned by non-root User in the Container at Build time

* comment the line in `Dockerfile` (this may not be necessary, after all):
```sh
VOLUME /home/$USER_NAME
```
* NOTE: There is no real need to specify VOLUME through Dockerfile, except for documenting the project, however this instruction ruins the files added to the directory which is the volume. It is imporant to suply no `-v` option when container is run

* add (or ucomment) the lines in `Dockerfile`:
```sh
USER $USER_NAME
ADD test.txt ./
RUN touch /home/$USER_NAME/test2.txt
```
* rebuild the image and run the container with command
```sh
docker run -u $DOCKER_USER -it  $IMAGE ls -l /home/$DOCKER_USER
```
you will see
```text
-rw-r--r--    1 root     root            31 Mar 23  2023 test.txt
-rw-r--r--    1 docker_u docker_u         0 Nov 17 23:49 test2.txt
```
the resource created by `ADD` instruction is owned by the root user even despite the `USER` instruction was provided. The resource created by `RUN` instruction honors the most recent preceding `USER` instruction
* restore the earlier commented
```sh
VOLUME /home/$USER_NAME
```
and rebuild the image
### Docker Compose

* build
```sh
export COMPOSE_HTTP_TIMEOUT=600
docker-compose build
```
* run
```sh
docker-compose run user-nano
```

Note, after the file is saved it does not appear to be visible, 

### Cleanup

```sh
docker-compose rm -f user-nano
```
```sh
docker container rm $(docker container ls -a | grep "$IMAGE" | cut -f 1 -d' ')
docker container prune -f
docker image rm $IMAGE
docker image prune -f
```
### NOTE 
 * observed errors in `docker-compose` on repeated rebuilds:
```sh
addgroup: group 'docker_user' in use
```
and occasionally in plain `docker` too:

```sh
/bin/sh: can't create /etc/apk/repositories: Permission denied
```
the only way to cure is to purging the containers and images, including `alpine;3.9.5`:

```sh
docker image rm $IMAGE
docker image rm alpine:3.9.5
```
### See Also

  * original "coding interview"-grade [discussion](https://www.cyberforum.ru/shell/thread2707382.html) (in Russian)
  * https://stackoverflow.com/questions/34031397/running-docker-on-ubuntu-mounted-host-volume-is-not-writable-from-container
  * [Docker user contfiguration](https://habr.com/ru/post/448480/) (in  Russian)
  * root/non-root user switch [related question](https://qna.habr.com/q/1319640) (in Russian)
  * [bash environment variables](https://www.shell-tips.com/bash/environment-variables/)
  * https://unix.stackexchange.com/questions/397232/adduser-addgroup-group-in-use
  * https://arkit.co.in/four-ways-non-interactively-set-passwords-linux/
 
  * handle `http://nl.alpinelinux.org/alpine/edge/main: UNTRUSTED signature` error - https://stackoverflow.com/questions/70445952/how-to-disable-ssl-verification-in-alpines-apk
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
