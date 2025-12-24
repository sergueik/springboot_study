### Info

### Usage

```sh
IMAGE_NAME='cron-plus'
CONTAINER_NAME='cron-plus'
docker build -t $IMAGE_NAME -f Dockerfile .
docker container rm -f $CONTAINER_NAME
docker run -d --name $CONTAINER_NAME -t $IMAGE_NAME
```
wait a minute
```sh
until [ "$(docker inspect -f '{{.State.Status}}' $CONTAINER_NAME)" == "running" ]; do sleep 1; done;
```
```sh
docker logs $CONTAINER_NAME
```
```txt
crond[6]: crond (busybox 1.29.3) started, log level 8
crond[6]: USER root pid   7 cmd /usr/local/bin/script.sh >> /var/log/script.log
crond[6]: USER root pid   8 cmd /usr/local/bin/script.sh >> /var/log/script.log
```
and so on
```sh
docker exec $CONTAINER_NAME cat /var/log/script.log 
```
```sh
Test
Test
Test

```
### See Also

 * https://stackoverflow.com/questions/37015624/how-to-run-a-cron-job-inside-a-docker-container
 * https://forums.docker.com/t/how-to-run-a-cron-job-inside-a-container-alpine/7759/8
 * http://man.gnu.org.ua/manpage/?8+crond
 * https://www.unix.com/unix-for-dummies-questions-and-answers/129369-how-change-log-level-cron.html
 * https://github.com/xordiv/docker-alpine-cron
 * https://hub.docker.com/r/justb4/cron - runs "Dillon's Cron" [dcron](http://www.jimpryor.net/linux/dcron.html)
  * https://hub.docker.com/r/willfarrell/crontab - wrapper over docker to all complex cron job to be run in other containers

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
