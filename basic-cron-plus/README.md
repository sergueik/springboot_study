### Info

### Usage

docker container rm -f $CONTAINER_NAME
```sh
IMAGE_NAME='cron-plus-image'
CONTAINER_NAME='cron-plus-container'
docker container rm -f $CONTAINER_NAME
```
```sh
docker build -t $IMAGE_NAME -f Dockerfile .
docker run -d --name $CONTAINER_NAME -t $IMAGE_NAME
```
```sh
docker container ls -a
docker attach $CONTAINER_NAME
```
wait a minute
```sh
crond[6]: USER root pid  18 cmd /usr/local/bin/script.sh >> /var/log/script.log
crond[6]: USER root pid  19 cmd /usr/local/bin/script.sh >> /var/log/script.log
```
and so on
```sh
docker exec -it $CONTAINER_NAME sh 
```
```sh
cat /var/log/script.log
Test
Test
Test

```
### See Also

# https://stackoverflow.com/questions/37015624/how-to-run-a-cron-job-inside-a-docker-container
# https://forums.docker.com/t/how-to-run-a-cron-job-inside-a-container-alpine/7759/8
# http://man.gnu.org.ua/manpage/?8+crond
# https://www.unix.com/unix-for-dummies-questions-and-answers/129369-how-change-log-level-cron.html
