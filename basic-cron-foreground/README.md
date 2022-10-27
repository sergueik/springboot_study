## Info 

Docker with foreground cron (crond on Alpine) server


### Usage
```sh
IMAGE=basic-cron
docker build -t $IMAGE -f Dockerfile .
```

```sh
NAME=basic-cron
docker run --name $NAME $IMAGE
```
NOTE: compared to crontab line examples shown in Docker crontab [posts](https://lostindetails.com/articles/How-to-run-cron-inside-Docker), we are not passing the USER (effectively we follow the [standard syntax](https://en.wikipedia.org/wiki/Cron)
```text
# ┌───────────── minute (0 - 59)
# │ ┌───────────── hour (0 - 23)
# │ │ ┌───────────── day of the month (1 - 31)
# │ │ │ ┌───────────── month (1 - 12)
# │ │ │ │ ┌───────────── day of the week (0 - 6) (Sunday to Saturday;
# │ │ │ │ │                                   7 is also Sunday on some systems)
# │ │ │ │ │
# │ │ │ │ │
# * * * * * <command to execute>
```
```text
*/1 * * * * root /bin/echo 'cron_works' >> /var/log/run_some_script.cron.log
*/1 * * * * /usr/local/lib/pyenv/versions/3.8.2/bin/python -c 'print("python cron works")' >> /var/log/run_some_script.cron.log
```
*  connct to container interactively from separate console
```sh
docker container exec -it $NAME  sh
```
```sh
ls -l /var/log
```
```text
total 4
-rw-r--r--    1 root     root             0 Oct 26 17:25 cron.log
-rw-r--r--    1 root     root            29 Oct 26 17:26 run_some_script.cron.log
```
```sh
/ # cat /var/log/run_some_script.cron.log
```
```text
cron_works
python cron works
cron_works
python cron works
cron_works
python cron works
cron_works
python cron works

```

### Cleanup
```sh
docker container stop $NAME
docker container rm $NAME
docker image rm $IMAGE
```

### See Also

  * https://stackoverflow.com/questions/37015624/how-to-run-a-cron-job-inside-a-docker-container
  * https://forums.docker.com/t/how-to-run-a-cron-job-inside-a-container-alpine/7759/2
  * https://devopsheaven.com/cron/docker/alpine/linux/2017/10/30/run-cron-docker-alpine.html
  * https://lostindetails.com/articles/How-to-run-cron-inside-Docker
  * https://qna.habr.com/q/1214210 (in Russian)

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
