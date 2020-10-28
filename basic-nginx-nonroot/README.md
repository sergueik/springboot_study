
### Usage

* launch basic-example Spring app to proxy
```sh
docker build -f ../basic/Dockerfile -t basic-example  ../basic/
docker run --name basic-example -d basic-example
```
* compile configure build and burn into image the nginx/alpine
```sh
IMAGE=nginx-example
docker build -t $IMAGE -f Dockerfile .
```
* launch `nginx-example` container linked to `basic-example`, with mapped volumeto have logs locally, and exposed port, in foreground without daemonization the server:
```sh
IMAGE=nginx-example
docker run --link basic-example -p 80:80 -v $(pwd)/logs:/logs:rw -it $IMAGE
```
ignoring the
```sh
10-listen-on-ipv6-by-default.sh: error: /etc/nginx/conf.d/default.conf differs from the packages version
```
* probe
```sh
wget -qO- http://localhost/basic
```

prints back
```sh
hello basic
```
* observe log directory to be updated (the files owned by root)
```sh
ls -l logs/
total 4
-rw-r--r-- 1 root root 108 Oct 28 17:38 access.log
-rw-r--r-- 1 root root   0 Oct 28 17:33 error.log
```
* recycle
```sh
docker ps | grep nginx-example |  awk '{print $1}' | xargs -IX  docker stop X
docker container prune -f 
docker image rm -f $IMAGE
```
### Running as non root user

* launch basic-example Spring app to proxy if not already
```sh
docker build -f ../basic/Dockerfile -t basic-example  ../basic/
docker run --name basic-example -d basic-example
```
* build nginx-apache derived image with custom configuration
```sh
IMAGE=nginx-nonroot
sed -i "s|UID=.*|UID=$UID|g" Dockerfile.$IMAGE
docker build -t $IMAGE -f Dockerfile.$IMAGE .
```
* launch proxy servicing the java application via port 8080 and running nginx under custom user with same `uid` as the developer
```sh
IMAGE=nginx-nonroot
export HOST_USER=$(id -u):$(id -g)
rm -fr logs ; mkdir logs
docker run --link basic-example -p 8080:8080 -v $(pwd)/logs:/logs:rw -it $IMAGE
```
ignoring the
```sh
10-listen-on-ipv6-by-default.sh: error: can not modify /etc/nginx/conf.d/default.conf (read-only file system?)

```
and the
```sh
2020/10/28 21:35:46 [warn] 1#1: the "user" directive makes sense only if the master process runs with super-user privileges, ignored in /etc/nginx/nginx.conf:5
nginx: [warn] the "user" directive makes sense only if the master process runs with super-user privileges, ignored in /etc/nginx/nginx.conf:5
```
* make a  request
```sh
wget -qO- http://localhost/basic?123
```
will respond with
```sh
hello basic
```
* observe the local logs folder to still be empty
```sh
find logs -type f
```
* attach to running container to inspect the log directory ownership and contents
```sh
IMAGE=nginx-nonroot
docker exec -it $(docker container ls | grep $IMAGE | awk '{print $1}') sh -c 'ls -l /tmp/logs/'
```
```sh
-rw-r--r--    1 myuser   myuser         100 Oct 28 21:11 access.log
-rw-r--r--    1 myuser   myuser           0 Oct 28 21:10 error.log
```

### See Also
  * https://docs.nginx.com/nginx/admin-guide/monitoring/logging/#access_log
  * https://stackoverflow.com/questions/18861300/how-to-run-nginx-within-a-docker-container-without-halting
  * https://stackoverflow.com/questions/42329261/running-nginx-as-non-root-user
  * [nginx in Docker without Root](http://pjdietz.com/2016/08/28/nginx-in-docker-without-root.html) - does not appear to cover the log file ownership that is somewhat critical for developers using Docker
  *  Как настроить права в Docker? [forum](https://qna.habr.com/q/872915) novice discussion of setting alternative user, ostly around doing it the `docker-compose.yaml` way (in Russian, with links)

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
