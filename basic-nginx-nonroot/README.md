### Usage

* launch basic-example Spring app to proxy
```sh
docker build -f ../basic/Dockerfile -t basic-example  ../basic/
docker run --name basic-example -d basic-example
```
* compile configure build and burn into image the nginx/alpine
```sh
docker build -t nginx-example -f Dockerfile .
```
* launch `nginx-example` container linked to `basic-example`, with mapped volumeto have logs locally, and exposed port, in foreground without daemonization the server:
```sh
docker run --link basic-example -p 80:80 -v $(pwd)/logs:/logs:rw -it nginx-example
```
* probe
```sh
wget -qO- http://localhost/basic
```

prints back
```sh
hello basic
```
```sh
ls -l logs/
total 4
-rw-r--r-- 1 root root 108 Oct 28 17:38 access.log
-rw-r--r-- 1 root root   0 Oct 28 17:33 error.log
```
* recycle
```sh
docker ps  | grep nginx-example |  awk '{print $1}' | xargs -IX  docker stop X
```
### Note
* the following maps the logs
```sh
docker run --link basic-example -p 80:80 -v $(pwd)/logs:/logs:rw -it nginx-example
```
* the following does not
```sh
docker run --link basic-example -p 80:80 --mount  source=./logs,target=/logs -it nginx-example
```
the user argument leads to user own worker proces but not master
```sh
docker exec -it $(docker container ls |  grep 'nginx' | awk '{print $1}') sh -c ' ps ax| grep nginx'
    1 root      0:00 nginx: master process nginx -g daemon off;
   29 myuser    0:00 nginx: worker process
   38 root      0:00 sh -c  ps ax| grep nginx
   45 root      0:00 sh -c  ps ax| grep nginx
```
the local directory is owned by root, 
```sh
ls -l logs/
total 4
-rw-r--r-- 1 root root 187 Oct 27 05:40 access.log
-rw-r--r-- 1 root root   0 Oct 27 05:38 error.log
```
not like with tomcat:
```sh
ls -l ../basic-logback/logs/App.log
-rw-r--r-- 1 sergueik systemd-journal 457 Oct  7 18:58 App.log
```
### Running as non root user
 * build image without `CMD` or `ENTRYPOINT` and run is via separate `Dockerfile`
```sh
IMAGE=nginx-custom-build
sed -i "s|UID=.*|UID=$UID|g" Dockerfile.$IMAGE
docker build -t $IMAGE -f Dockerfile.$IMAGE .
```
 * inspect the file ownership right away
```sh
docker run -it $IMAGE sh
```
run inside
```sh
ls -ld /tmp/logs  /run /var/cache/nginx /etc/nginx/conf.d
drwxr-xr-x    1 myuser   myuser        4096 Oct 28 17:18 /etc/nginx/conf.d
drwxrwxrwx    1 root     root          4096 Oct 21 09:23 /run
drwxrwxrwx    2 myuser   myuser        4096 Oct 28 17:18 /tmp/logs
drwxr-xr-x    1 myuser   myuser        4096 Oct 22 07:53 /var/cache/nginx
```
 * rebuild image with effective operating non-root user
```sh
IMAGE=nginx-nonroot
sed -i "s|UID=.*|UID=$UID|g" Dockerfile.$IMAGE
docker build -t $IMAGE -f Dockerfile.$IMAGE .
```
* confirm directory and file permissions still to hold:
```sh
```
* run the second image based container
```sh
IMAGE=nginx-nonroot
rm -fr logs
docker run --link basic-example -p 8080:8080 -v $(pwd)/logs:/tmp/logs:rw -it $IMAGE
```

this makes `/tmp/logs` desired user-owned in the container and outside:

```sh
ls -l logs/
total 4
-rw-r--r-- 1 sergueik systemd-network   0 Oct 27 21:11 access.log
-rw-r--r-- 1 sergueik systemd-network 169 Oct 27 21:11 error.log
```

```sh
curl  http://localhost:8080/basic?123
```
will respond with
```sh
Hello basic
```
and will write `access_log`:
```sh
tail logs/access.log 
```
will show:
```sh
172.17.0.1 - - [28/Oct/2020:01:13:46 +0000] "GET /basic?123 HTTP/1.1" 200 171 "-" "curl/7.58.0" "-"
```
### TODO

Quite often the directory  permissions are messed up by nginx start:
```sh
2020/10/28 17:37:26 [emerg] 1#1: open() "/tmp/logs/error.log" failed (13: Permission denied)
```
Note: the variant
```sh
IMAGE=nginx-nonroot
sudo rm -fr logs
docker run --link basic-example -p 80:80 --mount  source=./logs,target=/logs -it nginx-example
```

leaves host dir empty
while perfectly working inside:
```sh
IMAGE=nginx-nonroot
ID=$(docker container ls |  grep $IMAGE | awk '{print $1}')
docker exec -it $ID sh  -c "ls -l /tmp/logs/"
```
returns
```sh
total 8
-rw-r--r--    1 myuser   myuser         486 Oct 28 01:05 access.log
-rw-r--r--    1 myuser   myuser         338 Oct 28 01:04 error.log
```
### Running Nginx as non-root user and Mapping Log Directory
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
* make a  request
```sh
curl http://localhost:8080/basic?123
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
