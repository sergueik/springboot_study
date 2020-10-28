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
docker build -t $IMAGE -f Dockerfile.$IMAGE .
```
 * rebuild image with effective operating non-root user
```sh
IMAGE=nginx-nonroot
docker build -t $IMAGE -f Dockerfile.$IMAGE .
```
* run the second image based container
```sh
IMAGE=nginx-nonroot
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
Note: the variant
```sh
IMAGE=nginx-nonroot

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
### See Also
  * https://docs.nginx.com/nginx/admin-guide/monitoring/logging/#access_log
  * https://stackoverflow.com/questions/18861300/how-to-run-nginx-within-a-docker-container-without-halting
  * https://stackoverflow.com/questions/42329261/running-nginx-as-non-root-user
  * http://pjdietz.com/2016/08/28/nginx-in-docker-without-root.html#:~:text=When running Nginx as a,%2Fvar%2Frun%2Fnginx.

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
