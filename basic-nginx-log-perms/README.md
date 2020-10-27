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
### See Also
  * https://docs.nginx.com/nginx/admin-guide/monitoring/logging/#access_log
  * https://stackoverflow.com/questions/18861300/how-to-run-nginx-within-a-docker-container-without-halting

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
