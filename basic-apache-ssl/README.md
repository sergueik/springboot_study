### Info 


This directory contains a replica of [alpine apache2 image with ssl](https://github.com/nimmis/docker-alpine-apache) with the [missing dependnency fix](https://github.com/nimmis/docker-alpine-apache/pull/4) and a minor fix to enable `cgi-bin` as explained below in the __Troubleshooting__


###  Usage
```sh
IMAGE=alpine-apache
docker build -t $IMAGE -f Dockerfile .
```
```sh
NAME=basic-apache-alpine
docker container stop $NAME
docker container rm $NAME
docker run -d --name $NAME -p 80:80 -p 443:443 $IMAGE
```

```sh
docker logs $NAME
```

this will show:

```text
run: apache2: (pid 38) 5s
run: crond: (pid 39) 5s
run: rsyslogd: (pid 37) 5s
```
```sh
curl -ksI https://$(hostname -i ):443/index.html
```
```sh
curl -ksI https://$(docker-machine ip):443/index.html
```
```text
HTTP/1.1 200 OK
Date: Sat, 02 Sep 2023 16:11:07 GMT
Server: Apache/2.4.56 (Unix)
Last-Modified: Sat, 02 Sep 2023 00:23:55 GMT
ETag: "51b-604554add1cc0"
Accept-Ranges: bytes
Content-Length: 1307
Content-Type: text/html
```
```sh
curl -ksI https://$(hostname -i ):443/cgi-bin/statuscode.cgi?code=204
```
```sh
curl -ksI https://$(docker-machine ip):443/cgi-bin/statuscode.cgi?code=204
```
```text
HTTP/1.1 204 No Content
Date: Sat, 02 Sep 2023 16:11:16 GMT
Server: Apache/2.4.56 (Unix)
```
### Troubleshooting


On Windows / Docker Toolbox environment, may observe
```text
fail: apache2: unable to change to service directory: not a directory
run: crond: (pid 37) 5s
run: rsyslogd: (pid 36) 5s
```
this is because on Windows the soft link is not correctly checked out

```text
down: apache2: 1s, normally up, want up
```
this is because of line ending conversion
```sh
docker exec -it $NAME sh
```

#### Configure and Enable Password

* turns out to not be necessary once Apache configuration is fixed
```sh
htpasswd  -bc /etc/apache2/.htpasswd user password
```

```sh
curl -ksI https://192.168.0.64:443/index.html
```

```sh
HTTP/1.1 403 Forbidden
Date: Fri, 01 Sep 2023 18:18:29 GMT
Server: Apache/2.4.56 (Unix)
Last-Modified: Fri, 01 Sep 2023 18:02:01 GMT
ETag: "56c-6044ff5153840"
Accept-Ranges: bytes
Content-Length: 1388
Content-Type: text/html
```

```sh
curl -ksI https://$(docker-machine ip):443/index.html
```

 * Apply the PR [4](https://github.com/nimmis/docker-alpine-apache/pull/4/) and similar changes fixing the `ScriptAlias`


 * back out apache configurations

```sh
docker cp $NAME:/web/config/conf.d/httpd.conf .
docker cp $NAME:/web/config/httpd.conf .
```

### Testing CGI

```sh
curl -ksI https://localhost:443/cgi-bin/statuscode.cgi?code=204
```
* NOTE: on Windows __Docker Toolbox__ use the following:
```sh
curl -ksI https://$(docker-machine ip):443/cgi-bin/statuscode.cgi?code=204
```

```text
HTTP/1.1 204 No Content
Date: Fri, 01 Sep 2023 20:54:08 GMT
Server: Apache/2.4.56 (Unix)
```
```sh
curl -ksI https://localhost:443/cgi-bin/statuscode.cgi?code=999
```
```text
HTTP/1.1 406 Not Acceptable
Date: Fri, 01 Sep 2023 20:56:16 GMT
Server: Apache/2.4.56 (Unix)
```
### Cleanup
```sh
docker stop $NAME
docker container rm $NAME
docker image rm $IMAGE
docker volume prune -f
```


### See Also

  * https://www.digitalocean.com/community/tutorials/how-to-set-up-password-authentication-with-apache-on-ubuntu-20-04

  *  https://medium.com/@nh3500/how-to-create-self-assigned-ssl-for-local-docker-based-lamp-dev-environment-on-macos-sierra-ab606a27ba8a

  * https://codeburst.io/http-server-on-docker-with-https-7b5468f72874

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
