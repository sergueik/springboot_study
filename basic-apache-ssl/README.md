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
### Configure and Enable Password

* turns out to not be necessary once Apache configuration is fixed
```sh
htpasswd  -bc /etc/apache2/.htpasswd user password
```
### Troubleshooting

```sh
curl -ksI https://192.168.0.92/index.html
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

 * Apply the PR [4](https://github.com/nimmis/docker-alpine-apache/pull/4/) and similar changes fixing the `ScriptAlias`


 * back out apache configurations

```sh

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
