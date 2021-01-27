### Info

This directory containes basic JSP Tomcat application packaged into war file.
### Usage

```sh
IMAGE=basic-jsp
mvn package
```
```sh
docker container ls -a | grep $IMAGE| awk '{print $1}' | xargs docker container rm -f
```
```sh
IMAGE=basic-jsp; NAME='basic-jsp-container'; docker build -t $IMAGE -f Dockerfile . ; docker container rm -f $NAME ; docker run --name $NAME -p 8080:8080 -d $IMAGE start
```
```sh
curl http://localhost:8080/demo
```
will reply with

```html
<html><body><pre>Server:a8de1a512b97
Request URL: http://localhost:8080/demo/
APP_SERVER = null
application.setting = ${application.value}
application.setting(from file) = ${application.value}
</pre></body></html>
```
- no token expansion observed
### See Also

 * demo [app](https://github.com/vborrego/jsp-example) with bean / handler integration

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)

