### Info

This directory contains a [basic nginx configuration](https://hellokoding.com/spring-boot/docker/) proxying a 
 copy of [basic static page springboot app](https://github.com/sergueik/springboot_study/tree/master/basic-static) in `app`.

### Usage

* build springbot app
```sh
pushd app
mvn clean package
popd
```
* build cluster
```sh
export COMPOSE_HTTP_TIMEOUT=600
docker-compose up --build
```
* test from host
```sh
curl http://localhost:80/application/
```
* check nginx logs:
```sh
docker-compose logs -f nginx
```
this will show a mix of spring and nginx messages:
```text
app      | 2021-12-13 22:18:14.627  INFO 1 --- [nio-8080-exec-5] example.controller.HomeController        : Setting text from property application:application
app      | 2021-12-13 22:18:14.627  INFO 1 --- [nio-8080-exec-5] example.controller.HomeController        : Setting text from environment hostname:Server:981fad0be0c8
nginx    | 192.168.0.25 - - [13/Dec/2021:22:18:14 +0000] "GET /application HTTP/1.1" 200 876 "-" "curl/7.74.0" "-"
```
or
```sh
docket-compose exec -it nginx sh
```
```sh
cat /proc/1/fd/1
```
alternatively set additional logging to file `/var/log/nginx/access_custom.log` (the `/var/log/nginx/access.log` is alreadt symlinked to `stdout` in Docker base image)
and check via
```sh
docket-compose exec -it nginx sh
```

```sh
ls -l /var/log/nginx/access_custom.log
-rw-r--r--    1 root     root           104 Dec 13 22:27 /var/log/nginx/access_custom.log
```

```sh 
cat /var/log/nginx/access_custom.log

```
```text
192.168.0.25 - - [13/Dec/2021:22:27:09 +0000] "GET /application HTTP/1.1" 200 876 "-" "curl/7.74.0" "-"
```
NOTE: there is no need in explicit mapping of the `/application` in the nginx configuration. 

If there is need to troubleshoot the java instance
```sh
cd app
mvn clean package
docker build -t app-image -f Dockerfile .
docker run -it -p 8080:8080 app-image
```
follodwed by a test on the application port, in the container:
```sh
curl http://localhost:8080/application/
```
### Clean up
```sh
docker-compose rm -f
```
alterntively
```sh
docker rm -v $(docker ps -aq -f status=exited)
docker image prune -f
```
### See Also
  * example with [mysql, spring hibernate, nginx](https://github.com/opstree/spring3hibernate)
  * [note](https://nginx.org/ru/docs/http/ngx_http_proxy_module.html#proxy_pass)  about URI format
  * nginx admin [documentation](https://docs.nginx.com/nginx/admin-guide/monitoring/logging/)
