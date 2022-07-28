### Info

This directory contains a [basic nginx configuration](https://hellokoding.com/spring-boot/docker/) proxying a 
 copy of [basic static page springboot app](https://github.com/sergueik/springboot_study/tree/master/basic-static) in `app` with static content served from the nginx container as described in [configure serving static content with Nginx
](http://www.java2novice.com/nginx/configure-static-content/) document

### Usage

* remove static image - missing image is easy to discover. The fact the style sheet is missing too, can be seen indirectly through font change but one has to be aware of the desired page looks

```sh
IMAGEPATH=app/src/main/resources/static/images/cloud.png
rm $IMAGEPATH
```

* build springbot app
```sh
pushd app
mvn clean package
popd
```
* comment the volumes instruction in `docker-compose.yml`:

```YAML
  nginx:
  #  volumes:
  #    - ./app/src/main/resources/static:/var/www/

```
* build cluster

```sh

export COMPOSE_HTTP_TIMEOUT=600
docker-compose up --build
```

open page in the browser (may need to clear browsing history first):

![page with image problem](https://github.com/sergueik/springboot_study/blob/master/basic-nginx-static/screenshots/capture_broken_image_page.png)

confirm in the console logs the `No such file or directory` is logged by `nginx`:
```text
nginx    | 2022/07/27 17:21:34 [error] 21#21: *16 open() "/var/www/images/cloud.png" failed (2: No such file or directory), client: 192.168.0.25, server: , request: "GET /images/cloud.png HTTP/1.1", host: "192.168.0.29", referrer: "http://192.168.0.29/application"

```

* restore image
```sh
IMAGEPATH=app/src/main/resources/static/images/cloud.png
git checkout $IMAGEPATH
```

do not rebuild the java app

* uncomment the volumes instruction in `docker-compose.yml`:
```YAML
  nginx:
    volumes:
      - ./app/src/main/resources/static:/var/www/

```
* recycle and rebuild cluster
```sh
docker-compose stop
docker-compose rm -f
```

```text
Stopping nginx ... done
Stopping app   ... done
```
```text
Removing nginx ... done
Removing app   ... done

```
```sh
export COMPOSE_HTTP_TIMEOUT=600
docker-compose up --build
```
open page in the browser:

![page with static image](https://github.com/sergueik/springboot_study/blob/master/basic-nginx-static/screenshots/capture_page_with_image.png)

* test in console from host
```sh
curl http://localhost:80/application/
```
* connect to nginx and app continers to see where the image static resource is
```sh
NAME='static_nginx'
ID=$(docker container ls |  grep "$NAME" | awk '{print $1}' )
docker exec -it $ID ls /var/www/
 
```
this will show 
```text
css     images
```
 
```sh
NAME='static_app'
ID=$(docker container ls |  grep "$NAME" | awk '{print $1}' )
docker exec -it $ID ps
 ```
 
 this will show
 
```text
 PID   USER     TIME  COMMAND
    1 root      0:42 java -jar app.jar
   47 root      0:00 ps

```
 
verify there is no image inside the jar (will have to do it locally):
 
```sh
jar tvf app/target/example.static_page.jar  | grep 'BOOT-INF/classes/static'
```

```text
  0 Wed Jul 27 10:22:42 EDT 2022 BOOT-INF/classes/static/
  0 Wed Jul 27 10:22:42 EDT 2022 BOOT-INF/classes/static/css/
163 Wed Jul 27 10:22:42 EDT 2022 BOOT-INF/classes/static/css/core.css
```

- see no `cloud.png`
 
verify the operation of `nginx` server reporting missing files
```
curl http://localhost:80/css/missing.css
```
through the docker compose logs tagged by `nginx`:
```  
nginx    | 2022/07/27 15:51:45 [error] 22#22: *15 open() "/var/www/css/missing.css" failed (2: No such file or directory), client: 172.20.0.1, server: , request: "GET /css/missing.css HTTP/1.1", host: "localhost"
```
and

```sh
curl http://localhost:80/images/missing.png
```
logged in docker compose console log:
```
nginx    | 2022/07/27 15:52:15 [error] 22#22: *16 open() "/var/www/images/missing.png" failed (2: No such file or directory), client: 172.20.0.1, server: , request: "GET /images/missing.css HTTP/1.1", host: "localhost" 
```
 
repeat with existing static image and style resources - there will not be any logging in docker console
 
```sh
curl http://localhost:80/css/core.css
```
this will print the stylesheet

```sh
curl http://localhost:80/images/cloud.png -o /dev/null
```
this will show the progress (one can also check the status) 
 
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
