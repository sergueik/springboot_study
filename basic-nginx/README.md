### Info

This directory contains a [basic nginx configuration](https://hellokoding.com/spring-boot/docker/) proxying a 
 copy of [basic static page springboot app](https://github.com/sergueik/springboot_study/tree/master/basic-static) in `app`.

### Usage

```sh
pushd app
mvn clean package
```
```sh
export COMPOSE_HTTP_TIMEOUT=600
docker-compose up --build
```
```sh
curl http://localhost:80/application/
```
NOTE: there is no explicit mapping of the `/application` in the nginx configuguration. 

If there is need to troubleshoot the java instance
```sh
cd app
mvn clean package
docker build -t app-image -f Dockerfile .
docker run -it -p 8080:8080 app-image
```
follodwed by a test on the application port:
```sh
curl http://localhost:8080/application/
```
### Clean up
```sh
docker rm -v $(docker ps -aq -f status=exited)
docker image prune -f
```
### See Also
# https://github.com/opstree/spring3hibernate
