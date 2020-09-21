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
docker-compose up
```
```sh
curl http://localhost:80/
```
### Clean up
```sh
docker rm -v $(docker ps -aq -f status=exited)
docker image prune -f
```
### See Also
# https://github.com/opstree/spring3hibernate
