### Info

Basic ash shell script extracted from Docker compose service with sole purpose of synchronizarion of other services [example](https://github.com/dadarek/docker-wait-for-dependencies)
converted too use alpine base image, modified to accept the hostname argument

### Usage
To guarantee that `the_database` and `another_service` are ready before running `the_web_server`:

```sh
export COMPOSE_HTTP_TIMEOUT=600
docker-compose  -f docker-compose.yaml run --rm start_dependencies
```
where the service is defined as:
```yaml
  start_dependencies:
    image: dadarek/wait-for-dependencies
    environment:
      - SLEEP_LENGTH: 0.5
```
### See Also

  * [shell script](https://github.com/Dmitry-Shweikus/wait-for-it/blob/master/wait-for-it.sh) to wait for specified `HOST` and `PORT` with `TIMEOUT` (cloned locally)
  * [shell script](https://github.com/Dmitry-Shweikus/wait-for-it/blob/master/wait-for-it.sh) to wait for specified `HOST` and `PORT` with `TIMEOUT` (cloned locally)
  * [variant](https://github.com/eficode/wait-for/blob/master/wait-for) of the same script with support of TCP / HTTP "protocol" argument 
  * Docker Compose startup shutdown order [documentation](https://docs.docker.com/compose/startup-order/)

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)


  * [variant](https://github.com/eficode/wait-for/blob/master/wait-for) of the same script with support of TCP / HTTP "protocol" argument 
  * Docker Compose startup shutdown order [documentation](https://docs.docker.com/compose/startup-order/)

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)


