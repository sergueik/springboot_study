### Info

this project exercies the inline command in docker-compose.yaml

### Usage

* build the cluster
```sh
export COMPOSE_HTTP_TIMEOUT=600
docker-compose  -f docker-compose.yaml up --build
```
this will log to console:
```text
Creating network "basic-inline_default" with the default driver
Creating app ... done
Attaching to app
app    | SERVICE_HOST="slow-mysql-server"
app    | Debug logs in /tmp/debug.log
app    | Waiting on the slow-mysql-server 3306
app    | nc: bad address 'slow-mysql-server'
app    | Waiting 10 sec
app    | Waiting on the slow-mysql-server 3306
```
and continue repeating forever, when there is no `slow-mysql-server` service defined in the default network
* connect  to the container from a separate console to inspect the log file:
```sh
docker-compose exec app sh
```
NOTE - there is no `-it` option when running interactive session via `docker-compose`

* in the container run
```sh
less /tmp/debug.log
```
to observe the log of the wait loop


### Cleanup

```sh
docker-compose  -f docker-compose.yaml down

```
### See also
  * https://stackoverflow.com/questions/49897503/docker-compose-invalid-interpolation-format-for-environment-option-in-service
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)


