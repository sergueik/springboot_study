### Info


This directory contains 
vanilla Hibername Springboot project extracted from
the artcle  [Java-приложения и Docker (in Russian)](https://habr.com/company/billing/blog/350138/).
The original git repository of that article can be found on  [alexff91/Java-meetup-2018](https://github.com/alexff91/Java-meetup-2018).
use default Postgres 10 hub linked image and used maven plugin to
build application container image

### Test

* build the docker container by invoking maven plugin
```sh
IMAGE=app
mvn install
```
this will eventuallty print completion status information:
```text
[INFO] DOCKER> [app:latest]: Created docker-build.tar in 514 milliseconds
[INFO] DOCKER> [app:latest]: Built image sha256:c7d3a

```
* inspect the image
```sh
docker image ispect $IMAGE
```
check syntax
```sh
docker run -it $IMAGE sh
```
```sh
cd /app
/wait-for-it.sh jm-postgres:5432 -t 15
timeout: can't execute '15': No such file or directory
wait-for-it.sh: timeout occurred after waiting 15 seconds for jm-postgres:5432
```
* enable the and rebuild
```sh
docker container prune -f
mvn install
```
* compose the cluster
```sh
cd src/main/docker
export COMPOSE_HTTP_TIMEOUT=300
docker-compose up -d
```
```sh
export CONTAINER=$(docker container ls | grep $IMAGE| awk '{print $1}')
docker logs $CONTAINER
```
will show (eventually)
```sh
Started DemoApplication in 14.977 seconds (JVM running for 16.991)
```
* run the test
```sh
curl http://localhost:8080/
```
will print
```json
[
  {
    "id": 1,
    "name": "Hello"
  },
  {
    "id": 2,
    "name": "Java"
  },
  {
    "id": 3,
    "name": "Meet"
  },
  {
    "id": 4,
    "name": "Up"
  }
]
```
### Troubleshooting

The maven plugin imposes a "virtual" file layout in `Dockerfile`  making is useless for a standalone docker build.
To workaround this, do temporarily create a real one (not a soft link)
```sh
cd src/main/docker
mkdir maven
cp *sh maven
cp ../../../target/app.jar maven
OTHER_IMAGE=app1
docker build -f Dockerfile -t $OTHER_IMAGE .
docker image rm $OTHER_IMAGE
rm -f maven
```
### Cleanup


```sh
docker-compose stop
docker container prune -f
IMAGE=app
docker image rm $IMAGE  
docker image rm postgres:10.1
docker sysem prune -f
```

### Note

[Docker Toolbox on legacy Windows](https://docs.docker.com/toolbox/) essentially relies on running a VirtualBox Linux VM
### See Also

  * Docker Maven Plugin [repo](https://github.com/fabric8io/docker-maven-plugin) 
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)

