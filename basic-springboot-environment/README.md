### Info


### Usage

#### Properties
```sh
mvn spring-boot:run
```
```sh
view-source:http://localhost:8085/property/somevalue
```
```text
System Property somevalue: unknown
Application Property somevalue: green
```
```sh
mvn spring-boot:run -Drun.jvmArguments="-Dsomevalue=white"
```
```sh
curl http://localhost:8085/property/somevalue
```
```text
System Property somevalue: white
Application Property somevalue: white
```
NOTE: this example uses Spring 1.5.x syntax. With Spring 2.2.x use the following syntax: `-Dspring.boot.run.jvmArguments="-Dsomevalue=true"`

```sh
mvn -Dspring.config.location=other.properties spring-boot:run
```
```sh
curl http://localhost:8085/property/somevalue
```
```text
System Property somevalue: unknown
Application Property somevalue: red
```
#### Environment
```sh
set somevalue=violet
mvn spring-boot:run
```
```sh
curl http://localhost:8085/environment/somevalue
```
```text
Environment somevalue: violet
```
### Docker container
```sh
mnv clean package
```
build image
```sh
export IMAGE='basic-springboot-environment'
export NAME='example-springboot-environment'
docker build -t $IMAGE -f Dockerfile .
```
run
```sh
docker run --name $NAME --rm -p 8085:8085 $IMAGE
```
test
```sh
curl http://192.168.0.64:8085/property/somevalue
```
```txt
System Property somevalue: unknown Application Property somevalue: green
```
set the property
```sh
docker run --name $NAME --rm -p 8085:8085 -e name_env=somevalue -e value_env=black -it $IMAGE
```

```sh
docker exec -it $NAME sh
```
```sh
ps ax
```
```sh
java -Dsomevalue=black -jar app.jar
```
```sh
curl http://localhost:8085/property/somevalue

```
```text
System Property somevalue: black Application Property somevalue: black
```
##### Override value at build time
```sh
docker image rm -f $IMAGE
docker build -t $IMAGE -f Dockerfile --build-arg "name_arg=othervalue" --build-arg "value_arg=red" .
```
```sh
docker run --name $NAME --rm -p 8085:8085 $IMAGE
```
test
```sh
curl http://$(hostname -i):8085/property/othervalue
```

```txt
System Property othervalue: red
```
NOTE: each arg has to be  passed with separate `--build-arg`, forgetting this would lead to the error in run:
```sh
Error: Could not find or load main class value_arg=red=dockerfile_value
```

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
