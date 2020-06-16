### Info

This directory contains a basic exercise Java project to explore passthrough arguments for environment and variables settings into container hosted app in Docker

The application prints to `STDERR` what it gets in
```java
String propertyKey ="setting_prop";
System.err.println(propertyKey + " = " + System.getProperty(propertyKey));
```
and
```java
String envKey = "setting_env";
System.err.println(envKey + " = " + System.getenv(envKey));
```
### Testing
### Build App
```sh
mvn package
```
#### Setting from Dockerfile

```sh
export IMAGE='basic-environment'
export NAME='example-environment'
docker build -t $IMAGE -f Dockerfile .
docker run --name $NAME --rm -it $IMAGE
```
to avoid browsing the whole output, may use the command
```
export SETTING='setting_env'
2>&1 docker run --name $NAME --rm -it $IMAGE | grep $SETTING
```
will show
```sh
setting_env = dockerfile_value
```
#### Override value at build time 
```sh
docker image rm -f $IMAGE
docker build -t $IMAGE -f Dockerfile --build-arg "setting_arg=build_time_value" .
```
then
```sh
docker run --name $NAME --rm -it $IMAGE

```
will print the value defined at image build time:
```sh
setting_env = build_time_value
```
#### Override at container time (use same container as before)
```sh
2>&1 docker run --name $NAME --rm -e "setting_env=runtime_value" -it $IMAGE | grep $SETTING
```
will show
```sh
setting_env = runtime_value
```
### TODO:

The property file argument in `Dockerfile`
```sh
ENTRYPOINT ["java", "-cp", "app.jar", "-Dsetting=${setting_env}", "example.EnvironmentPrinter"]
```
is not processed correctly, the log shows:
```sh
setting = ${setting_env}
```
need fix the interpolation to take place.
e.g. adding an argument to the class 
```
```
and

`Dockerfile`
```sh
ARG setting_name="setting_env"
ENTRYPOINT ["java", "-cp", "app.jar", "-Dsetting=${setting_env}", "example.EnvironmentPrinter", "${setting_name}"]
```
seems to not being interpolated:
```
docker build -t $IMAGE -f Dockerfile .
docker inspect $IMAGE | jq '.[].ContainerConfig.Cmd' 
```
```js
[
  "/bin/sh",
  "-c",
  "#(nop) ",
  "ENTRYPOINT [\"java\" \"-cp\" \"app.jar\" \"-Dsetting=${setting_env}\" \"example.EnvironmentPrinter\" \"${setting_name}\"]"
]
```
and
```sh
docker run --name $NAME --rm -it $IMAGE
```
will show
```sh
Environment ${setting_name} = null
Property ${setting_name} = null
```
### See Also:
  * [How To Pass Environment Docker](https://blog.bitsrc.io/how-to-pass-environment-info-during-docker-builds-1f7c5566dd0e) 
  * [Docker ARG, ENV and .env - a Complete Guide](https://vsupalov.com/docker-arg-env-variable-guide/)
  * [Understanding Docker Build Args, Environment Variables and Docker Compose Variables](https://vsupalov.com/docker-env-vars/)
  * [stackoverflow](https://stackoverflow.com/questions/30494050/how-do-i-pass-environment-variables-to-docker-containers)

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)


