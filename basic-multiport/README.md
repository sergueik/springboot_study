### Info

Springboot basic multi-port project cloned from
[configuring Tomcat to listen on multiple ports through springboot](https://tech.asimio.net/2016/12/15/Configuring-Tomcat-to-Listen-on-Multiple-ports-using-Spring-Boot.html)
converted to run on alpine openjdk jre base image.

### Usage

* run locally
```sh
mvn clean spring-boot:run
```
or in Docker container
```sh
mvn clean package
IMAGE=basic-multiple-ports
docker build -f Dockerfile -t $IMAGE .
docker run -d -p 8880:8880 -p 8881:8881 -p 8882:8882 -p 8883:8883 $IMAGE
```
* test
```sh
curl http://localhost:8880/demo
```
```sh
demo
```
```sh
curl  http://localhost:8881/admin/info 2>/dev/null | jq
```
```js
{
  "app": {
    "name": "multiple-ports"
  },
  "build": {
    "version": "0.1:-SNAPSHOT"
  }
}
```


### See Also

 * https://stackoverflow.com/questions/51876390/spring-boot-multiple-ports
 * https://stackoverflow.com/questions/36357135/configure-spring-boot-with-two-ports
