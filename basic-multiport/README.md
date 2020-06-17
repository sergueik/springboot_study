### Info

Springboot basic multi-port project cloned from 
[springboot mySQL Docker container](https://tech.asimio.net/2016/12/15/Configuring-Tomcat-to-Listen-on-Multiple-ports-using-Spring-Boot.html) 
converted to run on alpine openjdk jre base image.

### Test

* run locally
```sh
mvn clean spring-boot:run
```

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
    "name": "springboot-tomcat-multiple-ports"
  },
  "build": {
    "version": "0-SNAPSHOT"
  }
}

```


### See Also

 * https://stackoverflow.com/questions/51876390/spring-boot-multiple-ports
 * https://stackoverflow.com/questions/36357135/configure-spring-boot-with-two-ports



{"timestamp":1592434821969,"status":404,"error":"Not Found","message":"No message available","path":"/demo"}
