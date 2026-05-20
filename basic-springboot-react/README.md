### Info

replica of  [ReactJS-Spring-Boot-CRUD-Full-Stack-App](https://github.com/RameshMF/ReactJS-Spring-Boot-CRUD-Full-Stack-App)
> NOTE `openjdk:8-jre-alpine3.9` is removed from the Docker Hub, as the official OpenJDK repository deprecated and removed standalone JRE tags, and stopped updating Alpine-based images for general availability (GA) releases. 

### Usage

build the package using JDK 1.8:


```sh
mvn -DskipTests package
```
```sh
sed -i 's|\r||g' docker-entrypoint.sh
IMAGE=nodejs-alpine
docker build -f Dockerfile -t $IMAGE . 
```
### Note

* install modules interactively
```sh
NAME=nodejs-alpine
docker run --name $NAME -p 3000:3000 -p 8080:8080 -it nodejs-alpine sh
```
in another console
```sh
NAME=nodejs-alpine
docker cp frontend $NAME:/tmp
docker cp backend $NAME:/tmp
```
back in first console
```sh
cd /tmp/frontend
npm install react-scripts --save
```
> NOTE: this step will be quite time consuming:
```text
added 1643 packages, and audited 1644 packages in 5m

63 packages are looking for funding
  run `npm fund` for details
```

```sh
NODE_OPTIONS=--openssl-legacy-provider npm start
```
```text
Starting the development server...

Browserslist: caniuse-lite is outdated. Please run:
npx browserslist@latest --update-db
```
```sh
docker exec -w /tmp/backend  -it $NAME java -jar target/springboot-backend-0.0.1-SNAPSHOT.jar
```
```
docker exec -w /tmp/backend  -it $NAME sh
```
```
apk add openjdk8-jre
java  -jar  target/springboot-backend-0.0.1-SNAPSHOT.jar
```


if run in Docker Toolbox,
```
docker-machine ssh
```
```text
   ( '>')
  /) TC (\   Core is distributed with ABSOLUTELY NO WARRANTY.
 (/-_--_-\)           www.tinycorelinux.net
```
```sh
netstat -atn | grep 3000 | grep LISTEN
```

```text
tcp        0      0 :::3000                 :::*                    LISTEN
```

Open in the browser `http://192.168.99.100:3000`

![capture-app](screenshots/capture-app.png)
> NOTE you will receive the error in Chrom Dev Tools Console:
```text
employee => {"firstName":"John","lastName":"Doe","emailId":"jd@example.com"}
localhost:8080/api/v1/employees:1  
localhost:8080/api/v1/employees:1  Failed to load resource: 
Failed to load resource: net::ERR_CONNECTION_REFUSED
```
to fix add the docker machine ip to `@CrossOrigin(origins`
### Cleanup
```sh

docker stop $NAME

```
### See Also
  * [fullstack-spring-boot-and-react](https://github.com/amigoscode/spring-boot-react-fullstack)



