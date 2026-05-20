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
to fix add the docker machine ip to `@CrossOrigin(origins = { "http://localhost:3000",... })`

### Cleanup

```sh
docker stop $NAME
docker container prune -f
docker image prune -f
docker image rm $IMAGE
rm ~/.docker/machine/machines/default/disk.vmdk
```

### Challenges with Testing

The SPA HTML page is just a generic bootstrap, it has no DOM value -  it often contains nothing  but:
```html
 <body>
    <noscript>You need to enable JavaScript to run this app.</noscript>
    <div id="root"></div>
 <script src="/static/js/bundle.js"></script>
 <script src="/static/js/0.chunk.js"></script>
 <script src="/static/js/main.chunk.js"></script>
 </body>
```

there is no easy way to perform the usual Selenium *visit page X, fill inputs, sumbit, observe page Y, confirm the inputs done earlier can be found and visible*
test - one has to take a deep mental shift:

accept that is true:

  * application state defines UI 
  * UI is generated dynamically 

switch to Javascript run time fully (cannot still use Java). 
Run javascript runtime to execute:

```js
renderToStaticMarkup(...)
```
and examine individual pages. For anything React-specific start fresh with [RTL](https://testing-library.com/docs/react-testing-library/intro/)
and [jest](https://jestjs.io/)

Technically, Selenium still somewhat works


You can still do:

* visit page X
* fill inputs
* click submit
* observe browser loads page Y
* validate rendered DOM state

But with an important difference:

you since one is forced to work with after JavaScript execution
artifacts,
the test design is somewhat longer and labor intensive: "Knowing is not enough! He has to see it."
 
 > NOTE: Technically, Selenium does not operate on raw HTML source or server output.

Instead, it operates on the live DOM inside a running browser instance, using a small set of stable primitives:

 * navigate (GET URL)
 * locate elements (CSS, XPath, id, etc.)
 * interact (click, send keys, submit)
 * query state (text, attributes, URL)
 * synchronize (wait conditions)

The one in danger is the last one . Now one has to enable
  * "wait until the DOM reaches a condition that makes the test meaningful"

React itself does not help with that. The usual traits remain:


 * element exists 
 * element is visible
 * element is clickable
 * text matches expected value
 * AJAX/network-driven state is complete 

The problem is: Selenium does not know anything about React, Angular, or rendering cycles. Overall
modern UI is a continuously evolving state machine, not a render-once system.

### See Also
  * [fullstack-spring-boot-and-react](https://github.com/amigoscode/spring-boot-react-fullstack)



