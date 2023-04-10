### Info

replica of __how To Make external REST API Calls In Express Web App__ [repository](https://github.com/codehandbook/make-api-call-express)
using Express, and Request modules in Alpine Docker container with node js with Elastic Node.js APM agent installed separately

### Usage

```sh
IMAGE=basic-express-site
docker build -t $IMAGE -f Dockerfile .
NAME=basic-express-site
docker container stop $NAME
docker container rm $NAME
```
run with default arguments
```sh
docker run --name $NAME -d -e PORT=3000 -p 3000:3000 $IMAGE
```
#### Verify Environment Usage
* alternatively set the application to launch socket listen port in the container through environment of the container run e.g. to `3001` and map it to e.g. `3000` on host:
```sh
docker run --name $NAME -d -e PORT=3001 -p 3000:3001 $IMAGE
```
```text
4e892d206ee680b5760e087d9404a63a99d2320bfeb213ac3f8df471ab0c3c41
```
check container logs

```sh
docker logs $NAME
```

```text
App listening on port 3001!
```

* check ports on server:
```sh
netstat -ant | grep :3000
```
```text
tcp        0      0 0.0.0.0:3000            0.0.0.0:*               LISTEN
tcp6       0      0 :::3000                 :::*                    LISTEN
```

you will be able to perform REST calls to `https://jsonplaceholder.typicode.com/todos/1` by visiting the `http://192.168.0.64:3000/api.html` and pressing the `Submit` button. The page will `POST` to the server from where the backend does the REST call to another APM monitored app to see __Distributed Transaction Tracing__ in action.


if seeing the error

```text

docker: Error response from daemon: endpoint with name basic-express-site already exists in network bridge.
```

will need to 
```sh
sudo systemctl  restart docker
```
Alternatively with `docker-compose`, define environment in `.env` file and test
```sh
docker-compose up --build
```
```text
Building nodejs-site
Step 1/8 : ARG NODE_VERSION=16.12.0
Step 2/8 : FROM node:${NODE_VERSION}-alpine3.11
 ---> 17c68d73d265
Step 3/8 : EXPOSE 3001
 ---> Using cache
 ---> 1c9819e59883
Step 4/8 : RUN apk update && apk add --update --no-cache curl   && rm -vrf /var/cache/apk/*
 ---> Using cache
 ---> aaf6c46341d0
Step 5/8 : WORKDIR /app
 ---> Using cache
 ---> 38cdd247ea31
Step 6/8 : ADD app.js package.json public/ ./
 ---> Using cache
 ---> 286fd6f93da3
Step 7/8 : RUN npm install --silent
 ---> Using cache
 ---> 1527a9880bbb
Step 8/8 : CMD ["node", "/app/app.js"]
 ---> Using cache
 ---> d5ab543e1f21

Successfully built d5ab543e1f21
Successfully tagged basic-nodejs-site_nodejs-site:latest
Recreating nodejs-site ... done
Attaching to nodejs-site
nodejs-site    | App listening on port 3001!
```

this will confirm port allocation and mapping:
```sh
docker container ls  --filter name=nodejs-site
```
```text
f83513fcdeac   basic-nodejs-site-nodejs-site   "docker-entrypoint.s…"   3 minutes ago   Up 3 minutes (healthy)   0.0.0.0:3000->3001/tcp   nodejs-site
``` 
```sh
curl --write-out 'HTTP %{http_code}' --fail --silent --output /dev/null http://localhost:3000/
```
```text
HTTP 200
```
### Cleanup
```sh
docker container stop $NAME
docker container prune -f
docker image prune -f
docker image rm $IMAGE
```
### Note

* uprade to `docker-compose` version 2.x, e.g. `2.15.0`:
```sh
curl -skL https://github.com/docker/compose/releases/download/v2.15.0/docker-compose-$(uname -s |tr 'L' 'l')-$(uname -m) -o docker-compose
sudo mv docker-compose /usr/local/bin/docker-compose
sudo chmod +x  /usr/local/bin/docker-compose
```

### See Also

  * __REST API Client to Backend__ [tutorial](https://stackabuse.com/building-a-rest-api-with-node-and-express/)
  * __Backend External REST API Call__ [tutorial](https://codehandbook.org/how-to-make-rest-api-calls-in-express-web-app/) and [repository](https://github.com/codehandbook/make-api-call-express)
  * https://stackoverflow.com/questions/8515872/simple-api-calls-with-node-js-and-express

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
