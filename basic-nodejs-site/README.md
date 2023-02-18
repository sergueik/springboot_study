### Info

replica of [blog website](https://github.com/HauwaAguillard/BlogWebsite)
using HTML, CSS, Boostrap, JavaScript, Nodejs, NPM, Express, EJS in Alpine Docker container with node js with Elastic Node.js APM agent installed separately

### Usage

```sh
IMAGE=basic-express-site
docker build -t $IMAGE -f Dockerfile .
NAME=basic-express-site
docker run --name $NAME -d -p 3000:3000 $IMAGE
```
you will be able to compose blogs on `http://localhost:3000/compose` and see on `http://localhost:3000/`

* NOTE: there will be connection error logged periodically to console:
```text
{"log.level":"error","@timestamp":"2023-02-18T22:09:22.759Z","log":{"logger":"elastic-apm-node"},"ecs":{"version":"1.6.0"},"message":"APM Server transport error (ECONNREFUSED): connect ECONNREFUSED 127.0.0.1:8200"}
```
```text
Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. 
```
### See Also
    * [elastic nodejs apm agent releases](https://www.elastic.co/guide/en/apm/agent/nodejs/index.html)
    * [elastic nodejs apm agent documentation](https://www.elastic.co/guide/en/apm/agent/nodejs/current/intro.html)

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
