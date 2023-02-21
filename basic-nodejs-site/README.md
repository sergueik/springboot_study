### Info

replica of __how To Make external REST API Calls In Express Web App__ [repository](https://github.com/codehandbook/make-api-call-express)
using Express, and Request modules in Alpine Docker container with node js with Elastic Node.js APM agent installed separately

### Usage

```sh
IMAGE=basic-express-site
docker build -t $IMAGE -f Dockerfile .
NAME=basic-express-site
docker run --name $NAME -d -p 3000:3000 $IMAGE
```
you will be able to perform REST calls to `https://jsonplaceholder.typicode.com/todos/1` by visiting the `http://localhost:3000/getAPIResponse`. One can direct the REST call to another APM monitored app to see __Distributed Transaction Tracing__ in action.

* NOTE: when application run standalone there will be APM server connection error logged periodically to console:
```text
{"log.level":"error","@timestamp":"2023-02-18T22:09:22.759Z","log":{"logger":"elastic-apm-node"},"ecs":{"version":"1.6.0"},"message":"APM Server transport error (ECONNREFUSED): connect ECONNREFUSED 127.0.0.1:8200"}
```
### See Also
    * __REST API from Client to Backend__ [tutorial](https://stackabuse.com/building-a-rest-api-with-node-and-express/) and [repository](https://github.com/jkasun/simple-rest-sever-express)
    * __Make External REST API Calls from Backend__ [tutorial](https://codehandbook.org/how-to-make-rest-api-calls-in-express-web-app/) and [repository](https://github.com/codehandbook/make-api-call-express)
    * https://stackoverflow.com/questions/8515872/simple-api-calls-with-node-js-and-express
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
