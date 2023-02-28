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
docker run --name $NAME -d -e PORT=3000 -p 3000:3000 $IMAGE
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
### See Also

  * __REST API Client to Backend__ [tutorial](https://stackabuse.com/building-a-rest-api-with-node-and-express/)
  * __Backend External REST API Call__ [tutorial](https://codehandbook.org/how-to-make-rest-api-calls-in-express-web-app/) and [repository](https://github.com/codehandbook/make-api-call-express)
  * https://stackoverflow.com/questions/8515872/simple-api-calls-with-node-js-and-express

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
