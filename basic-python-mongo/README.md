### Info
example MongoDB / Python container cluster with / without authentication
* based on [problem discussion in forum](https://qna.habr.com/q/1276934) (in Russian). NOTE: the `fastapi` part is not working yet (installing `unicorn` through pip needs compilers to be added to base image). 
### Usage

#### No Authentication

* use Ubuntu Mongo image (alternatively use)
```sh
IMAGE=mongo:4.4.6-bionic
CONTAINER=mongo_server
docker pull $IMAGE
docker container rm $CONTAINER
docker run -d -e MONGO_INITDB_DATABASE=db --name $CONTAINER $IMAGE
```
* connect to container and create database
```sh
docker exec -it $CONTAINER sh
```

```sh
mongo
```
```sh
use db;
db.db.insert({name:"john", age: 100 })
```
```sh
show databases
```
```text
admin   0.000GB
config  0.000GB
db      0.000GB
local   0.000GB
test    0.000GB
```
```sh
quit()
```
```sh
exit
```
* create the Python client container
```sh
CONTAINER_IMAGE=mongo_client_python
cp env.NO-AUTH .env
docker build -f Dockerfile -t $CONTAINER_IMAGE .
```
* NOTE: time-consuming! 

update the `.env` to have the name of MONGO_SERVER container apepar in the `DATABASE_URL` (`.env` appears to have precedence over arguments):
```text
```
```sh
CONTAINER_NAME=mongo_client_python
docker container rm $CONTAINER_NAME
MONGO_SERVER=mongo_server

docker run --link $MONGO_SERVER -e DATABASE_URL=mongodb://$MONGO_SERVER:27017/db --name $CONTAINER_NAME -it $CONTAINER_IMAGE 
```

this will print:
```text
Connected to the MongoDB database via connection string mongodb://mongo_server:27017/test MongoClient(host=['mongo_server:27017'], document_class=dict, tz_aware=False, connect=True) Database(MongoClient(host=['mongo_server:27017'], document_class=dict, tz_aware=False, connect=True), 'test')
oid:<pymongo.results.InsertOneResult object at 0x7efc9a223c70>
oid:<pymongo.results.InsertOneResult object at 0x7efc9a223dc0>
oid:<pymongo.results.InsertOneResult object at 0x7efc9a223d30>
oid:<pymongo.results.InsertOneResult object at 0x7efc9a223ca0>
oid:<pymongo.results.InsertOneResult object at 0x7efc9a2237c0>
...
MongoDB database connection were closed.
```
timing shows:
```text
real	0m2.728s
user	0m0.053s
sys	0m0.037s

```

#### With Authentication

* recycle container and start with auth options passed in
```sh
IMAGE=mongo:4.4.6-bionic
docker pull $IMAGE
MONGO_SERVER=mongo_server
docker stop $CONTAINER
docker container rm $CONTAINER
docker run -d -e MONGO_INITDB_ROOT_USERNAME=admin -e MONGO_INITDB_ROOT_PASSWORD=admin -e MONGO_INITDB_DATABASE=db --name $MONGO_SERVER $IMAGE
```
* connect to  mongodb container and run mongo in console with authentication:

```sh
docker exec -it mongodb sh
```
```sh
> show databases
```
```text
admin   0.000GB
config  0.000GB
local   0.000GB
```

```sh
> use db;
> db.db.insert({name:"john", age: 100 })
```
```sh
> show databases
```
```text
admin   0.000GB
config  0.000GB
db      0.000GB
local   0.000GB
test    0.000GB
```
```sh
> quit()
```
```sh
exit
```
* connect python client with authentication
```sh
cp env.WITH-CREDENTIALS .env
CONTAINER_IMAGE=test
docker build -f Dockerfile -t $CONTAINER_IMAGE .

```

```sh
CONTAINER=test
docker container rm $CONTAINER
MONGO_SERVER=mongo_server
docker run -e MONGO_INITDB_ROOT_USERNAME=admin --link $MONGO_SERVER -e MONGO_INITDB_ROOT_PASSWORD=admin -e MONGO_INITDB_DATABASE=db -e DATABASE_URL=mongodb://admin:admin@$MONGO_SERVER:27017/db --name $CONTAINER -it $CONTAINER $CONTAINER_IMAGE
```
no output

```sh
docker run --link mongodb -e MONGO_INITDB_ROOT_USERNAME=admin -e MONGO_INITDB_ROOT_PASSWORD=admin --link $MONGO_SERVER -e MONGO_INITDB_DATABASE=db -e DATABASE_URL=mongodb://admin:admin@$MONGO_SERVER:27017/db --name $CONTAINER -it $CONTAINER sh
```

```sh
python app.py
```

```text
Connected to the MongoDB database via connection string mongodb://admin:admin@mongodb:27017/db MongoClient(host=['mongodb:27017'], document_class=dict, tz_aware=False, connect=True) Database(MongoClient(host=['mongodb:27017'], document_class=dict, tz_aware=False, connect=True), 'db')

```
```text
Traceback (most recent call last):
  File "/app/app.py", line 10, in <module>
    print('Test:{}'.format(database.list_collections()))
  File "/usr/local/lib/python3.8/site-packages/pymongo/database.py", line 911, in list_collections
    return self.__client._retryable_read(_cmd, read_pref, session)
  File "/usr/local/lib/python3.8/site-packages/pymongo/_csot.py", line 105, in csot_wrapper
    return func(self, *args, **kwargs)
  File "/usr/local/lib/python3.8/site-packages/pymongo/mongo_client.py", line 1442, in _retryable_read
    with self._socket_from_server(read_pref, server, session) as (sock_info, read_pref):
  File "/usr/local/lib/python3.8/contextlib.py", line 113, in __enter__    return next(self.gen)
  File "/usr/local/lib/python3.8/site-packages/pymongo/mongo_client.py", line 1282, in _socket_from_server
    with self._get_socket(server, session) as sock_info:
  File "/usr/local/lib/python3.8/contextlib.py", line 113, in __enter__    return next(self.gen)
  File "/usr/local/lib/python3.8/site-packages/pymongo/mongo_client.py", line 1217, in _get_socket
    with server.get_socket(handler=err_handler) as sock_info:
  File "/usr/local/lib/python3.8/contextlib.py", line 113, in __enter__    return next(self.gen)
  File "/usr/local/lib/python3.8/site-packages/pymongo/pool.py", line 1407, in get_socket
    sock_info = self._get_socket(handler=handler)
  File "/usr/local/lib/python3.8/site-packages/pymongo/pool.py", line 1520, in _get_socket
    sock_info = self.connect(handler=handler)
  File "/usr/local/lib/python3.8/site-packages/pymongo/pool.py", line 1378, in connect
    sock_info.authenticate()
  File "/usr/local/lib/python3.8/site-packages/pymongo/pool.py", line 870, in authenticate
    auth.authenticate(creds, self)
  File "/usr/local/lib/python3.8/site-packages/pymongo/auth.py", line 549, in authenticate
    auth_func(credentials, sock_info)
  File "/usr/local/lib/python3.8/site-packages/pymongo/auth.py", line 475, in _authenticate_default
    return _authenticate_scram(credentials, sock_info, "SCRAM-SHA-1")
  File "/usr/local/lib/python3.8/site-packages/pymongo/auth.py", line 201, in _authenticate_scram
    res = sock_info.command(source, cmd)
  File "/usr/local/lib/python3.8/site-packages/pymongo/pool.py", line 767, in command
    return command(
  File "/usr/local/lib/python3.8/site-packages/pymongo/network.py", line 166, in command
    helpers._check_command_response(
  File "/usr/local/lib/python3.8/site-packages/pymongo/helpers.py", line 181, in _check_command_response
    raise OperationFailure(errmsg, code, response, max_wire_version)
pymongo.errors.OperationFailure: Authentication failed., full error: {'ok': 0.0, 'errmsg': 'Authentication failed.', 'code': 18, 'codeName': 'AuthenticationFailed'}
```
####  Fix Authentication Error
```sh
cp env.FIXED .env
CONTAINER_IMAGE=test
docker build -f Dockerfile -t $CONTAINER_IMAGE .
```
```sh
CONTAINER=test
docker stop $CONTAINER
docker container rm $CONTAINER
docker run --link mongodb -it -e DATABASE_URL=mongodb://mongodb:27017/db --name $CONTAINER $CONTAINER_IMAGE
```

```text
Connected to the MongoDB database via connection string mongodb://admin:admin@mo
ngodb:27017 MongoClient(host=['mongodb:27017'], document_class=dict, tz_aware=Fa
lse, connect=True) Database(MongoClient(host=['mongodb:27017'], document_class=d
ict, tz_aware=False, connect=True), 'db')
Test:{'name': 'db', 'type': 'collection', 'options': {}, 'info': {'readOnly': Fa
lse, 'uuid': Binary(b'R\xe6\xc9\nk\x17C\x1a\xa5\xa17\xde\x04\xde\xd8\x7f', 4)},
'idIndex': {'v': 2, 'key': {'_id': 1}, 'name': '_id_'}}
MongoDB database connection were closed.

```
### Docker Compose Failure
```sh
docker-compose up --build
```
observe the restarting
```sh
docker container ls
```

```text
CONTAINER ID        IMAGE                         COMMAND                  CREATED             STATUS              PORTS                              NAMES
d8d27351687b        basic-fastapi-mongo_fastapi   "python /app/app.py "   2 minutes ago       Up 5 seconds        0.0.0.0:8000->8000/tcp, 8080/tcp   fastapi
43ce944caad6        mongo:4.4.6-bionic            "docker-entrypoint.s"   2 minutes ago       Up 5 seconds        0.0.0.0:27017->27017/tcp           mongodb

```
```text
CONTAINER ID        IMAGE                         COMMAND                  CREAT
ED             STATUS                         PORTS               NAMES
d8d27351687b        basic-fastapi-mongo_fastapi   "python /app/app.py "   2 minutes ago       Restarting (0) 1 second ago                        fastapi
43ce944caad6        mongo:4.4.6-bionic            "docker-entrypoint.s"   2 minutes ago       Restarting (14) 1 second ago                       mongodb
```
```sh
docker-compose exec fastapi sh
```
```text
Error response from daemon: Container 90c06e816bf7dd432208f766b252e01b9db2d59087a84491c08c468f3dc27e9f is restarting, wait until the container is running
```
```text
mongodb    | 2023-05-04T13:08:07.989+0000 I CONTROL  [initandlisten] ** WARNING: Access control is not enabled for the database.
```
### TODO

   https://www.geeksforgeeks.org/python-mongodb-insert_one-query/

### See Also
https://www.mongodb.com/basics/create-database
