### Usage
this directory contains a Perl [Mango](https://metacpan.org/pod/Mango) test.
NOTE: the "Pure Perl" `Mango` module states to be is not correct strictly speaking - has a dependency on compiled module `Mojolicious`

in container with no build tools:
```sh
cpan -T -i Mango
```
ends up with
```text
Warning: Prerequisite 'Mojolicious => 5.40' for 'ODC/Mango-1.30.tar.gz' failed when processing 'SRI/Mojolicious-9.33.tar.gz' with 'make => NO'. Continuing, but chances to succeed are limited
```
### Note on CPAN Command

when installing interactively, e.g.
```sh
docker run --link mongodb --name basic-perl-mongo -it basic-perl-crypt-jasypt  sh
```
```sh
cpan -T -i Mango
```

```sh
cpan -T -i MongoDB
```
NOTE: Make sure to skip tests: without the `-T` option testing of every module by cpan will take 20...30 minutes for `MongoDB` to install 
and 5 min for `Mango` on a fast machine

### Usage

* use Ubuntu Mongo image (alternatively use alpine based)
```sh
IMAGE=mongo:4.4.6-bionic
MONGO_SERVER=mongo_server
docker pull $IMAGE
docker container rm $CONTAINER
docker run -d -e MONGO_INITDB_DATABASE=db --name $MONGO_SERVER $IMAGE
```
* create the Perl client container
```sh
CONTAINER_IMAGE=mongo_client_perl
docker build -f Dockerfile -t $CONTAINER_IMAGE .
```
run it in background
```sh
CONTAINER_NAME=mongo_client_perl
docker container rm -f $CONTAINER_NAME
MONGO_SERVER=mongo_server
docker run --link $MONGO_SERVER -d -e DATABASE_URL=mongodb://$MONGO_SERVER:27017/db --name $CONTAINER_NAME $CONTAINER_IMAGE
```


* connect into the conainer to run Perl script


```sh
docker exec -it $CONTAINER_NAME sh
```
```sh
perl -I . test.pl
```
will print
```text
Connect
Insert document
Object Id: 64b1c58054f5860048211b7d
Update document
Find document
JSON:PP
{
   "_id" : null,
   "bar" : "yada"
}
Data::Dumper
$VAR1 = {
          '_id' => bless( {
                            'oid' => 'd��+T��̚�'
                          }, 'Mango::BSON::ObjectID' ),
          'bar' => 'yada'
        };

```
connect to mongodb container to verify the data was inserted:

```sh
docker exec -it $MONGO_SERVER mongo
```
```
show databases;
```
```text
admin   0.000GB
config  0.000GB
local   0.000GB
test    0.000GB
```
```sh
use test;
```
```text
switched to db test
```
```sh
show collections;
```
```text
foo
```
```sh
db.foo.find();
```
```text
{ "_id" : ObjectId("64b1c22b54f5860018cc9add"), "bar" : "yada" }
{ "_id" : ObjectId("64b1c28054f586001b479a56"), "bar" : "yada" }
{ "_id" : ObjectId("64b1c2ae54f586002267c92b"), "bar" : "yada" }
{ "_id" : ObjectId("64b1c2f954f5860029f1fb99"), "bar" : "yada" }
```
- will observe multiple entries if run the `test.pl` more than once
### See Also

  * [note](https://www.mongodb.com/blog/post/the-mongodb-perl-driver-is-being-deprecated) on MongoDB Perl Driver deprecated in 2020.
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
