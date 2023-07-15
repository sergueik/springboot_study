### Usage
this directory contains Docker container with installed [Mango](https://metacpan.org/pod/Mango) and [MongoDB](https://metacpan.org/dist/MongoDB) Perl Drivers and test script to run for benchmarking.
NOTE: the "Pure Perl" `Mango` module states to be is not correct strictly speaking - has a dependency on compiled module `Mojolicious` and the attempt to install it via cpan in the container without build tools:
```sh
cpan -T -i 'Mango'
```
ends up with error
```text
Warning: Prerequisite 'Mojolicious => 5.40' for 'ODC/Mango-1.30.tar.gz' failed when processing 'SRI/Mojolicious-9.33.tar.gz' with 'make => NO'. Continuing, but chances to succeed are limited
```
### Note on CPAN Command

NOTE: when installing interactively, e.g.
```sh
docker run --link mongodb --name basic-perl-mongo -it basic-perl-crypt-jasypt  sh
```
Make sure to skip the tests: 
```sh
cpan -T -i Mango
```

```sh
cpan -T -i MongoDB
```
without the `-T` option testing of every module by cpan will take 20...30 minutes for `MongoDB` to install 
and 5 min for `Mango` on a fast developer machine and much longer on a slow developer machine

### Usage

* use Ubuntu Mongo image (alternatively use alpine based)
```sh
IMAGE=mongo:4.4.6-bionic
MONGO_SERVER=mongo_server
docker pull $IMAGE
docker container rm $MONGO_SERVER
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
* make sure the scripts are put into the container (seen 
```text
Step 7/10 : COPY test_mongodb.pl test_mango.pl ./
 ---> Using cache
 ---> 7299eed13f36
``` 
occasionally)

```sh
docker cp test_mango.pl $CONTAINER_NAME:/app
docker cp test_mongodb.pl $CONTAINER_NAME:/app
```

* connect into the conainer to run Perl script


```sh
docker exec -it $CONTAINER_NAME sh
```
```sh
perl -I . test_mango.pl
```
will print
```text
Connect
Insert 10000 documents
Object Id: 64b2f65150a7fc0025cf9241
Object Id: 64b2f65150a7fc0025d09241
Object Id: 64b2f65150a7fc0025d19241
Object Id: 64b2f65150a7fc0025d29241
...
Find document
$VAR1 = {
          '_id' => bless( {
                            'oid' => 'd^ X'
                          }, 'Mango::BSON::ObjectID' ),
          'tags' => [
                      'cotton'
                    ],
          'qty' => 100,
          'size' => {
                      'h' => 28,
                      'w' => '35.5',
                      'uom' => 'cm'
                    },
          'item' => 'bar9999'
        };
```
and benchmark
```text
real    0m55.005s
user    0m0.000s
sys     0m0.031s```
and
```
```sh
time perl test_mongodb.pl
```
```text
Connect
Insert 10000 documents
Object Id: 64b2f58c7ceb526b30b80418
Object Id: 64b2f58c7ceb526b30b80419
Object Id: 64b2f58c7ceb526b30b8041a
Object Id: 64b2f58c7ceb526b30b8041b
...
Find document
$VAR1 = {
          'tags' => [
                      'cotton'
                    ],
          'item' => 'bar9999',
          'size' => {
                      'h' => 28,
                      'w' => '35.5',
                      'uom' => 'cm'
                    },
          'qty' => 100,
          '_id' => bless( {
                            'oid' => 'd^⌂ X'
                          }, 'BSON::OID' )
        };

```
and benchmark:
```text
real    0m48.688s
user    0m0.000s
sys     0m0.047s

```

in silent mode (without printing the `oid`) the benchmarks become approximately the same:

```text
real    0m31.267s
user    0m0.015s
sys     0m0.015s
```


connect to mongodb container to verify the data was inserted, in the mongodb shell:


```sh
docker exec -it $MONGO_SERVER mongo --host localhost --port 27017
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
db.foo.count()
```
the result will vary  with how many tests have been run
```text
54019
```
show data using projection,sort and formatting mongo shell operations
```sh
db.foo.find({}, {_id:0}).sort({item:-1}).limit(1).pretty()
```
```json
{
        "item" : "bar9999",
        "tags" : [
                "cotton"
        ],
        "qty" : 100,
        "size" : {
                "h" : 28,
                "w" : 35.5,
                "uom" : "cm"
        }
}
```

```sh
db.foo.find({}, {_id:0,tags:1,item:1,"size.h":1}).sort({item:-1}).limit(1).pretty()
```
```json
{ "tags" : [ "cotton" ], "size" : { "h" : 28 }, "item" : "bar9999" }
```
* TODO: use `mongoimport` to load data to determine the `datatxt` document shape
* use `{timestamp: {$gte, ISODate(2020-02-20T10:15:00Z}}` for date filtering

### See Also

  * [note](https://www.mongodb.com/blog/post/the-mongodb-perl-driver-is-being-deprecated) on MongoDB Perl Driver deprecated in 2020.
  * https://perlmaven.com/mongodb-insert-and-delete-with-perl

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
