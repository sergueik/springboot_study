### Info

this directory containes practice projects for [connecting to Cassandra from Java](https://dzone.com/articles/connecting-cassandra-java)


based on https://stackoverflow.com/questions/54144291/cassandra-alpine-image-release-plan
 and https://github.com/smapira/alpine-cassandra/blob/master/Dockerfile

### Usage

* check the correct version of cassandra
```sh
wget -O - -q http://ftp.riken.jp/net/apache/cassandra/ | tee /tmp/a.html
cat /tmp/a.html| sed -n 's/^.*a href="\([0-9\].*\)\/">.*<\/a>.*$/\1/p'
```
this will print
```text
2.1.22
2.2.19
3.0.25
3.11.11
4.0.1
```
update 
```text
ENV CASSANDRA_VERSION 3.11.11
```
in the `Dockerfile.cassandra`.

* build
```sh
docker build -t cassandra -f Dockerfile.cassandra .
```

ports:

   --- | ---
   `7000` | intra-node communication
   `7001` | TLS intra-node communication
   `7199` | JMX
   `9042` | CQL
   `9160` | thrift service

```sh
docker run --name cassandra -p 9042:9042 -d cassandra
```



running `cqlsh` from `cslsh` container is currently failing:
```sh
docker build -t cqlsh -f Dockerfile.cqlsh .
```

```sh
docker run --link cassandra -it cqlsh sh
```

```sh
# cqlsh
```
```text
Connection error: ('Unable to connect to any servers', {'172.17.0.2:9042': ConnectionRefusedError(111, "Tried connecting to [('172.17.0.2', 9042)]. Last error: Connection refused")})
```

can run on `cassandra` container itself:

```sh
docker exec -it cassandra sh
```

```sh
cqlsh
Connected to Test Cluster at 127.0.0.1:9042.
[cqlsh 5.0.1 | Cassandra 3.11.11 | CQL spec 3.4.4 | Native protocol v4]
Use HELP for help.
cqlsh> 
```
* run java app test(unstable)
```sh
mvn clean test
```
run java app
```
mvn -Dmaven.test.skip=true clean 
```
this is currently failing
```text
```
```
 netstat -ant  |grep 9042
```

this is caused by locking the cassandra.yaml:
```yaml

# Setting listen_address to 0.0.0.0 is always wrong.                            
#                                                                               
listen_address: localhost                                                                                  
```
```sh
docker cp cassandra:/opt/cassandra/conf/cassandra.yaml .
```
setting it temporarily to "wrong" value
### See Also
   * [java developer cassandra guide](https://www.baeldung.com/cassandra-with-java)
   * [source code](https://github.com/eugenp/tutorials/tree/master/persistence-modules/java-cassandra)
   * [stubbed Cassandra](https://github.com/scassandra/scassandra-server)
   * [cassandra for java getting started](https://academy.datastax.com/resources/getting-started-apache-cassandra-and-java-part-i)
  * https://github.com/jsevellec/cassandra-unit/issues/225
