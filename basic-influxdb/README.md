### Info

this directory contains a minimally modified `InfluxDB::Client::Simple` Perl [module](https://metacpan.org/dist/InfluxDB-Client-Simple/source/lib/InfluxDB/Client/Simple.pm)

tested interacting with an InfluxDB __1.8__ [hosted on alpine](https://hub.docker.com/r/woahbase/alpine-influxdb/). Note, this build does not have web interface:

```sh
http://192.168.0.29:8086/
```

is
```text
404 page not found
```

### Testing
#### Run InfluxDB Server
* pull the image
```sh
docker pull woahbase/alpine-influxdb:x86_64
```
* run in the default configuration
```sh
docker run -d -p 8086:8086 woahbase/alpine-influxdb:x86_64
```
```sh
IMAGE=$(docker ps | grep 'woahbase/alpine-influxdb'| awk '{print $1}')
```
#### Create Schema
connect to shell in the container
docker exec -it $IMAGE sh
```
in the container use `influx` to create database:

```sh
/ # influx
```
```text
Connected to http://localhost:8086 version 1.8.3
InfluxDB shell version: 1.8.3
```

```SQL
> CREATE DATABASE example
>
> SHOW DATABASES
```
```text
name: databases
name
----
_internal
example
>
```
#### Test Client
disconnect from container, run Perl script from the host to post data
```sh
perl -I . test.pl
```
repeat a few times.

Note: currently there is a number of dependencie we intend to get eventually rid of, since we are likely not going to need UDP and will be able to use as pureperl module on `alpine-perl` image containers

run shell in the container again
```sh
docker exec -it $IMAGE sh
```
```sh
influx
```
switch to the database created earlier
```SQL
> show databases
```
```text
name: databases
name
----
_internal
example
```
```SQL
> use example
```
```text
Using database example
```
```SQL
> SELECT host,statement from testing
```
```text
name: testing
time                host        statement
----                ----        ---------
1654295907173950198 containment 42
1654296416171853241 containment 42
```

alternatively pull vendor __1.x__ image
```sh
docker pull influxdb:1.7-alpine
```
```sh
docker run -d -p 8086:8086 influxdb:1.7-alpine
```
update environment to connect to shell in the container
```sh
IMAGE=$(docker ps | grep 'influxdb:1.7-alpine'| awk '{print $1}')
```
### Cleanup
```sh
docker container stop $IMAGE
docker container rm $IMAGE
```

### Testing InfluxDB 2.x

pull a vendor image
```sh
docker pull influxdb:2.2.0-alpine
```

```sh
docker run -d -p 8086:8086 influxdb:2.2.0-alpine
```


With __2.x__ need to start in web interface `http://192.168.0.29:8086/`:

![setup Page](https://github.com/sergueik/springboot_study/blob/master/basic-influxdb/screenshots/capture-initial-setup.png)
```
in the container use `influx` to create database:

```sh
/ # influx
```
```text
Connected to http://localhost:8086 version 1.8.3
InfluxDB shell version: 1.8.3
```

```SQL
> CREATE DATABASE example
>
> SHOW DATABASES
```
```text
name: databases
name
----
_internal
example
>
```
disconnect from container, run Perl script from the host
to post data
```sh
perl -I . test.pl
```


NOTE: the default vendor image is based
```sh
docker pull influxdb:2.2.0
```
this makes the image a little heavy - over 300 MB:
```sh
docker image ls | grep influx
```
```text
influxdb                         2.2.0                   5b24dbcedefc   5 days ago       341MB
```
```sh
docker run -d -p 8086:8086 influxdb:2.2.0
```
also it appears to require authentication

### See Also

   * introductory [documentation](https://docs.influxdata.com/influxdb/v1.8/introduction/get-started/https://docs.influxdata.com/influxdb/v1.8/introduction/get-started/)
   * influxdb query language [documentation](https://docs.influxdata.com/influxdb/v1.7/query_language/)
   * advanced  InfluxDB client [module](https://metacpan.org/pod/InfluxDB) on CPAN
   * `LineProtocol` [documentation](https://docs.influxdata.com/influxdb/v1.8/write_protocols/line_protocol_tutorial/)
   * Prometheus endpoint provided by influxdb [documentation](https://docs.influxdata.com/influxdb/v1.8/supported_protocols/prometheus/)
   * https://github.com/JonasProgrammer/docker-influxdb
   * https://www.influxdata.com/the-best-way-to-store-collect-analyze-time-series-data/
   * https://github.com/ind9/influxdb-java - probably 2.x
   * https://github.com/influxdata/influxdb-java - official, too big
   * https://devconnected.com/how-to-create-a-database-on-influxdb-1-7-2-0/ - there apparently is a v2 / v1.x compatibility concern
documented for [backward](https://docs.influxdata.com/influxdb/v1.8/tools/api/) and for [forward](https://docs.influxdata.com/influxdb/v2.0/reference/api/influxdb-1x/)


### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
