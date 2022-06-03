### Info

this directory contains a minimally modified `InfluxDB::Client::Simple` Perl [module](https://metacpan.org/dist/InfluxDB-Client-Simple/source/lib/InfluxDB/Client/Simple.pm)

tested interacting with an old InfluxDB __1.x__ [hosted on alpine](https://hub.docker.com/r/woahbase/alpine-influxdb/)

### Testing
* pull the image
```sh
docker pull woahbase/alpine-influxdb:x86_64
```
* run in the default configuration
```sh
docker run -d -p 8086:8086 woahbase/alpine-influxdb:x86_64
```
connect to shell in the container
```sh
IMAGE=$(docker ps | grep 'woahbase/alpine-influxdb'| awk '{print $1}')
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
disconnect from container, run Perl script from the host
to post data
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
### NOTE
alternatively pull a vendor image
information in https://portal.influxdata.com/downloads

```sh

docker pull influxdb:2.2.0
```
NOTE: the image is a little heavy - over 300 MB:
```sh
docker image ls |  grep influx
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
  * `LineProtocol`[documentation](https://docs.influxdata.com/influxdb/v1.8/write_protocols/line_protocol_tutorial/)
  * Prometheus endpoint provided by influxdb [documentation](https://docs.influxdata.com/influxdb/v1.8/supported_protocols/prometheus/)
   * https://github.com/JonasProgrammer/docker-influxdb
   * https://www.influxdata.com/the-best-way-to-store-collect-analyze-time-series-data/



### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
