### Info

Plain Alpine 3.9 container installing Perl and installing few pure Perl modules (YAML, XML and JSON) and a replica of [Net::Prometheus::Pushgateway](https://metacpan.org/release/VRAG/Net-Prometheus-Pushgateway-0.03) module with [HTTP::Request](https://metacpan.org/pod/HTTP::Request) replaced with [HTTP::Tiny](https://metacpan.org/pod/HTTP::Tiny) which is part of core Perl and is pure Perl

### Usage

* build the images

```sh
IMAGE1=pushgateway
docker build --build-arg VERSION=1.4.2 -t $IMAGE1 -f Dockerfile.$IMAGE1 .
```
```sh
docker run --name $IMAGE1 -p 9091:9091 -d $IMAGE1
```
best done in separate console:
```sh
IMAGE2=basic-perl
docker build -t $IMAGE2 -f Dockerfile.$IMAGE2 .
```
* start run default command

```sh
docker run -it --link $IMAGE1 --name $IMAGE2 $IMAGE2 sh
```
* connect to container  and check verion of Perl
```sh
docker exec -it $NAME sh
```
```text
/ #
```
```sh
perl -v
```
```text
This is perl 5, version 26, subversion 3 (v5.26.3) built for x86_64-linux-thread-multi
```
* verify the custom module to be healthy:
```sh
 perl -MHTTP::Tiny -e 'print $HTTP::Tiny::VERSION'
```
```text
0.070/
```
* post sample data to pushgteway:

```sh
perl -I . test.pl
```
* confrm that the metric was recorded (from the host):
```sh
curl http://localhost:9091/metrics
```
(among other metrics shown)
```text
# TYPE pushgateway_http_requests_total counter
pushgateway_http_requests_total{code="200",handler="push",method="post"} 2
```
and the actual mertic
```text
# TYPE test untyped
test{instance="4db5808908fe(172.17.0.3)",job="my_custom_metrics",team="test"} 42
```

### Note
Alternatively can install a cpan capable Docker Alpine Perl [scottw/alpine-perl](https://hub.docker.com/r/scottw/alpine-perl/dockerfile) base image - at a cost of slight size increase


```sh
REPOSITORY           TAG       IMAGE ID       CREATED         SIZE
scottw/alpine-perl   latest    7e64285ecabb   14 months ago   291MB
```
Alternatively (not tested) is [melopt/alpine-perl-devel](https://hub.docker.com/r/melopt/alpine-perl-devel)


### See Also
  * Python [example](https://www.devopsschool.com/blog/prometheus-pushgateway-installation-configuration-and-using-tutorials/)
   
NOTE: the [cpanminus](https://github.com/miyagawa/cpanminus) repo directory layout has changes and single binary release that is needed for `https://hub.docker.com/r/scottw/alpine-perl/dockerfile` is not available

  * https://stackoverflow.com/questions/35586898/is-there-any-way-to-fetch-web-pages-in-pure-perl?rq=1

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
