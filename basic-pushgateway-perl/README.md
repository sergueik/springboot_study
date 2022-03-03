### Info

Plain Alpine 3.9 container installing Perl and installing a pure Perl module replica of [Net::Prometheus::Pushgateway](https://metacpan.org/release/VRAG/Net-Prometheus-Pushgateway-0.03/view/lib/Net/Prometheus/Pushgateway.pm) module with [HTTP::Request](https://metacpan.org/pod/HTTP::Request) replaced with [HTTP::Tiny](https://metacpan.org/pod/HTTP::Tiny) which is part of core Perl and is pure Perl and [LWP::UserAgent](https://metacpan.org/pod/LWP::UserAgent) removed to avoid introducing compiled cpan module dependencies

### Usage

* build the images

```sh
PUSHGATEWAY=pushgateway
docker build --build-arg VERSION=1.4.2 -t $PUSHGATEWAY -f Dockerfile.$PUSHGATEWAY .
```
```sh
docker run --name $PUSHGATEWAY -p 9091:9091 -d $PUSHGATEWAY
```
verify the applicartion launches:
```sh
docker logs $PUSHGATEWAY
```
```text
{"caller":"level.go:63","level":"info","msg":"starting pushgateway","ts":"2022-03-03T02:37:41.715Z","version":"(version=1.4.2, branch=HEAD, revision=99981d7be923ab18d45873e9eaa3d2c77477b1ef)"}

```
testing best done in separate console:
```sh
sudo rm -f .ash_history
```
```sh
BASIC_PERL=basic-perl
docker build -t $BASIC_PERL -f Dockerfile.$BASIC_PERL .
```
* start run default command

```sh
docker container rm $BASIC_PERL
```
```sh
docker run -it --link $PUSHGATEWAY --name $BASIC_PERL -v $(pwd):/root $BASIC_PERL sh
```
* connect to container  and check verion of Perl
```sh
docker exec -it $BASIC_PERL sh
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
The installed Perl Modules are in 
`/usr/share/perl5/core_perl` and `/usr/lib/perl5/core_perl`

__NOTE:__ The `Getopt::Long` is also available but for some reason does not print its `$VERSION`

```sh
perl -MData::Dumper -e 'print $Data::Dumper::VERSION'
```
```text
2.167
```

* verify installed Pure Perl modules to be available

```sh
perl -I. -MYAML::Tiny -e 'print $YAML::Tiny::VERSION;'
```
```text
1.73
```
```sh
perl -I. -MJSON::PP -e 'print $JSON::PP::VERSION;'
```
```text
4.06
```
```sh
perl -I. -MXML::TreePP -e 'print $XML::TreePP::VERSION;'
```
```text

0.43
```

* post sample data to pushgteway:

```sh
perl -I . test.pl
```
this will print to console:
```text
# TYPE test untyped
test{} 42
```
* confirm checking the pushgateway logs that the request was processed successfuly:

```sh
docker container logs $PUSHGATEWAY
```
```text
{"caller":"level.go:63","file":"/pushgateway/history.log","level":"info","msg":"metrics persisted","ts":"2022-03-03T03:17:37.621Z"}


```
* confirm that the metric was recorded (run the command from the host):

```sh
curl http://localhost:9091/metrics
```
(along with other metrics)
```text
# TYPE pushgateway_http_requests_total counter
pushgateway_http_requests_total{code="200",handler="push",method="post"} 2
```
and the actual metric
```text
# TYPE test untyped
test{instance="4db5808908fe(172.17.0.3)",job="my_custom_metrics",team="test"} 42
```
### Note

the `/api/ui/metric`
```perl
my $opt =  {
  '-host' => 'pushgateway',
  '-port' => 9091,
  '-path' => '/api/ui/metrics'
  # 404 
  };
my $o = Pushgateway::Tiny->new(%$opt ); 

```
calls currently do not work and return a __404__
### Note
Alternatively can install a cpan capable Docker Alpine Perl [scottw/alpine-perl](https://hub.docker.com/r/scottw/alpine-perl/dockerfile) base image - at a cost of slight size increase


```sh
REPOSITORY           TAG       IMAGE ID       CREATED         SIZE
scottw/alpine-perl   latest    7e64285ecabb   14 months ago   291MB
```
Alternatively (not tested) is [melopt/alpine-perl-devel](https://hub.docker.com/r/melopt/alpine-perl-devel)

### Note

For a 32 bit Windows one can download __ActivePerl__ without registration from e.g. [Programming Software Download Center (PSDC)](https://www.bookofnetwork.com/2598/psdc/Download-ActivePerl-5-26-3-software-for-windows-pc) - the __ActiveState__ now charges for it.


It comes with Perl Package Manager and `Net::HTTP` and `Data::Dumper` among other packages.

This can be a good starting point for Powershell / .Net version of the same.


### See Also
  * Python [example](https://www.devopsschool.com/blog/prometheus-pushgateway-installation-configuration-and-using-tutorials/)
   
NOTE: the [cpanminus](https://github.com/miyagawa/cpanminus) repo directory layout has changes and single binary release that is needed for `https://hub.docker.com/r/scottw/alpine-perl/dockerfile` is not available

  * https://stackoverflow.com/questions/35586898/is-there-any-way-to-fetch-web-pages-in-pure-perl?rq=1

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
