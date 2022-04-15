### Info

Pure Perl module based on [Net::Prometheus::Pushgateway](https://metacpan.org/release/VRAG/Net-Prometheus-Pushgateway-0.03/view/lib/Net/Prometheus/Pushgateway.pm) module
with [HTTP::Request](https://metacpan.org/pod/HTTP::Request)
replaced with [HTTP::Tiny](https://metacpan.org/pod/HTTP::Tiny) which is part of core Perl and is also a pure Perl
and with [LWP::UserAgent](https://metacpan.org/pod/LWP::UserAgent) removed
to avoid introducing compiled cpan module dependencies to use in Docker
Plain Alpine 3.9 container with installed Perl but no compilers

the fixes back ported to

### Usage

#### Centos
* build the pushgateway target image

```sh
IMAGE1=pushgateway
VERSION=1.4.2
docker build --build-arg VERSION=$VERSION -t $IMAGE1 -f Dockerfile.$IMAGE1 .
```

```sh
NAME1=pushgateway
docker container rm $NAME1
docker run --name $NAME1 -p 9091:9091 -d $IMAGE1
```

use the stock Centos Perl __5.2.4__ container image (NOTE: image size is over 600 Mb)
```sh
docker pull centos/perl-524-centos7
```
* run Centos interactively, as root
```
IMAGE2=centos/perl-524-centos7
NAME2=perl-centos
docker run --link $NAME1 --name $NAME2 -v $(pwd):/tmp -u root -w /tmp -it $IMAGE2 sh
```

* verify the copy-installed Pure Perl module to be available

```sh
perl -I. -MPushgateway::Tiny -e 'print $Pushgateway::Tiny::VERSION;'
```
```text
0.0.2
```

* install dependencies interactively
```sh
cpan -i LWP::UserAgent
```

```text
Would you like to configure as much as possible automatically? [yes] yes
```
```text
What approach do you want?  (Choose 'local::lib', 'sudo' or 'manual')
 [local::lib]
```
- you will need to run the cpan command 2 times
```text
...
/usr/bin/make install  -- OK
```
* verify the CPAN Perl module to be healthy

```sh
perl -I. -MNet::Prometheus::Pushgateway -e 'print $Net::Prometheus::Pushgateway::VERSION;'
```
```text
0.0.4
```

```sh
perl -I . test-centos.pl -value 25
```

verify by checking the metrics:
```sh
curl -s http://localhost:9091/metrics | grep perl_counter
```
```text
# TYPE perl_counter counter
perl_counter{instance="069a9676946f(172.17.0.3)",job="my_custom_metrics",perl_label="custom label",team="test"} 25
```
* rename the container to keep
```sh
OLD_NAME=$(docker container ls -a | grep $IMAGE2 | awk '{print $1}')
NAME2=perl-centos
docker container rename $OLD_NAME $NAME2
```

if the container already exist, simply start it
```sh
docker container start $NAME2
docker exec -it $NAME2 sh
```
alternatively
```sh
IMAGE3=centos-perl
docker build -t $IMAGE3 -f Dockerfile.$IMAGE3 .
```
one may encouter
```text
Use of uninitialized value $_[0] in substitution (s///) at /usr/share/perl5/File/Basename.pm line 341.
fileparse(): need a valid pathname at /usr/share/perl5/CPAN/FirstTime.pm line 1354.
```

re-running the docker build command immediately after appears to resolve this issue
the build process turns out to be quite time consuming


#### Alpine

* build the pushgateway target image

```sh
IMAGE1=pushgateway
VERSION=1.4.2
docker build --build-arg VERSION=$VERSION -t $IMAGE1 -f Dockerfile.$IMAGE1 .
```
```sh
NAME1=pushgateway
docker run --name $NAME1 -p 9091:9091 -d $IMAGE1
```
verify the application launches:
```sh
docker logs $IMAGE1
```
```text
{"caller":"level.go:63","level":"info","msg":"starting pushgateway","ts":"2022-03-03T02:37:41.715Z","version":"(version=1.4.2, branch=HEAD, revision=99981d7be923ab18d45873e9eaa3d2c77477b1ef)"}

```
testing is easiers to perform from a separate console:
```sh
sudo rm -f .ash_history
```
build the perl container
```sh
IMAGE2=alpine-perl
docker build -t $IMAGE2 -f Dockerfile.$IMAGE2 .
```
* start run default command

```sh
NAME2=alpine-perl
docker container rm $NAME2
docker run -it --link $NAME1 --name $NAME2 -v $(pwd):/root $IMAGE2 sh
```
* in the container check the verion of Perl
```sh
~ #
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
0.070
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

* verify the copy-installed Pure Perl module to be available

```sh
perl -I. -MPushgateway::Tiny -e 'print $Pushgateway::Tiny::VERSION;'
```
```text
0.0.2
```

* post sample data to pushgteway:

```sh
perl -I . test.pl -value 5
```
this will print to console:
```text
# TYPE test untyped
test{} 42
```
* confirm checking the pushgateway logs that the request was processed successfuly:

```sh
docker container logs $IMAGE1
```
```text
{"caller":"level.go:63","file":"/pushgateway/history.log","level":"info","msg":"metrics persisted","ts":"2022-03-03T03:17:37.621Z"}


```
* confirm that the metric was recorded (run the command from the host):

```sh
curl http://localhost:9091/metrics | grep perl_counter
```
- focusing on custom mettic just added - there is a big number of other metrics
```text
100  8455    0# TYPE perl_counter counter
 perl_counter{instance="f9fab499033c(172.17.0.4)",job="my_custom_metrics",perl_label="custom label",team="test"} 5

```

also one can use the [prom2json](https://hub.docker.com/r/prom/prom2json):
```sh
VERSION=v1.3.0
docker pull prom/prom2json:$VERSION
```
* and dump the metrics
```sh
NAME1=pushgateway
VERSION=v1.3.0
docker run --link $NAME1 prom/prom2json:$VERSION http://$NAME1:9091/metrics | jq -cr '.[].name|select(.| contains("perli_counter"))'
```
```text
perl_counter
```

```sh
NAME1=pushgateway
VERSION=v1.3.0
docker run --link $NAME1 prom/prom2json:$VERSION http://$NAME1:9091/metrics | jq -r '.|.[]|select(.name| contains("perl_counter"))'
```
```json
{
  "name": "perl_counter",
  "help": "",
  "type": "COUNTER",
  "metrics": [
    {
      "labels": {
        "instance": "f9fab499033c(172.17.0.4)",
        "job": "my_custom_metrics",
        "perl_label": "custom label",
        "team": "test"
      },
      "value": "5"
    }
  ]
}
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
REPOSITORY           TAG       IMAGE1 ID       CREATED         SIZE
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
  * [python gauge exaple](https://gist.github.com/codersquid/17f61049c1a817f26da250a4bd2df16d)
  * silent Perl module install how to https://www.google.com/search?q=perl%20install%20cpan%20module%20silently
  * Prometheus text-based exposition format [documentation](https://prometheus.io/docs/instrumenting/exposition_formats/) for providing text-based file to node_exporter

### Note

The original module author(s) is [Pavel Andryushin](vrag867@gmail.com)

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)

