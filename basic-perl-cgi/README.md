f### Info

Plain Alpine 3.9 container installing the apache and Perl using some code from [Alpine microcontainer with Apache2, perl5 and FCGI.pm](https://github.com/kjetillll/docker-alpine-apache-perl-fcgi) - and launching httpd without using init.d of [nimmis/docker-alpine-micro](https://github.com/nimmis/docker-alpine-micro) and installing few pure Perl modules (YAML, XML and JSON) and Angular and Bootstrap to explore Angular CGI-BIN pages

### Testing

* build the image
```sh
IMAGE=basic-perl-apache
docker build -t $IMAGE -f Dockerfile . --progress=plain
```
* start run default command
```sh
NAME=basic-perl-cgi
ID=$(docker container ls -a | grep $NAME|cut -f 1 -d ' ')
docker start $ID
```
```sh
docker ps

```
```text
CONTAINER ID        IMAGE               COMMAND                  CREATED     STATUS              PORTS                                         NAMES
c2312b18f192        basic-perl-apache   "sh -c 'PIDFILE='/ru"   2 weeks ago    Up 2 minutes        0.0.0.0:9090->80/tcp, 0.0.0.0:9443->443/tcp   basic-perl-cgi
```

```
* run new instance of default command

```sh
NAME=basic-perl-cgi
docker container rm $NAME
docker run -d -p 9090:80 -p 9443:443 --name $NAME $IMAGE
docker logs -f $NAME
```
alternatively
```
docker run -d -p $(hostname -i):9090:80 -p $(hostname -i):9443:443 --name $NAME $IMAGE
```
or if integrated with InfluxDB then
```sh
export INFLUXDB=boring_williams
docker run -d -p 9090:80 -p 9443:443 --link $INFLUXDB --name $NAME $IMAGE
```

this will respond with
```sh
wait for apache pid
apache is running with ID 7
```
the value of `ID` varies. Sometimes the script output is not shown immediately, re-running the `logs` command shows it

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

exit the container
* verify the vanilla httpd to run inside Docker
```sh
curl http://$(hostname -i):9090/
```
if [Docker Toolbox](https://github.com/docker-archive/toolbox) is used from Windows host, find the ip address of the irtual Box container via the command:
```sh
DOCKER_IP=$(docker-machine ip)
echo $DOCKER_IP
```
```sh
curl http://$DOCKER_IP:9090/
```
it will print the default apache welcome page
```html
<html><body><h1>It works!</h1></body></html>
```

* NOTE: the `$(hostname -i):` argument added as workaround of forced ipv6 switch
```sh
Error starting userland proxy: listen tcp6 [::]:8086:
socket: address family not supported by protocol
```
observed in Docker version __20.10.6__ on a host where ipv6 was [turned off](https://linuxconfig.org/how-to-disable-ipv6-address-on-ubuntu-18-04-bionic-beaver-linux)

if an error is reported

```HTML
<!DOCTYPE HTML PUBLIC "-//IETF//DTD HTML 2.0//EN">
<html><head>
<title>500 Internal Server Error</title>
</head><body>
<h1>Internal Server Error</h1>
<p>The server encountered an internal error or
misconfiguration and was unable to complete
your request.</p>
<address>Apache/2.4.46 (Unix) Server at 192.168.99.100 Port 9090</address>
</body></html>
```
see the apache error log:
```sh
docker exec -it $NAME tail /var/log/apache2/error.log
```
* run smoke test
call cgi inside container directly:
```sh
docker exec $NAME perl -d /var/www/localhost/cgi-bin/list.cgi
```
this will print
```text
/var/www/localhost/cgi-bin/list.cgi syntax OK
```
```sh
docker exec $NAME /var/www/localhost/cgi-bin/list.cgi
```
replies with
```json
Content-Type: application/json

{
   "results" : [
      {
         "text" : "Lorem ipsum dolor sit amet"
      },
      {
         "text" : "consectetur adipiscing elit"
      },
      {
         "text" : "sed do eiusmod tempor incididunt"
      },
      {
         "text" : "ut labore et dolore magna aliqua"
      }
   ]

```
and
```sh
docker exec $NAME /var/www/localhost/cgi-bin/table.cgi
```
responds with
```json
Content-Type: application/json

{
   "results" : [
      {
         "column1" : "row 1 column 1",
         "column3" : "row 1 column 3",
         "column2" : "row 1 column 2"
      },
      {
         "column1" : "row 2 column 1",
         "column3" : "row 2 column 3",
         "column2" : "row 2 column 2"
      },
      {
         "column2" : "row 3 column 2",
         "column3" : "row 3 column 3",
         "column1" : "row 3 column 1"
      }
   ]
}
```
and
```sh
docker exec $NAME /var/www/localhost/cgi-bin/select.cgi
```
```txt
Content-Type: application/json

[
   "apple",
   "pear",
   "orange",
   "plum"
]
```
### Config CGI Test

```sh
curl http://$(hostname -i):9090/cgi-bin/config.cgi
```
```JSON
{
   "result" : {
      "red" : "green"
   },
   "status" : "OK"
}

```

NOTE that currently the query params are not fullly implemented. The `config.cgi` compares the file modification time of `example_config.cgi` to
`Mon Aug 14 23:42:23 CEST 2023`: 
```Perl
my $check_epoch     = 1692049343;
```

change it to `1692089343` to trigger the other branch of he logic:
```Perl
date --date='@1692089343'
```
```text
Tue Aug 15 10:49:03 CEST 2023
```
and rebuild:
```sh
docker stop $NAME
docker rm $NAME
docker build -t $IMAGE -f Dockerfile .
docker run -d -p 9090:80 -p 9443:443 --name $NAME $IMAGE

```

observe this time the `config.cgi` to return an "error":
```sh
curl http://$(hostname -i):9090/cgi-bin/config.cgi
```
```JSON
{
   "status" : "error",
   "result" : "Config /var/www/localhost/cgi-bin/example_config.json is older than Tue Aug 15 08:49:03 2023"
}
```
finally, restoring the `$check_epoch` but breaking the JSON example config
```JSON
{"red": "green",}
```
the same call will return the exception info:

```sh
curl http://$(hostname -i):9090/cgi-bin/config.cgi
```
```JSON
{
   "status" : "error",
   "result" : "Error reading Config /var/www/localhost/cgi-bin/example_config.json: unexpected end of string while parsing JSON string, at character offset 17 (before \"\\n\") at /var/www/localhost/cgi-bin/config.cgi line 82.\n"
}

```

Alternatively pass query parameters normally:
```sh
export QUERY_STRING='newer=1692049343&inputfile=example_config.json'
perl config.cgi
```
```JSON
{
   "result" : {
      "red" : "green"
   },
   "status" : "OK"
}

```
```
export QUERY_STRING='newer=1692089343&inputfile=example_config.json'
perl config.cgi

```
```JSON
{
   "status" : "error",
   "result" : "Config /home/sergueik/src/springboot_study/basic-perl-cgi/cgi-bin/example_config.json is older than Tue Aug 15 10:49:03 2023"
}

```
or through REST call:
```sh
curl 'http://$(hostname -i):9090/cgi-bin/config.cgi?newer=1692089343&inputfile=example_config.json'
```
```JSON
{
   "status" : "error",
   "result" : "Config /var/www/localhost/cgi-bin/example_config.json is older than Tue Aug 15 08:49:03 2023"
}

```
and

```sh
curl 'http://$(hostname -i):9090/cgi-bin/config.cgi?newer=1692049343&inputfile=example_config.json'
```
```JSON
{
   "result" : {
      "red" : "green"
   },
   "status" : "OK"
}

```

#### REST Call Tests


* by calling the web server inside container by invoking requests through exorted port, confirm the cgi-bin run successtully by Apache in the contaiter.
Basically will observe same outputs (sans the header) as cgi-bin call via Docker exec:

```sh
curl -s http://$(hostname -i):9090/cgi-bin/list.cgi
```
```json
{
   "results" : [
      {
         "text" : "Lorem ipsum dolor sit amet"
      },
      {
         "text" : "consectetur adipiscing elit"
      },
      {
         "text" : "sed do eiusmod tempor incididunt"
      },
      {
         "text" : "ut labore et dolore magna aliqua"
      }
   ]
}
```
post the data to `form.cgi`:
```sh
curl -X POST -d 'a=b&c=d' http://$(hostname -i):9090/cgi-bin/form.cgi
```
this will echo data back as a JSON object with keys and values constructed from FORM elements:
```text
Content-Type: application/json

{
   "c" : "d",
   "a" : "b"
}
```
testing the AJAX page in console will display static content:
```sh
curl http://$(hostname -i):9090/inventory.html
```

* this will show the plain page:
```html
<!doctype html>
<html ng-app='Application'>
<head>
<meta charset="UTF-8">
<title>Title</title>
<script type="text/javascript" src="js/angular.js">
</script>
<link rel="stylesheet" href="css/bootstrap.min.css"/>
<link rel="stylesheet" href="css/main.css"/>
<script type="text/javascript" src="js/script.js">
</script>
</head>
<body>
<p>Enter Name: <input type="text" ng-model="name"> </p>
<p>Hello {{name}}</p>

<div ng-controller="ListController">
Server Data: <br />

<div ng-controller="TableController">
<table class="table-bordered" style="width: 800px;">
  <tr data-ng-repeat="row in table_rows">
    <td style="width: 25%">{{ row.column1 }}</td>
    <td style="width: 25%">{{ row.column2 }}</td>
    <td style="width: 50%">{{ row.column3 }}</td>
  </tr>
</table>
  <ul>
    <li ng-repeat="row in list_rows">{{ row.text }}</li>
  </ul>
</div>
<div ng-controller="SelectController">
  <select ng-model="selectedName" ng-options="item for item in names"></select>
</div>
</div>
</body>
</html>
```
one needs to open page in the browser to see the data being pulled from the backend by Angular dynamically. To verify, stop and rerun the container with default ports `80` and `443` published as `9090` and `9443` on all network interfaces:
```sh
docker stop $NAME
docker container rm $NAME
docker run -d -p 9090:80 -p 9443:443 --name $NAME $IMAGE
```
and navigate to the url in the browser with ip address of the hosting node:
```sh
chromium-browser http://$(hostname -i):9090/inventory.html &
```
![Example](https://github.com/sergueik/springboot_study/blob/master/basic-perl-cgi/screenshots/capture.png)

 - inspect the developer tools tab to observe that the Angular indeed polls the server

### Cleanup

```sh
docker stop $NAME
docker container prune -f
docker image prune -f
docker image rm $IMAGE
```
### Running Angular part from filesystem

one can start the page in th browser from file explorer via URI:

```cmd
file:///C:/developer/sergueik/springboot_study/basic-perl-cgi/html/inventory.html
```
on a Windows machine or
```sh
google-chrome file://$(pwd)/html/inventory.html & 
```
on a Linux machine

it will have slightly smaller amount of data but enough for debugging the visual part.

![Example](https://github.com/sergueik/springboot_study/blob/master/basic-perl-cgi/screenshots/capture_file.png)


### Add CGI for Legacy Testing

the container so far does have [FCGI](https://metacpan.org/pod/FCGI) version __0.78__ but no CGI.pm in the image.
installed:
```sh
perl -MFCGI -e 'print $FCGI::VERSION'
```
```text
0.78
```
Alternatively one can install [CGI-Tiny](https://metacpan.org/dist/CGI-Tiny/view/lib/CGI/Tiny.pod) which has no unmet dependencies.
```sh
wget https://cpan.metacpan.org/authors/id/D/DB/DBOOK/CGI-Tiny-1.002.tar.gz
wget https://cpan.metacpan.org/authors/id/D/DB/DBOOK/CGI-Tiny-1.002.tar.gz
tar xzvf CGI-Tiny-1.002.tar.gz
mkdir cgi-bin/CGI;
cp -R -l CGI-Tiny-1.002/lib/CGI cgi-bin/.
```

### Upload Datafile by Curl

```sh
curl -F "data=@$(pwd)/data.txt" -X POST "http://localhost:9090/cgi-bin/upload.cgi?type=send&new=1"
```

```text
<!DOCTYPE html>
<html>
<head>
<meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
<title>Thanks!</title>
<style type="text/css">
img {border: none;}
</style>
</head>
<body>
<p>Thanks for uploading data</p>
<p><img src="/upload/data.txt" alt="data" /></p>
</body>
</html>
```
alternatively a simpler `curl` command (does not work entirely perfectly):
```sh
curl -T data.txt -X POST "http://192.168.0.92:9090/cgi-bin/upload.cgi?type=send&new=1"
```
will lead to happy message (except for the filename of the uploaded file)
```text
<!DOCTYPE html>
<html>
<head>
<meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
<title>Thanks!</title>
<style type="text/css">
img {border: none;}
</style>
</head>
<body>
<p>Thanks for uploading data</p>
<p><img src="/upload/unknown file" alt="data" /></p>
</body>
</html>

```
NOTE: passing the form arguments together with `data` does not work

```sh
curl -F "type=send&new=1&data=@$(pwd)/data.txt" -X POST http://192.168.0.92:9090/cgi-bin/upload.cgi
```
leads to
```
500 Internal Server Error
```
and the following message is logged in apache `/var/log/apache2/error.log`: 
```text
$VAR1 = {: /var/www/localhost/cgi-bin/upload.cgi
 'filename' => undef,: /var/www/localhost/cgi-bin/upload.cgi
 'new' => undef,: /var/www/localhost/cgi-bin/upload.cgi
 'loadtype' => 'send&new=1&data=@/home/sergueik/src/springbo
 };: /var/www/localhost/cgi-bin/upload.cgi
upload content: Use of uninitialized value $file_content in print at /var/www/localhost/cgi-bin/upload.cgi line 53.
 Not a GLOB reference at /var/www/localhost/cgi-bin/upload.cgi line 188.
```
because form argument `new` is aparently lost and the `CGi::Lite` seeminly does not support reading from `File::Temp` file handle returned by

```perl
$query->upload('data');
```
altenatively open `http://localhost:9090/upload.html` in the browser and provide file to upload via File dialog

![Upload](https://github.com/sergueik/springboot_study/blob/master/basic-perl-cgi/screenshots/capture-upload.png)


it will respond with confirmation message

![Confirmed Upload](https://github.com/sergueik/springboot_study/blob/master/basic-perl-cgi/screenshots/capture-confirmed-upload.png)

The `data.txt` contains practically unformatted output from `free`, `uptime`
```text
Mem:        1863756      665656      157580
Swap:       2720764       24832     2695932
load_average: 0.16 0.08 0.08 1/460 32100
rpm: 104
date: Sun Jun 26 18:54:31 EDT 2022
computer: lenovo120S.private.org
uptime: 18:56:03 up 1 day,  3:44,  3 users,  load average: 0.07, 0.10, 0.09
disk: /dev/sda1 27G 22G 3.6G 86% /

```
the `upload.cgi` successfully logs its contents to apache log `/var/www/logs/error.log`:
```text
[Sun Jun 26 23:53:17.609136 2022] [cgi:error] [pid 4966] [client 192.168.0.25:51639] AH01215: Mem:        1863756      665656      157580: /var/www/localhost/cgi-bin/upload.cgi, referer: http://192.168.0.92:9090/upload.html
[Sun Jun 26 23:53:17.609220 2022] [cgi:error] [pid 4966] [client 192.168.0.25:51639] AH01215: Swap:       2720764       24832     2695932: /var/www/localhost/cgi-bin/upload.cgi, referer: http://192.168.0.92:9090/upload.html
[Sun Jun 26 23:53:17.609297 2022] [cgi:error] [pid 4966] [client 192.168.0.25:51639] AH01215: load_average: 0.16 0.08 0.08 1/460 32100: /var/www/localhost/cgi-bin/upload.cgi, referer: http://192.168.0.92:9090/upload.html
[Sun Jun 26 23:53:17.609322 2022] [cgi:error] [pid 4966] [client 192.168.0.25:51639] AH01215: rpm: 104: /var/www/localhost/cgi-bin/upload.cgi, referer: http://192.168.0.92:9090/upload.html
[Sun Jun 26 23:53:17.609389 2022] [cgi:error] [pid 4966] [client 192.168.0.25:51639] AH01215: date: Sun Jun 26 18:54:31 EDT 2022: /var/www/localhost/cgi-bin/upload.cgi, referer: http://192.168.0.92:9090/upload.html
[Sun Jun 26 23:53:17.609453 2022] [cgi:error] [pid 4966] [client 192.168.0.25:51639] AH01215: computer: lenovo120S.private.org: /var/www/localhost/cgi-bin/upload.cgi, referer: http://192.168.0.92:9090/upload.html
[Sun Jun 26 23:53:17.609588 2022] [cgi:error] [pid 4966] [client 192.168.0.25:51639] AH01215: uptime: 18:56:03 up 1 day,  3:44,  3 users,  load average: 0.07, 0.10, 0.09: /var/www/localhost/cgi-bin/upload.cgi, referer: http://192.168.0.92:9090/upload.html
[Sun Jun 26 23:53:17.609655 2022] [cgi:error] [pid 4966] [client 192.168.0.25:51639] AH01215: disk: /dev/sda1 27G 22G 3.6G 86% /: /var/www/localhost/cgi-bin/upload.cgi, referer: http://192.168.0.92:9090/upload.html
```
### Note

if apache is started in debug mode
```sh
/usr/sbin/httpd -X
```
only one of the polling controllers in the page `inventory.html` will be exercised:
![Example](https://github.com/sergueik/springboot_study/blob/master/basic-perl-cgi/screenshots/capture_debug.png)

### Standalone Example

* one can work on rendering the page with Angular JS with no need of the backend, e.g. providing data directly into the page and updating it dynamically

![Examplei Single Page](https://github.com/sergueik/springboot_study/blob/master/basic-perl-cgi/screenshots/capture-singlepage.png)


### Notes


```sh
wget https://cpan.metacpan.org/authors/id/O/OA/OALDERS/URI-5.10.tar.gz
tar zxvf URI-5.10.tar.gz
cp -R URI-5.10/lib/* cgi-bin
```
### Posting to InfluxDB

in developer machine:
```sh
curl -F "data=@$(pwd)/data.txt" -X POST "http://192.168.0.92:9090/cgi-bin/upload.cgi?type=send&new=1"
```
```text
<!DOCTYPE html>
<html>
<head>
<meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
<title>Thanks!</title>
<style type="text/css">
img {border: none;}
</style>
</head>
<body>
<p>Thanks for uploading data</p>
<p><img src="/upload/data.txt" alt="data" /></p>
</body>
</html>
```

in `basic-perl-cgi` container apache log `error.log` 
For better readability the timestamp portion was removed from the logfragmen below `[Tue Jun 28 00:16:40.909223 2022] [cgi:error] [pid 12] [client 192.168.0.92:50706] fields` :
```text

     upload content:   
     Mem:        1863756      665656      157580  
     Swap:       2720764       24832     2695932  
     load_average: 0.16 0.08 0.08 1/460 32100  
     rpm: 104  
     date: Sun Jun 26 18:54:31 EDT 2022  
     computer: lenovo120S.private.org  
     uptime: 18:56:03 up 1 day,  3:44,  3 users,  load average: 0.07, 0.10, 0.09  
     disk: /dev/sda1 27G 22G 3.6G 86% /  
       
     do ingestion of Mem:        1863756      665656      157580  
     Swap:       2720764       24832     2695932  
     load_average: 0.16 0.08 0.08 1/460 32100  
     rpm: 104  
     date: Sun Jun 26 18:54:31 EDT 2022  
     computer: lenovo120S.private.org  
     uptime: 18:56:03 up 1 day,  3:44,  3 users,  load average: 0.07, 0.10, 0.09  
     disk: /dev/sda1 27G 22G 3.6G 86% /  
       
     $VAR1 = {  
               'disk' => '/dev/sda1 27G 22G 3.6G 86% /',  
               'swap' => '2720764       24832     2695932',  
               'load_average' => '0.16 0.08 0.08 1/460 32100',  
               'date' => 'Sun Jun 26 18:54:31 EDT 2022',  
               'mem' => '1863756      665656      157580',  
               'rpm' => '104',  
               'uptime' => '18:56:03 up 1 day,  3:44,  3 users,  load average: 0.07, 0.10, 0.09',  
               'computer' => 'lenovo120S.private.org'  
             };  
       
     $VAR1 = {  
               'load_average' => '0.08',  
               'computer' => 'lenovo120S.private.org',  
               'time' => 1658861671,  
               'swap' => '0.912684819411018'  
             };  
       

     $VAR1 = [  
               'testing,host=lenovo120S.private.org,appid=FOO,operation=write value=0.912684819411018 1658861671000000000',  
               'testing,host=lenovo120S.private.org,appid=BAR,operation=write value=0.912684819411018 1658861671000000000',  
               'testing,host=lenovo120S.private.org,appid=BAZ,operation=write value=0.912684819411018 1658861671000000000'  
             ];  
       
     Use of uninitialized value $content in scalar chomp at /var/www/localhost/cgi-bin/InfluxDB/Client/SimpleAlpine.pm line 224.  
     $VAR1 = {  
               'raw' => {  
                          'success' => 1,  
                          'url' => 'http://boring_williams:8086/write?db=example',  
                          'protocol' => 'HTTP/1.1',  
                          'headers' => {  
                                         'x-request-id' => '955e2e99-f677-11ec-800a-0242ac110002',  
                                         'content-type' => 'application/json',  
                                         'x-influxdb-version' => '1.7.11',  
                                         'x-influxdb-build' => 'OSS',  
                                         'date' => 'Tue, 28 Jun 2022 00:16:40 GMT',  
                                         'request-id' => '955e2e99-f677-11ec-800a-0242ac110002'  
                                       },  
                          'status' => 204,  
                          'reason' => 'No Content'  
                        },  
               'error' => undef  
             };  
       
```
in InfluxDB  container:

```sh
influx
```
```sh
use example
show series
```
```text
key
---
testing,appid=BAR,host=lenovo120S.private.org,operation=write
testing,appid=BAZ,host=lenovo120S.private.org,operation=write
testing,appid=FOO,host=lenovo120S.private.org,operation=write
```
```sh
> select * from testing
```
```text
name: testing
time                appid host                   operation value
----                ----- ----                   --------- -----
1658861671000000000 BAR   lenovo120S.private.org write     0.912684819411018
1658861671000000000 BAZ   lenovo120S.private.org write     0.912684819411018
1658861671000000000 FOO   lenovo120S.private.org write     0.912684819411018
```
### Config
* in the Docker container create `config.cgi`
* start with JSON file `config.json`:
```JSON
{
  "sergueik53": {
    "PORTS": [
      22,
      443,
      3306
    ]
  },
  "sergueik71": {
    "PORTS": [
      5432
    ]
  },
  "sergueik119": {}
}


```

convert it to Perl object:
```sh
perl json_pp_example.pl  -input config.json -dump -debug
```
this will print sample data
```Perl
$VAR1 = {
          'sergueik53' => {
                            'PORTS' => [
                                         22,
                                         443,
                                         3306
                                       ]
                          },
          'sergueik71' => {
                            'PORTS' => [
                                         5432
                                       ]
                          },
          'sergueik119' => {}
        };
```

to be include into `config.cgi`:
```Perl
#!/usr/bin/perl

use strict;

use Getopt::Long;


use YAML::Tiny;
use JSON::PP;
my $data = {
          'sergueik53' => {
                            'PORTS' => [
                                         22,
                                         443,
                                         3306
                                       ]
                          },
          'sergueik71' => {
                            'PORTS' => [
                                         5432
                                       ]
                          },
          'sergueik119' => {}
        };

our $json_pp = JSON::PP->new->ascii->pretty->allow_nonref;
print "Content-Type: application/json\n\n", $json_pp->encode($data);
```

this will print it back:
![Example](https://github.com/sergueik/springboot_study/blob/master/basic-perl-cgi/screenshots/capture-config.png)

The Java app can produce a very similar JSON result when uses `Maps` and `Arrays` internally:
```Java
	@ResponseBody
	@GetMapping(value = "/config", produces = {
			MediaType.APPLICATION_JSON_VALUE })
	public List<Map<String, Object>> arrayConfig() {
		List<Map<String, Object>> results = new ArrayList<>();
		Map<String, Object> row = new HashMap<>();
		Map<String, Object> data = new HashMap<>();
		row.put("sergueik119", data);
		results.add(row);
		row = new HashMap<>();
		data = new HashMap<>();
		List<Integer> ports = Arrays.asList(new Integer[] { 5432 });
		data.put("PORTS", ports);
		row.put("sergueik71", data);
		results.add(row);
		row = new HashMap<>();
		data = new HashMap<>();
		ports = Arrays.asList(new Integer[] { 22, 443, 3306 });
		data.put("PORTS", ports);
		row.put("sergueik53", data);
		results.add(row);
		return results;
	}

```
```JSON
[
  {
    "sergueik119": {}
  },
  {
    "sergueik71": {
      "PORTS": [
        5432
      ]
    }
  },
  {
    "sergueik53": {
      "PORTS": [
        22,
        443,
        3306
      ]
    }
  }
]
```

while when using a POJO
```java
public static class ServerConfig {

  private String name;

  public String getName() {
    return name;
  }

  public void setName(String data) {
    name = data;
  }

  public ServerConfig(String name) {
    this.name = name;
  }

  public ServerConfig() {
  }

  List<Integer> ports = new ArrayList<>();

  public List<Integer> getPorts() {
    return ports;
  }

  public void setPorts(List<Integer> data) {
    ports = data;
  }

}

@ResponseBody
@GetMapping(value = "/config/strong", produces = {
    MediaType.APPLICATION_JSON_VALUE })
public List<ServerConfig> arrayStrongConfig() {

  listServerConfig = new ArrayList<>();
  serverConfig = new ServerConfig("sergueik119");
  listServerConfig.add(serverConfig);
  serverConfig = new ServerConfig("sergueik71");
  ports = Arrays.asList(new Integer[] { 5432 });
  serverConfig.setPorts(ports);
  listServerConfig.add(serverConfig);

  ports = Arrays.asList(new Integer[] { 22, 443, 3306 });
  serverConfig = new ServerConfig("sergueik53");
  serverConfig.setPorts(ports);
  listServerConfig.add(serverConfig);
  data.put("PORTS", ports);
  row.put("sergueik53", data);
  return listServerConfig;
}

```
the resulting JSON would look like:
```JSON
[
  {
    "name": "sergueik119",
    "ports": []
  },
  {
    "name": "sergueik71",
    "ports": [
      5432
    ]
  },
  {
    "name": "sergueik53",
    "ports": [
      22,
      443,
      3306
    ]
  }
]
```
### REST Status Codes
to return one of the known [REST status Codes](https://www.softwaretestinghelp.com/rest-api-response-codes/) use
```sh

curl -sIX GET http://192.168.99.101:9090/cgi-bin/statuscode.cgi
```
```text
HTTP/1.1 200 OK
Date: Wed, 30 Aug 2023 22:50:17 GMT
Server: Apache/2.4.46 (Unix)
Content-Length: 78
Content-Type: application/json
```
```
curl -sI http://192.168.99.101:9090/cgi-bin/statuscode.cgi
```
```text
HTTP/1.1 200 OK
Date: Wed, 30 Aug 2023 22:50:57 GMT
Server: Apache/2.4.46 (Unix)
Content-Type: application/json
```
to see the returned page, remove `I` option
```sh
curl -s http://192.168.99.101:9090/cgi-bin/statuscode.cgi
```
```JSON
{
   "result" : null,
   "status" : "OK",
   "remote_addr" : "192.168.99.1"
	}
```
to make the back end return specific status code, pass the needed statuc code value via `code` request argument 
```sh
curl -sI http://192.168.99.101:9090/cgi-bin/statuscode.cgi?code=304
```
```text
HTTP/1.1 304 Not Modified
Date: Wed, 30 Aug 2023 22:52:23 GMT
Server: Apache/2.4.46 (Unix)
```
```sh
curl -Is http://192.168.99.101:9090/cgi-bin/statuscode.cgi?code=208
```
```text
HTTP/1.1 208 Already Reported
Date: Wed, 30 Aug 2023 22:52:53 GMT
Server: Apache/2.4.46 (Unix)
```

NOTE: no `"result"` is returned when a error code is requested in the payload, ony the `"status"`:
```JSON
{
   "status" : "error"
}
```
NOTE: in the standard protocol, no response body is required with status code `208` and `304`.

* all regular status code are returned as requested:
```sh
curl -sIX GET http://localhost:9090/cgi-bin/statuscode.cgi?code=404
```
```text
HTTP/1.1 404 Not Found
Date: Wed, 17 Apr 2024 16:50:31 GMT
Server: Apache/2.4.46 (Unix)
Content-Length: 45
Access-Control-Allow-Origin: *
Content-Type: text/html;charset=UTF-8
```
```sh
curl -sIX GET http://localhost:9090/cgi-bin/statuscode.cgi?code=401
```
```text
HTTP/1.1 401 Unauthorized
Date: Wed, 17 Apr 2024 16:51:04 GMT
Server: Apache/2.4.46 (Unix)
Content-Length: 45
Access-Control-Allow-Origin: *
Content-Type: text/html;charset=UTF-8
```


NOTE: currently non standard `code` arguments is returned as `406` through `$cgi->error_handler` that is defined to return the error and status:
```sh
curl -sIX GET http://192.168.99.101:9090/cgi-bin/statuscode.cgi?code=999
```
```text
HTTP/1.1 406 Not Acceptable
Date: Wed, 30 Aug 2023 22:46:00 GMT
Server: Apache/2.4.46 (Unix)
Content-Length: 149
Content-Type: text/html;charset=UTF-8
```
```sh
curl -sX GET http://192.168.99.101:9090/cgi-bin/statuscode.cgi?code=999
```

```JSON
{
   "status" : "error",
   "result" : "Attempted to set unknown HTTP response status 999 at /var/www/localhost/cgi-bin/statuscode.cgi line 69.\n"
}

```

the apache log has this:
```text
code=999: /var/www/localhost/cgi-bin/statuscode.cgi
returning HTTP Status 999: /var/www/localhost/cgi-bin/statuscode.cgi
in error handler: Attempted to set unknown HTTP response status 999 at
```

### Updating Scripts
```sh
docker cp cgi-bin/statuscode.cgi $NAME:/var/www/localhost/cgi-bin
```

NOTE: it may not always work in __Docker Toolbox__ - use vi and clipboard in this case

### Troubleshooting 

The Server 500 error
```powershell
. .\getstatuscode.ps1
```
```text
Exception (intercepted): The remote server returned an error: (500) Internal Server Error.
Page: ""
Exception (intercepted): The remote server returned an error: (500) Internal Server Error.
HTTP Stasus: -1
```
and the apache `/var/www/logs/error.log` message
```text
[Thu Aug 31 16:50:50.487137 2023] [cgi:error] [pid 13] [client 192.168.99.1:50902] AH01215: (2)No such file or directory: exec of '/var/www/localhost/cgi-bin/statuscode.cgi' failed: /var/www/localhost/cgi-bin/statuscode.cgi
```
confirmed by shell error
```sh
sh: /var/www/localhost/cgi-bin/statuscode.cgi: not found
```

indicates line endings issue, fixed via
```sh
sed -i 's|\r||g' /var/www/localhost/cgi-bin/statuscode.cgi
```

The success case should look like

```text
Page: {
    "status":  "error",
    "code":  "208"
}
HTTP Stasus: 208
```
and

```powershell
. .\getstatuscode.ps1 -url http://192.168.99.100:9090/cgi-bin/statuscode.cgi
```
```text

Page: {
    "result":  null,
    "status":  "OK",
    "code":  200,
    "remote_addr":  "192.168.99.1"
}
HTTP Stasus: 200
```

### Download Update Config

```sh
curl -so example_config.json  "http://192.168.99.101:9090/cgi-bin/file_hash.cgi?inputfile=example_config.json&hash=xxxxx"
```

```sh
ls -l example_config.json
```
```text
-rw-r--r-- 1 Serguei 197609 18 Sep  5 19:05 example_config.json
```
on the client
```sh
md5sum.exe  exmple_config.json
```
```text
9f8377db38593544a5e994006fe4e9e4 *example_config.json
```
on the server
```sh
md5sum  /var/www/localhost/cgi-bin/example_config.json
```
```text
9f8377db38593544a5e994006fe4e9e4  example_config.json
```
```sh
DOCKER_MACHINE_HOST=$(docker-machine ip)
curl -s "http://$DOCKER_MACHINE_HOST:9090/cgi-bin/file_hash.cgi?inputfile=example_config.json&hash=9f8377db38593544a5e994006fe4e9e4"
```
```JSON	

{
   "status" : "error",
   "result" : "Config /var/www/localhost/cgi-bin/example_config.json is unchanged"
}

```

### Process Config File HTTP status

```sh
IMAGE=basic-perl-apache
NAME=basic-perl-cgi
docker run -d -p 9090:80 -p 9443:443 --name $NAME $IMAGE
```
or

```sh
docker start $NAME
```

```
DOCKER_MACHINE_HOST=$(docker-machine ip)
curl -so example_config.json  "http://192.168.99.101:9090/cgi-bin/file_hash_status.cgi?inputfile=example_config.json&hash=xxxxx"
```
```sh
curl -vs "http://$DOCKER_MACHINE_HOST:9090/cgi-bin/file_hash_status.cgi?inputfile=example_config.json&hash=9f8377db38593544a5e994006fe4e9e4"
```
```text
* Uses proxy env variable no_proxy == '192.168.99.103,192.168.99.100'
*   Trying 192.168.99.100:9090...
* Connected to 192.168.99.100 (192.168.99.100) port 9090 (#0)
> GET /cgi-bin/file_hash_status.cgi?inputfile=example_config.json&hash=9f8377db3
8593544a5e994006fe4e9e4 HTTP/1.1
> Host: 192.168.99.100:9090
> User-Agent: curl/7.75.0
> Accept: */*
>
* Mark bundle as not supporting multiuse
< HTTP/1.1 304 Not Modified
< Date: Mon, 16 Oct 2023 20:17:33 GMT
< Server: Apache/2.4.46 (Unix)
<
* Connection #0 to host 192.168.99.100 left intact
```
```sh
perl client.pl  -url 'http://192.168.99.100:9090/cgi-bin/file_hash_status.cgi?inputfile=example_config.json&hash=9f8377db38593544a5e994006fe4e9e4'
```
```text
ERROR: Not Modified
```
```sh
perl client.pl  -url 'http://192.168.99.100:9090/cgi-bin/file_hash_status.cgi?inputfile=example_config.json&hash=9f8377db38593544a5e994006fe4e9e3'
```
```JSON
{"red": "green"}
```
* the file check sum is preserved:
```sh
$ md5sum.exe a.json
```
```text
9f8377db38593544a5e994006fe4e9e4 *a.json
```
while on the container:
```sh
docker exec $NAME md5sum /var/www/localhost/cgi-bin/example_config.json
```
```text
9f8377db38593544a5e994006fe4e9e4  /var/www/localhost/cgi-bin/example_config.json
```
### Troubleshooting

if see curl error:
```text
connect to 192.168.99.100 port 9090 failed: Connection refused
Failed to connect to 192.168.99.100 port 9090: Connection refused
Closing connection 0
```
check
```sh
$ docker container ls -a
CONTAINER ID        IMAGE                      COMMAND                  CREATED
            STATUS                      PORTS               NAMES
611834b2c3af        basic-perl-apache          "sh -c 'PIDFILE='/ru"   6 weeks ago         Exited (0) 1 second ago                         basic-perl-cgi
```

then
```sh
$ docker logs $NAME
```
```text
[Thu Aug 31 13:50:20.835067 2023] [alias:warn] [pid 6] AH00671: The Alias directive in /etc/apache2/conf.d/mod_fcgid.conf at line 4 will probably never match be
cause it overlaps an earlier ScriptAlias.
AH00558: httpd: Could not reliably determine the server's fully qualified domain name, using 172.17.0.2. Set the 'ServerName' directive globally to suppress this message
0
wait for apache pid
apache is running with ID 7
[Mon Oct 16 20:07:01.923646 2023] [alias:warn] [pid 6] AH00671: The Alias directive in /etc/apache2/conf.d/mod_fcgid.conf at line 4 will probably never match because it overlaps an earlier ScriptAlias.
AH00558: httpd: Could not reliably determine the server's fully qualified domain name, using 172.17.0.2. Set the 'ServerName' directive globally to suppress this message
0
apache is running with ID 7
apache is gone
```
### Note

```sh
perl -pi -e '$_ =~ s|(<Directory "/var/www/localhost/htdocs">)|$1\nHeader set Access-Control-Allow-Origin "*"|og unless /^\s*Header set Access-Control-Allow-Origin/' /etc/apache2/httpd.conf
```
is not idempotent, updates the file in every run, because emulates sed line-oriented operation

#### Testing the CORS

```sh
docker container stop basic-perl-cgi

docker-compose up --build --no-color
```

the latter command will start two identicalcontainers listening to two TCP ports mapped on host: `8080` and `9090`. The page running on former `http://192.168.99.100:8080/form_post.html` will call the service run in the latter:

![Example CORS](https://github.com/sergueik/springboot_study/blob/master/basic-perl-cgi/screenshots/capture-cors.png)


and does require CORS support by `echo_json.cgi` to work, which is accomplished by returning response headers
```text
Access-Control-Allow-Headers: *
```
and
```text
Access-Control-Allow-Origin: *
```

![Example CORS Headers](https://github.com/sergueik/springboot_study/blob/master/basic-perl-cgi/screenshots/capture-needed-headers.png)

The page `http://192.168.0.92:9090/form_post.html` will call service on same host and port, and will not be blocked by CORS even without the headers.

Without the call in `echo-json.cgi`
```perl
$query = $query->add_response_header('Access-Control-Allow-Origin' => '*');
```

NOTE, alternatively, one may provide the header in the apache configuration `/etc/apache2/httpd.conf`
```text
<Directory "/var/www/localhost/cgi-bin">
Header set Access-Control-Allow-Origin "*"
    AllowOverride All
    Options None
    Require all granted
</Directory>
```

it appears to be harmless to have duplication in `Access-Control-Allow-Origin` headers

![Example CORS Error](https://github.com/sergueik/springboot_study/blob/master/basic-perl-cgi/screenshots/cature-preflightmissingalloworiginheader.png)
### Custom Headers with Legacy `CGI.pm`
  
The legacy `CGI.pm` does not have method to print custom headers. The header method misundertands the arguments:
```sh
perl -MCGI -e 'my $o = new CGI;print $o->header("X-Server" => "Apache Next Generation 10.0"  )'
```
will produce 
```text
Status: Apache Next Generation 10.0
Content-Type: X-Server; charset=ISO-8859-1
```
One will need to install the `mod_perl` (but this will not be possible without the apache source)
to use the [method](https://perl.apache.org/docs/1.0/api/Apache.html)
```Perl
$r->header_out( $header, $value );
```
Alternatively one may simply [print header fully, directly](https://docstore.mik.ua/orelly/weblinux2/modperl/ch06_11.htm)
```Perl
print qq|
HTTP/1.1 200 OK 
Date: Tue, 10 Apr 2001 03:01:36 GMT Server: Apache/1.3.19 (Unix) mod_perl/1.25 
Connection: close
Access-Control-Allow-Origin: * 
Access-Control-Allow-Headers: *
Content-Type: text/plain |;

```
### Text Area
```sh
docker cp html/process_textarea.html $NAME:/var/www/localhost/htdocs
```
```sh
docker cp html/js/csv.js $NAME:/var/www/localhost/htdocs/js
```

```sh
docker cp cgi-bin/host_status.cgi $NAME:/var/www/localhost/cgi-bin/
```

proceed with `http://192.168.99.100:9090/process_textarea.html`
paste 
```text
column1,column2,column3
host1,true,PROD
host2,true,PROD
host3,false,STAGING

```
into textarea
NOTE: the minimal information to paste is single column with header (additional columns reserved for later use):
```text
column1
host1
host2
host3
host4
host5
```
will see the host statuses evaluated via REST calls
```javascript
$http.get('http://' + url + ':' + port + '/cgi-bin/host_status.cgi' + '?hostname=' + hostname, '', { headers: { 'Referrer-Policy': 'origin' } }).then(
  function(response) { $scope.msg = 'success'; $scope.results.push( response.data )}, 
  function(response) { $scope.msg = 'error'}
);
```
as

![Example Host Status](https://github.com/sergueik/springboot_study/blob/master/basic-perl-cgi/screenshots/capture-host-status.png)

### Styling Rows 
```sh
docker cp html/styled_row.html $NAME:/var/www/localhost/htdocs
```
start `styled_row.html` from file or host:
paste sample monitoring data
```json
[
{
  "host": "host1",
  "columns": [
    {
      "column": "hostname",
      "value": "host1",
      "class": "hostclass"
    },
    {
      "column": "UPTIME",
      "value": 123.45,
      "class": "uptime"
    },
    {
      "column": "MEMORY",
      "value": 16,
      "class": "memoryclass"
    },
    {
      "column": "TIME",
      "value": "11:58:28",
      "class": "time"
    }
  ]
}
]
```
into textarea
![Example Host Status](https://github.com/sergueik/springboot_study/blob/master/basic-perl-cgi/screenshots/capture-styled-rows.png)

![Example Host Status](https://github.com/sergueik/springboot_study/blob/master/basic-perl-cgi/screenshots/capture-styled-rows-2.png)


### Testing Just HTTP Status code

* Powershell appears to introduce a custom excwption class for HTTP Status codes `40x` `50x`

```sh
curl -sv http://192.168.99.100:9090/cgi-bin/statuscode.cgi?code=408
```
```text
{
   "status" : "error",
   "code" : "408"
}
* processing: http://192.168.99.100:9090/cgi-bin/statuscode.cgi?code=408
* Uses proxy env variable no_proxy == '192.168.99.100'
*   Trying 192.168.99.100:9090...
* Connected to 192.168.99.100 (192.168.99.100) port 9090
> GET /cgi-bin/statuscode.cgi?code=408 HTTP/1.1
> Host: 192.168.99.100:9090
> User-Agent: curl/8.2.1
> Accept: */*
>
< HTTP/1.1 408 Request Timeout
< Date: Wed, 03 Jan 2024 02:24:36 GMT
< Server: Apache/2.4.46 (Unix)
< Content-Length: 45
< Access-Control-Allow-Origin: *
< Connection: close
< Content-Type: text/html;charset=UTF-8
<
{ [45 bytes data]
* Closing connection


```

```powershell
. .\getconfig3.ps1 -base_url http://192.168.99.100:9090/cgi-bin/statuscode.cgi?code=408 -debug
```
try vanilla Powershell cmdlets:
```powershell
invoke-restmethod -method Get -uri http://192.168.99.100:9090/cgi-bin/statuscode.cgi?code=408
```
get exception
```text
invoke-restmethod : { "code" : "408", "status" : "error" }
At line:1 char:1
+ invoke-restmethod -method Get -uri http://192.168.99.100:9090/cgi-bin/statuscode ...
+ ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    + CategoryInfo          : InvalidOperation: (System.Net.HttpWebRequest:HttpWebRequest) [Invoke-RestMethod], WebException
    + FullyQualifiedErrorId : WebCmdletWebResponseException,Microsoft.PowerShell.Commands.InvokeRestMethodCommand
```
```powershell
Invoke-WebRequest -method Get -uri http://192.168.99.100:9090/cgi-bin/statuscode.cgi?code=408
```
```text
Invoke-WebRequest : { "status" : "error", "code" : "408" }
At line:1 char:1
+ Invoke-WebRequest -method Get -uri http://192.168.99.100:9090/cgi-bin/statuscode ...
+ ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    + CategoryInfo          : InvalidOperation: (System.Net.HttpWebRequest:HttpWebRequest) [Invoke-WebRequest], WebException
    + FullyQualifiedErrorId : WebCmdletWebResponseException,Microsoft.PowerShell.Commands.InvokeWebRequestCommand

```

there are two different classes `Microsoft.PowerShell.Commands.InvokeRestMethodCommand` and `Microsoft.PowerShell.Commands.InvokeWebRequestCommand` involved and neither
returns simply the HTTP status for non-`20x` cases
### Upload CSV Data

* the simplest is to create a text file `a.csv` with contents:
```text
foo,bar,baz
10,20,30
100,200,300
```

and upload it via `curl` in the body of the message. With curl one does not have to specify the `POST` method but need to be aware of the required `--data-binary` option:
```sh
 curl -sX POST -H 'Content-type: application/octet-stream' --data-binary @a.csv http://192.168.99.100:9090/cgi-bin/csv.cgi
```
prints back the page with payload embedded
```HTML
<!DOCTYPE html>
<html>
<head>
<meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
<title>Thanks!</title>
<style type="text/css">
img {border: none;}
</style>
</head>
<body>
<p>Thanks for uploading data:</p>
foo,bar,baz
10,20,30
100,200,300

</body>
</html>

```

and logs it normally
```text
[Wed Jan 17 23:46:47.309873 2024] [cgi:error] [pid 11] [client 192.168.99.1:5852 8] AH01215: $VAR1 = 'foo,bar,baz: 
[Wed Jan 17 23:46:47.309928 2024] [cgi:error] [pid 11] [client 192.168.99.1:5852 8] AH01215: 10,20,30 
[Wed Jan 17 23:46:47.309980 2024] [cgi:error] [pid 11] [client 192.168.99.1:5852 8] AH01215: 100,200,300 
[Wed Jan 17 23:46:47.310007 2024] [cgi:error] [pid 11] [client 192.168.99.1:5852 8] AH01215: '; 
```
while omitting the flag


```sh
curl -X POST -H 'Content-type: application/octet-stream' -d@a.csv http://192.168.99.100:9090/cgi-bin/csv.cgi

```
illustrates how `curl` clobbers line endings:
```HTML
<!DOCTYPE html>
<html>
<head>
<meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
<title>Thanks!</title>
<style type="text/css">
img {border: none;}
</style>
</head>
<body>
<p>Thanks for uploading data:</p>
foo,bar,baz10,20,30100,200,300
</body>
</html>
```
in the `error.log`
```text
[Wed Jan 17 23:02:20.993501 2024] [cgi:error] [pid 15] [client 192.168.99.1:5844 6] AH01215: $VAR1 = 'foo,bar,baz10,20,30100,200,300';
n/csv.cgi
```
```sh
curl -s http://192.168.99.100:9090/cgi-bin/csv.cgi -X POST --data-binary @a.csv -H 'Content-type: application/octet-stream'
```
produces the JSON  serialized  object:
```JSON
[
   {
      "foo" : "10",
      "bar" : "20",
      "baz" : "30"
   },
   {
      "baz" : "300",
      "foo" : "100",
      "bar" : "200"
   }
]

```
### More Upload
```sh
curl -F "data=@$(pwd)/data.txt" -X POST "http://192.168.99.100:9090/cgi-bin/upload.cgi?type=send&new=0"
```
```
curl -F "data=@$(pwd)/data.txt" -X POST "http://192.168.99.100:9090/cgi-bin/csv_upload.cgi?type=send"
```
### Troubleshooting
```sh
docker container start $NAME
```
```sh
docker container ls  -a
```
```text
CONTAINER ID        IMAGE               COMMAND                  CREATED     STATUS                      PORTS               NAMES
948fd2134e76        basic-perl-apache   "sh -c 'PIDFILE='/ru"   5 days ago    Exited (0) 12 seconds ago                       basic-perl-cgi
```
```sh
docker container logs $NAME
```
```text
[Fri Jan 19 23:01:53.207868 2024] [alias:warn] [pid 6] AH00671: The Alias directive in /etc/apache2/conf.d/mod_fcgid.conf at line 4 will probably never match because it overlaps an earlier ScriptAlias.AH00558: httpd: Could not reliably determine the server's fully qualified domain name, using 172.17.0.2. Set the 'ServerName' directive globally to suppress this message
0
apache is running with ID 7
apache is gone

```
restart of `docker-machine` does not help. giving up
```sh
mkdir /var/www/localhost/cgi-bin/Text
```
```sh
curl -F "data=@$(pwd)/a.csv" -sX POST "http://192.168.99.100:9090/cgi-bin/csv_upload.cgi?type=send"
```
```JSON
[
   {
      "bar" : "20",
      "foo" : "10",
      "baz" : "30"
   },
   {
      "baz" : "300",
      "foo" : "100",
      "bar" : "200"
   }
]
```
```sh
curl -sX POST -H 'Content-type: application/octet-stream' --data-binary @a.csv http://192.168.99.100:9090/cgi-bin/csv.cgi
```
```JSON
[
   {
      "baz" : "30",
      "foo" : "10",
      "bar" : "20"
   },
   {
      "baz" : "300",
      "bar" : "200",
      "foo" : "100"
   }
]

```
```sh
curl -sX POST -H 'Content-type: application/octet-stream' --data-binary $'foo,bar,baz\n10,20,30\n100,200,300' http://192.168.99.100:9090/cgi-bin/csv.cgi
```
```JSON
[
   {
      "foo" : "10",
      "bar" : "20",
      "baz" : "30"
   },
   {
      "foo" : "100",
      "bar" : "200",
      "baz" : "300"
   }
]

```

### Upload With AntularJS

* Using [example](https://www.tutorialspoint.com/angularjs/angularjs_upload_file.htm) - uses [custom directive](https://docs.angularjs.org/guide/directive) to enable Angular access plain file input form element and [service](https://docs.angularjs.org/guide/services) and [formdata](https://docs.angularjs.org/guide/forms) - handy for debugging and rendering the csv

![Example](https://github.com/sergueik/springboot_study/blob/master/basic-perl-cgi/screenshots/capture-angularjs-upload.png)

NOTE, AngularJS uploads files via `multipart/form-data` - modifications were required to cgi-bin `csv.cgi` to accept both

### See Also

  * https://stackoverflow.com/questions/19408011/angularjs-error-argument-firstctrl-is-not-a-function-got-undefined/19408070
  * https://stackoverflow.com/questions/13671031/server-polling-with-angularjs
  * https://blog.guya.net/2016/08/08/simple-server-polling-in-angularjs-done-right/
  * https://www.js-tutorials.com/angularjs-tutorial/simple-example-angularjs-interval-timeout/
  * https://stackoverflow.com/questions/42701048/how-to-pass-vm-to-a-settimeout-in-angularjs-changes-to-scope-dont-update-dom-v
  * [curl post file](https://reqbin.com/req/c-dot4w5a2/curl-post-file)
  * [CPAN](https://metacpan.org/pod/HTTP::Request::Common) `HTTP::Request::Common`
  * [how to find Docker Toolbox IP address](https://devilbox.readthedocs.io/en/latest/howto/docker-toolbox/find-docker-toolbox-ip-address.html)
  * `HTTP::Tiny` [Pure Perl HTTP Client](https://metacpan.org/pod/HTTP::Tiny)
  * https://stackoverflow.com/questions/32121479/get-json-code-from-textarea-and-parse-it
  * http://www.java2s.com/Tutorials/Javascript/AngularJS_Example/Controller/Call_function_in_controller_with_onchange_event.htm
  * https://www.w3schools.com/angular/tryit.asp?filename=try_ng_ng-change
  * [AngularJS Developer Guide](https://docs.angularjs.org/guide)
  * [AngularJS API](https://docs.angularjs.org/api) 
  * [Angular - HTTP GET Request Examples](https://jasonwatmore.com/post/2019/09/06/angular-http-get-request-examples)
  * the full `HttpResponse` [class documentation](https://angular.io/api/common/http/HttpResponse#properties)
  * AngularJS $http [reference](https://docs.angularjs.org/api/ng/service/$http)
  * CGI file upload [document](https://www.sitepoint.com/uploading-files-cgi-perl/)
  * `mirror` [method](https://metacpan.org/pod/HTTP::Tiny#mirror) in `HTTP::Tiny`
  * `mirror` [method](https://metacpan.org/pod/LWP::Simple#mirror) in `LWP::Simple`
  * `getstore` [method](https://metacpan.org/pod/LWP::Simple#getstore) in `LWP::Simple`
  * [enable CORS in Apache Web Server](https://ubiq.co/tech-blog/enable-cors-apache-web-server/)
  * [insert line after match using sed or perl](https://stackoverflow.com/questions/15559359/insert-line-after-match-using-sed)
  * https://stackoverflow.com/questions/39069206/how-to-set-custom-headers-for-httptiny-in-perl
  * [mozilla documentation](https://developer.mozilla.org/en-US/docs/Web/HTTP/CORS/Errors/CORSMissingAllowOrigin) on `cross-origin resource sharing error preflightmissingalloworiginheader` error 
  * [vanilla JavaScript CSV (comma-separated values) parser](https://github.com/cparker15/CSV-js/blob/master/src/csv.js)
  * [Pure-perl CVS module](https://metacpan.org/pod/Text::CSV_PP) (NOTE: without installing `https://fastapi.metacpan.org/source/ISHIGAKI/Text-CSV-2.04/lib/Text/CSV.pm`)
  * https://stackoverflow.com/questions/3872427/how-to-send-line-break-with-curl
  * https://stackoverflow.com/questions/46233809/how-to-upload-a-file-from-post-request-content-type-application-octet-stream
  * Angular JS file upload [tutorial](https://github.com/folio3/AngularJS-Tutorial-Example-Code)
  * [AngularJS FormData Multi-part File Upload](https://www.codeproject.com/Articles/5292552/AngularJS-FormData-Multi-part-File-Upload)
  * [send FormData with other field in AngularJS](https://stackoverflow.com/questions/37039852/send-formdata-with-other-field-in-angularjs)
  * [CGI::Tiny::Cookbook](https://metacpan.org/pod/CGI::Tiny::Cookbook)
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)


