### Info

Plain Alpine 3.9 container installing the apache and Perl using some code from [Alpine microcontainer with Apache2, perl5 and FCGI.pm](https://github.com/kjetillll/docker-alpine-apache-perl-fcgi) - and launching httpd without using init.d of [nimmis/docker-alpine-micro](https://github.com/nimmis/docker-alpine-micro) and installing few pure Perl modules (YAML, XML and JSON) and Angular and Bootstrap to explore Angular CGI-BIN pages

### Testing

* build the image
```sh
IMAGE=basic-perl-apache
docker build -t $IMAGE -f Dockerfile .
```
* start run default command

```sh
NAME=basic-perl-cgi
docker run -d -p 9090:80 -p 9443:443 --name $NAME $IMAGE
docker logs -f $NAME
```
alternatively
```
docker run -d -p $(hostname -i):9090:80 -p $(hostname -i):9443:443 --name $NAME $IMAGE
```
 or if integrted with InfluxDB then
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

NOTE that currently the query params are not yet implemented. The `config.cgi` compares the file modification time of `example_config.cgi` to
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
Alternatively one can install [CGI-Tiny](https://metacpan.org/dist/CGI-Tiny/view/lib/CGI/Tiny.pod)  which has no unmet dependencies.
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

NOTE: no `"result"` is returned when a error code is requested, ony the `"status"`:
```JSON
{
   "status" : "error"
}
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
curl -s "http://192.168.99.101:9090/cgi-bin/file_hash.cgi?inputfile=example_config.json&hash=9f8377db38593544a5e994006fe4e9e4"
```
```JSON	

{
   "status" : "error",
   "result" : "Config /var/www/localhost/cgi-bin/example_config.json is unchanged"
}

```
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
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)


