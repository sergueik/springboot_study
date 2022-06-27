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
docker run -d -p 8080:80 -p 9443:443 --name $NAME $IMAGE
docker logs $NAME
```
alternatively
```
docker run -d -p $(hostname -i):8080:80 -p $(hostname -i):9443:443 --name $NAME $IMAGE
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
curl http://$(hostname -i):8080/
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


* run smoke test
call cgi inside container directly:
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
verify web server to run cgi inside container. Basically will observe same output 
results (sans the header) as cgi-bin :
```sh
curl -s http://$(hostname -i):8080/cgi-bin/list.cgi
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
curl -X POST -d 'a=b&c=d' http://$(hostname -i):8080/cgi-bin/form.cgi
```
this will echo data back as a JSON
```sh
Content-Type: application/json

{
   "c" : "d",
   "a" : "b"
}
```
testing the AJAX page in console:
```sh
curl http://$(hostname -i):8080/inventory.html
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
one needs to open page in the browser to see the dynamic data being pulled. To verify, stop and rerun the container with default pors pusblsued as `8080` and `9443` on all network interfaces:
```sh
docker stop $NAME
docker container rm $NAME
docker run -d -p 8080:80 -p 9443:443 --name $NAME $IMAGE
```
and navigate to the url in the browser with ip address of the hosting node:
```sh
chromium-browser http://$(hostname -i):8080/inventory.html &
```
![Example](https://github.com/sergueik/springboot_study/blob/master/basic-perl-cgi/screenshots/capture.png)

 - open developer tools tab eeto that the Angular does poll the server
![Example](https://github.com/sergueik/springboot_study/blob/master/basic-perl-cgi/screenshots/capture.png)

### Cleanup

```sh
docker stop $NAME
docker container prune -f
docker image prune -f
docker image rm $IMAGE
```
### Running Angular part from filesystem

one can start the page in th browser from file explorer via:

```cmd
file:///C:/developer/sergueik/springboot_study/basic-perl-cgi/html/inventory.html
```
on a Windows machine or
```sh
google-chrome file://$(pwd)/html/inventory.html & 
```
on a Linux machine

it will look slightly less data but enough for debugging the visual part.

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

### Upload via Curl

```sh
curl -F "data=@$(pwd)/data.txt" -X POST "http://192.168.0.29:8080/cgi-bin/upload.cgi?type=send&new=1"
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

NOTE: need to work on passing  form arguments

```sh
curl -F "type=send&new=1&data=@$(pwd)/data.txt" -X POST http://192.168.0.29:8080/cgi-bin/upload.cgi
```
leads to
```
500 Internal Server Error
```

because form argument `new` is aparently lost and the `CGi::Lite` seeminly does not support reading from `File::Temp` file handle returned by

```perl
$query->upload('data');
```
altenatively open `http://localhost:8080/upload.html` in the browser and provide file to upload via File dialog

![Example Upload](https://github.com/sergueik/springboot_study/blob/master/basic-perl-cgi/screenshots/capture-upload.png)
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
[Sun Jun 26 23:53:17.609136 2022] [cgi:error] [pid 4966] [client 192.168.0.25:51639] AH01215: Mem:        1863756      665656      157580: /var/www/localhost/cgi-bin/upload.cgi, referer: http://192.168.0.29:8080/upload.html
[Sun Jun 26 23:53:17.609220 2022] [cgi:error] [pid 4966] [client 192.168.0.25:51639] AH01215: Swap:       2720764       24832     2695932: /var/www/localhost/cgi-bin/upload.cgi, referer: http://192.168.0.29:8080/upload.html
[Sun Jun 26 23:53:17.609297 2022] [cgi:error] [pid 4966] [client 192.168.0.25:51639] AH01215: load_average: 0.16 0.08 0.08 1/460 32100: /var/www/localhost/cgi-bin/upload.cgi, referer: http://192.168.0.29:8080/upload.html
[Sun Jun 26 23:53:17.609322 2022] [cgi:error] [pid 4966] [client 192.168.0.25:51639] AH01215: rpm: 104: /var/www/localhost/cgi-bin/upload.cgi, referer: http://192.168.0.29:8080/upload.html
[Sun Jun 26 23:53:17.609389 2022] [cgi:error] [pid 4966] [client 192.168.0.25:51639] AH01215: date: Sun Jun 26 18:54:31 EDT 2022: /var/www/localhost/cgi-bin/upload.cgi, referer: http://192.168.0.29:8080/upload.html
[Sun Jun 26 23:53:17.609453 2022] [cgi:error] [pid 4966] [client 192.168.0.25:51639] AH01215: computer: lenovo120S.private.org: /var/www/localhost/cgi-bin/upload.cgi, referer: http://192.168.0.29:8080/upload.html
[Sun Jun 26 23:53:17.609588 2022] [cgi:error] [pid 4966] [client 192.168.0.25:51639] AH01215: uptime: 18:56:03 up 1 day,  3:44,  3 users,  load average: 0.07, 0.10, 0.09: /var/www/localhost/cgi-bin/upload.cgi, referer: http://192.168.0.29:8080/upload.html
[Sun Jun 26 23:53:17.609655 2022] [cgi:error] [pid 4966] [client 192.168.0.25:51639] AH01215: disk: /dev/sda1 27G 22G 3.6G 86% /: /var/www/localhost/cgi-bin/upload.cgi, referer: http://192.168.0.29:8080/upload.html
```
### Note

if apache is started in debug mode
```sh
/usr/sbin/httpd -X
```
only one of the polling controllers in the page `inventory.html` will be exercised:
![Example](https://github.com/sergueik/springboot_study/blob/master/basic-perl-cgi/screenshots/capture_debug.png)

### See Also
  * https://stackoverflow.com/questions/19408011/angularjs-error-argument-firstctrl-is-not-a-function-got-undefined/19408070
  * https://stackoverflow.com/questions/13671031/server-polling-with-angularjs
  * https://blog.guya.net/2016/08/08/simple-server-polling-in-angularjs-done-right/
  * https://www.js-tutorials.com/angularjs-tutorial/simple-example-angularjs-interval-timeout/
  * https://stackoverflow.com/questions/42701048/how-to-pass-vm-to-a-settimeout-in-angularjs-changes-to-scope-dont-update-dom-v
  * [curl post file](https://reqbin.com/req/c-dot4w5a2/curl-post-file)

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)

