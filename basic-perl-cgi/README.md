### Info

Plain Alpine 3.9 container installing the apache and Perl using some code from [Alpine microcontainer with Apache2, perl5 and FCGI.pm](https://github.com/kjetillll/docker-alpine-apache-perl-fcgi) - and launching httpd without using init.d of [nimmis/docker-alpine-micro](https://github.com/nimmis/docker-alpine-micro) and installing few pure Perl modules (YAML, XML and JSON) and Angular and Bootstrap to explore Angular CGI-BIN pages

### Testing

* build the image
```sh
NAME=basic-perl-cgi-container
docker build -t $NAME -f Dockerfile .
```
* start run default command

```sh
docker run -d -p $(hostname -i):8080:80 -p $(hostname -i):9443:443 --name $NAME $NAME
docker logs $NAME
```
this will respond with
```sh
wait for apache pid
apache is running with ID 7
```
(the value of `ID` varies)

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


* verify the vanilla httpd to run in Docker
```sh
curl http://$(hostname -i):8080/
```
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
call cgi directly:
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
similar results (sans the header) as cgi-bin :
```sh
curl http://$(hostname -i):8080/cgi-bin/list.cgi
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
and use `form.cgi` to post the data:
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
testing the page, console:
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
one needs to open page in the browser to see  the dynamic data. To verify, stop and rerun the container with default pors pusblsued as `8080` and `9443` on all network interfaces:
```sh
docker stop $NAME
docker container rm $NAME
docker run -d -p 8080:80 -p 9443:443 --name $NAME $NAME
```
and navigate to the url in the browser with ip address of the hosting node:
![Example](https://github.com/sergueik/springboot_study/blob/master/basic-perl-cgi/screenshots/capture.png)

 - opened developer tools tab shows that the Angular is doing polling the server
### Cleanup

```sh
docker stop $NAME
docker container prune -f
docker image prune -f
docker image rm $NAME
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
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
