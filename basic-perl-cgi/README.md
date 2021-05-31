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
docker run -d -p 8080:80 -p 9443:443 --name $NAME $NAME
docker logs $NAME
```
this will respond with
```sh
wait for apache pid
apache is running with ID 7
```
(the id may vary)

* tweak directory structure
```sh
for D in css js ; do docker exec $NAME mkdir /var/www/localhost/htdocs/$D; done
for D in JSON YAML ; do docker exec $NAME mkdir /var/www/localhost/cgi-bin/$D; done
```
* copy individual files: frontend

```sh
docker cp html/inventory.html $NAME:/var/www/localhost/htdocs
for F in $(ls -1 html/css) ; do docker cp html/css/$F $NAME:/var/www/localhost/htdocs/css; done
for F in $(ls -1 html/js) ; do docker cp html/js/$F $NAME:/var/www/localhost/htdocs/js; done
```
* backend
```sh
for F in $(ls -1 cgi-bin) ; do docker cp cgi-bin/$F $NAME:/var/www/localhost/cgi-bin ;done
docker cp JSON/PP.pm $NAME:/var/www/localhost/cgi-bin/JSON
docker cp YAML/Tiny.pm $NAME:/var/www/localhost/cgi-bin/YAML
```
```sh
for F in $(ls -1 cgi-bin) ; do docker exec $NAME chmod 775 /var/www/localhost/cgi-bin/$F ; done
```
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
curl http://localhost:8080/cgi-bin/list.cgi
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
testing the page, console:
```sh
curl http://localhost:8080/inventory.html
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
one needs to run in the browser to see  the dynamic data

![Example](https://github.com/sergueik/springboot_study/blob/master/basic-perl-cgi/screenshots/capture.png)

 - opened developer tools tab shows that the Angular is doing polling the server
### Cleanup

```sh
docker stop $NAME
docker container prune -f
docker image prune -f
docker image rm $NAME
sudo rm -fr web/
```
### Running Angular part from filesystem

one can start the page in th browser from file explorer via:

```cmd
file:///C:/developer/sergueik/springboot_study/basic-perl-cgi/html/inventory.html
```
it will look slightly less data but enough for debugging the visual part.

![Example](https://github.com/sergueik/springboot_study/blob/master/basic-perl-cgi/screenshots/capture_file.png)
### Note

if apache is run in debug mode
```sh
/usr/sbin/httpd -X
```
only one of the polling controllers will be exercised:
![Example](https://github.com/sergueik/springboot_study/blob/master/basic-perl-cgi/screenshots/capture_debug.png)

### See Also
  * https://stackoverflow.com/questions/19408011/angularjs-error-argument-firstctrl-is-not-a-function-got-undefined/19408070
  * https://stackoverflow.com/questions/13671031/server-polling-with-angularjs
  * https://blog.guya.net/2016/08/08/simple-server-polling-in-angularjs-done-right/
  * https://www.js-tutorials.com/angularjs-tutorial/simple-example-angularjs-interval-timeout/
  * https://stackoverflow.com/questions/42701048/how-to-pass-vm-to-a-settimeout-in-angularjs-changes-to-scope-dont-update-dom-v

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
