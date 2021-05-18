### Info

Container based on [Alpine microcontainer with Apache2, perl5 and FCGI.pm](https://github.com/kjetillll/docker-alpine-apache-perl-fcgi) - only using the apache and Perl
using pure perl YAML and JSON modules

### Testing

* build image
```sh
NAME=basic-perl-cgi-container
docker build -t $NAME -f Dockerfile .
```
* run default commands

```sh
mkdir web
docker run -d -p 8080:80 -p 9443:443 -v $(pwd)/web:/web --name $NAME $NAME
```

* tweak directory structure
```sh
for D in /web/html/css /web/html/js /web/cgi-bin/JSON /web/cgi-bin/YAML ; do
``
* copy individual files: frontend
```
docker cp html/inventory.html $NAME:web/html
docker cp css/main.css $NAME:web/html/css
docker cp js/script.js $NAME:web/html/js
```
* backend
```sh
docker cp cgi-bin/list.cgi $NAME:web/cgi-bin
docker cp cgi-bin/table.cgi $NAME:web/cgi-bin	
docker cp cgi-bin/select.cgi $NAME:web/cgi-bin	
docker cp JSON/PP.pm $NAME:web/cgi-bin/JSON
docker cp YAML/Tiny.pm $NAME:web/cgi-bin/YAML
```
```sh
docker exec $NAME chmod 775 /web/cgi-bin/list.cgi
docker exec $NAME chmod 775 /web/cgi-bin/table.cgi
docker exec $NAME chmod 775 /web/cgi-bin/select.cgi
```
* run smoke test
run cgi directly:
```sh
docker exec $NAME /web/cgi-bin/list.cgi
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
docker exec $NAME /web/cgi-bin/table.cgidocker exec $NAME /web/cgi-bin/table.cgi
```
with
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
docker exec $NAME /web/cgi-bin/select.cgi
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
<script src="https://ajax.googleapis.com/ajax/libs/angularjs/1.5.8/angular.js">
</script>
<link rel="stylesheet" href="/css/main.css"/>
</script>
<script type="text/javascript" src="/js/script.js">
</script>
</head>
<div ng-controller="ListController">
  Data: <br />
  <ul>
    <li ng-repeat="row in rows">{{ row.text }}</li>
  </ul>
</div>
<div ng-controller="SelectController">
  <select ng-model="selectedName" ng-options="item for item in names"></select>
</div>



<div ng-controller="TableController">
<table class="table-bordered">
  <tr data-ng-repeat="row in rows">
    <td>{{ row.column1 }}</td>
    <td>{{ row.column2 }}</td>
    <td>{{ row.column3 }}</td>
  </tr>
</table>
</div>
</body>
</html>
```
 need to run in the browser to see

![Example](https://github.com/sergueik/springboot_study/blob/master/basic-perl-cgi/screenshots/capture.png)

 - opened developer tools tab shows that the Angular is doing polling the server
### Cleanup

```sh
docker stop $NAME
docker container prune -f
docker image prune -f
docker image rm $NAME
```
### See Also
  * https://stackoverflow.com/questions/19408011/angularjs-error-argument-firstctrl-is-not-a-function-got-undefined/19408070
  * https://stackoverflow.com/questions/13671031/server-polling-with-angularjs
  * https://blog.guya.net/2016/08/08/simple-server-polling-in-angularjs-done-right/
  * https://www.js-tutorials.com/angularjs-tutorial/simple-example-angularjs-interval-timeout/
  * https://stackoverflow.com/questions/42701048/how-to-pass-vm-to-a-settimeout-in-angularjs-changes-to-scope-dont-update-dom-v

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
