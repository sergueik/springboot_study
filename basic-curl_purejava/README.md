### Info

copy of [pure-java curl utility](https://github.com/rockswang/java-curl) switched to use JDK 1.8 and JDK 11.
Curious case of failing unit tests

### Usage 

* currently all tests are failing, so need to skip explicitly. Note, every test is also marked with the `@Ignore`

```sh
mvn -Dmaven.test.skip=true clean package
```
 * get request, HTML
```sh
java -jar target/example.java-curl.jar -k https://httpbin.org/html
```
```text
[ERR] [2022-02-04 20:02:32.069] Load default trust manager
[ERR] [2022-02-04 20:02:32.987] Skip TLS validation
```
```html

[ERR] [2022-02-05 02:24:26.325] Load default trust manager
[ERR] [2022-02-05 02:24:26.928] Skip TLS validation
<!DOCTYPE html>
<html>
  <head>
  </head>
  <body>
      <h1>Herman Melville - Moby-Dick</h1>

      <div>
Availing himself of the mild, summer-cool weather ...
```
this can be processed with Powershell:
```powerhell
$rawresult = invoke-expression -command "java.exe -jar target\example.java-curl.jar -k https://httpbin.org/html"

[xml]$result_object = ( $rawresult -join '')
write-output $result_object.html.body.h1
```
```text
Herman Melville - Moby-Dick div
```
or
```powershell
write-output $result_object.html.body.selectNodes('//h1[1]').'#text'
```
```text
Herman Melville - Moby-Dick
```
* get request returning JSON:
```sh
java -jar target/example.java-curl.jar -k https://httpbin.org/json
```


```json
{
  "slideshow": {
    "author": "Yours Truly",
    "date": "date of publication",
    "slides": [
      {
        "title": "Wake up to WonderWidgets!",
        "type": "all"
      },
      {
        "items": [
          "Why <em>WonderWidgets</em> are great",
          "Who <em>buys</em> WonderWidgets"
        ],
        "title": "Overview",
        "type": "all"
      }
    ],
    "title": "Sample Slide Show"
  }
}
```
* this can be processed by `jq`:
```sh
java -jar target/example.java-curl.jar -k https://httpbin.org/json| jq '.slideshow.title'
```
"Sample Slide Show"
```
* post
```sh
java -jar target/example.java-curl.jar -F hello=world -F foo=bar http://httpbin.org/post
```
```text
{
  "args": {},
  "data": "",
  "files": {},
  "form": {
    "foo": "bar",
    "hello": "world"
  },
  "headers": {
    "Accept": "*/*",
    "Cache-Control": "no-cache",
    "Content-Length": "204",
    "Content-Type": "multipart/form-data; boundary=------------aia113jBkadk7289"
,
    "Host": "httpbin.org",
    "Pragma": "no-cache",
    "User-Agent": "Java-CURL version 1.2.2 by Rocks Wang(https://github.com/rock
swang)",
    "X-Amzn-Trace-Id": "Root=1-61fdcd4f-0aac0f3c7125787249f997ab"
  },
  "json": null,
  "origin": "xxx.xxx.xxx.xxx",
  "url": "http://httpbin.org/post"
}

```

* file upload ( Windows example)
```sh
echo > a.txt
```
```sh
java -jar target/example.java-curl.jar -F hello=world -F foo=bar -F data=@a.txt -k https://httpbin.org/post
```
```json
{
  "args": {},
  "data": "",
  "files": {
    "data": "ECHO is on.\r\n"
  },
  "form": {
    "foo": "bar",
    "hello": "world"
  },
  "headers": {
    "Accept": "*/*",
    "Cache-Control": "no-cache",
    "Content-Length": "375",
    "Content-Type": "multipart/form-data; boundary=------------aia113jBkadk7289"
,
    "Host": "httpbin.org",
    "Pragma": "no-cache",
    "User-Agent": "Java-CURL version 1.2.2 by Rocks Wang(https://github.com/rockswang)",
    "X-Amzn-Trace-Id": "Root=1-61fdcdc2-17ba25461284c7d94b2e267c"
  },
  "json": null,
  "origin": "xxx.xxx.xxx.xxx",
  "url": "http://httpbin.org/post"
}
```

* run in verbose mode

```sh
java -jar target/example.java-curl.jar -k https://httpbin.org/html --verbose --head
```
```text
[ERR] [2022-02-04 22:49:44.993] Load default trust manager
[ERR] [2022-02-04 22:49:45.681] Prepare open connection - URL.openConnection()
[ERR] [2022-02-04 22:49:45.886] Done prepare open connection
[ERR] [2022-02-04 22:49:45.889] Skip TLS validation
[ERR] [2022-02-04 22:49:45.891] Prepare headers
[ERR] [2022-02-04 22:49:45.894] Done preparing headers
[ERR] [2022-02-04 22:49:45.898] Get HTTP Response Code.
[ERR] [2022-02-04 22:49:47.536] Set response code: 200
[ERR] [2022-02-04 22:49:47.542] HTTP Response Code: 200
[ERR] [2022-02-04 22:49:47.543] Getting input stream
[ERR] [2022-02-04 22:49:47.545] Start reading output
[ERR] [2022-02-04 22:49:47.547] Done reading output
```

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
