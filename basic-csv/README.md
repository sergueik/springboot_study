###  Info

example based on [introduction to Apache Commons CSV](https://www.baeldung.com/apache-commons-csv)

### Usage
```sh
mvn test
```
```sh
mvn spring-boot:run
```

```sh
curl -sX POST http://localhost:8085/encodeddata -d "author,title
Dan Simmons,Hyperion
Douglas Adams,The Hitchhiker's Guide to the Galaxy
" -H 'Content-type: application/x-www-form-urlencoded'
```
```text
"[]"

```
```text
 curl -sX POST -T src/test/resources/book.csv  http://localhost:8085/encodeddata -H 'Content-type: application/x-www-form-urlencoded'
```
```text
"[]"
```
will log (NOTE the trailing `=` in the "decoded body"):
```text
2024-01-11 20:46:10.243  INFO 13116 --- [nio-8085-exec-4] example.controller.Typ
edController       : Decoded Body: "author,title
Dan Simmons,Hyperion
Douglas Adams,The Hitchhiker's Guide to the Galaxy
="
2024-01-11 20:46:10.243  INFO 13116 --- [nio-8085-exec-4] example.controller.Typ
edController       : Before Reading:

```

```sh
curl -sX POST -T src/test/resources/book.csv  http://localhost:8085/data -H 'Content-type: multipart/form-data'
```
this leads to internal server error
```text

org.apache.tomcat.util.http.fileupload.FileUploadException: the request was rejected because no multipart boundary was found
```
```json
{"timestamp":"Jan 11, 2024 8:08:20 PM","status":500,"error":"Internal Server Error","message":"","path":"/data"}
```


and
```sh
curl -sX POST -T src/test/resources/book.csv  http://localhost:8085/data -H 'Content-type: application/x-www-form-urlencoded'
```
is failing to be CSV parsed:  the iterator shows no data was found
```text

2024-01-11 20:11:26.681  INFO 12896 --- [nio-8085-exec-3] example.controller.Typ
edController       : Body: "author%2Ctitle%0ADan+Simmons%2CHyperion%0ADouglas+Ad
ams%2CThe+Hitchhiker%27s+Guide+to+the+Galaxy%0A="
2024-01-11 20:11:26.683  INFO 12896 --- [nio-8085-exec-3] example.controller.Typ
edController       : Before Reading:

```
```text
"[]"

```

```sh
curl -sX POST -T src/test/resources/book.csv http://localhost:8085/data
```
or 
```sh
curl -sX POST -T src/test/resources/book.csv  http://localhost:8085/data  -H 'Content-type: application/octet-stream'
```
will log
```text
2024-01-11 20:34:16.584  INFO 13980 --- [nio-8085-exec-1] example.controller.Typ
edController       : Body: "author,title
Dan Simmons,Hyperion
Douglas Adams,The Hitchhiker's Guide to the Galaxy
"
2024-01-11 20:34:16.585  INFO 13980 --- [nio-8085-exec-1] example.controller.Typ
edController       : Before Reading:
2024-01-11 20:34:16.585  INFO 13980 --- [nio-8085-exec-1] example.controller.Typ
edController       : Reading:
2024-01-11 20:34:16.586  INFO 13980 --- [nio-8085-exec-1] example.controller.Typ
edController       : Read: Data {title=Hyperion author=Dan Simmons}
2024-01-11 20:34:16.586  INFO 13980 --- [nio-8085-exec-1] example.controller.Typ
edController       : Reading:
2024-01-11 20:34:16.587  INFO 13980 --- [nio-8085-exec-1] example.controller.Typ
edController       : Read: Data {title=The Hitchhiker's Guide to the Galaxy auth
or=Douglas Adams}
```
and return
```json
"[{\"status\":false,\"author\":\"Dan Simmons\",\"title\":\"Hyperion\"},{\"status\":false,\"author\":\"Douglas Adams\",\"title\":\"The Hitchhiker\\u0027s Guide to the Galaxy\"}]"
```
```sh
curl -sX POST http://localhost:8085/data -d "author,title
Dan Simmons,Hyperion
Douglas Adams,The Hitchhiker's Guide to the Galaxy
" -H 'Content-type: application/octet-stream'
```
will also succeed:
```text
"[{\"status\":false,\"author\":\"Dan Simmons\",\"title\":\"Hyperion\"},{\"status\":false,\"author\":\"Douglas Adams\",\"title\":\"The Hitchhiker\\u0027s Guide to the Galaxy\"}]"

### See Also
   * https://www.baeldung.com/spring-url-encoded-form-data

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
