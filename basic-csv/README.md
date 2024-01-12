###  Info

example based on [introduction to Apache Commons CSV](https://www.baeldung.com/apache-commons-csv)

### Usage

```sh
curl -sX POST http://localhost:8085/data -d "author,title
Dan Simmons,Hyperion
Douglas Adams,The Hitchhiker's Guide to the Galaxy
" -H 'Content-type: application/x-www-form-urlencoded'
```
```text
"[]"

```
```sh
curl -sX POST -T src/test/resources/book.csv  http://localhost:8085/data -H 'Content-type: application/x-www-form-urlencoded'
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

### See Also

https://github.com/apache/commons-csv
