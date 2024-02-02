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
2024-01-11 20:46:10.243  INFO 13116 --- [nio-8085-exec-4] example.controller.TypedController       : Decoded Body: "author,title
Dan Simmons,Hyperion
Douglas Adams,The Hitchhiker's Guide to the Galaxy
="
2024-01-11 20:46:10.243  INFO 13116 --- [nio-8085-exec-4] example.controller.TypedController       : Before Reading:

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

2024-01-11 20:11:26.681  INFO 12896 --- [nio-8085-exec-3] example.controller.TypedController       : Body: "author%2Ctitle%0ADan+Simmons%2CHyperion%0ADouglas+Adams%2CThe+Hitchhiker%27s+Guide+to+the+Galaxy%0A="
2024-01-11 20:11:26.683  INFO 12896 --- [nio-8085-exec-3] example.controller.TypedController       : Before Reading:

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
2024-01-11 20:27:26.162  INFO 13980 --- [nio-8085-exec-2] example.controller.TypedController       : Body: "author,title,year,isbn
Dan Simmons,Hyperion,,978-8576576013
Douglas Adams,"The Hitchhiker's Guide to the Galaxy",1979,978-0345391803
Lynne Truss,"Eats, Shoots and Leaves",2003,978-1861976123"
2024-01-12 14:44:58.283  INFO 2816 --- [nio-8085-exec-1] example.controller.TypedController       : Before Reading:
2024-01-12 14:44:58.284  INFO 2816 --- [nio-8085-exec-1] example.controller.TypedController       : Reading:
2024-01-12 14:44:58.285  INFO 2816 --- [nio-8085-exec-1] example.controller.TypedController       : Read: Data {title=Hyperion author=Dan Simmons}
2024-01-12 14:44:58.285  INFO 2816 --- [nio-8085-exec-1] example.controller.TypedController       : Reading:
2024-01-12 14:44:58.286  INFO 2816 --- [nio-8085-exec-1] example.controller.TypedController       : Data year: "1979"
2024-01-12 14:44:58.288  INFO 2816 --- [nio-8085-exec-1] example.controller.TypedController       : Read: Data {title=The Hitchhiker's Guide to the Galaxy author=Douglas Adams year=1979}
2024-01-12 14:44:58.288  INFO 2816 --- [nio-8085-exec-1] example.controller.TypedController       : Reading:
2024-01-12 14:44:58.289  INFO 2816 --- [nio-8085-exec-1] example.controller.TypedController       : Data year: "2003"
2024-01-12 14:44:58.289  INFO 2816 --- [nio-8085-exec-1] example.controller.TypedController       : Read: Data {title=Eats, Shoots and Leaves author=Lynne Truss year=2003}
```
and return
```json
[
  {
    "status": false,
    "author": "Dan Simmons",
    "title": "Hyperion"
  },
  {
    "status": false,
    "author": "Douglas Adams",
    "title": "The Hitchhiker's Guide to the Galaxy"
  },
  {
    "status": false,
    "author": "Lynne Truss",
    "title": "Eats, Shoots and Leaves"
  }
]

```

```sh
curl -sX POST http://localhost:8085/data -d "author,title,year,isbn
Dan Simmons,Hyperion,,978-8576576013
Douglas Adams,\"The Hitchhiker's Guide to the Galaxy\",1979,978-0345391803
Lynne Truss,\"Eats, Shoots and Leaves\",2003,978-1861976123" -H 'Content-type: application/octet-stream'
```
will also succeed:
```json
[
  {
    "status": false,
    "author": "Dan Simmons",
    "title": "Hyperion"
  },
  {
    "status": false,
    "author": "Douglas Adams",
    "title": "The Hitchhiker's Guide to the Galaxy"
  },
  {
    "status": false,
    "author": "Lynne Truss",
    "title": "Eats, Shoots and Leaves"
  }
]

```
```sh
curl -sX POST http://localhost:8085/data -d "author,title
Dan Simmons,Hyperion
Douglas Adams,The Hitchhiker's Guide to the Galaxy
" -H 'Content-type: application/octet-stream'
```
will also succeed, though not every columns defined in theconsumer


```java
public static final String[] HEADERS = { "author", "title", "year", "isbn" };
CSVFormat csvFormat = CSVFormat.DEFAULT.builder().setHeader(HEADERS).setSkipHeaderRecord(true).build();
```
have been present in the payload:
```json
[
  {
    "status": false,
    "author": "Dan Simmons",
    "title": "Hyperion"
  },
  {
    "status": false,
    "author": "Douglas Adams",
    "title": "The Hitchhiker's Guide to the Galaxy"
  }
]
```

```sh
curl -s -X POST "http://localhost:8085/upload?operation=send&param=something&servername=localhost" -F file=@/c/temp/book.csv
```

```json
[
  {
    "status": false,
    "author": "Dan Simmons",
    "title": "Hyperion",
    "year": 0
  },
  {
    "status": false,
    "author": "Douglas Adams",
    "title": "The Hitchhiker's Guide to the Galaxy",
    "year": 1979
  },
  {
    "status": false,
    "author": "Lynne Truss",
    "title": "Eats, Shoots and Leaves",
    "year": 2003
  }
]

```
### Powershell
```powershell
. .\send_csvdata.ps1 -url 'http://localhost:8085/upload'
```
```text

             status author              title                              year
             ------ ------              -----                              ----
              False Dan Simmons         Hyperion                              0
              False Douglas Adams       The Hitchhiker's...                1979
              False Lynne Truss         Eats, Shoots and...                2003

```
### Uploading Through the Browser
alternatively upload the `book.csv` through the browser (the upload endpoint  accepts any CSV data but internally tries to construct a `Book` object from it, so some specic fields presence in the CSV is required (the `author`, the `title`, and the `year`)

the csv contents are returnd by the end point  for debugging, as JSON payload, but currentlty only visible through Developer Tools:
```JSON
[
  {
    "status": false,
    "author": "Dan Simmons",
    "title": "Hyperion",
    "year": 0
  },
  {
    "status": false,
    "author": "Douglas Adams",
    "title": "The Hitchhiker's Guide to the Galaxy",
    "year": 1979
  },
  {
    "status": false,
    "author": "Lynne Truss",
    "title": "Eats, Shoots and Leaves",
    "year": 2003
  }
]

```

![Broswer](https://github.com/sergueik/springboot_study/blob/master/basic-csv/screenshots/capture-uploadpage.png)

#### NOTE
Without the thymeleaf dependency attempt to add the `/upload` endpoint rendering `upload.html` will fail with
```text
javax.servlet.ServletException: Circular view path [upload]: would dispatch back to the current handler URL [/upload] again. Check your ViewResolver setup! (Hint: This may be the result of an unspecified view, due to default view name generation.)
```
### See Also
   * https://www.baeldung.com/spring-url-encoded-form-data
   * [Pure-perl CVS module](https://metacpan.org/pod/Text::CSV_PP)

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
