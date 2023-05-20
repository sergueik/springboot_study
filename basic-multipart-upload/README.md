### Info

Basic POST `multipart-`handler REST server from __Spring Boot File Upload / Download Rest API Example__  [repository](https://github.com/callicoder/spring-boot-file-upload-download-rest-api-example)

with a static, thymeleaf and JSP landing pages
supporting `/uploadFile` and `/uploadMultipleFiles` end points and also the `/downloadFile/{fileName}`

### Usage

* launch the server
```sh
mvn spring-boot:test
```

create a *small*  text file `test.txt`:
```sh
echo 'this is a test' > test.txt
```

* test through curl in bash or git bash:

```sh
curl -X POST http://localhost:8080/uploadFile -F file=@$(pwd)/test.txt
```
it will print to console

```text
{
  "fileName": "test.txt",
  "fileDownloadUri": "http://localhost:8080/downloadFile/test.txt",
  "fileType": "text/plain",
  "size": 15
}

```
get the file back:

```sh
curl -s -X GET http://localhost:8080/downloadFile/test.txt
```

this will print to console
```text
this is a test
```

NOTE: the `downloadFile` method is still using `javax.servlet.http.HttpServletRequest`

### Upload Multiple Files
* create few dummy files
```sh
for i in $(seq 1 1 4) ; do echo "data $i" >  $i.txt; done
```
* use legacy page with repeated "files" input for multple files
![Mutlple Files](https://github.com/sergueik/springboot_study/blob/master/basic-multipart-upload/screenshots/capture-legacy-upload-files.png)

After page is  submitted the result is displayed:
```json
[
  {
    "fileName": "1.txt",
    "fileDownloadUri": "http://localhost:8080/downloadFile/1.txt",
    "fileType": "text/plain",
    "size": 7
  },
  {
    "fileName": "2.txt",
    "fileDownloadUri": "http://localhost:8080/downloadFile/2.txt",
    "fileType": "text/plain",
    "size": 7
  },
  {
    "fileName": "3.txt",
    "fileDownloadUri": "http://localhost:8080/downloadFile/3.txt",
    "fileType": "text/plain",
    "size": 7
  },
  {
    "fileName": null,
    "fileDownloadUri": null,
    "fileType": null,
    "size": 0
  }
]
```
 
* use Angular page to pass multiple file arguments (still work in progress):

![Mutlple Files](https://github.com/sergueik/springboot_study/blob/master/basic-multipart-upload/screenshots/capture-upload-files.png)

the page will show the backend response

![Mutlple Files](https://github.com/sergueik/springboot_study/blob/master/basic-multipart-upload/screenshots/capture-drag-and-drop-upload-files.png)

the console log will show

```text
2023-05-19 20:01:28.789  INFO 9908 --- [nio-8080-exec-2] example.controller.File
UploadController  : upload 3 files: [a.txt, b.txt, c.txt]
2023-05-19 20:01:28.789  INFO 9908 --- [nio-8080-exec-2] example.controller.File
UploadController  : upload file: a.txt
2023-05-19 20:01:28.851  INFO 9908 --- [nio-8080-exec-2] example.controller.File
UploadController  : upload file: b.txt
2023-05-19 20:01:28.851  INFO 9908 --- [nio-8080-exec-2] example.controller.File
UploadController  : upload file: c.txt
```

all files appear in the download directory:
```powershell
dir C:\temp\a.txt,C:\temp\b.txt,C:\temp\c.txt

```
```text

    Directory: C:\temp


Mode                LastWriteTime         Length Name
----                -------------         ------ ----
-a----        5/19/2023   7:56 PM              4 a.txt
-a----        5/19/2023   7:56 PM              4 b.txt
-a----        5/19/2023   7:56 PM              6 c.txt

```

### Usage (Untested)
* optionally test through Powershell / powershell console:
```powershell
. .\client1.ps1 -filepath (resolve-path 'test.txt' -url 'http://localhost:8080/uploadFile'
```
```text
Version             : 1.1
Content             : System.Net.Http.StreamContent
StatusCode          : OK
ReasonPhrase        :
Headers             : {[Date, System.String[]]}
RequestMessage      : Method: POST, RequestUri:
                      'http://localhost:8085/basic/upload', Version: 1.1,
                      Content: System.Net.Http.MultipartFormDataContent,
                      Headers:
                      {
                        Content-Type: multipart/form-data;
                      boundary="39a3ee01-b0f9-4c90-95e7-5d34f45994a8"
                        Content-Length: 189
                      }
IsSuccessStatusCode : True
```
```powershell
. .\client2.ps1 -filepath C:\temp\pstest.txt -url 'http://localhost:8085/basic/upload'
```
```text
Invoke-RestMethod -Uri http://localhost:8085/basic/upload -Method Post -ContentType "multipart/form-data; boundary=`"a1abe753-03af-4116-b5b5-799781773e42`"" -Body --a1abe753-03af-4116-b5b5-799781773e42
Content-Disposition: form-data; name="file"; filename="temp.txt"
Content-Type: application/octet-stream

test data

--1ec7a4ed-92ea-496b-81da-3afda4355c7f--


```


* see in all three cases the backend prints and saves the file:
```text
2021-10-06 21:22:45.676  INFO 9728 --- [           main] example.ExampleApplication               : Started ExampleApplication in 3.965 seconds (JVM running for 15.892)
2021-10-06 21:22:49.905  INFO 9728 --- [nio-8085-exec-1] o.a.c.c.C.[Tomcat].[localhost].[/]       : Initializing Spring FrameworkServlet 'dispatcherServlet'
2021-10-06 21:22:49.908  INFO 9728 --- [nio-8085-exec-1] o.s.web.servlet.DispatcherServlet        : FrameworkServlet 'dispatcherServlet': initialization started
2021-10-06 21:22:49.994  INFO 9728 --- [nio-8085-exec-1] o.s.web.servlet.DispatcherServlet        : FrameworkServlet 'dispatcherServlet': initialization completed in 83 ms


Processing pstest.txt
test data

Processing pstest.txt
test data
Processing temp.txt
test data
```

with only difference is how Powershell client scripts transmit the file.

### See Also
  * [file Upload with Spring MVC](https://www.baeldung.com/spring-file-upload)
  * [Multipart Request Handling in Spring](https://www.baeldung.com/sprint-boot-multipart-requests)
  * [Spring Boot File upload example with Multipart File](https://www.bezkoder.com/spring-boot-file-upload/)
  * [testing a Spring Multipart POST Request](https://www.baeldung.com/spring-multipart-post-request-test)
  * [MockMvcRequestBuilders class methods](https://docs.spring.io/spring-framework/docs/current/javadoc-api/org/springframework/test/web/servlet/request/MockMvcRequestBuilders.html)

  * REST pagination [article](https://www.baeldung.com/rest-api-pagination-in-spring) 
  * handling partial content [forum question](https://qna.habr.com/q/1258736)(in Russian), with code example
  * [guide](https://www.baeldung.com/spring-enable-config-properties) to `@EnableConfigurationProperties`
   
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
