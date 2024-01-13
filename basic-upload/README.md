### Info

Basic POST `multipart-`handler REST server from __File Upload with Spring MVC__  [tutorial](https://www.baeldung.com/spring-file-upload) converted from MVN to REST methid and sans the exception handler to test Powershell client exmple suggested in __How to send multipart/form-data with PowerShell__ [post](https://newbedev.com/how-to-send-multipart-form-data-with-powershell-invoke-restmethod)
 and one suggested in [post](https://stackoverflow.com/questions/36268925/powershell-invoke-restmethod-multipart-form-data)

### Usage

* launch the server
```sh
mvn spring-boot:test
```

create a *small*  text file `c:\temp\pstest.log`:
```text
test data
```

* test through curl / git bash:

```sh
curl -s -X POST http://localhost:8085/basic/upload -F file=@/c/temp/pstest.log
```

this will respond with
```JSON
{
  "timestamp": "2024-01-12T23:30:55.983+00:00",
  "status": 400,
  "error": "Bad Request",
  "message": "",
  "path": "/basic/upload"
}
```

and log the error in applicaton console:
```text
2024-01-12 18:28:19.796  WARN 8948 --- [nio-8085-exec-1] .w.s.m.s.DefaultHandlerExceptionResolver : Resolved [org.springframework.web.bind.MissingServletRequestParameterException: Required String parameter 'operation' is not present]
2024-01-12 18:28:19.796  WARN 8948 --- [nio-8085-exec-1] .w.s.m.s.DefaultHandlerExceptionResolver : Resolved [org.springframework.web.bind.MissingServletRequestParameterException: Required String parameter 'param' is not present]
2024-01-12 18:28:38.099  WARN 8948 --- [nio-8085-exec-3] .w.s.m.s.DefaultHandlerExceptionResolver : Resolved [org.springframework.web.HttpMediaTypeNotSupportedException: Content type '' not supported]
2024-01-12 18:29:02.344  WARN 8948 --- [nio-8085-exec-4] .w.s.m.s.DefaultHandlerExceptionResolver : Resolved [org.springframework.web.bind.MissingServletRequestParameterException: Required String parameter 'servername' is not present]
```
after adding the required parameters to query string (`operation` needs to be `send`, other parameters can have arbitrary values)
```sh
curl -s -X POST "http://localhost:8085/basic/upload?operation=send&param=something&servername=localhost" -F file=@/c/temp/pstest.log
```

curl will successfully print back the file contents:

```text
test data
```
and log
```text
2024-01-12 18:29:26.619  INFO 8948 --- [nio-8085-exec-5] example.controller.Controller            : Processing pstest.log
2024-01-12 18:29:26.628  INFO 8948 --- [nio-8085-exec-5] example.controller.Controller            : data : test data

```
* test through Powershell / powershell console:
```powershell
. .\client1.ps1 -filepath C:\temp\pstest.txt -url 'http://localhost:8085/basic/upload'
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

  * [Multipart Request Handling in Spring](https://www.baeldung.com/sprint-boot-multipart-requests)
  * [Spring Boot File upload example with Multipart File](https://www.bezkoder.com/spring-boot-file-upload/)
  * [testing a Spring Multipart POST Request](https://www.baeldung.com/spring-multipart-post-request-test)
  * [MockMvcRequestBuilders class methods](https://docs.spring.io/spring-framework/docs/current/javadoc-api/org/springframework/test/web/servlet/request/MockMvcRequestBuilders.html)

  * REST pagination [article](https://www.baeldung.com/rest-api-pagination-in-spring) 
  * handling partial content [forum question](https://qna.habr.com/q/1258736)(in Russian), with code example
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
