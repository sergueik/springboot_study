### Info

Basic POST `multipart-`handler REST server from __File Upload with Spring MVC__  [tutorial](https://www.baeldung.com/spring-file-upload) converted from MVN to REST methid and sans the exception handler to test Powershell client exmple suggested in __How to send multipart/form-data with PowerShell__ [post](https://newbedev.com/how-to-send-multipart-form-data-with-powershell-invoke-restmethod)
 and one suggested in [post](https://stackoverflow.com/questions/36268925/powershell-invoke-restmethod-multipart-form-data)

### Usage

* launch the server
```sh
mvn spring-boot:test
```

create a *small*  text file `c:\temp\pstest.log` (not in current directory):
```text
test data
```

* test single file upload through curl / git bash:

```sh
 curl  -v -s -X POST http://localhost:8085/basic/simpleupload -F "file=@/c/temp/pstest.log"

```
```text
* processing: http://localhost:8085/basic/simpleupload
* Uses proxy env variable no_proxy == '192.168.99.100,192.168.99.101'
*   Trying [::1]:8085...
* Connected to localhost (::1) port 8085
> POST /basic/simpleupload HTTP/1.1
> Host: localhost:8085
> User-Agent: curl/8.2.1
> Accept: */*
> Content-Length: 213
> Content-Type: multipart/form-data; boundary=------------------------2c8f3f65e97e6e05
>
} [213 bytes data]
* We are completely uploaded and fine
< HTTP/1.1 200
< Content-Length: 0
< Date: Fri, 01 Mar 2024 16:56:31 GMT
<
* Connection #0 to host localhost left intact
```
the `simpleuload` endpoint does not perform any argument validation

* atempt to upload a big file. Make `/c/temp/pstest.log` bigger (over 10K will be enough):
```sh
ls -lh /c/temp/pstest.log
```
```text
-rw-r--r-- 1 Serguei 197609 18K Mar  1 12:44 /c/temp/pstest.log
```
```sh
curl -s -X POST "http://localhost:8085/basic/simpleupload" -F file=@/c/temp/pstest.log
```
```json
{
  "timestamp": "2024-03-01T17:19:13.866+00:00",
  "status": 500,
  "error": "Internal Server Error",
  "message": "",
  "path": "/basic/simpleupload"
}

```
the application console log will show:
```text	
2024-03-01 12:46:39.752 ERROR 5392 --- [nio-8085-exec-6] o.a.c.c.C.[.[.[/].[dispatcherServlet]    : Servlet.service() for servlet [dispatcherServlet] in context with path [] threw exception [Request processing failed; 

nested exception is org.springframework.web.multipart.MaxUploadSizeExceededException: Maximum upload size exceeded; 

nested exception is java.lang.IllegalStateException: org.apache.tomcat.util.http.fileupload.impl.SizeLimitExceededException: the request was rejected because its size (18232) exceeds the configured maximum (10240)] with root cause

org.apache.tomcat.util.http.fileupload.impl.SizeLimitExceededException: the request was rejected because its size (18232) exceeds the configured maximum (10240)
```
* re-create a small file to proceed
```sh
curl -s -X POST http://localhost:8085/basic/upload -F "file=@/c/temp/pstest.log"
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

application logs the following error in applicaton console:
```text
2024-01-12 18:28:19.796  WARN 8948 --- [nio-8085-exec-1] .w.s.m.s.DefaultHandlerExceptionResolver : Resolved [org.springframework.web.bind.MissingServletRequestParameterException: Required String parameter 'operation' is not present]
2024-01-12 18:28:19.796  WARN 8948 --- [nio-8085-exec-1] .w.s.m.s.DefaultHandlerExceptionResolver : Resolved [org.springframework.web.bind.MissingServletRequestParameterException: Required String parameter 'param' is not present]
2024-01-12 18:28:38.099  WARN 8948 --- [nio-8085-exec-3] .w.s.m.s.DefaultHandlerExceptionResolver : Resolved [org.springframework.web.HttpMediaTypeNotSupportedException: Content type '' not supported]
2024-01-12 18:29:02.344  WARN 8948 --- [nio-8085-exec-4] .w.s.m.s.DefaultHandlerExceptionResolver : Resolved [org.springframework.web.bind.MissingServletRequestParameterException: Required String parameter 'servername' is not present]
```
after adding the required parameters to query string (`operation` parameter value needs to be `send`, other parameters can have arbitrary values)
```sh
curl -s -X POST "http://localhost:8085/basic/upload?operation=send&param=something&servername=localhost" -F "file=@/c/temp/pstest.log"
```

`curl` will successfully print back the file contents:

```text
test data
```
and the console log will show
```text
2024-01-12 18:29:26.619  INFO 8948 --- [nio-8085-exec-5] example.controller.Controller            : Processing pstest.log
2024-01-12 18:29:26.628  INFO 8948 --- [nio-8085-exec-5] example.controller.Controller            : data : test data
```

NOTE: the `-F` parameter is for multipart MIME data. To see list of supported options for curl in git bash enter
```sh
curl --help all
```

```sh
curl -s -X POST "http://localhost:8085/basic/upload?operation=send&param=something&servername=localhost" -F file=@/c/temp/pstest.log
```
```json
{
  "timestamp": "2024-03-01T17:11:37.729+00:00",
  "status": 500,
  "error": "Internal Server Error",
  "message": "",
  "path": "/basic/upload"
}
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

### NOTE

the `org.springframework.mock.web.MockMultipartFile` class does not read `applicatio.properties` for 
```java
spring.servlet.multipart.max-request-size = 10KB
```
therefore to test the `org.springframework.web.multipart.MaxUploadSizeExceededException` exception one needs to upload a real file

### Background Info

the original flow was effectively:
```
browser uploads binary
 ->
server converts to base64
 ->
server returns base64 string
 ->
React stores giant string in browser memory
 ->
user clicks upload
 ->
browser sends giant JSON payload
 ->
backend decodes again
 ->
processing
```
Meaning:

the file crosses the network **twice**

and

gets transformed unnecessarily in the middle.

So the system is paying for:

|Step	|Cost|
|-------|----|
|upload raw file	|expensive |
|encode to base64	|CPU |
|return base64 to browser	|very expensive |
|React/browser stores payload	|memory |
|upload base64 JSON again|	extremely expensive|
|decode base64	|CPU|
|actual work	|finally|


```sh
curl \
  -F "foo=alpha" \
  -F "bar=beta" \
  -F "file=@input.dat" \
  http://host/upload
```
is automatically multipart.

The traditional web page 
```html
<form enctype="multipart/form-data" method="post">
  <input type="file" name="file">
  <input type="submit">
</form>
``` 

instructs the browser to  handled:

* multipart boundaries
* streaming
* buffering
* content disposition
* filenames
* upload state
* memory management

which is does automatically - The developer barely had to think about transport mechanics

SPA frameworks shifted responsibility into JavaScript:

browser
 ->
JS application runtime
 ->
virtual page construction
 ->
state orchestration
 ->
custom API choreography

So suddenly frontend code became responsible for:

transport
serialization
retries
orchestration
upload state
encoding choices

That enabled:

richer UX
dynamic interactions
client-side workflows

but also encouraged:

reinventing lower-level mechanisms poorly.
3. File upload is one of the clearest examples

Old world:

<form enctype=multipart/form-data>

Done.

Modern world:

FileReader
ArrayBuffer
Blob
base64
fetch
Redux state
upload progress
JSON wrapping

Many more moving parts.

Sometimes for legitimate UX reasons.
Sometimes because:

the frontend became a general-purpose application platform.
4. Ironically browsers were already very good at uploads

Native multipart upload had decades of optimization:

streaming
low memory overhead
interoperability
content-type handling

And tools like curl naturally matched that model.

Which is why your earlier instinct:

multipart was trivial for clients

was correct in the traditional web architecture sense.

5. React did not create the problem alone

The deeper shift was:

“everything becomes an API”

Once backend communication became:

JSON-first
SPA-driven
transport-abstracted

teams began preferring:

single payload shape
DTO uniformity
OpenAPI friendliness
state-centric UI models

even when binary transport suffered.

6. Your system is almost a symptom of that evolution

The flow you described:

file
 ->
base64
 ->
JSON
 ->
workflow trigger

feels less like:

HTTP upload

and more like:

remote procedure invocation with embedded payload.

That is a very “post-form-browser” design mentality.

7. There is a broader historical irony here

Older “primitive” web stacks often had:

simpler transport
fewer copies
less memory pressure
better streaming semantics

because they leaned on:

browser-native protocol machinery.

Modern stacks gained:

flexibility
interactivity
composability

but sometimes lost:

transport discipline.
8. Your educational upload project is actually illustrating this evolution

You now effectively have:

Style	Era
multipart form upload	classic web
JSON/base64 DTO upload	SPA/API era
synthetic SDK caller	automation/integration era

That is a surprisingly useful comparison framework.

9. One subtle point

React itself does NOT require base64 uploads.

Modern browsers can still upload multipart via:

FormData
fetch
streaming APIs

So:

the architecture choice was cultural/organizational,

not technically mandated by React.

That distinction matters.

### See Also

  * [Multipart Request Handling in Spring](https://www.baeldung.com/sprint-boot-multipart-requests)
  * [Spring Boot File upload example with Multipart File](https://www.bezkoder.com/spring-boot-file-upload/)
  * [testing a Spring Multipart POST Request](https://www.baeldung.com/spring-multipart-post-request-test)
  * [MockMvcRequestBuilders class methods](https://docs.spring.io/spring-framework/docs/current/javadoc-api/org/springframework/test/web/servlet/request/MockMvcRequestBuilders.html)

  * REST pagination [article](https://www.baeldung.com/rest-api-pagination-in-spring)
  * handling partial content [forum question](https://qna.habr.com/q/1258736)(in Russian), with code example
  * [testing a Spring Multipart POST Request](https://www.baeldung.com/spring-multipart-post-request-test)
  * https://stackoverflow.com/questions/21800726/using-spring-mvc-test-to-unit-test-multipart-post-request

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
