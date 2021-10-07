### Info

Basic POST `multipart-`handler REST server from __File Upload with Spring MVC__  [tutorial](https://www.baeldung.com/spring-file-upload) converted from MVN to REST methid and sans the exception handler to test Powershell client exmple suggested in __How to send multipart/form-data with PowerShell__ [post](https://newbedev.com/how-to-send-multipart-form-data-with-powershell-invoke-restmethod)
 and one suggested in [post](https://stackoverflow.com/questions/36268925/powershell-invoke-restmethod-multipart-form-data)

### Usage

* launch the server
```sh
mvn spring-boot:test
```

create a *small*  text file:
```text
test data
```

* test through curl / git bash:

```sh
curl -X POST  http://localhost:8085/basic/upload -F file=@/c/temp/pstest.log
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
Invoke-RestMethod -Uri http://localhost:8085/basic/upload -Method Post -ContentT
ype "multipart/form-data; boundary=`"a1abe753-03af-4116-b5b5-799781773e42`"" -Bo
dy --a1abe753-03af-4116-b5b5-799781773e42
Content-Disposition: form-data; name="file"; filename="temp.txt"
Content-Type: application/octet-stream
```


see in all three cases the backend prints and saves the file:
```text
2021-10-06 21:22:45.676  INFO 9728 --- [           main] example.ExampleApplicat
ion               : Started ExampleApplication in 3.965 seconds (JVM running for
 15.892)
2021-10-06 21:22:49.905  INFO 9728 --- [nio-8085-exec-1] o.a.c.c.C.[Tomcat].[loc
alhost].[/]       : Initializing Spring FrameworkServlet 'dispatcherServlet'
2021-10-06 21:22:49.908  INFO 9728 --- [nio-8085-exec-1] o.s.web.servlet.Dispatc
herServlet        : FrameworkServlet 'dispatcherServlet': initialization started

2021-10-06 21:22:49.994  INFO 9728 --- [nio-8085-exec-1] o.s.web.servlet.Dispatc
herServlet        : FrameworkServlet 'dispatcherServlet': initialization complet
ed in 83 ms


Processing pstest.txt
test data

Processing pstest.txt
test data
Processing temp.txt
test data
```

with only difference is how Powershell client scripts transmit the file.

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
