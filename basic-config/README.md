### Usage

* compile and launch config server stub skipping unit tests
```cmd
mvn -Dmaven.test.skip=true spring-boot:run
```
```sh
curl -s http://localhost:8085/configs/xxx/list?newer=1690604340
```
```JSON
{"base:c":1690604632}
```
```sh
curl -s http://localhost:8085/configs/xxx/list
```
```JSON
{"a":1690604332,"b":1690604335,"base:c":1690604632}
```

```sh
curl -s  http://localhost:8085/configs/foo/load
```

```JSON
{"result":{},"status":"ok"}
```

```sh
curl -s http://localhost:8085/configs/foo/load?newer=1690604340
```
```JSON
{"result":"error message","status":"error"}
```
#### Hash and Newer Query String

```sh
curl -s "http://localhost:8085/configs/file_hash?filename=a.txt&hash=x"
```
```JSON
{
  "result":"hash",
  "status":"error"
}
```

```sh
curl -s "http://localhost:8085/configs/file_hash?filename=a.txt&newer=12345"
```
```JSON
{
  "result":"error message",
  "status":"error"
}
```

```sh
curl -sv "http://localhost:8085/configs/file_hash_status?filename=a.txt&hash=x"
```
```text
Host: localhost:8085
> User-Agent: curl/7.74.0
> Accept: */*
>
* Mark bundle as not supporting multiuse
< HTTP/1.1 304
< Date: Fri, 06 Oct 2023 00:09:54 GMT
<
* Connection #0 to host localhost left intact
```

```powershell
```
will print

```text
Status code: 304
Status code: 304
Body: ""
Exception (intercepted): The remote server returned an error: (304) Not Modified
.
Status Description:
Status code: 304
Status code: 304
HTTP Stasus: 304
```
several alternative ways of collecting the status are exercized in the Powershell script hence multiple redundant messages about the status

```sh
curl -sv "http://localhost:8085/configs/file_hash_status?filename=a.txt&newer=12345"
```
```text
HTTP/1.1 208
< Content-Type: application/json;charset=UTF-8
< Transfer-Encoding: chunked
< Date: Fri, 06 Oct 2023 00:10:23 GMT
<
{ [48 bytes data]
* Connection #0 to host localhost left intact

```sh
curl -s "http://localhost:8085/configs/file_hash_status?filename=a.txt&newer=12345"
```

```JSON
{
  "result":"newer: 12345",
  "status":"error"
}
```

```powershell

.\getstatuscode.ps1 -url  "http://localhost:8085/configs/file_hash_status?filename=a.txt&newer=12345"
```
will print
```text

Body: {
    "result":  "newer: 12345",
    "status":  "error"
}
HTTP Stasus: 208
processing result
processing status
ERROR: newer: 12345
```
several alternative ways of collecting the status are exercized in the Powershell script hence multiple redundant messages about the status and result


* successful case:
* create a JSON file in a app resource directory (the directory is defined in `config.dir` in `application.properties`, fallback to `C:\TEMP` for simplicity) and download it

```sh
curl -o data.json -s "http://localhost:8085/configs/file_hash?filename=data.json"
```
verify hashes to match
```sh
md5sum.exe ./data.json  /c/temp/data.json
```
```text
571076c15c60e93c3b4484f10e45499b *./data.json
571076c15c60e93c3b4484f10e45499b */c/temp/data.json
```

```sh
curl -o data.json -s "http://localhost:8085/configs/file_hash_status?filename=data.json"
```
in the java console will see the md5sum of the requested file logged (the actal logic of processing the hash is still WIP, the controller returns the error whenever the `hash` parameter is provided, ignoring the value of the query parameter):
```text
2023-10-06 22:10:09.645  INFO 4884 --- [nio-8085-exec-1] example.controller.Controller            : md5Sum of c:\temp\data.json: 571076c15c60e93c3b4484f10e45499b
4
```

```sh
md5sum.exe ./data.json  /c/temp/data.json
```
```text
571076c15c60e93c3b4484f10e45499b *./data.json
571076c15c60e93c3b4484f10e45499b */c/temp/data.json
```

```powershell
dir .\data.json,C:\temp\data.json
```
```text
`

    Directory: C:\developer\sergueik\springboot_study\basic-config


Mode                LastWriteTime         Length Name
----                -------------         ------ ----
-a----        10/6/2023   9:20 AM            148 data.json


    Directory: C:\temp


Mode                LastWriteTime         Length Name
----                -------------         ------ ----
-a----        10/6/2023   8:00 AM            148 data.json

```
```cmd
md5sum.exe ./data.json  /c/temp/data.json
```
0dfa1329f15fefa8648856794eb33244 *./data.json
0dfa1329f15fefa8648856794eb33244 */c/temp/data.json
```
alternatively
```cmd
c:\cygwin\bin\md5sum.exe c:\temp\data.json data.json
```
this will complain about the command line but confirm the hashes are identical:
```text
cygwin warning:
  MS-DOS style path detected: c:\temp\data.json
  Preferred POSIX equivalent is: /cygdrive/c/temp/data.json
  CYGWIN environment variable option "nodosfilewarning" turns off this warning.
  Consult the user's guide for more details about POSIX paths:
    http://cygwin.com/cygwin-ug-net/using.html#using-pathnames
\0dfa1329f15fefa8648856794eb33244 *c:\\temp\\data.json
0dfa1329f15fefa8648856794eb33244 *data.json
```

```cmd
c:\cygwin\bin\md5sum.exe /cygdrive/c/temp/data.json data.json
```
```text
0dfa1329f15fefa8648856794eb33244 */cygdrive/c/temp/data.json
0dfa1329f15fefa8648856794eb33244 *data.json
```
### Loading Config Providing Md5 Hash

```sh
curl -s "http://localhost:8085/configs/file_hash_status?filename=data.json"
```
```JSON
  {
  "host1": {
    "netstat": [
      22,
      443,
      3306
    ]
  },
  "host2": {
    "netstat": [
    ]
  },
  "host3": {}
}

```

```sh
curl -s "http://localhost:8085/configs/file_hash_status?filename=data.json&hash=0dfa1329f15fefa8648856794eb33241"
```
```JSON
  {
  "host1": {
    "netstat": [
      22,
      443,
      3306
    ]
  },
  "host2": {
    "netstat": [
    ]
  },
  "host3": {}
}

```


```sh
curl -vs "http://localhost:8085/configs/file_hash_status?filename=data.json&hash=0dfa1329f15fefa8648856794eb33244"
```
```text
HTTP/1.1 304
```

```sh
curl -s "http://localhost:8085/configs/file_hash?filename=data.json&hash=0dfa1329f15fefa8648856794eb33244"
```
```JSON
{"result":"hash","status":"error"}
```

### Updating Config File

```powershell
rm .\data.json

```
```powershell
.\getconfig3.ps1 -base_url http://localhost:8085/configs/file_hash_status

```
```text
GET http://localhost:8085/configs/file_hash_status?filename=data.json
invoke-restmethod -uri  -method GET -contenttype "application/json"
invoke-restmethod -uri  -method GET -contenttype "application/json" -OutFile C:\
Users\Serguei\AppData\Local\Temp\data.json
saved the server response into C:\Users\Serguei\AppData\Local\Temp\data.json
{ "host1": { "netstat": [  22,  443,  3306 ] }, "host2": { "netstat": [ ] }, "host3": {}}

```
```text
HTTP Stasus: 200
Body: { "host1": { "netstat": [ 22, 443, 3306 ] }, "host 2": { "netstat": [ ] }, "host3": {}}
```
```text
converting the page to JSON
Updating: C:\developer\sergueik\springboot_study\basic-config\data.json
{ "host1": { "netstat": [ 22, 443, 3306 ] }, "host2": { "netstat": [ ] }, "host3": {}}

```
```powershell
.\getconfig3.ps1 -base_url http://localhost:8085/configs/file_hash_status

```
```text

GET http://localhost:8085/configs/file_hash_status?filename=data.json&hash=0DFA1329F15FEFA8648856794EB33244

invoke-restmethod -uri  -method GET -contenttype "application/json"
invoke-restmethod -uri  -method GET -contenttype "application/json" -OutFile C:\
Users\Serguei\AppData\Local\Temp\data.json
Exception (intercepted): System.Net.WebException The remote server returned an e
rror: (304) Not Modified.
7 ProtocolError
Status code: 304
Status code: 304
HTTP Stasus: 304
```
(no longer supported)
```powershell
.\getconfig.ps1 -base_url http://localhost:8085/configs/file_hash
```
```text
GET http://localhost:8085/configs/file_hash?filename=data.json&hash=0DFA1329F15F
EFA8648856794EB33244
invoke-restmethod -uri -method GET -contenttype "application/json"
invoke-restmethod -uri -method GET -contenttype "application/json" -OutFile C:\
Users\Serguei\AppData\Local\Temp\data.json
saved the server response into C:\Users\Serguei\AppData\Local\Temp\data.json
{"result":"hash","status":"error"}
HTTP Stasus: 200
Body: {"result":"hash","status":"error"}
converting the page to JSON
ERROR: hash

```

* alternative version

```powershell
rm .\data.json
```

```powershell
.\getconfig4.ps1 -base_url http://localhost:8085/configs/file_hash_status
```
```text
GET http://localhost:8085/configs/file_hash_status?filename=data.json
Invoke-WebRequest -uri http://localhost:8085/configs/file_hash_status?filename=data.json -OutFile C:\Users\Serguei\AppData\Local\Temp\data.json -passthru
status code: 200
```
```text
saved the server response into C:\Users\Serguei\AppData\Local\Temp\data.json
{  "host1": {    "netstat": [      22,      443,      3306    ]  },  "host2": { "netstat": [    ]  },  "host3": {}}
```
```text
converting the page to JSON
Updating: C:\developer\sergueik\springboot_study\basic-config\data.json
```

```powershell
.\getconfig4.ps1 -base_url http://localhost:8085/configs/file_hash_status
GET http://localhost:8085/configs/file_hash_status?filename=data.json&hash=0DFA1329F15FEFA8648856794EB33244
Invoke-WebRequest -uri http://localhost:8085/configs/file_hash_status?filename=data.json&hash=0DFA1329F15FEFA8648856794EB33244 -OutFile C:\Users\Serguei\AppData\Local\Temp\data.json -passthru
Exception (intercepted): The remote server returned an error: (304) Not Modified.
```
```text
Status Description:
Status code: 304
Status code: 304
HTTP Stasus: 304
```


```powershell
rm .\data.json
```
(no longer supported)
```powershell
.\getconfig2.ps1 -base_url http://localhost:8085/configs/file_hash
```
```text

GET http://localhost:8085/configs/file_hash?filename=data.json
Invoke-WebRequest -uri http://localhost:8085/configs/file_hash?filename=data.json -OutFile C:\Users\Serguei\AppData\Local\Temp\data.json -passthru
status code: 200
saved the server response into C:\Users\Serguei\AppData\Local\Temp\data.json
{  "host1": {    "netstat": [      22,      443,      3306    ]  },  "host2": {   "netstat": [    ]  },  "host3": {}}

HTTP Stasus: 200
Body: {  "host1": {    "netstat": [      22,      443,      3306    ]  },  "host2": {    "netstat": [    ]  },  "host3": {}}
converting the page to JSON
Updating: C:\developer\sergueik\springboot_study\basic-config\data.json
```
```text

{  "host1": {    "netstat": [      22,      443,      3306    ]  },  "host2": {   "netstat": [    ]  },  "host3": {}}
```
(no longer supported)
```powershell
.\getconfig2.ps1 -base_url http://localhost:8085/configs/file_hash
```
```text
GET http://localhost:8085/configs/file_hash?filename=data.json&hash=0DFA1329F15FEFA8648856794EB33244
Invoke-WebRequest -uri http://localhost:8085/configs/file_hash?filename=data.json&hash=0DFA1329F15FEFA8648856794EB33244 -OutFile C:\Users\Serguei\AppData\Local\Temp\data.json -passthru
status code: 200
```
```text

saved the server response into C:\Users\Serguei\AppData\Local\Temp\data.json
{"result":"hash","status":"error"}
HTTP Stasus: 200
Body: {"result":"hash","status":"error"}
converting the page to JSON
```
```text
ERROR: hash
```
file is unchanged:
```powershell
dir .\data.json

```
```text
    Directory: C:\developer\sergueik\springboot_study\basic-config


Mode                LastWriteTime         Length Name
----                -------------         ------ ----
-a----        10/7/2023   7:32 PM            148 data.json
```

### See Also

   * [Spring Controller download an Image or a File](https://www.baeldung.com/spring-controller-return-image-file)
   * [ways to read a file into String](https://stackoverflow.com/questions/3402735/what-is-simplest-way-to-read-a-file-into-string)
   * [generate file MD5 checksum in Java](https://www.baeldung.com/java-md5-checksum-file)
   * [code2flow](https://app.code2flow.com) - online graphviz-like flowchart creator Copyright © 2013-2022, Code Charm, Inc.

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
