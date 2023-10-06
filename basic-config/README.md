### Usage
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

* successful case:
* create a JSON file in a app resource directory (`C:\TEMP`  for simplicity) and download it 

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

### See Also

   * [Spring Controller download an Image or a File](https://www.baeldung.com/spring-controller-return-image-file)
   * [ways to read a file into String](https://stackoverflow.com/questions/3402735/what-is-simplest-way-to-read-a-file-into-string)

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
