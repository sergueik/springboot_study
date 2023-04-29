
### Info
Plain directory JSON file reporter
### Usage

```sh
curl -s -d '{"name": "john", "plays": "drums", "id":1}' -H "Content-type:application/json"  -XPOST http://localhost:8080/updatedata/paul
```
```text
bad name
```
```sh
curl -s -d '{"name": "john", "plays": "drums", "id":1}' -H "Content-type:application/json"  -XPOST http://localhost:8080/updatedata/john
```
```JSON
{"status":"OK"}
```
### Todo 

Acceptance Test list HttpResponse processing
### See Also
  * https://www.baeldung.com/spring-rest-template-list
  * https://www.baeldung.com/spring-bean
 
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)


