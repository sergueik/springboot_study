### Info
Plain directory file based configuration file loader controller


### Testing
```sh
mvn spring-boot test
```
* success call `http://localhost:8080/server/hosts1`

```json
["host1","host2","host3","host4"]
```
* bad data
`http://localhost:8080/server/cyclic_hosts1`


```text
["cyclic #include detected in line #include cyclic_hosts1"]
```
* bad data
`http://localhost:8080/server/hosts3`
```text
["inaccessible file: hosts2"]
```

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)


