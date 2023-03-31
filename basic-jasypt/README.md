### Info
replica of
[Springboot Jasypt Encrypt Demo](https://github.com/techragesh/springboot-jasypt-encrypt-demo) combined with __jasypt-spring-boot-demo-simple__ [example](https://github.com/ulisesbocchio/jasypt-spring-boot-samples)
### Usage

```sh
curl  -LOsk https://github.com/jasypt/jasypt/releases/download/jasypt-1.9.3/jasypt-1.9.3-dist.zip
unzip jasypt-1.9.3-dist.zip
chmod +x  ./jasypt-1.9.3/bin/encrypt.sh

```
* encrypt the default password
```sh
  ./jasypt-1.9.3/bin/encrypt.sh  input=password password=secret
```
this will print
```text
----ENVIRONMENT-----------------

Runtime: Oracle Corporation Java HotSpot(TM) 64-Bit Server VM 25.161-b12



----ARGUMENTS-------------------

input: password
password: secret



----OUTPUT----------------------

b6pYDEOHlJ0BzvZ4uqm3i0Guh1lYxxFx
```
 update `src/main/resources/application.properties`
with password and encrypted value

```java
jasypt.encryptor.algorithm=PBEWithMD5AndDES
jasypt.encryptor.password=secret
defaultPassword=ENC( b6pYDEOHlJ0BzvZ4uqm3i0Guh1lYxxFx)
password=${PASSWORD:${defaultPassword}}
username=user
endpoint=https://${username}:${password}@localhost:30000
```

* run the app:
```sh
mvn spring-boot:run
```
this will print

```text
##############################
Username is -------->user
Endpoint is -------->https://user:password@localhost:30000

```
### Cleanup

```sh
rm -rf jasypt-1.9.3
rm jasypt-1.9.3-dist.zip

```
### See Also

  * [Spring Boot Configuration with Jasypt](https://www.baeldung.com/spring-boot-jasypt)
  * [python library](https://github.com/Telmediq/jasypt-2-python) (not tested)
  * [another](https://github.com/lemonprogis/python-jasypt)
  * [another](https://github.com/fareliner/jasypt4py)


### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
