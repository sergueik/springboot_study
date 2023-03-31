### Info
replica of
[Springboot Jasypt Encrypt Demo](https://github.com/techragesh/springboot-jasypt-encrypt-demo) combined with __jasypt-spring-boot-demo-simple__ [example](https://github.com/ulisesbocchio/jasypt-spring-boot-samples)
### Usage

```sh
VERSION=1.9.3
curl -LOsk https://github.com/jasypt/jasypt/releases/download/jasypt-$VERSION/jasypt-$VERSION-dist.zip
unzip jasypt-$VERSION-dist.zip
chmod +x  ./jasypt-$VERSION/bin/encrypt.sh ./jasypt-$VERSION/bin/decrypt.sh

```
* encrypt the default password
```sh
./jasypt-1.9.3/bin/encrypt.sh input=password password=secret
```
this will print to console somewhat verbosely
```text
----ENVIRONMENT-----------------

Runtime: Oracle Corporation Java HotSpot(TM) 64-Bit Server VM 25.161-b12



----ARGUMENTS-------------------

input: password
password: secret



----OUTPUT----------------------

b6pYDEOHlJ0BzvZ4uqm3i0Guh1lYxxFx
```

```sh
  ./jasypt-1.9.3/bin/decrypt.sh  input='U0VwDbxlS/tJ3SKO39tLv7VHYVy8WiPy' password=secret./jasypt-1.9.3/bin/decrypt.sh  input='U0VwDbxlS/tJ3SKO39tLv7VHYVy8WiPy' password=secret
```
```text
----ENVIRONMENT-----------------

Runtime: Oracle Corporation Java HotSpot(TM) 64-Bit Server VM 25.161-b12



----ARGUMENTS-------------------

input: U0VwDbxlS/tJ3SKO39tLv7VHYVy8WiPy
password: secret



----OUTPUT----------------------

password

```
 update `src/main/resources/application.properties`
with `jasypt.encryptor.password` and encrypted value, formated with `ENC()` notation:

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
  * `Crypt::PBE` [module](https://metacpan.org/pod/Crypt::PBE)
  * [discussion](https://www.perlmonks.org/?dislaytype=print;node_id=845861;replies=1) about implementing Jasypt in Perl
  * [python library](https://github.com/Telmediq/jasypt-2-python) (not tested)
  * [another python library](https://github.com/lemonprogis/python-jasypt)
  * [another python library](https://github.com/fareliner/jasypt4py)

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
