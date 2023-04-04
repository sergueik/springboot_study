### Info
replica of
[Springboot Jasypt Encrypt Demo](https://github.com/techragesh/springboot-jasypt-encrypt-demo) combined with __jasypt-spring-boot-demo-simple__ [example](https://github.com/ulisesbocchio/jasypt-spring-boot-samples)
### Usage

```sh
VERSION=1.9.3
curl -LOsk https://github.com/jasypt/jasypt/releases/download/jasypt-$VERSION/jasypt-$VERSION-dist.zip
unzip jasypt-$VERSION-dist.zip
chmod +x  ./jasypt-$VERSION/bin/encrypt.sh ./jasypt-$VERSION/bin/encrypt.sh
chmod +x  ./jasypt-$VERSION/bin/encrypt.sh ./jasypt-$VERSION/bin/decrypt.sh
chmod +x  ./jasypt-$VERSION/bin/encrypt.sh ./jasypt-$VERSION/bin/listAlgorithms.sh

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
### Profiling the Run

* timing 100 encryption runs

```sh
time ./m1.sh
```
```text
real    0m44.187s
user    1m35.484s
sys     0m4.074s
```
```sh
time ./m2.sh
```
```text
real    0m3.457s
user    0m3.045s
sys     0m0.372s
```
* timing 100 decryption runs
```sh
time ./m3.sh
```
```text
real    0m44.502s
user    1m35.537s
sys     0m4.251s

```
```sh
time ./m4.sh
```
```text
real    0m3.738s
user    0m3.360s
sys     0m0.382s
```

* Note: `cpan` fails to install `Devel::DProf`:
```text
in file included from DProf.xs:4:0:
DProf.c:854:10: error: static declaration of ‘XS_Devel__DProf_END’ follows non-static declaration
 XS_EUPXS(XS_Devel__DProf_END); /* prototype to pass -Wmissing-prototypes */
          ^
  FLORA/Devel-DProf-20110802.00.tar.gz
  /usr/bin/make -- NOT OK

```
the
```sh
cpan install Devel::SmallProf
```
also fails
```text

Running make test
PERL_DL_NONLAZY=1 "/usr/bin/perl" "-MExtUtils::Command::MM" "-MTest::Harness" "-e" "undef *Test::Harness::Switches; test_harness(0, 'blib/lib', 'blib/arch')" t/*.t
t/part1.t .. Can't use 'defined(@array)' (Maybe you should just omit the defined()?) at /home/sergueik/.cpan/build/Devel-SmallProf-2.02-1/blib/lib/Devel/SmallProf.pm line 35.
Compilation failed in require at t/part1.t line 1.
BEGIN failed--compilation aborted at t/part1.t line 2.

Test Summary Report
-------------------
t/part1.t (Wstat: 65280 Tests: 0 Failed: 0)
  Non-zero exit status: 255
  Parse errors: No plan found in TAP output
t/part2.t (Wstat: 0 Tests: 10 Failed: 10)
  Failed tests:  1-10
t/part3.t (Wstat: 65280 Tests: 0 Failed: 0)
  Non-zero exit status: 255
  Parse errors: No plan found in TAP output
t/part4.t (Wstat: 0 Tests: 2 Failed: 1)
  Failed test:  1
Files=5, Tests=12,  0 wallclock secs ( 0.02 usr  0.00 sys +  0.04 cusr  0.00 csys =  0.06 CPU)
Result: FAIL
Failed 4/5 test programs. 11/12 subtests failed.
Makefile:842: recipe for target 'test_dynamic' failed
make: *** [test_dynamic] Error 255
  SALVA/Devel-SmallProf-2.02.tar.gz
  /usr/bin/make test -- NOT OK
//hint// to see the cpan-testers results for installing this module, try:
  reports SALVA/Devel-SmallProf-2.02.tar.gz

```

### Alternative Algorithms

```sh
./jasypt-1.9.3/bin/listAlgorithms.sh |sed 's|\s|\n|g '

```
```text
PBEWITHHMACSHA1ANDAES_128
PBEWITHHMACSHA1ANDAES_256
PBEWITHHMACSHA224ANDAES_128
PBEWITHHMACSHA224ANDAES_256
PBEWITHHMACSHA256ANDAES_128
PBEWITHHMACSHA256ANDAES_256
PBEWITHHMACSHA384ANDAES_128
PBEWITHHMACSHA384ANDAES_256
PBEWITHHMACSHA512ANDAES_128
PBEWITHHMACSHA512ANDAES_256
PBEWITHMD5ANDDES
PBEWITHMD5ANDTRIPLEDES
PBEWITHSHA1ANDDESEDE
PBEWITHSHA1ANDRC2_128
PBEWITHSHA1ANDRC2_40
PBEWITHSHA1ANDRC4_128
PBEWITHSHA1ANDRC4_40
```

it does not appear all algorithms are workimg:

```sh
ALGORITHMS="PBEWITHHMACSHA1ANDAES_128 PBEWITHHMACSHA1ANDAES_256 PBEWITHHMACSHA224ANDAES_128 PBEWITHHMACSHA224ANDAES_256 PBEWITHHMACSHA256ANDAES_128 PBEWITHHMACSHA256ANDAES_256 PBEWITHHMACSHA384ANDAES_128 PBEWITHHMACSHA384ANDAES_256 PBEWITHHMACSHA512ANDAES_128 PBEWITHHMACSHA512ANDAES_256 PBEWITHMD5ANDDES PBEWITHMD5ANDTRIPLEDES PBEWITHSHA1ANDDESEDE PBEWITHSHA1ANDRC2_128 PBEWITHSHA1ANDRC2_40 PBEWITHSHA1ANDRC4_128 PBEWITHSHA1ANDRC4_40"


(for A in $ALGORITHMS ; do echo $A; echo ./jasypt-1.9.3/bin/encrypt.sh algorithm=$A input=test password=secret; ./jasypt-1.9.3/bin/encrypt.sh algorithm=$A input=test password=secret ;done ) 2>&1 | tee a.log

```
in the log:
```text
PBEWITHHMACSHA1ANDAES_128
./jasypt-1.9.3/bin/encrypt.sh algorithm=PBEWITHHMACSHA1ANDAES_128 input=test password=secret

----ENVIRONMENT-----------------

Runtime: Oracle Corporation Java HotSpot(TM) 64-Bit Server VM 25.161-b12



----ARGUMENTS-------------------

input: test
algorithm: PBEWITHHMACSHA1ANDAES_128
password: secret



----ERROR-----------------------

Operation not possible (Bad input or parameters)


PBEWITHHMACSHA1ANDAES_256
./jasypt-1.9.3/bin/encrypt.sh algorithm=PBEWITHHMACSHA1ANDAES_256 input=test password=secret

----ENVIRONMENT-----------------

Runtime: Oracle Corporation Java HotSpot(TM) 64-Bit Server VM 25.161-b12



----ARGUMENTS-------------------

input: test
algorithm: PBEWITHHMACSHA1ANDAES_256
password: secret



----ERROR-----------------------

Operation not possible (Bad input or parameters)


PBEWITHHMACSHA224ANDAES_128
./jasypt-1.9.3/bin/encrypt.sh algorithm=PBEWITHHMACSHA224ANDAES_128 input=test password=secret

----ENVIRONMENT-----------------

Runtime: Oracle Corporation Java HotSpot(TM) 64-Bit Server VM 25.161-b12



----ARGUMENTS-------------------

input: test
algorithm: PBEWITHHMACSHA224ANDAES_128
password: secret



----ERROR-----------------------

Operation not possible (Bad input or parameters)


PBEWITHHMACSHA224ANDAES_256
./jasypt-1.9.3/bin/encrypt.sh algorithm=PBEWITHHMACSHA224ANDAES_256 input=test password=secret

----ENVIRONMENT-----------------

Runtime: Oracle Corporation Java HotSpot(TM) 64-Bit Server VM 25.161-b12



----ARGUMENTS-------------------

input: test
algorithm: PBEWITHHMACSHA224ANDAES_256
password: secret



----ERROR-----------------------

Operation not possible (Bad input or parameters)


PBEWITHHMACSHA256ANDAES_128
./jasypt-1.9.3/bin/encrypt.sh algorithm=PBEWITHHMACSHA256ANDAES_128 input=test password=secret

----ENVIRONMENT-----------------

Runtime: Oracle Corporation Java HotSpot(TM) 64-Bit Server VM 25.161-b12



----ARGUMENTS-------------------

input: test
algorithm: PBEWITHHMACSHA256ANDAES_128
password: secret



----ERROR-----------------------

Operation not possible (Bad input or parameters)


PBEWITHHMACSHA256ANDAES_256
./jasypt-1.9.3/bin/encrypt.sh algorithm=PBEWITHHMACSHA256ANDAES_256 input=test password=secret

----ENVIRONMENT-----------------

Runtime: Oracle Corporation Java HotSpot(TM) 64-Bit Server VM 25.161-b12



----ARGUMENTS-------------------

input: test
algorithm: PBEWITHHMACSHA256ANDAES_256
password: secret



----ERROR-----------------------

Operation not possible (Bad input or parameters)


PBEWITHHMACSHA384ANDAES_128
./jasypt-1.9.3/bin/encrypt.sh algorithm=PBEWITHHMACSHA384ANDAES_128 input=test password=secret

----ENVIRONMENT-----------------

Runtime: Oracle Corporation Java HotSpot(TM) 64-Bit Server VM 25.161-b12



----ARGUMENTS-------------------

input: test
algorithm: PBEWITHHMACSHA384ANDAES_128
password: secret



----ERROR-----------------------

Operation not possible (Bad input or parameters)


PBEWITHHMACSHA384ANDAES_256
./jasypt-1.9.3/bin/encrypt.sh algorithm=PBEWITHHMACSHA384ANDAES_256 input=test password=secret

----ENVIRONMENT-----------------

Runtime: Oracle Corporation Java HotSpot(TM) 64-Bit Server VM 25.161-b12



----ARGUMENTS-------------------

input: test
algorithm: PBEWITHHMACSHA384ANDAES_256
password: secret



----ERROR-----------------------

Operation not possible (Bad input or parameters)


PBEWITHHMACSHA512ANDAES_128
./jasypt-1.9.3/bin/encrypt.sh algorithm=PBEWITHHMACSHA512ANDAES_128 input=test password=secret

----ENVIRONMENT-----------------

Runtime: Oracle Corporation Java HotSpot(TM) 64-Bit Server VM 25.161-b12



----ARGUMENTS-------------------

input: test
algorithm: PBEWITHHMACSHA512ANDAES_128
password: secret



----ERROR-----------------------

Operation not possible (Bad input or parameters)


PBEWITHHMACSHA512ANDAES_256
./jasypt-1.9.3/bin/encrypt.sh algorithm=PBEWITHHMACSHA512ANDAES_256 input=test password=secret

----ENVIRONMENT-----------------

Runtime: Oracle Corporation Java HotSpot(TM) 64-Bit Server VM 25.161-b12



----ARGUMENTS-------------------

input: test
algorithm: PBEWITHHMACSHA512ANDAES_256
password: secret



----ERROR-----------------------

Operation not possible (Bad input or parameters)


PBEWITHMD5ANDDES
./jasypt-1.9.3/bin/encrypt.sh algorithm=PBEWITHMD5ANDDES input=test password=secret

----ENVIRONMENT-----------------

Runtime: Oracle Corporation Java HotSpot(TM) 64-Bit Server VM 25.161-b12



----ARGUMENTS-------------------

input: test
algorithm: PBEWITHMD5ANDDES
password: secret



----OUTPUT----------------------

dXzqjV8RIAKXZJY9YTGixQ==


PBEWITHMD5ANDTRIPLEDES
./jasypt-1.9.3/bin/encrypt.sh algorithm=PBEWITHMD5ANDTRIPLEDES input=test password=secret

----ENVIRONMENT-----------------

Runtime: Oracle Corporation Java HotSpot(TM) 64-Bit Server VM 25.161-b12



----ARGUMENTS-------------------

input: test
algorithm: PBEWITHMD5ANDTRIPLEDES
password: secret



----OUTPUT----------------------

0NEOANIDacCLxSm6insuig==


PBEWITHSHA1ANDDESEDE
./jasypt-1.9.3/bin/encrypt.sh algorithm=PBEWITHSHA1ANDDESEDE input=test password=secret

----ENVIRONMENT-----------------

Runtime: Oracle Corporation Java HotSpot(TM) 64-Bit Server VM 25.161-b12



----ARGUMENTS-------------------

input: test
algorithm: PBEWITHSHA1ANDDESEDE
password: secret



----OUTPUT----------------------

cXX80fyZD+mIbsUIQnLpLA==


PBEWITHSHA1ANDRC2_128
./jasypt-1.9.3/bin/encrypt.sh algorithm=PBEWITHSHA1ANDRC2_128 input=test password=secret

----ENVIRONMENT-----------------

Runtime: Oracle Corporation Java HotSpot(TM) 64-Bit Server VM 25.161-b12



----ARGUMENTS-------------------

input: test
algorithm: PBEWITHSHA1ANDRC2_128
password: secret



----OUTPUT----------------------

Q0+bNtDD6wwveV2VX9hFGQ==


PBEWITHSHA1ANDRC2_40
./jasypt-1.9.3/bin/encrypt.sh algorithm=PBEWITHSHA1ANDRC2_40 input=test password=secret

----ENVIRONMENT-----------------

Runtime: Oracle Corporation Java HotSpot(TM) 64-Bit Server VM 25.161-b12



----ARGUMENTS-------------------

input: test
algorithm: PBEWITHSHA1ANDRC2_40
password: secret



----OUTPUT----------------------

rR4F7KuJ+mQIIejpJZacDg==


PBEWITHSHA1ANDRC4_128
./jasypt-1.9.3/bin/encrypt.sh algorithm=PBEWITHSHA1ANDRC4_128 input=test password=secret

----ENVIRONMENT-----------------

Runtime: Oracle Corporation Java HotSpot(TM) 64-Bit Server VM 25.161-b12



----ARGUMENTS-------------------

input: test
algorithm: PBEWITHSHA1ANDRC4_128
password: secret



----OUTPUT----------------------

gBfP4Z7pbWnga0IJ


PBEWITHSHA1ANDRC4_40
./jasypt-1.9.3/bin/encrypt.sh algorithm=PBEWITHSHA1ANDRC4_40 input=test password=secret

----ENVIRONMENT-----------------

Runtime: Oracle Corporation Java HotSpot(TM) 64-Bit Server VM 25.161-b12



----ARGUMENTS-------------------

input: test
algorithm: PBEWITHSHA1ANDRC4_40
password: secret



----OUTPUT----------------------

9vaGV86+5EfZkJSm



```
* Note, all `AES` algorythms are returning the error:
```
----ERROR-----------------------

Operation not possible (Bad input or parameters)
```
### Cleanup

```sh
rm -rf jasypt-1.9.3
rm jasypt-1.9.3-dist.zip
```

### Encrypting at Build Time

#### Check Operation
```sh
mvn jasypt:encrypt-value -Djasypt.encryptor.password=secret -Djasypt.plugin.value="password"
```
this will print (the result will vary):
```text
UKLK01PbcKLilMvB8H84emuvR7yleDoN4HXDQkF7KdLmq99NlbuxtgIsilns3M+L
```
NOTE: if seeing the error

```text
[ERROR] No plugin found for prefix 'jasypt' in the current project and in the plugin groups
```
that means the version of the plugin and main jar are too old.

#### encrypt the property
* comment the earlier value added by hand and add the `DEC` one in the `application.properties`:

```java
defaultPassword = DEC(password)
# defaultPassword = ENC( b6pYDEOHlJ0BzvZ4uqm3i0Guh1lYxxFx)
```
* run
```sh
mvn jasypt:encrypt -Djasypt.encryptor.password=secret-Djasypt.plugin.path="file:src/main/resources/application.properties" -Djasypt.encryptor.algorithm=PBEWithMD5AndDES
```
* verify `aplication.properties`
```text
jasypt.encryptor.algorithm=PBEWithMD5AndDES
jasypt.encryptor.password=secret
# converting at build time
defaultPassword = ENC(/QXDB/JASqv5tHKcbdL3x+4yMgdNnp621FfBLP268Yk=)
# defaultPassword = ENC( b6pYDEOHlJ0BzvZ4uqm3i0Guh1lYxxFx)
password = ${PASSWORD:${defaultPassword}}
username = user
endpoint = https://${username}:${password}@localhost:30000

```
#### Note 

the arguments of the new release of jasypt are not compatible or tricky to set rght:
```sh
mvn jasypt:decrypt-value -Djasypt.encryptor.password=secret -Djasypt.plugin.value="5MiW4WoXPfaKnm4ufkRAev7T7wP2j/ZnnImEkWYUE3U=" -Djasypt.encryptor.algorithm=PBEWithMD5AndDES
```

gives

```text
[INFO] Converting PropertySource systemProperties [org.springframework.core.env.PropertiesPropertySource] to EncryptableMapPropertySourceWrapper
[INFO] Converting PropertySource systemEnvironment [org.springframework.boot.env.SystemEnvironmentPropertySourceEnvironmentPostProcessor$OriginAwareSystemEnvironmentPropertySource] to EncryptableSystemEnvironmentPropertySourceWrapper
[INFO] Converting PropertySource random [org.springframework.boot.env.RandomValuePropertySource] to EncryptablePropertySourceWrapper
[INFO] Property Filter custom Bean not found with name 'encryptablePropertyFilter'. Initializing Default Property Filter
[INFO] Started MavenCli in 1.177 seconds (JVM running for 3.605)
[INFO] Active Profiles: Default
[INFO] Decrypting value 5MiW4WoXPfaKnm4ufkRAev7T7wP2j/ZnnImEkWYUE3U=
[INFO] String Encryptor custom Bean not found with name 'jasyptStringEncryptor'. Initializing Default String Encryptor
[INFO] Encryptor config not found for property jasypt.encryptor.key-obtention-iterations, using default value: 1000
[INFO] Encryptor config not found for property jasypt.encryptor.pool-size, using default value: 1
[INFO] Encryptor config not found for property jasypt.encryptor.provider-name, using default value: null
[INFO] Encryptor config not found for property jasypt.encryptor.provider-class-name, using default value: null
[INFO] Encryptor config not found for property jasypt.encryptor.salt-generator-classname, using default value: org.jasypt.salt.RandomSaltGenerator
[INFO] Encryptor config not found for property jasypt.encryptor.iv-generator-classname, using default value: org.jasypt.iv.RandomIvGenerator
[INFO] Encryptor config not found for property jasypt.encryptor.string-output-type, using default value: base64
[INFO] ------------------------------------------------------------------------
[INFO] BUILD FAILURE
[INFO] ------------------------------------------------------------------------
[INFO] Total time: 2.800 s
[INFO] Finished at: 2023-04-04T18:49:08-04:00
[INFO] Final Memory: 26M/248M
[INFO] ------------------------------------------------------------------------
[ERROR] Failed to execute goal com.github.ulisesbocchio:jasypt-maven-plugin:3.0.3:decrypt-value (default-cli) on project springboot-jasypt-encrypt-demo: Error Decrypting: null: EncryptionOperationNotPossibleException -> [Help 1]
[ERROR]

```

### Python Modules

https://github.com/fareliner/jasypt4py/blob/master/jasypt4py/encryptor.py


 
 NOTE:
 
the code is not compatible with Python past 3.8 
 
```text
module 'time' has no attribute 'clock'
```

Using the suggeed in  https://stackoverflow.com/questions/58569361/attributeerror-module-time-has-no-attribute-clock-in-python-3-8

leads to a new error:

```text
Traceback (most recent call last):
  File "app.py", line 5, in <module>
    cryptor.encrypt('secret', 'test', 1000)
  File "/app/encryptor.py", line 95, in encrypt
    encrypted_message = cipher.encrypt(self.pad(AES.block_size, text))
  File "/usr/local/lib/python3.8/site-packages/Crypto/Cipher/_mode_cbc.py", line 178, in encrypt
    c_uint8_ptr(plaintext),

  File "/usr/local/lib/python3.8/site-packages/Crypto/Util/_raw_api.py", line 242, in c_uint8_ptr
  raise TypeError("Object type %s cannot be passed to C code" % type(data))
TypeError: Object type <class 'str'> cannot be passed to C code
```

### See Also

  * [Spring Boot Configuration with Jasypt](https://www.baeldung.com/spring-boot-jasypt)
  * `Crypt::PBE` [module](https://metacpan.org/pod/Crypt::PBE)
  * [discussion](https://www.perlmonks.org/?dislaytype=print;node_id=845861;replies=1) about implementing Jasypt in Perl
  * [python library](https://github.com/Telmediq/jasypt-2-python) (not tested)
  * [another python library](https://github.com/lemonprogis/python-jasypt) - Python 2.7
  * [another python library](https://github.com/fareliner/jasypt4py)
  * [stackoverflow](https://stackoverflow.com/questions/4371714/how-do-i-profile-my-perl-programs)
  * [spring Boot Password Encryption Using Jasypt](https://www.appsdeveloperblog.com/spring-boot-password-encryption-using-jasypt/)

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
