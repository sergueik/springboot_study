### Info
replica of
[Springboot Jasypt Encrypt Demo](https://github.com/techragesh/springboot-jasypt-encrypt-demo) combined with __jasypt-spring-boot-demo-simple__ [example](https://github.com/ulisesbocchio/jasypt-spring-boot-samples)
### Usage

```sh
VERSION=1.9.3
curl -LOsk https://github.com/jasypt/jasypt/releases/download/jasypt-$VERSION/jasypt-$VERSION-dist.zip
unzip jasypt-$VERSION-dist.zip
chmod +x  ./jasypt-$VERSION/bin/encrypt.sh ./jasypt-$VERSION/bin/decrypt.sh ./jasypt-$VERSION/bin/listAlgorithms.sh

```
* encrypt the default password
```sh
./jasypt-1.9.3/bin/encrypt.sh input=password password=secret
```

Alternatively build it in docker
```sh
docker build -t jasypt -f Dockerfile .
```
```sh
docker run -it jasypt encrypt input=password password=secret
```
this will print to console somewhat verbosely
```text
----ENVIRONMENT-----------------

Runtime: Oracle Corporation Java HotSpot(TM) 64-Bit Server VM 25.161-b12



----ARGUMENTS-------------------

input: password
password: secret



----OUTPUT----------------------
I4TFuoFbppF19ccd9yIVOXVdIFYW1Asd

```
* check to decrypt by Java
```sh
docker run -it jasypt decrypt password=secret input=I4TFuoFbppF19ccd9yIVOXVdIFYW1Asd
```
```text
----ENVIRONMENT-----------------

Runtime: IcedTea OpenJDK 64-Bit Server VM 25.212-b04



----ARGUMENTS-------------------

input: I4TFuoFbppF19ccd9yIVOXVdIFYW1Asd
password: secret



----OUTPUT----------------------

password
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

* NOTE:
setting enviroment variable works. 

```sh
export JASYPT_ENCRYPTOR_PASSWORD=secret
mvn spring-boot:run
```
leads to
```text
##############################
Username is -------->user
Endpoint is -------->https://user:password@localhost:30000
##############################
```
forgetting to export the env 

```sh
JASYPT_ENCRYPTOR_PASSWORD=secret
mvn spring-boot:run
```
leads to
```text
Error creating bean with name 'myTest': Injection of autowired dependencies failed; nested exception is java.lang.IllegalStateException: either 'jasypt.encryptor.password' or one of ['jasypt.encryptor.private-key-string', 'jasypt.encryptor.private-key-location'] must be provided for Password-based or Asymmetric encryption

```

setting secret password through `key.txt` placed in fixed location or as resource into the package requires custom bean
```sh
mvn spring-boot:run
```
```text
   main] c.u.j.encryptor.DefaultLazyEncryptor     : Found Custom Encryptor Bean org.jasypt.encryption.pbe.PooledPBEStringEncryptor@1051817b with name: jasyptStringEncryptor
2023-05-12 23:51:27.217  INFO 9819 --- [           main] tConfig$$EnhancerBySpringCGLIB$$d74e9abb : loading src/main/resources/key.txt
2023-05-12 23:51:27.218  INFO 9819 --- [           main] tConfig$$EnhancerBySpringCGLIB$$d74e9abb : Read: "secret"

2023-05-12 23:30:34.515  INFO 7726 --- [           main] o.s.s.concurrent.ThreadPoolTaskExecutor  : Initializing ExecutorService 'applicationTaskExecutor'
2023-05-12 23:30:35.478  INFO 7726 --- [           main] o.s.b.w.embedded.tomcat.TomcatWebServer  : Tomcat started on port(s): 8080 (http) with context path ''
2023-05-12 23:30:35.507  INFO 7726 --- [           main] s.SpringbootJasyptEncryptDemoApplication : Started SpringbootJasyptEncryptDemoApplication in 4.952 seconds (JVM running for 5.724)
##############################
Username is -------->user
Endpoint is -------->https://user:password@localhost:30000
##############################

```
NOTE: need to read and remove newlines, not doing so leads to:
```text
2023-05-12 23:30:33.793  INFO 7726 --- [           main] tConfig$$EnhancerBySpringCGLIB$$d74e9abb : Read: "secret
"
Exception encountered during context initialization - 
 cancelling refresh attempt: org.springframework.beans.factory.BeanCreationException: 
 Error creating bean with name 'myTest': Injection of autowired dependencies failed; 
 nested exception is org.jasypt.exceptions.EncryptionInitializationException: 
 java.security.spec.InvalidKeySpecException: Password is not ASCII
 

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

* Note, all `AES` algorythms are returning the error:
```
----ERROR-----------------------

Operation not possible (Bad input or parameters)
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
mvn jasypt:encrypt -Djasypt.encryptor.password=secret -Djasypt.plugin.path="file:src/main/resources/application.properties" -Djasypt.encryptor.algorithm=PBEWithMD5AndDES
```
alternatively, write the password into file `src/main/resources/key.txt` and repeat the previous command sans the `-Djasypt.encryptor.password` argument:

```sh
mvn jasypt:encrypt -Djasypt.encryptor.private-key-location="file:src/main/resources/key.txt" -Djasypt.plugin.path="file:src/main/resources/application.properties" -Djasypt.encryptor.algorithm=PBEWithMD5AndDES
```
* NOTE: the command needs more arguments


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
the `src/main/resorces/application.properties` will be modified from:
```text
defaultPassword = DEC(password)
```
to
```text
defaultPassword = ENC(07yqh79SUIS5aptXZ7RgDI4zxmcqbNTDSYc/B8a7932ZwW2exVi4cdeA4gA78Fv5)
```
* NOTE: error in runtime

```text
org.springframework.beans.factory.BeanCreationException: Error creating bean with name 'myTest': Injection of autowired dependencies failed; nested exception is com.ulisesbocchio.jasyptspringboot.exception.DecryptionException: Unable to decrypt: ENC(07yqh79SUIS5aptXZ7RgDI4zxmcqbNTDSYc/B8a7932ZwW2exVi4cdeA4gA78Fv5). Decryption of Properties failed,  make sure encryption/decryption passwords match
        at org.springframework.beans.factory.annotation.AutowiredAnnotationBeanPostProcessor.postProcessProperties(AutowiredAnnotationBeanPostProcessor.java:405) ~[spring-beans-5.2.9.RELEASE.jar:5.2.9.RELEASE]

```
#### Perl Java Interability


```sh
mvn jasypt:encrypt-value -Djasypt.encryptor.password=secret -Djasypt.plugin.value="password"
```
```text
[INFO] Scanning for projects...
[INFO]
[INFO] ------------------------------------------------------------------------
[INFO] Building springboot-jasypt-encrypt-demo 0.2.0-SNAPSHOT
[INFO] ------------------------------------------------------------------------
[INFO]
[INFO] --- jasypt-maven-plugin:3.0.3:encrypt-value (default-cli) @ springboot-jasypt-encrypt-demo ---
[INFO] Starting MavenCli v3.3.9 on lenovoy40-1 with PID 4391 (/opt/apache-maven-3.3.9/lib/maven-embedder-3.3.9.jar started by sergueik in /home/sergueik/src/springboot_study/basic-jasypt)
[INFO] No active profile set, falling back to default profiles: default
[INFO] Post-processing PropertySource instances
[INFO] Converting PropertySource configurationProperties [org.springframework.boot.context.properties.source.ConfigurationPropertySourcesPropertySource] to AOP Proxy
[INFO] Converting PropertySource systemProperties [org.springframework.core.env.PropertiesPropertySource] to EncryptableMapPropertySourceWrapper
[INFO] Converting PropertySource systemEnvironment [org.springframework.boot.env.SystemEnvironmentPropertySourceEnvironmentPostProcessor$OriginAwareSystemEnvironmentPropertySource] to EncryptableSystemEnvironmentPropertySourceWrapper
[INFO] Converting PropertySource random [org.springframework.boot.env.RandomValuePropertySource] to EncryptablePropertySourceWrapper
[INFO] Property Filter custom Bean not found with name 'encryptablePropertyFilter'. Initializing Default Property Filter
[INFO] Started MavenCli in 1.306 seconds (JVM running for 4.07)
[INFO] Active Profiles: Default
[INFO] Encrypting value password
[INFO] String Encryptor custom Bean not found with name 'jasyptStringEncryptor'. Initializing Default String Encryptor
[INFO] Encryptor config not found for property jasypt.encryptor.algorithm, using default value: PBEWITHHMACSHA512ANDAES_256
[INFO] Encryptor config not found for property jasypt.encryptor.key-obtention-iterations, using default value: 1000
[INFO] Encryptor config not found for property jasypt.encryptor.pool-size, using default value: 1
[INFO] Encryptor config not found for property jasypt.encryptor.provider-name, using default value: null
[INFO] Encryptor config not found for property jasypt.encryptor.provider-class-name, using default value: null
[INFO] Encryptor config not found for property jasypt.encryptor.salt-generator-classname, using default value: org.jasypt.salt.RandomSaltGenerator
[INFO] Encryptor config not found for property jasypt.encryptor.iv-generator-classname, using default value: org.jasypt.iv.RandomIvGenerator
[INFO] Encryptor config not found for property jasypt.encryptor.string-output-type, using default value: base64
[INFO]
```

```text
ENC(eVNwzOOiu7Vtt32+S2aJm61Bm71KIxXjrZdyAgaSetZ41CsTYmNMvSBlbRTkgw4N)

```
the reverse
```sh
mvn jasypt:decrypt-value -Djasypt.encryptor.password=secret -Djasypt.plugin.value="eVNwzOOiu7Vtt32+S2aJm61Bm71KIxXjrZdyAgaSetZ41CsTYmNMvSBlbRTkgw4N"
```
succeeds and prints a lot of information about the configuration used:
  
```text
[INFO] Scanning for projects...
[INFO]
[INFO] ------------------------------------------------------------------------
[INFO] Building springboot-jasypt-encrypt-demo 0.2.0-SNAPSHOT
[INFO] ------------------------------------------------------------------------
[INFO]
[INFO] --- jasypt-maven-plugin:3.0.3:decrypt-value (default-cli) @ springboot-jasypt-encrypt-demo ---
[INFO] Starting MavenCli v3.3.9 on lenovoy40-1 with PID 4431 (/opt/apache-maven-3.3.9/lib/maven-embedder-3.3.9.jar started by sergueik in /home/sergueik/src/springboot_study/basic-jasypt)
[INFO] No active profile set, falling back to default profiles: default
[INFO] Post-processing PropertySource instances
[INFO] Converting PropertySource configurationProperties [org.springframework.boot.context.properties.source.ConfigurationPropertySourcesPropertySource] to AOP Proxy
[INFO] Converting PropertySource systemProperties [org.springframework.core.env.PropertiesPropertySource] to EncryptableMapPropertySourceWrapper
[INFO] Converting PropertySource systemEnvironment [org.springframework.boot.env.SystemEnvironmentPropertySourceEnvironmentPostProcessor$OriginAwareSystemEnvironmentPropertySource] to EncryptableSystemEnvironmentPropertySourceWrapper
[INFO] Converting PropertySource random [org.springframework.boot.env.RandomValuePropertySource] to EncryptablePropertySourceWrapper
[INFO] Property Filter custom Bean not found with name 'encryptablePropertyFilter'. Initializing Default Property Filter
[INFO] Started MavenCli in 1.18 seconds (JVM running for 3.512)
[INFO] Active Profiles: Default
[INFO] Decrypting value eVNwzOOiu7Vtt32+S2aJm61Bm71KIxXjrZdyAgaSetZ41CsTYmNMvSBlbRTkgw4N
[INFO] String Encryptor custom Bean not found with name 'jasyptStringEncryptor'. Initializing Default String Encryptor
[INFO] Encryptor config not found for property jasypt.encryptor.algorithm, using default value: PBEWITHHMACSHA512ANDAES_256
[INFO] Encryptor config not found for property jasypt.encryptor.key-obtention-iterations, using default value: 1000
[INFO] Encryptor config not found for property jasypt.encryptor.pool-size, using default value: 1
[INFO] Encryptor config not found for property jasypt.encryptor.provider-name, using default value: null
[INFO] Encryptor config not found for property jasypt.encryptor.provider-class-name, using default value: null
[INFO] Encryptor config not found for property jasypt.encryptor.salt-generator-classname, using default value: org.jasypt.salt.RandomSaltGenerator
[INFO] Encryptor config not found for property jasypt.encryptor.iv-generator-classname, using default value: org.jasypt.iv.RandomIvGenerator
[INFO] Encryptor config not found for property jasypt.encryptor.string-output-type, using default value: base64
[INFO]
```
```text
password
```
the `PBEWITHHMACSHA512ANDAES_256` corresponds to `PBEWithHmacSHA512AndAES_256` in Perl JCE notation:



- updated `jasypt.pl` to use the `PBEWithHmacSHA512AndAES_256` algorithm
```Perl
my $password = $secret;
my $pbe      = PBEWithHmacSHA512AndAES_256($password);
my $encrypted = decode_base64($value);
print $pbe->decrypt($encrypted), $/;
```
```sh
perl jasypt.pl -operation decrypt -secret secret -value "eVNwzOOiu7Vtt32+S2aJm61Bm71KIxXjrZdyAgaSetZ41CsTYmNMvSBlbRTkgw4N"
```
```text
password
```

### NOTE
the arguments of the older release of jasypt are not compatible or tricky to set rght:
```sh
./jasypt-1.9.3/bin/decrypt.sh input="eVNwzOOiu7Vtt32+S2aJm61Bm71KIxXjrZdyAgaSetZ41CsTYmNMvSBlbRTkgw4N" password=secret algorithm=PBEWITHHMACSHA512ANDAES_256
```
```text
----ENVIRONMENT-----------------

Runtime: Oracle Corporation Java HotSpot(TM) 64-Bit Server VM 25.161-b12



----ARGUMENTS-------------------

algorithm: PBEWITHHMACSHA512ANDAES_256
input: eVNwzOOiu7Vtt32+S2aJm61Bm71KIxXjrZdyAgaSetZ41CsTYmNMvSBlbRTkgw4N
password: secret



----ERROR-----------------------

Operation not possible (Bad input or parameters)

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

### Cleanup

```sh
rm -rf jasypt-1.9.3
rm jasypt-1.9.3-dist.zip
```

### See Also

  * [Spring Boot Configuration with Jasypt](https://www.baeldung.com/spring-boot-jasypt)
  * [spring Boot Password Encryption Using Jasypt](https://www.appsdeveloperblog.com/spring-boot-password-encryption-using-jasypt/)
  * https://www.geeksforgeeks.org/how-to-encrypt-passwords-in-a-spring-boot-project-using-jasypt/

  * Perl 
    + `Crypt::PBE` [module](https://metacpan.org/pod/Crypt::PBE)
    + [discussion](https://www.perlmonks.org/?dislaytype=print;node_id=845861;replies=1) about implementing Jasypt in Perl
    + [stackoverflow](https://stackoverflow.com/questions/4371714/how-do-i-profile-my-perl-programs)

  * .Net

  + [translate PBE Codes from Java to C#](https://cuteprogramming.wordpress.com/2015/02/20/translate-pbe-codes-from-java-to-c/) - article covers `PBEWithMD5AndDES` algorythm
  + [Emulating PBEWithMD5AndDES Encryption under .NET](https://www.codeproject.com/Articles/16450/Emulating-PBEWithMD5AndDES-Encryption-under-NET)
  + https://www.cryptosys.net/pki/dotnetpki/html/AllMembers_T_CryptoSysPKI_Pbe.htm
  + https://www.example-code.com/csharp/crypt2_pbes2.asp - requires Chilkat .NET Assemblies - commercial - 30 day evaluation available on [nuget](https://www.nuget.org/packages/chilkat-x64#supportedframeworks-body-tab) -  works with .NET Framework net481, net451 and earlier - separate dlls for net 451 through 472 in the archive: ChilkatDotNet45.dll and the like
  + `PbeParameters` Class - not available for .NET Framework 4.8.1, only .Net 7 [link](https://learn.microsoft.com/en-us/dotnet/api/system.security.cryptography.pbeparameters?view=net-7.0&viewFallbackFrom=netframework-4.8.1)

* Python Modules
  + [python2.7 example](https://github.com/lemonprogis/python-jasypt) - need conversion of string / byte / unicode variables
  + [python3.x example](https://github.com/fareliner/jasypt4py/blob/master/jasypt4py/encryptor.py) - the code is not compatible with Python __3.8__ and later, error is: `module 'time' has no attribute 'clock'`. Attempt to Use the alternative module suggeested in [stackoverflow](https://stackoverflow.com/questions/58569361/attributeerror-module-time-has-no-attribute-clock-in-python-3-8) leads to a new error: `TypeError: Object type <class 'str'> cannot be passed to C code`
  * [python library](https://github.com/Telmediq/jasypt-2-python) (not tested)
  * [another python library](https://github.com/lemonprogis/python-jasypt) - Python 2.7
  * [another python library](https://github.com/fareliner/jasypt4py)

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
