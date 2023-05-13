### Info
Docker Alpine [container](https://hub.docker.com/layers/library/gradle/7.3.1-jdk17-alpine) with JDK 17.x and Gradle 7.x  for [jasyt gradle plugin](https://github.com/ximtech/jasypt-encrypt-plugin) testing

### Usage
```sh
docker pull gradle:7.3.1-jdk17-alpine
```
```sh
IMAGE=basic-jasypt-gradle
docker build -t $IMAGE -f Dockerfile.alpine-jdk17-gradle .
```
* NOTE: not binding on Windows host
```sh
docker container run -u root -it $IMAGE sh
```
in the container

```sh
PASSWORD=secret
gradle encryptProperties --file-filter-pattern='application\.properties' --password=$PASSWORD --value-extract-pattern="DEC((.*))"
```
the `application.properties` will be modified
from
```text
property=ENCRYPT(password)
```
to 
```text
property=ENC(ucrB8GQqqH+6s5k2Id2xt1eSn1o1gmwpl8PQfblK60kaJEwV66cUyDvK6bCg5+jV)
```
```sh
gradle decryptText --password=$PASSWORD --text='ucrB8GQqqH+6s5k2Id2xt1eSn1o1gmwpl8PQfblK60kaJEwV66cUyDvK6bCg5+jV'
```
will print
```text
> Task :decryptText
Decrypted text: password

BUILD SUCCESSFUL in 6s
1 actionable task: 1 executed
```
```sh
gradle encryptProperties --file-filter-pattern='application\.properties'
```
will print 
```text
> Task :decryptProperties
No of files found: 1
No of values changed: 2
Files processed:
[application.properties]

```
### Cleanup

```sh
docker container prune -f
docker image prune -f
docker image rm $IMAGE
```

### See Also

  * https://github.com/moberwasserlechner/jasypt-gradle-plugin/blob/master/build.gradle - compatible with JDK 1.8

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)


