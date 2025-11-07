### Note
#### Tests Not Discovered
```sh
mvn -q -Dtest=UserControllerTest surefire:test -X | grep -A3 "provider"
```
should show
```text
Using provider org.apache.maven.surefire.junitplatform.JUnitPlatformProvider
```
```sh
mvn dependency:tree | grep junit
```
