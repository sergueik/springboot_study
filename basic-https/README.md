### Info

contains example sources of the [Spring Boot app on HTTPS using Self-Signed server Certificate ](https://www.baeldung.com/spring-boot-https-self-signed-certificate) application and test combined with minimal basic-auth code to illustrate the  client code for the same

### Usage
* generate self-signed certificate for site
```cmd
keytool -genkeypair -alias basic -keyalg RSA -keysize 2048 -storetype PKCS12 -keystore basic.p12 -validity 3650
```

```
keytool -genkeypair -alias basic -keyalg RSA -keysize 2048 -storetype PKCS12 -keystore basic.p12 -validity 3650 -ext "SAN:c=DNS:localhost,IP:127.0.0.1"
```

Use the `password` password
```cmd
Enter keystore password:
Re-enter new password:
```
fill the item details e.g.

```cmd
What is your first and last name?
  [Unknown]:  test
What is the name of your organizational unit?
  [Unknown]:  example
What is the name of your organization?
  [Unknown]:  example
What is the name of your City or Locality?
  [Unknown]:  miami
What is the name of your State or Province?
  [Unknown]:  FL
What is the two-letter country code for this unit?
  [Unknown]:  US
Is CN=test, OU=example, O=example, L=miami, ST=FL, C=US correct?
  [no]:
What is your first and last name?
  [test]:  test user
What is the name of your organizational unit?
  [example]:  example
What is the name of your organization?
  [example]:  example
What is the name of your City or Locality?
  [miami]:  Miami
What is the name of your State or Province?
  [FL]:  FL
What is the two-letter country code for this unit?
  [US]:  US
```
```cmd
Is CN=test user, OU=example, O=example, L=Miami, ST=FL, C=US correct?
  [no]:  yes
```


This creates `basic.p12`
Make sure to put the password filename and alias into `application.properties`.

```sh
mvn  -Dtest.skip=true spring-boot:run
```
```sh
curl -v https://localhost:8443/welcome
```
```text
curl: (60) SSL certificate problem: self signed certificate
```
```sh
curl -k -v https://localhost:8443/welcome
```
```text
welcome
```

```sh
curl -k -v https://localhost:8443/employees
```
```text
HTTP Status 401 - Full authentication is required to access this resource
```
```
curl -u user:password -k -v https://localhost:8443/employees
```
```text
HTTP Status 401 - Bad credentials
```
```sh
curl -u admin:password -k -v https://localhost:8443/employees
```
```text
{}
```
### Unit Testing

Currently can only enable one of 3 tests which exercise Basic Authentication over HTTPS:
```text
[INFO] Tests run: 2, Failures: 0, Errors: 0, Skipped: 0, Time elapsed: 8.952 s - in example.BasicAuthTests
```
```text
[INFO] Tests run: 1, Failures: 0, Errors: 0, Skipped: 0, Time elapsed: 10.425 s - in example.WrongCredentialsTests
```
```text
[INFO] Tests run: 1, Failures: 0, Errors: 0, Skipped: 0, Time elapsed: 13.176 s - in example.PlainIntegrationTest
```
when run together:
```text
***************************
APPLICATION FAILED TO START
***************************

Description:
Web server failed to start. Port 8443 was already in use.
Action:
Identify and stop the process that's listening on port 8443 or configure this application to listen on another port.
***************************
APPLICATION FAILED TO START
***************************

Description:
Web server failed to start. Port 8443 was already in use.
Action:
Identify and stop the process that's listening on port 8443 or configure this application to listen on another port.
[ERROR]   PlainIntegrationTest.test1 » IllegalState Failed to load ApplicationContext
[ERROR]   WrongCredentialsTests.test3 » IllegalState Failed to load ApplicationContext
```
the `@Disabled` attribute does not help . Changing the `@SpringBootTest` properties
```
@SpringBootTest(classes = example.Launcher.class, 
 webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT, properties = {
"serverPort=8445" } )
```
or cloning and referencing alternative `applicatiton.properties` does not override the HTTPS port 8443.
### See Also

  * [hint](https://stackoverflow.com/questions/50928061/certificate-for-localhost-doesnt-match-any-of-the-subject-alternative-names) for solving `Certificate for <localhost> doesn't match any of the subject alternative names: []`
  * https://github.com/eugenp/tutorials/blob/master/spring-security-modules/spring-security-web-boot-2/WebContent/META-INF/MANIFEST.MF

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
