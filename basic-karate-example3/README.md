### Usage
* run oauth server stub
```
pushd oauth
mvn -DskipTests spring-boot:run -Dspring-boot.run.arguments="--example.username=test --example.password=test"
```
```txt
2026-03-21T08:49:48.650-04:00  INFO 36308 --- [           main] o.s.b.w.embedded.tomcat.TomcatWebServer  : Tomcat started on port 8085 (http) with context path '/'
2026-03-21T08:49:48.660-04:00  INFO 36308 --- [           main] example.Application                      : Started Application in 1.787 seconds (process running for 2.136
```
```sh
curl -sX POST -H "Content-Type: application/json" http://localhost:8085/token -d '{"username": "test", "password": "test"}' | jq '.'
```
```json
{
  "access_token": "eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJ0ZXN0Iiwicm9sZSI6IlVTRVIiLCJpc3MiOiJodHRwOi8vbG9jYWxob3N0OjgwODUiLCJpYXQiOjE3NzQwNTk0NjMsImV4cCI6MTc3NDA2MzA2M30.ZJy6Pa4eU7MglhpIlV90iZVoKpOExwLn54s6Hlxl_I0",
  "expires_in": 3600,
  "token_type": "Bearer"
}

```

### Background

What a JWT actually is

has 3 parts separated by dots: `header.payload.signature`

Header → algorithm info (Base64URL encoded JSON)

Payload → claims (Base64URL encoded JSON)

Signature → cryptographic signature (not human-readable)

How to decode:

```sh
curl -sX POST -H "Content-Type: application/json" http://localhost:8085/token -d '{"username": "test", "password": "test"}' |jq -rc '.access_token | split(".")|.[1]' | base64 -d - 2> /dev/null | jq '.'

```
```json
{
  "sub": "test",
  "role": "USER",
  "iss": "http://localhost:8085",
  "iat": 1774059916,
  "exp": 1774063516
}

```

### Karate Run

start with canonical oath exercise `feature` file:
```feature
Feature: OAuth2 JWT token generation (stub server)

Background:
  * url 'http://localhost:8085'
  * def credentials = { username: 'test', password: 'test' }

Scenario: Obtain access token

  Given path 'token'
  And request credentials
  When method post
  Then status 200

  And match response.access_token != null
  And match response.token_type == 'Bearer'
  And match response.expires_in == 3600

  # extract token
  * def token = response.access_token
  * print token

  # split JWT correctly
  * def parts = token.split('\\.')
  * match parts.length == 3

  # decode header + payload
  # header validation
  * def headerpart = karate.fromBase64(parts[0])
  * print headerpart
  * match headerpart contains { alg: 'HS256' }

  # payload validation
  * def payload = karate.fromBase64(parts[1])
  * print payload
  * match payload.sub == 'test'
  * match payload.role == 'USER'
  * match payload.iss == 'http://localhost:8085'

  # timestamps
  * match payload.iat == '#number'
  * match payload.exp == '#number'
  * assert payload.exp > payload.iat

```
apply small tweaks to solve some problems encountered while keeping the original logic:
```feature

Feature: OAuth2 JWT token generation (stub server)

Background:
  * url 'http://localhost:8085'
  * def credentials = { username: 'test', password: 'test' }

Scenario: Obtain access token

  Given path 'token'
  And request credentials
  When method post
  Then status 200

  # basic response validation
  And match response.access_token != null
  And match response.token_type == 'Bearer'
  And match response.expires_in == 3600

  # extract token
  * def token = response.access_token

  # examine JWT parts
  * def parts = java.util.Arrays.asList(token.split("."))
  * def len = parts.length
  * match len == 3

  * def decode = function(obj){ return new java.lang.String(java.util.Base64.getUrlDecoder().decode(obj)) }

  * def headerpart = karate.fromString(decode(parts[0]))
  * print headerpart
  * match headerpart contains { alg: 'HS256' }

  * def payload = karate.fromString(decode(parts[1]))
  * print payload
  * match payload.sub == 'test'
  * match payload.role == 'USER'
  * match payload.iss == 'http://localhost:8085'

  # timestamps
  * match payload.iat == '#number'
  * match payload.exp == '#number'
  * assert payload.exp > payload.iat

```
run the test
```sh
mvn clean package -ntp -q -B
```
> Note - there will be no output from above. If you like to see maven messages omit the `-q` flag

```cmd
java -cp target\lib\* com.intuit.karate.cli.Main features\oauth.feature
```

![Html Report](screenshots/capture-karate-oauth2-1.png)

```text


12:34:23.526 [main]  INFO  com.intuit.karate - Karate version: 1.4.1
12:34:23.802 [main]  INFO  com.intuit.karate.Suite - backed up existing 'target\karate-reports' dir to: target\karate-reports_1774110863798
12:34:24.828 [main]  DEBUG com.intuit.karate - request:
1 > POST http://localhost:8085/auth/token
1 > Content-Type: application/json; charset=UTF-8
1 > Content-Length: 37
1 > Host: localhost:8085
1 > Connection: Keep-Alive
1 > User-Agent: Apache-HttpClient/4.5.14 (Java/17.0.12)
1 > Accept-Encoding: gzip,deflate
{"username":"test","password":"test"}

12:34:24.865 [main]  DEBUG com.intuit.karate - response time in milliseconds: 36
1 < 200
1 < X-Content-Type-Options: nosniff
1 < X-XSS-Protection: 0
1 < Cache-Control: no-cache, no-store, max-age=0, must-revalidate
1 < Pragma: no-cache
1 < Expires: 0
1 < X-Frame-Options: DENY
1 < Content-Type: application/json
1 < Transfer-Encoding: chunked
1 < Date: Sat, 21 Mar 2026 16:34:24 GMT
1 < Keep-Alive: timeout=60
1 < Connection: keep-alive
{"access_token":"eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJ0ZXN0Iiwicm9sZSI6IlVTRVIiLCJpc3MiOiJodHRwOi8vbG9jYWxob3N0OjgwODUiLCJpYXQiOjE3NzQxMTA4NjQsImV4cCI6MTc3NDExNDQ2NH0.J6gHv4LF_xHJZZHwKmV75flqkQRvqrhZb1dABcdj_Bw","expires_in":3600,"token_type":"Bearer"}

12:34:24.978 [main]  INFO  com.intuit.karate - [print] {
  "alg": "HS256"
}

12:34:24.981 [main]  INFO  com.intuit.karate - [print] {
  "sub": "test",
  "role": "USER",
  "iss": "http://localhost:8085",
  "iat": 1774110864,
  "exp": 1774114464
}

12:34:24.994 [main]  DEBUG com.intuit.karate - request:
2 > GET http://localhost:8085/api/hello
2 > Authorization: Bearer eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJ0ZXN0Iiwicm9sZSI6IlVTRVIiLCJpc3MiOiJodHRwOi8vbG9jYWxob3N0OjgwODUiLCJpYXQiOjE3NzQxMTA4NjQsImV4cCI6MTc3NDExNDQ2NH0.J6gHv4LF_xHJZZHwKmV75flqkQRvqrhZb1dABcdj_Bw
2 > Host: localhost:8085
2 > Connection: Keep-Alive
2 > User-Agent: Apache-HttpClient/4.5.14 (Java/17.0.12)
2 > Accept-Encoding: gzip,deflate


12:34:25.004 [main]  DEBUG com.intuit.karate - response time in milliseconds: 10
2 < 200
2 < X-Content-Type-Options: nosniff
2 < X-XSS-Protection: 0
2 < Cache-Control: no-cache, no-store, max-age=0, must-revalidate
2 < Pragma: no-cache
2 < Expires: 0
2 < X-Frame-Options: DENY
2 < Content-Type: text/plain;charset=UTF-8
2 < Content-Length: 10
2 < Date: Sat, 21 Mar 2026 16:34:25 GMT
2 < Keep-Alive: timeout=60
2 < Connection: keep-alive
hello test

12:34:25.006 [main]  INFO  com.intuit.karate - [print] RESPONSE: hello test
---------------------------------------------------------
feature: features/oauth.feature
scenarios:  1 | passed:  1 | failed:  0 | time: 0.4955
---------------------------------------------------------

12:34:25.589 [main]  INFO  com.intuit.karate.Suite - <<pass>> feature 1 of 1 (0 remaining) features/oauth.feature
Karate version: 1.4.1
======================================================
elapsed:   1.88 | threads:    1 | thread time: 0.50
features:     1 | skipped:    0 | efficiency: 0.26
scenarios:    1 | passed:     1 | failed: 0
======================================================

HTML report: (paste into browser to view) | Karate version: 1.4.1
file:///C:/developer/sergueik/springboot_study/basic-karate-example3/target/karate-reports/karate-summary.html
===================================================================

```
review results in the browser
```cmd
"C:\Program Files\Google\Chrome\Application\chrome.exe" -url "file:///C:/developer/sergueik/springboot_study/basic-karate-example3/target/karate-reports/karate-summary.html"
```
### Passing Authentication Tokens

run the following feature:
```feature
Feature: API hello with credentials

Background:
  * url 'http://localhost:8085'

  # System property for username (default Pete)
  * def username = karate.properties['username'] || 'Pete'
  * def password = karate.properties['password'] || 'test'

  * print 'USERNAME:', username
  * print 'PASSWORD:', password

Scenario: Authenticate and call protected API

  # 1. Request token with credentials
  Given path '/auth/token'
  And request { username: '#(username)', password: '#(password)' }
  When method post
  Then status 200

  # Extract token
  * def token = response.access_token
  * print 'TOKEN:', token

  # 2. Call protected API
  Given path '/api/hello'
  And header Authorization = 'Bearer ' + token
  When method get
  Then status 200

  # 3. Assert response
  * print 'RESPONSE:', response
  * match response == 'hello ' + username

```
* relaunch the app with a different "authorized user":
```cmd
mvn -DskipTests spring-boot:run -Dspring-boot.run.arguments="--example.username=Paul --example.password=test"
```

test accessing protected api `/api/hello` with JWT token:
```cmd
java -cp Dusername=Paul target\lib\* com.intuit.karate.cli.Main features\hello.feature
```

the server will log __Paul__ is visiting  
```text
2026-03-21T13:33:27.022-04:00  INFO 33688 --- [nio-8085-exec-1] example.controller.TokenController       : token request for Paul/test
```

the Karate test run log shows success

```text
13:33:25.590 [main]  INFO  com.intuit.karate - Karate version: 1.4.1
13:33:25.833 [main]  INFO  com.intuit.karate.Suite - backed up existing 'target\karate-reports' dir to: target\karate-reports_1774114405830
13:33:26.598 [main]  INFO  com.intuit.karate - [print] USERNAME: Paul
13:33:26.601 [main]  INFO  com.intuit.karate - [print] PASSWORD: test
13:33:26.874 [main]  DEBUG com.intuit.karate - request:
1 > POST http://localhost:8085/auth/token
1 > Content-Type: application/json; charset=UTF-8
1 > Content-Length: 37
1 > Host: localhost:8085
1 > Connection: Keep-Alive
1 > User-Agent: Apache-HttpClient/4.5.14 (Java/17.0.12)
1 > Accept-Encoding: gzip,deflate
{"username":"Paul","password":"test"}

13:33:27.076 [main]  DEBUG com.intuit.karate - response time in milliseconds: 201
1 < 200
1 < X-Content-Type-Options: nosniff
1 < X-XSS-Protection: 0
1 < Cache-Control: no-cache, no-store, max-age=0, must-revalidate
1 < Pragma: no-cache
1 < Expires: 0
1 < X-Frame-Options: DENY
1 < Content-Type: application/json
1 < Transfer-Encoding: chunked
1 < Date: Sat, 21 Mar 2026 17:33:27 GMT
1 < Keep-Alive: timeout=60
1 < Connection: keep-alive
{"token_type":"Bearer","expires_in":3600,"access_token":"eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJQYXVsIiwicm9sZSI6IlVTRVIiLCJpc3MiOiJodHRwOi8vbG9jYWxob3N0OjgwODUiLCJpYXQiOjE3NzQxMTQ0MDcsImV4cCI6MTc3NDExODAwN30.KuoWas8FaNJ0-30QyPomXKAtLrwrjsdCqjHSzq21i9E"}

13:33:27.079 [main]  INFO  com.intuit.karate - [print] TOKEN: eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJQYXVsIiwicm9sZSI6IlVTRVIiLCJpc3MiOiJodHRwOi8vbG9jYWxob3N0OjgwODUiLCJpYXQiOjE3NzQxMTQ0MDcsImV4cCI6MTc3NDExODAwN30.KuoWas8FaNJ0-30QyPomXKAtLrwrjsdCqjHSzq21i9E
13:33:27.088 [main]  DEBUG com.intuit.karate - request:
2 > GET http://localhost:8085/api/hello
2 > Authorization: Bearer eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJQYXVsIiwicm9sZSI6IlVTRVIiLCJpc3MiOiJodHRwOi8vbG9jYWxob3N0OjgwODUiLCJpYXQiOjE3NzQxMTQ0MDcsImV4cCI6MTc3NDExODAwN30.KuoWas8FaNJ0-30QyPomXKAtLrwrjsdCqjHSzq21i9E
2 > Host: localhost:8085
2 > Connection: Keep-Alive
2 > User-Agent: Apache-HttpClient/4.5.14 (Java/17.0.12)
2 > Accept-Encoding: gzip,deflate


13:33:27.114 [main]  DEBUG com.intuit.karate - response time in milliseconds: 25
2 < 200
2 < X-Content-Type-Options: nosniff
2 < X-XSS-Protection: 0
2 < Cache-Control: no-cache, no-store, max-age=0, must-revalidate
2 < Pragma: no-cache
2 < Expires: 0
2 < X-Frame-Options: DENY
2 < Content-Type: text/plain;charset=UTF-8
2 < Content-Length: 10
2 < Date: Sat, 21 Mar 2026 17:33:27 GMT
2 < Keep-Alive: timeout=60
2 < Connection: keep-alive
hello Paul

13:33:27.117 [main]  INFO  com.intuit.karate - [print] RESPONSE: hello Paul
---------------------------------------------------------
feature: features/hello.feature
scenarios:  1 | passed:  1 | failed:  0 | time: 0.6298
---------------------------------------------------------

13:33:27.619 [main]  INFO  com.intuit.karate.Suite - <<pass>> feature 1 of 1 (0 remaining) features/hello.feature
Karate version: 1.4.1
======================================================
elapsed:   1.85 | threads:    1 | thread time: 0.63
features:     1 | skipped:    0 | efficiency: 0.34
scenarios:    1 | passed:     1 | failed: 0
======================================================

HTML report: (paste into browser to view) | Karate version: 1.4.1
file:///C:/developer/sergueik/springboot_study/basic-karate-example3/target/karate-reports/karate-summary.html
===================================================================

```
indicating the user name was successfully extracted from the JWT token

![Html Report](screenshots/capture-karate-oauth2-2.png)

### Notes on Karate + GraalVM compatibility and Java interop

During implementation, a few Karate JS helper functions (e.g. `karate.fromBase64`, `karate.json`) were found to behave inconsistently, resulting in errors such as `Unknown identifier: json`.

The root cause is believed to be related to classpath inconsistencies between Karate __1.4.x__ and fast evolving __GraalVM__ __JS__/__polyglot__ libraries. 
When multiple or mismatched __GraalJS__ artifacts are present, the __Karate__ ScenarioBridge may fail confused to properly expose __JS__ helper functions to the execution context.

As a practical workaround, Java standard library one-liner equivalents were used instead:

`java.util.Base64.getUrlDecoder()` is used for Base64URL decoding of JWT segments instead of `karate.fromBase64`, ensuring correct handling of URL-safe encoding used in JWT tokens.

`java.util.Arrays.asList(token.split("\\."))` is used to split and structure JWT parts, avoiding inconsistencies observed with JS `split()` in this runtime.

Intermediate variables (e.g. `def len = parts.length`) are introducedto force evaluation and avoid type coercion issues in the __Karate__ matcher layer.

These adjustments keep the original test logic intact while improving reliability by avoiding fragile JS interop paths and relying on deterministic Java implementations for core operations such as decoding and parsing.

### Troublehooting

```text
>>> failed features:
js failed:
>>>>
01: karate.json(headerJsonString)
<<<<
org.graalvm.polyglot.PolyglotException: TypeError: invokeMember (json) on com.intuit.karate.core.ScenarioBridge failed due to: Unknown identifier: json
- <js>.:program(Unnamed:1)

features/oauth.feature:31
```

```cmd
unzip -ql target\karate-tests.jar | findstr -i ScenarioBridge
```
```text
     1263  2023-10-16 05:51   com/intuit/karate/core/ScenarioBridge$LogWrapper.class
    41695  2023-10-16 05:51   com/intuit/karate/core/ScenarioBridge.class
    40789  2023-10-16 05:51   com/intuit/karate/core/ScenarioBridge.java
      903  2023-10-16 05:51   com/intuit/karate/core/ScenarioBridge$ValueIndex.class
     1615  2023-10-16 05:51   com/intuit/karate/core/ScenarioBridge$LogFacade.class
     1343  2023-10-16 05:51   com/intuit/karate/core/ScenarioBridge$StringValueIndex.class
     1346  2023-10-16 05:51   com/intuit/karate/core/ScenarioBridge$NumberValueIndex.class
```

```sh
mvn clean package
```
```sh
java -cp target\lib\* com.intuit.karate.cli.Main features\oauth.feature
```
classpath includes incomplete or conflicting Graal / polyglot artifacts which ruins  Karate 1.4.x that depends on a consistent GraalVM / JS runtime setup, and the the bridge methods silently fail to register

```cmd
dir target\lib\ | grep -iE "(polyglot|js|graal)"
```
```text
03/21/2026  10:01 AM           719,452 graal-sdk-22.3.3.jar
03/21/2026  10:01 AM           122,937 jackson-datatype-jsr310-2.15.2.jar
03/21/2026  10:01 AM        23,560,510 js-22.3.3.jar
03/21/2026  10:01 AM             1,832 js-25.0.2.pom
03/21/2026  10:01 AM        26,423,853 js-language-25.0.2.jar
03/21/2026  10:01 AM            73,567 js-scriptengine-22.3.3.jar
03/21/2026  10:01 AM           277,582 json-path-2.8.0.jar
03/21/2026  10:01 AM           120,206 json-smart-2.4.10.jar
03/21/2026  10:01 AM            19,936 jsr305-3.0.2.jar
03/21/2026  10:01 AM           515,471 polyglot-25.0.2.jar

```
__Karate__ __1.4.1__ was built and tested against __GraalVM__ __22.x__ in 2022.

However:

❗ Maven Central no longer reliably publishes the full __22.x__ __GraalJS__ stack in a way that cleanly resolves via standard dependencies.

### Misc.

Karate “fat JAR” is not something one would pull as aready-made artifact from Maven Central in the sense of a self-contained executable

What is available in Maven Central are individual Karate modules to assemble via Maven Shade / Assembly plugin...

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
