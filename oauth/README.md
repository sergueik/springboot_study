### Info


This directory contains a replica of springboot/oath project from
[spring-rest-service-oauth](https://github.com/royclarkson/spring-rest-service-oauth)
upgraded to `spring-boot-starter-parent`  version __1.5.4.RELEASE__

### Usage 
```sh
mvn -Dmaven.test.skip=true clean package spring-boot:run
```

```sh
curl -sv http://localhost:8080/greeting
```
With `spring-boot` __1.5.4.RELEASE__ will receive a 302 redirect

```text
* Uses proxy env variable no_proxy == '192.168.99.103,192.168.99.100'
*   Trying 127.0.0.1:8080...
* Connected to 127.0.0.1 (127.0.0.1) port 8080 (#0)
> GET /greeting HTTP/1.1
> Host: 127.0.0.1:8080
> User-Agent: curl/7.75.0
> Accept: */*
>
* Mark bundle as not supporting multiuse
< HTTP/1.1 302
< X-Content-Type-Options: nosniff
< X-XSS-Protection: 1; mode=block
< Cache-Control: no-cache, no-store, max-age=0, must-revalidate
< Pragma: no-cache
< Expires: 0
< X-Frame-Options: DENY
< Set-Cookie: JSESSIONID=5043FA6A990E4CF6E036E260359CB090; Path=/; HttpOnly
< Location: http://127.0.0.1:8080/login
< Content-Length: 0
< Date: Sun, 16 Jun 2024 15:19:35 GMT
<

```
```sh
curl -XPOST -s http://127.0.0.1:8080/login
```
```json
{
  "timestamp": 1718551424298,
  "status": 403,
  "error": "Forbidden",
  "message": "Could not verify the provided CSRF token because your session was not found.",
  "path": "/login"
}
```
```sh
curl -X POST -vsu clientapp:123456 http://127.0.0.1:8080/oauth/token -H "Accept: application/json" -d "password=spring&username=roy&grant_type=password&scope=read%20write&client_secret=123456&client_id=clientapp"
```
```text
{"access_token":"815d7333-85ec-42d8-976c-d2408df8aa68","token_type":"bearer","refresh_token":"9fff459f-feb9-4440-aee8-967a54135570","expires_in":43172,"scope":"read write"}* Uses proxy env variable no_proxy == '192.168.99.103,192.168.99.100'
*   Trying 127.0.0.1:8080...
* Connected to 127.0.0.1 (127.0.0.1) port 8080 (#0)
* Server auth using Basic with user 'clientapp'
> POST /oauth/token HTTP/1.1
> Host: 127.0.0.1:8080
> Authorization: Basic Y2xpZW50YXBwOjEyMzQ1Ng==
> User-Agent: curl/7.75.0
> Accept: application/json
> Content-Length: 108
> Content-Type: application/x-www-form-urlencoded
>
} [108 bytes data]
* Mark bundle as not supporting multiuse
< HTTP/1.1 200
< X-Content-Type-Options: nosniff
< X-XSS-Protection: 1; mode=block
< Cache-Control: no-cache, no-store, max-age=0, must-revalidate
< Pragma: no-cache
< Expires: 0
< X-Frame-Options: DENY
< Content-Type: application/json;charset=UTF-8
< Transfer-Encoding: chunked
< Date: Sun, 16 Jun 2024 15:25:30 GMT
<
{ [178 bytes data]
* Connection #0 to host 127.0.0.1 left intact
```

```sh
curl -s -H '{"access_token":"815d7333-85ec-42d8-976c-d2408df8aa68","token_type":"bearer","refresh_token":"9fff459f-feb9-4440-aee8-967a54135570","expires_in":42711,"scope":"read write"}' -XPOST -s http://127.0.0.1:8080/login
```
```json
{
  "timestamp": 1718552007993,
  "status": 403,
  "error": "Forbidden",
  "message": "Could not verify the provided CSRF token because your session was not found.",
  "path": "/login"
}

```
### See also

  * Example [wingyplus/robotframework-serverspeclibrary](https://github.com/wingyplus/robotframework-serverspeclibrary)
  * [RESTful API with JWT](https://auth0.com/blog/implementing-jwt-authentication-on-spring-boot/)
  * [REST APIs with JWT](https://medium.com/@hantsy/protect-rest-apis-with-spring-security-and-jwt-5fbc90305cc5)
  * [example project](https://github.com/hantsy/spring-webmvc-jwt-sample)
  * JWT operation [diagram](https://www.javainuse.com/spring/boot-jwt)
