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
mvn spring-boot:run
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
HTTP Status 401 - Full authentication is required to access this resource
```
```
curl -u user:password -k -v https://localhost:8443/welcome
```
```text
HTTP Status 401 - Bad credentials
```
```sh
curl -u admin:password -k -v https://localhost:8443/welcome
```
```text
* Connected to localhost (127.0.0.1) port 8443 (#0)
* ALPN, offering h2
* ALPN, offering http/1.1
* successfully set certificate verify locations:
*  CAfile: C:/Program Files/Git/mingw64/ssl/certs/ca-bundle.crt
*  CApath: none
} [5 bytes data]
* TLSv1.3 (OUT), TLS handshake, Client hello (1):
} [512 bytes data]
* TLSv1.3 (IN), TLS handshake, Server hello (2):
{ [81 bytes data]
* TLSv1.2 (IN), TLS handshake, Certificate (11):
{ [881 bytes data]
* TLSv1.2 (IN), TLS handshake, Server key exchange (12):
{ [333 bytes data]
* TLSv1.2 (IN), TLS handshake, Server finished (14):
{ [4 bytes data]
* TLSv1.2 (OUT), TLS handshake, Client key exchange (16):
} [70 bytes data]
* TLSv1.2 (OUT), TLS change cipher, Change cipher spec (1):
} [1 bytes data]
* TLSv1.2 (OUT), TLS handshake, Finished (20):
} [16 bytes data]
* TLSv1.2 (IN), TLS handshake, Finished (20):
{ [16 bytes data]
* SSL connection using TLSv1.2 / ECDHE-RSA-AES128-GCM-SHA256
* ALPN, server did not agree to a protocol
* Server certificate:
*  subject: C=US; ST=FL; L=Miami; O=example; OU=example; CN=test user
*  start date: Dec  4 07:59:34 2021 GMT
*  expire date: Dec  2 07:59:34 2031 GMT
*  issuer: C=US; ST=FL; L=Miami; O=example; OU=example; CN=test user
*  SSL certificate verify result: self signed certificate (18), continuing anyway.
* Server auth using Basic with user 'admin'
} [5 bytes data]
> GET /welcome HTTP/1.1
> Host: localhost:8443
> Authorization: Basic YWRtaW46cGFzc3dvcmQ=
> User-Agent: curl/7.74.0
> Accept: */*
>
{ [5 bytes data]
* Mark bundle as not supporting multiuse
< HTTP/1.1 200
< Set-Cookie: JSESSIONID=CAAEF2A6AA642CCD49F74A12DF7D041C; Path=/; Secure; HttpOnly
< X-Content-Type-Options: nosniff
< X-XSS-Protection: 1; mode=block
< Cache-Control: no-cache, no-store, max-age=0, must-revalidate
< Pragma: no-cache
< Expires: 0
< Strict-Transport-Security: max-age=31536000 ; includeSubDomains
< X-Frame-Options: DENY
< Content-Type: text/plain;charset=UTF-8
< Content-Length: 7
< Date: Sat, 04 Dec 2021 06:34:27 GMT
<
{ [7 bytes data]

welcome

```
https://www.baeldung.com/spring-boot-https-self-signed-certificate
https://github.com/eugenp/tutorials/blob/master/spring-security-modules/spring-security-web-boot-2/WebContent/META-INF/MANIFEST.MF
