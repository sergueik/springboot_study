### Info

contains example sources of the [Spring Boot app on HTTPS using Self-Signed server Certificate ](https://www.baeldung.com/spring-boot-https-self-signed-certificate) application and test combined with minimal basic-auth code to illustrate the  client code for the same

### Usage
* generate self-signed certificate for site
```cmd
keytool -genkeypair -alias basic -keyalg RSA -keysize 2048 -storetype PKCS12 -keystore basic.p12 -validity 3650
```

```sh
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
  [Unknown]:  test user
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
  [no]:  yes
```


This creates `basic.p12` - copy to `src/main/resources/keystore/basic.p12`.
Make sure to put the password filename and alias into `application.properties`.

```sh
mvn  -Dmaven.test.skip=true spring-boot:run
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
or cloning and referencing alternative `applicatiton.properties` does not override the HTTPS port 8443. This is currently solved through [tagging and filtering](https://www.baeldung.com/junit-filtering-tests) with __Junit 5__:

```sh
mvn -Pgroup1 clean test
mvn -Pgroup2 clean test
mvn -Pgroup2 clean test
```
### Feed to Prometheus

* place the app into Docker container
```sh
mvn -Dmaven.test.skip=true clean package
```
```sh
export IMAGE=application
docker build -t $IMAGE -f Dockerfile .
```
```sh
NAME=application
docker container rm -f $NAME
docker run --name $NAME -p 8443:8443 -d $IMAGE
```
```sh
docker logs $NAME
```
verify that `/metrics` is available on host:
```sh

curl -s -k https://192.168.0.29:8443/metrics
```

```text

# HELP requests_total Total number of requests.
# TYPE requests_total counter
requests_total 0.0
# HELP requests_latency_seconds Request latency in seconds.
# TYPE requests_latency_seconds histogram
requests_latency_seconds_bucket{le="0.005",} 0.0
requests_latency_seconds_bucket{le="0.01",} 0.0
requests_latency_seconds_bucket{le="0.025",} 0.0
requests_latency_seconds_bucket{le="0.05",} 0.0
requests_latency_seconds_bucket{le="0.075",} 0.0
requests_latency_seconds_bucket{le="0.1",} 0.0
requests_latency_seconds_bucket{le="0.25",} 0.0
requests_latency_seconds_bucket{le="0.5",} 0.0
requests_latency_seconds_bucket{le="0.75",} 0.0
requests_latency_seconds_bucket{le="1.0",} 0.0
requests_latency_seconds_bucket{le="2.5",} 0.0
requests_latency_seconds_bucket{le="5.0",} 0.0
requests_latency_seconds_bucket{le="7.5",} 0.0
requests_latency_seconds_bucket{le="10.0",} 0.0
requests_latency_seconds_bucket{le="+Inf",} 0.0
requests_latency_seconds_count 0.0
requests_latency_seconds_sum 0.0

```
* pull prometheus image
```sh
VERSION=2.27.0
docker pull prom/prometheus:v$VERSION
```
* add configuration file `prometheus.yml` listing `application` host:
```yaml
scrape_configs:
  - job_name:       'application'
    scheme: https
    tls_config:
      insecure_skip_verify: true

    scrape_interval: 10s
    metrics_path: /metrics
    honor_labels: true

    static_configs:
      - targets: ['application:8443']
        labels:
          group: 'application'
```
* run
```sh
docker run --link application  -p 9090:9090  -v $(pwd)/prometheus.yml:/etc/prometheus/prometheus.yml prom/prometheus:v$VERSION
```
* note if seeing
```text
Get "https://application:8443/metrics": x509: certificate is valid for localhost, not application
```
then redo the command
```sh
keytool -genkeypair -alias basic -keyalg RSA -keysize 2048 -storetype PKCS12 -keystore basic.p12 -validity 3650 -ext "SAN:c=DNS:application,IP:172.17.0.2"
```
and place the updated key into application resources
or when observing the error
```text
Get "https://application:8443/metrics": x509: certificate signed by unknown authority
```

make sure to set Prometheus to ignore insecure keys. The other alternaive would be to copy the __CA__ info ???
* open in the browser `http://192.168.0.64:9090/`

![Sample Page](https://github.com/sergueik/springboot_study/blob/master/basic-https-prometheus-counter/screenshots/capture_https_metrics.png)

### See Also

  * [hint](https://stackoverflow.com/questions/50928061/certificate-for-localhost-doesnt-match-any-of-the-subject-alternative-names) for solving `Certificate for <localhost> doesn't match any of the subject alternative names: []`
  * https://github.com/eugenp/tutorials/blob/master/spring-security-modules/spring-security-web-boot-2/WebContent/META-INF/MANIFEST.MF
  * https://javabydeveloper.com/junit-5-tag-and-filtering-tags-with-examples/
  * https://stackoverflow.com/questions/64810671/spring-boot-random-sslexception-connection-reset-in-kubernetes-with-jdk11
  * [junit 5 and junit4 test tagging](https://www.baeldung.com/junit-filtering-tests)
  * tutorial on [customizing](basic-perl-cgi-java/src/test/java/example/controller/MockServiceTest.java) the  Spring RestTemplate Configuration to manage ConnectionPooling and KeepAlive
  * [stackoverflow](https://stackoverflow.com/questions/63507002/x509-certificate-signed-by-unknown-authority-for-prometheus) on suppressing *x509: certificate signed by unknown authority* error from Prometheus

### Author

[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
