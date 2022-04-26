### Info
directory contains elementary API key-auth security example based on
[springboot-keyauth](https://github.com/manyu235/springboot-keyauth)

### Usage

* acess nonsecure endpoint
```sh
curl -s -v http://localhost:8081/api/v1/nonsecure
```
will receive an `HTTP 200 OK` response.
```text
HTTP/1.1 200
Content-Length: 0
```

* attempt to send a request to the secure endpoint:
```sh
curl -v http://localhost:8081/api/v1/secure
```
```text
HTTP/1.1 403
```

* send a request to the secure endpoint with an API key:
```sh
curl -s -v --header "API_KEY: somedata" http://localhost:8081/api/v1/secure
```
will still get a 403
* provide the correct key:
```sh
curl -s --header "API_KEY: d3ebf20f-d202-4d9c-bcc9-80cb8de64901" http://localhost:8081/api/v1/secure
```
will be handled by controller:
```text
Processed request: /api/v1/secure
```
### See Also
  * `API_KEY` with credentials database [springboot example](https://github.com/gregwhitaker/springboot-apikey-example)
  * * `API_KEY` with credentials database [springboot webflux example](https://github.com/gregwhitaker/springboot-webflux-apikey-example)
  * https://learning.postman.com/docs/writing-scripts/script-references/test-examples/#asserting-a-response-value-against-a-variable

#### Postman Resources
  * [discussion of test iterations in Postman](https://automated-testing.info/t/posledovatelnost-zaprosov-s-menyayushhimisya-parametrami-v-postman/21884/3)(in Russian)
  * https://community.postman.com/t/test-if-response-body-include-any-of-values/24290
  * https://www.postman.com/cloudy-station-145681/workspace/e358eb9c-1956-4820-9dd8-8e73734fb917
  * https://fooobar.com/questions/2403780/postman-how-to-loop-request-until-i-get-a-specific-response  (in Russian)
  * [big Postman guide](https://testengineer.ru/gajd-po-testirovaniyu-v-postman/) (in Russian)
  * [intro to Postman](https://habr.com/ru/company/kolesa/blog/351250/) (in Russian)
  * https://github.com/poynt/postman-runner - standalone runner


### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
