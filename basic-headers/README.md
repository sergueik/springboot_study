### Testing

* locally
```sh
mvn spring-boot:run
```
```sh
curl -H "Caller: test" -X GET http://localhost:8085/headers/map
```

will print out
```sh
Listed 4 headers:
Header "host" = localhost:8085
Header "user-agent" = curl/7.58.0
Header "accept" = */*
Header "caller" = test
```
alternatively

```sh
curl -H "Caller: test" -X GET http://localhost:8085/headers/typed
```
will print the same (can print more)

finally
```sh
curl -H "optional: set" -X GET http://localhost:8085/headers/check
```
wihh print
```sh
The optional header was present
```

### See Also:
 * [How to Read HTTP Headers in Spring REST Controllers](https://www.baeldung.com/spring-rest-http-headers)
 * https://stackoverflow.com/questions/52030672/how-to-send-multiple-header-values-with-the-same-name-using-curl
