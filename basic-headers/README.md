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
  * nginx [virtual hosts](https://rav.pw/nginx-virtual-hosts/) (in Russan)
  * https://www.digitalocean.com/community/tutorials/how-to-set-up-nginx-server-blocks-virtual-hosts-on-ubuntu-16-04
  * [](https://stackoverflow.com/questions/11973047/adding-and-using-header-http-in-nginx)
  * Puppet module and configuration to install and test [nginx](https://github.com/bconner22/puppet-nginx)
  * https://www.linode.com/docs/development/java/how-to-deploy-spring-boot-applications-nginx-ubuntu-16-04/
  * https://ru.stackoverflow.com/questions/832327/Настройка-nginx-java-web
