### Info

This directory contains code from the [article](https://www.knowledgefactory.net/2021/09/spring-webflux-file-download-rest-api.html)  downgraded to Java 1.8 and SpringBoot parent `2.3.4.RELEASE`

### Testing

*  make dummy download file a little bigger
```sh
for n in $(seq 0 1  1000) ; do echo 'Dummy text......Dummy text......Dummy text......' >> src/main/resources/dummy.txt ;  done
```
* run spring boot webflux app:
```sh
mvn spring-boot:run
```
* download file via curl:
```sh
curl http://localhost:8080/download/dummy.txt
```

file will printed to console:
```txt
Dummy text......Dummy text......Dummy text......
...
```
### See Also
  * [regular Spring REST controller returning file](https://www.baeldung.com/spring-controller-return-image-file)
  * origial article [source repository](https://github.com/knowledgefactory4u/KnowledgeFactory)
  * https://howtodoinjava.com/spring-webflux/spring-webflux-tutorial/
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
