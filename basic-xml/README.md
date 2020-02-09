### Info

Springboot Docker classic app-config.xml with commandline argument exercise project based on [Java Annotation and XML Bean Configuration](https://www.concretepage.com/spring-boot/spring-boot-xml-configuration-example) article

### Standalone test

```sh
mvn -Dtitle="Java Spring" -Dcategory="Programming" spring-boot:run
```
observe in  console:
```sh
Article: Programming "Java Spring"
```
access via GET request:
```sh
curl http://localhost:8080/user/articles
```
```sh
[ {
  "title" : "Java Spring",
  "category" : "Programming"
} ]

```
### Dockerized test

* package the jar
```sh
mvn -Dmaven.test.skip=true clean package
```
* build the image
```sh
docker build -f Dockerfile -t basic-xml-example --build-arg "title=\"Java Spring\"" --build-arg "category=Programming" .

```
* inspect the image
```sh
docker image inspect basic-xml-example | jq '.[].ContainerConfig.Cmd'
```
[

  "/bin/sh",
  "-c",
  "#(nop) ",
  "ENTRYPOINT [\"java\" \"-Dtitle=${title_env} -Dcategory=${category_env}\" \"-jar\" \"app.jar\"]"
]
```


```sh
docker image inspect basic-xml-example | jq '.[].ContainerConfig.Env'
```
```sh
[
  "PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/lib/jvm/java-1.8-openjdk/jre/bin:/usr/lib/jvm/java-1.8-openjdk/bin",
  "LANG=C.UTF-8",
  "JAVA_HOME=/usr/lib/jvm/java-1.8-openjdk/jre",
  "JAVA_VERSION=8u212",
  "JAVA_ALPINE_VERSION=8.212.04-r0",
  "title_env=\"Java Spring\"",
  "category_env=Programming"
]
```
* run docker container

```sh
docker run --rm -p 8080 basic-xml-example
```

* in console observe the debugging message listing the injected params:
```java
@SpringBootApplication
@ImportResource("classpath:app-config.xml")
public class Launcher {
  public static void main(String[] args) {
                ApplicationContext applicationContext = SpringApplication.run(Launcher.class, args);
                ArticleService articleService = applicationContext.getBean(ArticleService.class);

System.err.println(articleService.processMsg());
```
```sh
Started Launcher in 13.282 seconds (JVM running for 27.857)
Article: Programming "Java Spring"
```
```sh
curl http://localhost:8080/user/articles
```
### See Also:

https://www.springboottutorial.com/spring-boot-java-xml-context-configuration
https://www.javaguides.net/2018/09/spring-boot-xml-configuration-example.html
https://stackoverflow.com/questions/46877559/read-system-properties-in-spring-xml-config-application-with-expression-language
https://stackoverflow.com/questions/55261848/spring-load-command-line-arguments-from-xml-application-context

After run: [example.entity.Article@226884dc, example.entity.Article@62d06d22, example.entity.Article@2afc2de0]

https://www.baeldung.com/properties-with-spring
https://www.baeldung.com/constructor-injection-in-spring
