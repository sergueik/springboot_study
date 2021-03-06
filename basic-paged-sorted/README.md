﻿### Info

This direcory contains replica of the [Spring Boot JPA Pagination and Sorting](https://github.com/bezkoder/spring-boot-jpa-paging-sorting) repository with SQLite instead of MySQL JPA.
that  is covered in [example document](https://bezkoder.com/spring-boot-pagination-sorting-example/)


#### Usage

* initialize table in the database
```cmd
%userprofile%\desktop\springboot.db
```
via `sqlite3`:
```cmd
sqlite3.exe "springboot.db" "DROP TABLE IF EXISTS tutorials; CREATE TABLE `tutorials` (`id` integer,`title` varchar,`description` varchar,`published` integer, PRIMARY KEY(`id`));"
for /L  %. in ( 1 1 30)  do @sqlite3.exe "springboot.db" "INSERT INTO `tutorials` (title,description,published) VALUES( 'title %.' ,	'description %.' ,1 );"
sqlite3.exe "springboot.db" "select * from tutorials"
```
or by running SQL in [DB Browser (SQLite)](https://sqlitebrowser.org/) using [SQLITE3 syntax](https://sqlite.org/lang_with.html)
```sql
CREATE TABLE `tutorials` ( `id`	integer, `title` varchar, `description` varchar, `published` integer, PRIMARY KEY(`id`));
DELETE FROM TUTORIALS;
WITH RECURSIVE
  cnt(num) AS (VALUES(1) UNION ALL SELECT num + 1 FROM cnt WHERE num < 30)
INSERT INTO `tutorials` (title,description,published) SELECT 'title ' || num ,  'description ' || num , num FROM cnt ;
SELECT * FROM tutorials;
```
* run the application
```cmd
mvn spring-boot:run
```
see the paged JSON response
```cmd
curl.exe "http://localhost:8080/api/tutorials?size=3&page=2" 2>NUL > %temp%\a.json
type %temp%\a.json |c:\tools\jq-win64.exe -M "."
```
this will output a set
```json
{
  "totalItems": 30,
  "tutorials": [
    {
      "id": 24,
      "title": "title 24",
      "description": "description 24",
      "published": true
    },
    {
      "id": 23,
      "title": "title 23",
      "description": "description 23",
      "published": true
    },
    {
      "id": 22,
      "title": "title 22",
      "description": "description 22",
      "published": true
    }
  ],
  "totalPages": 10,
  "currentPage": 2
}
```
### Notes

* with parent Springboot 1.5.4 the project does not compile:
```sh
Compilation failure:
[ERROR] src/main/java/example/controller/TutorialController.java:[73,83] cannot find symbol
[ERROR]   symbol:   method by(java.util.List<org.springframework.data.domain.Sort.Order>)
[ERROR]   location: class org.springframework.data.domain.Sort
[ERROR] src/main/java/example/controller/TutorialController.java:[109,78] cannot find symbol
[ERROR]   symbol:   method by(java.util.List<org.springframework.data.domain.Sort.Order>)
[ERROR]   location: class org.springframework.data.domain.Sort
[ERROR] src/main/java/example/controller/TutorialController.java:[139,54] cannot find symbol
[ERROR]   symbol:   method of(int,int)
[ERROR]   location: class org.springframework.data.domain.PageRequest
[ERROR] src/main/java/example/controller/TutorialController.java:[161,69] cannot find symbol
[ERROR]   symbol:   method findById(long)
[ERROR]   location: variable tutorialRepository of type example.repository.TutorialRepository
[ERROR] src/main/java/example/controller/TutorialController.java:[185,69] cannot find symbol
[ERROR] src/main/java/example/controller/TutorialController.java:[203,43] cannot find symbol
[ERROR]   symbol:   method deleteById(long)
[ERROR]   location: variable tutorialRepository of type example.repository.TutorialRepository
```
in addition there is no way to switch parent via profile:
```sh
Malformed POM. Unrecognised tag: 'parent'
```

* latest 2.x release of [Maven Surefire Plugin](https://mvnrepository.com/artifact/org.apache.maven.plugins/maven-surefire-plugin) is __2.22.2__.
The __2.20___ that is performing just fine in other projects, here leads to tests being completely skipped:
```sh
Tests run: 0, Failures: 0, Errors: 0, Skipped: 0
```
* observed conflict between [Maven Surefire Plugin](https://maven.apache.org/surefire/maven-surefire-plugin/) and [JaCoCo Maven Plugin](https://www.eclemma.org/jacoco/trunk/doc/maven.html) configurations :
after uncommenting the `argLine` in `pom.xml`:
```xml
      <plugin>
        <groupId>org.apache.maven.plugins</groupId>
        <artifactId>maven-surefire-plugin</artifactId>
        <version>${maven-surefire-plugin.version}</version>
        <configuration>
          <trimStackTrace>false</trimStackTrace>
          <encoding>${project.build.sourceEncoding}</encoding>
<!--
          <argLine>-Dfile.encoding=${project.build.sourceEncoding} -DAPP_LOG_ROOT=c:/temp</argLine>
-->
        </configuration>
      </plugin>

```
the code coverage is broken:
```sh
Skipping JaCoCo execution due to missing execution data file.
```


Normally one should see something like
```sh
[INFO] --- jacoco-maven-plugin:0.8.6:report (report) @ basic-paged-sorted ---
[INFO] Loading execution data file target\jacoco.exec
[INFO] Analyzed bundle 'basic-paged-sorted' with 8 classes
```


This is because `jacoco` itself modifies the `argLine`:
```sh
[INFO] --- jacoco-maven-plugin:0.8.6:prepare-agent (default) @ basic-paged-sorted ---
[INFO] argLine set to -javaagent:C:\\Users\\Serguei\\.m2\\repository\\org\\jacoco\\org.jacoco.agent\\0.8.6\\org.jacoco.agent-0.8.6-runtime.jar=destfile=C:\\developer\\sergueik\\springboot_study\\basic-paged-sorted\\target\\jacoco.exec
```


NOTE: replacing the method

```java

@GetMapping(value = "/tutorials")
public ResponseEntity<Map<String, Object>> getAllTutorialsPage(
	@RequestParam(required = false) String title,
	@RequestParam(defaultValue = "0") int page,
	@RequestParam(defaultValue = "3") int size,
	@RequestParam(defaultValue = "id,desc") String[] sort) {
	try {
		List<Order> orders = new ArrayList<>();
		if (sort[0].contains(",")) {
			// will sort more than 2 fields
			// sortOrder="field, direction"
			for (String sortOrder : sort) {
				String[] _sort = sortOrder.split(",");
				orders.add(new Order(getSortDirection(_sort[1]), _sort[0]));
			}
		} else {
			// sort=[field, direction]
			orders.add(new Order(getSortDirection(sort[1]), sort[0]));
		}
		List<Tutorial> tutorials = new ArrayList<>();
		Pageable pagingSort = PageRequest.of(page, size, Sort.by(orders));
		Page<Tutorial> pageTuts;

 // ...
}
```

with
```java
@GetMapping(value = "/tutorials")
public ResponseEntity<Map<String, Object>> getAllTutorialsPage(
	Pageable pagingSort, @RequestParam(required = false) String title) {
		try {
			List<Tutorial> tutorials = new ArrayList<>();
			Page<Tutorial> pageTuts;
// rest of the method unchanged
}

```
leads to test error:
```txt
org.springframework.web.util.NestedServletException: Request processing failed;
nested exception is: java.lang.IllegalStateException: No primary or default constructor found for interface
org.springframework.data.domain.Pageable
at org.springframework.web.method.annotation.ModelAttributeMethodProcessor.createAttribute(ModelAttributeMethodProcessor.java:219)
```

adding both leads to compiation error:
```txt
org.springframework.beans.factory.BeanCreationException:
Error creating bean with name 'requestMappingHandlerMapping' defined in class path resource
[org/springframework/boot/autoconfigure/web/servlet/WebMvcAutoConfiguration$EnableWebMvcConfiguration.class]:
Invocation of init method failed
nested exception is java.lang.IllegalStateException:
Ambiguous mapping. Cannot map 'tutorialController' method
example.controller.TutorialController#getAllTutorialsPage(Pageable, String)
to {GET /api/tutorials, produces [application/json]}:
There is already 'tutorialController' bean method
example.controller.TutorialController#getAllTutorialsPage(String, int, int, String[]) mapped.
```
### See Also

  * [H2DB example](https://reflectoring.io/spring-boot-paging/)
  * basics of [pagination and sorting using Spring Data JPA](https://www.baeldung.com/spring-data-jpa-pagination-sorting)
  * another [Spring boot pagination and sorting example](https://howtodoinjava.com/spring-boot2/pagination-sorting-example/) tutrial document
  * [discussion](https://stackoverflow.com/questions/52355490/no-primary-or-default-constructor-found-for-interface-org-springframework-data-d) of resolving the parameter resolution conflict arising with `Pageable`.
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
