### Info

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
### Note

With parent Springboot 1.5.4 the project does not compile:
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
### See Also

  * [H2DB example](https://reflectoring.io/spring-boot-paging/)
  * basics of [pagination and sorting using Spring Data JPA](https://www.baeldung.com/spring-data-jpa-pagination-sorting)
  * another [Spring boot pagination and sorting example](https://howtodoinjava.com/spring-boot2/pagination-sorting-example/) tutrial document

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
