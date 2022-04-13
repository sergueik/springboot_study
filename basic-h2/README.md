### Info

this directory contains a replica Java class [H2 File Database Example](https://www.javatips.net/blog/h2-file-database-example) performing SQL on h2 via JDBC
`jdbc:h2` with few additional queries for exploring conversion to reactive flux observables

### Usage
* build
```sh
mvn package
```
* run
```sh
java -jar target/example.basic-h2.jar
```
the application will do some queries and  stay waiting for input
```text
H2 Database inserted through Statement
Id 1 Name Sales
Id 2 Name Marketing
Id 3 Name Human Resources
Id 4 Name Manufacturing
Id 5 Name Accounting
H2 Database inserted through PreparedStatement
Id: 1 First Name: Bob Last Name: Smith Department Id: 4
Id: 2 First Name: John Last Name: Green Department Id: 1
Id: 3 First Name: Sally Last Name: Wilson Department Id: 3
Id: 4 First Name: Harold Last Name: Smith Department Id: 2
Id: 5 First Name: Joe Last Name: White Department Id: 5
Id: 6 First Name: Arnold Last Name: Shoemaker Department Id: 4
H2 Database queried through PreparedStatement
Id: 1 First Name: Bob Last Name: Smith Department name: Manufacturing
Press Enter to quit
```
install [H2 Console Application jar](http://www.h2database.com/html/quickstart.html). Alternatibely run any `h2.jar` from Maven repository cache
in the console
```cmd
java -jar c:\Users\Serguei\.m2\repository\com\h2database\h2\2.1.210\h2-2.1.210.jar
```
This will launch the default browser and navigate to [h2 database browser page](http://192.168.99.1:8082/). Also can open [h2 database browser page](http://192.168.99.1:8082/) in the browser.
Enter `jdbc:h2:file:~/test` into the __JDBC URL__ input field and connect. Browse __EMPLOYEE__ or __DEPARTMENT__ table:


![h2 database in the h2 browser](https://github.com/sergueik/springboot_study/blob/master/basic-h2/screenshots/h2_browser_query_capture.png)
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)

