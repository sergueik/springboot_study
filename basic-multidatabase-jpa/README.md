###  Info

  this directory contains code extracted from [connecting to two databases simultaneously via JPA](https://www.baeldung.com/spring-data-jpa-multiple-databases) tutorial repository. Currentlt there is no controllers in the example appplication so it only initializes and then quits
  Added a dummy controller and H2DB console to explore the databases and tables while application is running


### Usage

```sh
mvn clean spring-boot:run
```

open H2DB console and connect to two databases and explore tables:

![User](https://github.com/sergueik/springboot_study/blob/master/basic-multidatabase-jpa/screenshots/capture-user.png)

![Product](https://github.com/sergueik/springboot_study/blob/master/basic-multidatabase-jpa/screenshots/capture-product.png)

### See Also

  * [forum topic](https://qna.habr.com/q/1316532)(in Russian)
  * [spring Boot With H2 Database](https://www.baeldung.com/spring-boot-h2-database)

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)


