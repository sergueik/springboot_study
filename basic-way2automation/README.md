### Info
local replica of the [Way2Automation Banking App](http://www.way2automation.com/angularjs-protractor/banking/#/login) dummy website for Testing Selenium WebDriver / Protractor Automation scripts material by
[Way2Automation](https://www.way2automation.com/protractor-angularjs-practice-website.html)

```sh
export FILES="app.js user.service.js account.service.js transaction.service.js mockDataLoadService.js customer.data.js config.js date.search.filter.js accountViewController.js addCustomerController.js customerViewController.js bodyController.js depositController.js listCustomerController.js mainController.js managerViewController.js openAccountController.js optionsController.js transactionSummaryController.js withdrawlController.js"
pushd js
for F in $FILES ;do curl -skLO http://www.way2automation.com/angularjs-protractor/banking/$F ;done
popd
pushd css
export FILES=style.css
for F in $FILES ;do curl -skLO http://www.way2automation.com/angularjs-protractor/banking/$F ;done
popd
export FILES="account.html depositTx.html main.html openAccount.html customerList.html home.html managerView.html options.html customerView.html listTx.html newCustomer.html withdrawlTx.html"
for F in $FILES ;do curl -skLO http://www.way2automation.com/angularjs-protractor/banking/$F ;done
```

```sh
mvn clean -DskipTests package
```
```sh
unzip -ql target\example.way2automation.jar
```
```sh
java -jar target\example.way2automation.jar
```
Use `http://localhost:8080/application#/login` as in cucumber 

### See Also

  * [original static thymeleaf project](https://github.com/kolorobot/spring-boot-thymeleaf)
  * [linking the thymeleaft page resources stored in local directory](https://stackoverflow.com/questions/29460618/inserting-an-image-from-local-directory-in-thymeleaf-spring-framework-with-mave)
  * [thymeleaf in a Spring MVC application](https://www.baeldung.com/thymeleaf-in-spring-mvc)
  * [properties](https://www.baeldung.com/properties-with-spring) with Spring and Spring Boot
  * [static resources](https://www.baeldung.com/spring-mvc-static-resources) - customizing path to support legacy non-standard layouts
  * [DAO patterns](https://www.baeldung.com/java-dao-pattern)

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
