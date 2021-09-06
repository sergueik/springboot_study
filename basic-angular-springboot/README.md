### Info 
Replica of
   [sample app AngularJS and Spring Boot service](https://github.com/nagypeter/angularjs-springboot-CreditScoreApp) to convert into poller skeleton
(minimal fix added to make runnable and not fail with `net::ERR_NAME_NOT_RESOLVED`)

### TODO:
  * simplify the project `pom.xml` (currently blocked due to emerging 

```text
org.springframework.context.ApplicationContextException:
Unable to start EmbeddedWebApplicationContext
due to missing EmbeddedServletContainerFactory bean.
```
  * switch from `javax.servlet.jstl` to more traditional jsp
### See Also
   * https://www.journaldev.com/2090/jstl-tutorial-jstl-tags-example
   * https://github.com/alphabetY/springboot-angularjs
   * https://github.com/OKaluzny/springboot-rest-api-angularjs-https
   * https://www.baeldung.com/spring-template-engines
   * https://stackoverflow.com/questions/21783391/spring-boot-unable-to-start-embeddedwebapplicationcontext-due-to-missing-embedd 
