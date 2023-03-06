### Info

This directory contains example application which exercises explicit startup bean implementation from the article
[Details of properly writing custom startup bean of a Spring application](https://www.baeldung.com/running-setup-logic-on-startup-in-spring#2-the-initializingbean-interface)
for loading a complex json / yaml configuration for situation when a plain SpringBoot annotation is not nsufficient

The example defines a plain key named `name` in SpringBoot fashion, via the `application.properties`
but in a `PostConstruct` scope, Application has a method to loads a (heavy) `configuration.yaml` fragment with amended value of that key
In the current revision all logic is defined in the `ExampleApplication` class, the breakdown by classes is WIP.

For a bootstrapped configuration loading sample code from [YAML with SnakeYAML](https://www.baeldung.com/java-snake-yaml) is used

### See also:
  [forum](https://qna.habr.com/q/801303)(in Russian) where the topic was discussed
  [details of AppProperties](https://www.baeldung.com/properties-with-spring)
  [Guide to @ConfigurationProperties in Spring Boot](https://www.baeldung.com/configuration-properties-in-spring-boot) 

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
