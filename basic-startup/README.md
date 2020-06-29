### Info

This directory exercises explicit startup bean implementation outlined in 
[Details of properly wriging custom startup bean of a Spring application](https://www.baeldung.com/running-setup-logic-on-startup-in-spring#2-the-initializingbean-interface) for loading a complex json / yaml configuration which plain SpringBoot annotation may be insufficient. 

The example defines a simple key `name` in SpringBoot fashion, in `application.properties` but in a `PostConstruct` scope, loads a (heavy) configuration.yaml fragment relevant for that value. In the current revision everything is happening in the `ExampleApplication` class, the breakdown by classes is WIP.

For a bookstrapped configuration loading sample code from [YAML with SnakeYAML](https://www.baeldung.com/java-snake-yaml) is used

### See also:
  [forum](https://qna.habr.com/q/801303)(in Russian) where the topic was discussed
  [details of AppProperties](https://www.baeldung.com/properties-with-spring)
  [Guide to @ConfigurationProperties in Spring Boot](https://www.baeldung.com/configuration-properties-in-spring-boot) 
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
