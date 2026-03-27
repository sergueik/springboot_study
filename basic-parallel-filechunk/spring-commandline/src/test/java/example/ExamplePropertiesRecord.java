package example;

import java.util.List;
import java.util.Map;

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

/*

* Configuration holder for application.properties entries under prefix "example".
*
* Using a Java record removes boilerplate:
* * no fields
* * no getters
* * no setters
* * immutable by design
*
* Spring Boot binds configuration using the record's canonical constructor.
  */
  @Component
  @ConfigurationProperties(prefix = "example")
  public record ExamplePropertiesRecord (

  // Spring binds comma-separated properties into List automatically
  List<String> goodCopybooks,

  List<String> badCopybooks,

  // Spring supports Map binding natively:
  // example.metricExtractors.load_average=regex
  Map<String, String> metricExtractors,

  // base paths used by tests
  String baseBadPath,
  String baseGoodPath

) {}
