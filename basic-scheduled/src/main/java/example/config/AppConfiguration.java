package example.config;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.PropertySource;
import org.springframework.scheduling.annotation.EnableScheduling;

@Configuration
@EnableScheduling
@ComponentScan("example")
// NOTE: the @PropertySource is required
// for "application.yaml" to be read
// see also;
// https://github.com/eugenp/tutorials/blob/master/spring-scheduling/src/main/java/com/baeldung/scheduling/ScheduledAnnotationExample.java
// NOTE: one should NOT use @PropertySource for YAML - it does not work
// @PropertySource("classpath:application.yaml")
public class AppConfiguration {
}
