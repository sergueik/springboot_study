package example.config;

import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.PropertySource;

@Configuration
@ComponentScan("example")
// NOTE: the @PropertySource is required
// for "application.properties" to be read
// see also;
// https://github.com/eugenp/tutorials/blob/master/spring-scheduling/src/main/java/com/baeldung/scheduling/ScheduledAnnotationExample.java

@PropertySource("classpath:application.properties")
public class AppConfiguration {
}
