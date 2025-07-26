package example;

import org.springframework.boot.builder.SpringApplicationBuilder;

import org.springframework.boot.logging.LogLevel;
import ch.qos.logback.classic.Level;
import org.springframework.boot.logging.LogLevel;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.CommandLineRunner;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.WebApplicationType;
import org.springframework.boot.autoconfigure.SpringBootApplication;
// https://docs.spring.io/spring-boot/api/java/org/springframework/boot/autoconfigure/logging/ConditionEvaluationReportLoggingListener.html
// forLogLevel
import org.springframework.boot.autoconfigure.logging.ConditionEvaluationReportLoggingListener;

import org.springframework.core.metrics.jfr.FlightRecorderApplicationStartup;
import org.springframework.boot.autoconfigure.SpringBootApplication;
// since 3.0.0
@SpringBootApplication
public class AppInitializer {
// static
	public static void main(String[] args) {
		var context = new SpringApplicationBuilder(AppInitializer.class).child(WebConfig.class)
				.initializers(/* ConditionEvaluationReportLoggingListener.forLogLevel(LogLevel.DEBUG) */ )
				.web(WebApplicationType.SERVLET).applicationStartup(new FlightRecorderApplicationStartup()).run(args);
	}
}
