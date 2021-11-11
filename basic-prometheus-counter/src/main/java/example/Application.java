package example;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

import io.prometheus.client.spring.boot.EnablePrometheusEndpoint;
import io.prometheus.client.spring.boot.EnableSpringBootMetricsCollector;
 
@SpringBootApplication
// @EnablePrometheusEndpoint
// @EnableSpringBootMetricsCollector
// missing class, need jar in pom.xml
// org.springframework.boot.actuate.endpoint.PublicMetrics
public class Application {

	public static void main(String[] args) {
		SpringApplication.run(Application.class, args);
	}
}