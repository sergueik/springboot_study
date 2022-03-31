package example.config;

import io.micrometer.core.instrument.binder.jvm.JvmGcMetrics;
import io.micrometer.core.instrument.binder.jvm.JvmMemoryMetrics;
import io.micrometer.core.instrument.binder.jvm.JvmThreadMetrics;
import io.micrometer.core.instrument.binder.system.ProcessorMetrics;
import io.micrometer.core.instrument.binder.system.UptimeMetrics;
import io.micrometer.prometheus.PrometheusConfig;
import io.micrometer.prometheus.PrometheusMeterRegistry;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.servlet.config.annotation.EnableWebMvc;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;

import example.controller.AppController;

/**
 * Configure Spring Rest applications with a Prometheus registry.
 */

@Configuration
// @ComponentScan("example")
public class ApplicationConfiguration {

	private static final Logger logger = LoggerFactory
			.getLogger(ApplicationConfiguration.class);

	// NOTE: uncommenting the following method leads to
	// disappearance of core metrics:
	// curl -sIv http://localhost:8080/actuator/prometheus
	// 	HTTP/1.1 200
	// 	Content-Type: text/plain; version=0.0.4;charset=utf-8
	// 	Content-Length: 0
	// NOTE: returning null from the method leads to
	// error in runtime
	// Unsatisfied dependency expressed through method 'webMvcMetricsFilter'
	// parameter 0;
	// nested exception is
	// org.springframework.beans.factory.NoSuchBeanDefinitionException:
	// No qualifying bean of type 'io.micrometer.core.instrument.MeterRegistry'
	// available:
	// expected at least 1 bean which qualifies as autowire candidate. Dependency
	// annotations: {}
	/*
	@Bean
	public PrometheusMeterRegistry prometheusMeterRegistry() {
		logger.info("configured PrometheusMeterRegistry");
		
		final PrometheusMeterRegistry registry = new PrometheusMeterRegistry(
				PrometheusConfig.DEFAULT);
		new JvmThreadMetrics().bindTo(registry);
		new JvmGcMetrics().bindTo(registry);
		new JvmMemoryMetrics().bindTo(registry);
		// new DiskSpaceMetrics(new File("/")).bindTo(registry);
		new ProcessorMetrics().bindTo(registry);
		new UptimeMetrics().bindTo(registry);
		return registry;
		
		// return null;
	}
	*/
}
