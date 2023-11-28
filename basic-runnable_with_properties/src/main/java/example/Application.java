package example;

import java.io.File;
import java.util.Properties;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

import org.apache.commons.configuration.PropertiesConfiguration;
import org.apache.commons.configuration.reloading.FileChangedReloadingStrategy;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.SpringApplication;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.autoconfigure.condition.ConditionalOnBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.context.event.ApplicationReadyEvent;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Primary;
import org.springframework.context.event.EventListener;
import org.springframework.core.task.SimpleAsyncTaskExecutor;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.stereotype.Component;
import java.io.File;
import java.util.Properties;

import org.apache.commons.configuration.PropertiesConfiguration;
import org.apache.commons.configuration.reloading.FileChangedReloadingStrategy;
import org.springframework.beans.factory.annotation.Value;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.autoconfigure.condition.ConditionalOnBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Primary;

import example.config.ReloadableProperties;
import example.task.EventLoggingTask;

@SpringBootApplication
@Component
@EnableScheduling
@SuppressWarnings({ "unused", "rawtypes" })
public class Application {

	@Autowired
	EventLoggingTask eventLoggingTask;

	@Bean
	@ConditionalOnProperty(name = "spring.config.location", matchIfMissing = false)
	public PropertiesConfiguration propertiesConfiguration(
			// now loaded via command line property, fall back to default location
			// see also https://www.concretepage.com/spring-5/spring-value-default
			@Value("${spring.config.location:src/main/resources/appliction.properties}") String path,
			// NOTE: not reached via maven spring-boot:run goal phase
			@Value("${spring.properties.refreshDelay}") long refreshDelay)
			throws Exception {
		// assume standard protocol notation
		String filePath = null;
		PropertiesConfiguration configuration = null;
		if (path.matches("^file://.*$")) {
			filePath = path.substring("file://".length());
			configuration = new PropertiesConfiguration(new File(filePath));
		} else {
			filePath = path;
			configuration = new PropertiesConfiguration(
					new File(filePath).getCanonicalPath());
		}
		System.err.println("application properties file path: " + filePath);
		FileChangedReloadingStrategy fileChangedReloadingStrategy = new FileChangedReloadingStrategy();
		fileChangedReloadingStrategy.setRefreshDelay(refreshDelay);
		configuration.setReloadingStrategy(fileChangedReloadingStrategy);
		return configuration;
	}

	@Bean
	@ConditionalOnBean(PropertiesConfiguration.class)
	@Primary
	public Properties properties(PropertiesConfiguration propertiesConfiguration)
			throws Exception {
		ReloadableProperties properties = new ReloadableProperties(
				propertiesConfiguration);
		return properties;
	}

	public static void main(String[] args) {
		SpringApplication.run(Application.class, args);
	}

	@EventListener(ApplicationReadyEvent.class)
	public void ready() {
		// both work
		executeAsyncTask();
		executeTask();
	}

	private void executeAsyncTask() {
		SimpleAsyncTaskExecutor executor = new SimpleAsyncTaskExecutor();
		executor.execute(eventLoggingTask);
	}

	private void executeTask() {
		ExecutorService executorService = Executors.newSingleThreadExecutor();
		Future future = executorService.submit(eventLoggingTask);
		executorService.shutdown();
	}
}
