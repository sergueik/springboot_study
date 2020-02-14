package example;

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

import example.configs.ReloadableProperties;
import example.resource.Worker;

@SpringBootApplication
public class Launcher {
	@Bean
	@ConditionalOnProperty(name = "spring.config.location", matchIfMissing = false)
	public PropertiesConfiguration propertiesConfiguration(
			// now loaded via command line property, fall back to default location
			// see also https://www.concretepage.com/spring-5/spring-value-default
			@Value("${spring.config.location:appliction.properties}") String path,
			// NOTE: not reached via maven spring-boot:run goal phase
			@Value("${spring.properties.refreshDelay}") long refreshDelay) throws Exception {
		// assume standard protocol notation
		String filePath = null;
		PropertiesConfiguration configuration = null;
		if (path.matches("^file://.*$")) {
			filePath = path.substring("file://".length());
			configuration = new PropertiesConfiguration(new File(filePath));
		} else {
			filePath = path;
			configuration = new PropertiesConfiguration(new File(filePath).getCanonicalPath());
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
	public Properties properties(PropertiesConfiguration propertiesConfiguration) throws Exception {
		ReloadableProperties properties = new ReloadableProperties(propertiesConfiguration);
		return properties;
	}

	public static void main(String[] args) {
		SpringApplication.run(Launcher.class, args);
	}
}
