package example.configuration;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.PropertySource;

import javax.annotation.PostConstruct;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

@Configuration
public class ValuesConfig {

	@Value("${value}")
	private String valueFromFile;

	public String getValueFromFile() {
		return valueFromFile;
	}

	public ValuesConfig() {
		System.out.println("xxx: " + valueFromFile);
	}
}
