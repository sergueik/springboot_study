package example;

import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
// Spring 1.5.x
import org.springframework.boot.autoconfigure.jdbc.DataSourceBuilder;
// Spring 2.x
//import org.springframework.boot.jdbc.DataSourceBuilder;
// see also:
// https://stackoverflow.com/questions/50011577/spring-boot-2-0-0-datasourcebuilder-not-found-in-autoconfigure-jar

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Bean;

import javax.sql.DataSource;

// NOTE: SpringBootApplication cannot be in default package

@SpringBootApplication
public class Application {
	public static void main(String[] args) {
		SpringApplication.run(Application.class, args);
	}

	@ConfigurationProperties(prefix = "spring.datasource")
	@Bean
	@Qualifier("data_mysql")
	public DataSource getDataSource() {
		return DataSourceBuilder.create().build();
	}
}
