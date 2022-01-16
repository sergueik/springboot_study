package example;

import javax.sql.DataSource;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.CommandLineRunner;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.Bean;
import org.springframework.core.io.Resource;
import org.springframework.jdbc.datasource.init.ScriptUtils;

@SpringBootApplication
public class App {
	public static void main(String[] args) {
		SpringApplication.run(App.class, args);
	}

	// Gets the sql statements from init.sql
	@Value("classpath:tables.sql")
	private Resource initSqlScript;

	// Sql statements are executed

	@Bean
	public CommandLineRunner init(DataSource ds) {
		return args -> {
			ScriptUtils.executeSqlScript(ds.getConnection(), initSqlScript);
		};
	}

}