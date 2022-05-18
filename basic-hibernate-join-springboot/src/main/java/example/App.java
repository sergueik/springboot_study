package example;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.autoconfigure.domain.EntityScan;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.data.jpa.repository.config.EnableJpaRepositories;
import org.springframework.transaction.annotation.EnableTransactionManagement;

// https://stackoverflow.com/questions/45663025/spring-data-jpa-multiple-enablejparepositories

@EnableTransactionManagement
@EntityScan("example")
@ComponentScan({ "example.services", "example.controller" })
@EnableJpaRepositories("example.repository")

@SpringBootApplication

public class App {

	public static void main(String[] args) {
		SpringApplication.run(App.class, args);
	}
}
