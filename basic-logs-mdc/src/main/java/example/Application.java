package example;

import org.springframework.boot.CommandLineRunner;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.ComponentScan;

import co.elastic.apm.api.ElasticApm;
import co.elastic.apm.api.Transaction;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@ComponentScan(basePackages = { "example" })
@SpringBootApplication
public class Application implements CommandLineRunner {

    private static final Logger log =
        LoggerFactory.getLogger(Application.class);

	public static void main(String[] args) {
		SpringApplication.run(Application.class, args);
	}

    @Override
    public void run(String... args) {
        Transaction transaction = ElasticApm.startTransaction();
        try {
            transaction.setName("hello-lab");
            transaction.setType("lab");

            transaction.activate();
            log.info("Hello structured logging");
        } finally {
            transaction.end();
            System.exit(0);
        }
    }
}
