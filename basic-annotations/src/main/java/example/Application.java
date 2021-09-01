package example;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

@SpringBootApplication
public class Application {

	private static final Logger logger = LogManager.getLogger(Application.class);

	@Value("static value")
	private static String staticValue;

	@Value("${message}")
	private static String message;

	public static void main(String[] args) {
		logger.info("staticValue=" + staticValue + " message=" + message); 
		// null,null
		SpringApplication.run(Application.class, args);
	}
}
