package example;

import java.util.Arrays;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.ApplicationContext;

@SpringBootApplication
public class Application {
	private final static boolean debug = false;

	public static void main(String[] args) {
		ApplicationContext ctx = SpringApplication.run(Application.class, args);


		if (debug) {
			System.err.println("Let's inspect the beans provided by Spring Boot:");
			String[] beanNames = ctx.getBeanDefinitionNames();
			Arrays.sort(beanNames);
			for (String beanName : beanNames) {
				System.err.println(beanName);
			}
		}
	}

}