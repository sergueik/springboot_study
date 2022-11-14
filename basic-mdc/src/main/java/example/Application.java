package example;

import java.util.Arrays;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.ApplicationContext;

@SpringBootApplication
public class Application {
	private final static boolean debug = false;

	public static void main(String[] args) {
		// keep the context in the variable
		ApplicationContext ctx = SpringApplication.run(Application.class, args);

		if (debug) {
			System.err.println("Beans:");
			String[] beanNames = ctx.getBeanDefinitionNames();
			Arrays.sort(ctx.getBeanDefinitionNames());
			for (String beanName : beanNames) {
				System.err.println(beanName);
			}
		}
	}

}