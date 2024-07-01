package example;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.ApplicationContext;
import org.springframework.beans.factory.xml.XmlBeanFactory;
import org.springframework.context.ApplicationContext;
import org.springframework.context.support.ClassPathXmlApplicationContext;
import org.springframework.core.io.ClassPathResource;

import example.application.Movie;

@SpringBootApplication

// @SpringBootConfiguration
// @EnableAutoConfiguration
// @ComponentScan

public class Launcher {

	public static void main(String[] args) {
		ApplicationContext applicationContext = SpringApplication
				.run(Launcher.class, args);

		for (String name: applicationContext.getBeanDefinitionNames()) 
		System.err.println(name);
		// does not work
		Movie x = new Movie();

		System.out.println("Movie actor: " + x.getActor());

		ApplicationContext applicationContext2 = new ClassPathXmlApplicationContext(
				"applicationContext.xml");
		Movie y = (Movie) applicationContext2.getBean("movie");
		System.err.println("Another movie" + y);
	}
}
