package example;

import org.mybatis.spring.annotation.MapperScan;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.ApplicationContext;
import org.springframework.context.support.ClassPathXmlApplicationContext;

@SpringBootApplication
@MapperScan(basePackages = "example.dao")
public class Application {

	public static void main(String[] args) {
		// possibly not needed - kept for Spring 4 
		// ApplicationContext appContext = new ClassPathXmlApplicationContext("applicationContext.xml");
		SpringApplication.run(Application.class, args);
	}
}
