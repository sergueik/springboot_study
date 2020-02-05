package example;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.ImportResource;

import example.service.ArticleService;

@SpringBootApplication
@ImportResource("classpath:app-config.xml")
public class Launcher {
	public static void main(String[] args) {
		ApplicationContext applicationContext = SpringApplication.run(Launcher.class, args);
		ArticleService articleService = applicationContext.getBean(ArticleService.class);
		System.err.println(articleService.processMsg());
	}
}