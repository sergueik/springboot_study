package example;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.web.servlet.support.SpringBootServletInitializer;
import example.util.Globals;

import java.io.File;

@SpringBootApplication
public class Application extends SpringBootServletInitializer {
	public static void main(String[] args) {
		File file = new File(Globals.DOC_PATH);
		if (!file.exists()) {
			file.mkdirs();
		}
		SpringApplication.run(Application.class, args);
	}
}