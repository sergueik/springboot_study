package example;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

import example.utils.Utils;

@SpringBootApplication
public class Application {

	public static void main(String[] args) {
		Utils utils = Utils.getInstance();
		utils.setDebug(true);
		String version = utils.getVersion(false, false);
		System.err.println("Version: " + version);
		version = utils.getVersion(true, true);
		System.err.println("Version: " + version);
		SpringApplication.run(Application.class, args);
	}
}
