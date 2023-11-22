package example;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.stereotype.Component;

import example.utils.Utils;

@SpringBootApplication
public class Application {

	public static void main(String[] args) {
		Utils utils = Utils.getInstance();
		utils.setDebug(true);
		String version = utils.getVersion(false);
		System.err.println("Version: " + version);
		version = utils.getVersion(true);
		System.err.println("Version: " + version);
		SpringApplication.run(Application.class, args);
	}
}

