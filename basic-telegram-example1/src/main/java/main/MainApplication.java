package main;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.telegram.telegrambots.ApiContextInitializer;
import org.telegram.telegrambots.meta.TelegramBotsApi;

@SpringBootApplication
@EnableScheduling
public class MainApplication {

	public static void main(String[] args) {
		// https://www.tabnine.com/code/java/methods/org.telegram.telegrambots.ApiContextInitializer/init
		// https://stackoverflow.com/questions/60701997/unable-to-add-a-telegram-bot-library-beginners-fail
		ApiContextInitializer.init();

		// TelegramBotsApi botsApi = new TelegramBotsApi();
		SpringApplication.run(MainApplication.class, args);

	}

}
