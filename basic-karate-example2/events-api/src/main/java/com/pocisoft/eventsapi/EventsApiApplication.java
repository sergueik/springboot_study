package com.pocisoft.eventsapi;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.web.servlet.config.annotation.EnableWebMvc;

@EnableWebMvc
@SpringBootApplication
public class EventsApiApplication {

	public static void main(String[] args) {

		DBInitializer.initializeH2DBServer();
		SpringApplication.run(EventsApiApplication.class, args);
	}
}
