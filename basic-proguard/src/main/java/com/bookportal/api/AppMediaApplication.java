package com.bookportal.api;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.WebApplicationType;
import org.springframework.boot.autoconfigure.SpringBootApplication;

@SpringBootApplication
public class AppMediaApplication {

    public static void main(String[] args) {
        SpringApplication app = new SpringApplication(AppMediaApplication.class);
        app.setWebApplicationType(WebApplicationType.REACTIVE);
        app.run(args);
    }

}
