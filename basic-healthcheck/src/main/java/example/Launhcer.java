package example;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.builder.SpringApplicationBuilder;

import org.springframework.beans.factory.annotation.Autowired;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

// only needed for war packaging
// import org.springframework.boot.web.support.SpringBootServletInitializer;

@SpringBootApplication
public class Launhcer {

	// @Autowired
	// LogHelper loghelper;
	/*
	 * @Override protected SpringApplicationBuilder
	 * configure(SpringApplicationBuilder application) { return
	 * application.sources(SpringBootHeartbeatApplication.class); }
	 */
	public static void main(String[] args) throws Exception {
		SpringApplication.run(Launhcer.class, args);
	}
}
