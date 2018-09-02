package org.github.example;

import java.io.File;
import java.io.IOException;

import org.openqa.selenium.server.SeleniumServer;
import org.openqa.selenium.server.htmlrunner.HTMLLauncher;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.CommandLineRunner;
import org.springframework.boot.SpringApplication;
import org.springframework.context.ApplicationContext;
import org.springframework.stereotype.Service;

@Service
public class SeleniumRunner implements CommandLineRunner {

	@Autowired
	private ApplicationContext context;

	@Autowired
	TestConfig config;

	private Integer timeOutInSeconds = 60;
	private Boolean multiWindow = true;

	@Override
	public void run(String... arg0) {
		SeleniumServer server;
		try {
			server = new SeleniumServer();
			server.start();

			String result;

			for (String suite : config.getSuites()) {
				result = runTestSuite(server, suite);
			}

			server.stop();

		} catch (IOException e_io) {
			// TODO Auto-generated catch block
			e_io.printStackTrace();
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		SpringApplication.exit(context);
	}

	private String runTestSuite(SeleniumServer server, String suite)
			throws IOException {
		HTMLLauncher launcher = new HTMLLauncher(server);
		File results = new File("results_" + suite + ".html");
		String suite_url = "/selenium-server/selenium/" + suite + ".html";
		return launcher.runHTMLSuite(config.getBrowser(), config.getStartUrl(),
				suite_url, results, timeOutInSeconds, multiWindow);
	}

}
