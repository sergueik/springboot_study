package org.github.example;

import java.util.List;

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Configuration;

@ConfigurationProperties(prefix = "test")
@Configuration
public class TestConfig {

	private String browser;
	private String startUrl;
	private List<String> suites;

	public List<String> getSuites() {
		return suites;
	}

	public String getBrowser() {
		return browser;
	}

	public String getStartUrl() {
		return startUrl;
	}

	public void setSuites(List<String> suites) {
		this.suites = suites;
	}

	public void setBrowser(String browser) {
		this.browser = browser;
	}

	public void setStartUrl(String startUrl) {
		this.startUrl = startUrl;
	}
}
