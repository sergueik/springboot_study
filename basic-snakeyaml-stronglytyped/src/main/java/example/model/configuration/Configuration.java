package example.model.configuration;

import example.model.configuration.Services;
import java.util.List;

public class Configuration {
	private String version;
	// NOTE: has to be named "Services" , not "Service"
	private Settings settings;

	public Settings getSettings() {
		return settings;
	}

	public void setSettings(Settings settings) {
		this.settings = settings;
	}

	public String getVersion() {
		return version;
	}

	public void setVersion(String version) {
		this.version = version;
	}

	private List<Services> services;

	public List<Services> getServices() {
		return services;
	}

	public void setServices(List<Services> services) {
		this.services = services;
	}
}
