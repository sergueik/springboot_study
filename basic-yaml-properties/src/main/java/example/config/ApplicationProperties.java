package example.config;

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

import example.model.Menu;
import example.model.Server;
import example.model.Theme;

import java.util.ArrayList;
import java.util.List;

@Component
@ConfigurationProperties("application")
public class ApplicationProperties {

	private Theme themes;
	private List<Server> servers = new ArrayList<>();

	public Theme getThemes() {
		return themes;
	}

	public void setThemes(Theme themes) {
		this.themes = themes;
	}

	public List<Server> getServers() {
		return servers;
	}

	public void setServers(List<Server> servers) {
		this.servers = servers;
	}

	@Override
	public String toString() {
		return this.getClass().getSimpleName() + "{" + "themes=" + themes
				+ ", servers=" + servers + '}';
	}
}
