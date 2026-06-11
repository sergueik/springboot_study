package example.config;

import java.util.ArrayList;
import java.util.List;

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

import example.model.Menu;

@Component
// no prefix, loads root level settings
@ConfigurationProperties
public class GlobalProperties {

	private List<Menu> menus = new ArrayList<>();
	// thread-pool , relax binding
	private int threadPool;
	private String email;

	public int getThreadPool() {
		return threadPool;
	}

	public void setThreadPool(int threadPool) {
		this.threadPool = threadPool;
	}

	public String getEmail() {
		return email;
	}

	public void setEmail(String email) {
		this.email = email;
	}

	public List<Menu> getMenus() {
		return menus;
	}

	public void setMenus(List<Menu> menus) {
		this.menus = menus;
	}

	@Override
	public String toString() {
		return this.getClass().getSimpleName() + "{" + "menus=" + menus
				+ "threadPool=" + threadPool + ", email='" + email + '\'' + '}';
	}
}
