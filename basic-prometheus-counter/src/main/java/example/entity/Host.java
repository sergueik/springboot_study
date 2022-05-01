package example.entity;

import java.util.UUID;

import java.io.Serializable;
import java.sql.Timestamp;

@SuppressWarnings("serial")
public class Host {

	private String hostname;
	private String app;
	private String domain;
	private String environment;
	private static String staticInfo;
	private int id;

	public String getHostname() {
		return hostname;
	}

	public void setHostname(String value) {
		hostname = value;
	}

	public String getApp() {
		return app;
	}

	public void setApp(String value) {
		app = value;
	}

	public String getDomain() {
		return domain;
	}

	public void setDomain(String value) {
		domain = value;
	}

	public String getEnvironment() {
		return environment;
	}

	public void setEnvironment(String value) {
		environment = value;
	}

	public int getId() {
		return id;
	}

	public void setId(int value) {
		id = value;
	}

	public Host() {
		staticInfo = UUID.randomUUID().toString();
	}

	public /* static */ String getStaticInfo() {
		return Host.staticInfo;
	}

	public Host(int id, String hostname, String domain, String environment,
			String app) {
		super();
		if (Host.staticInfo == null) {
			Host.staticInfo = UUID.randomUUID().toString();
		}
		this.hostname = hostname;
		this.id = id;
		this.domain = domain;
		this.environment = environment;
		this.app = app;
	}

	@Override
	public String toString() {
		return "Host{" + "id=" + this.id + ", hostname=" + this.hostname
				+ ", domain=" + this.domain + ", environment=" + this.environment
				+ ", app=" + this.app
				/* + ", addtime=" + addtime */ + '}';
	}

}
