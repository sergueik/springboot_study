package example.utils;

import java.util.UUID;

public class Host {

	private String hostname;
	private String app;
	private String dc;
	private static String staticInfo;
	private int id;

	public String getHostname() {
		return hostname;
	}

	public void setHostname(String data) {
		hostname = data;
	}

	public String getApp() {
		return app;
	}

	public void setApp(String data) {
		app = data;
	}

	public String getDc() {
		return dc;
	}

	public void setDc(String data) {
		dc = data;
	}

	public int getId() {
		return id;
	}

	public void setId(int data) {
		id = data;
	}

	public Host() {
		staticInfo = UUID.randomUUID().toString();
	}

	public /* static */ String getStaticInfo() {
		return Host.staticInfo;
	}

	public Host(int id, String hostname, String dc, String app) {
		super();
		if (Host.staticInfo == null) {
			Host.staticInfo = UUID.randomUUID().toString();
		}
		this.hostname = hostname;
		this.id = id;
		this.dc = dc;
		this.app = app;
	}

}
