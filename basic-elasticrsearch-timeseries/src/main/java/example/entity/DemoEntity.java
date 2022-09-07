package example.entity;

import java.util.Date;
import java.util.UUID;

// import javax.persistence.Column;
public class DemoEntity {

	private static String staticInfo;
	private String environment;
	private String dc;
	private Date createTime;
	private String hostname;
	private String appId;
	private float cpu;
	private int rpm;
	private float memory;

	// uncertain if one would ever need this
	public static String getStaticInfo() {
		return DemoEntity.staticInfo;
	}

	public DemoEntity() {
		staticInfo = UUID.randomUUID().toString();
	}

	public int getRpm() {
		return rpm;
	}

	public void setRpm(int value) {
		rpm = value;
	}

	public float getCpu() {
		return cpu;
	}

	public void setCpu(float value) {
		cpu = value;
	}

	public float getMemory() {
		return memory;
	}

	public void setMemory(float value) {
		memory = value;
	}

	// @Id
	// @GeneratedValue(strategy = GenerationType.AUTO)
	// @Column(name = "dc")
	public String getDc() {
		return dc;
	}

	public void setDc(String value) {
		dc = value;
	}

	// @Id
	// @GeneratedValue(strategy = GenerationType.AUTO)
	// @Column(name = "environment")
	public String getEnvironment() {
		return environment;
	}

	public void setEnvironment(String value) {
		environment = value;
	}

	// @Id
	// @GeneratedValue(strategy = GenerationType.AUTO)
	// @Column(name = "appid")
	public String getAppId() {
		return appId;
	}

	public void setAppId(String value) {
		appId = value;
	}

	// @Column(name = "hostname", nullable = false, length = 10)
	public String getHostname() {
		return hostname;
	}

	public void setHostname(String value) {
		hostname = value;
	}

	public Date getCreateTime() {
		return createTime;
	}

	public void setCreateTime(Date value) {
		createTime = value;
	}

	// TODO: add ip. can use Java InetAddress class
	// String pattern="([0-9]*\.){3}[0-9]*";
	// try {
	// InetAddress address = InetAddress.getByName(ip);
	// if (address instanceof Inet6Address) {
	//
	// } else if (address instanceof Inet4Address) {
	// }
	// } catch(UnknownHostException e) {
	// }
	// see also:
	// https://github.com/HKShuttle/IPAddressCalculator/blob/master/src/IPv4.java
}
