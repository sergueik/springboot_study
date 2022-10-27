package example.model;

import java.util.ArrayList;
import java.util.List;

import example.model.OsInfo;
import example.model.Slot;

//generated with help of https://www.site24x7.com/tools/json-to-java.html
//NOTE: poor code generation

public class Node {
	private String id;
	private String uri;
	private float maxSessions;
	OsInfo osInfo;
	private float heartbeatPeriod;
	private String availability;
	private String version;
	List<Slot> slots = new ArrayList<>();

	public OsInfo getOsInfo() {
		return osInfo;
	}

	public void setOsInfo(OsInfo data) {
		osInfo = data;
	}

	public List<Slot> getSlots() {
		return slots;
	}

	public void setReady(List<Slot> data) {
		slots = data;
	}

	public String getId() {
		return id;
	}

	public String getUri() {
		return uri;
	}

	public float getMaxSessions() {
		return maxSessions;
	}

	public float getHeartbeatPeriod() {
		return heartbeatPeriod;
	}

	public String getAvailability() {
		return availability;
	}

	public String getVersion() {
		return version;
	}

	public void setId(String data) {
		id = data;
	}

	public void setUri(String data) {
		uri = data;
	}

	public void setMaxSessions(float data) {
		maxSessions = data;
	}

	public void setHeartbeatPeriod(float data) {
		heartbeatPeriod = data;
	}

	public void setAvailability(String data) {
		availability = data;
	}

	public void setVersion(String data) {
		version = data;
	}

}
