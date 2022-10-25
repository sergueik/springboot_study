package example.controller;
/**
 * Copyright 2022 Serguei Kouzmine
 */

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;

import example.service.ExampleService;

@RestController
@RequestMapping("/basic")
public class GridStatusController {

	private boolean debug = false;
	// see also about writing SpringBoot application tests without relying on
	// SpringBoot field injection
	// https://reflectoring.io/unit-testing-spring-boot/
	@Autowired
	private ExampleService service;

	@Autowired
	public GridStatusController(ExampleService data) {
		service = data;
	}

	private static final Gson gson = new GsonBuilder().setPrettyPrinting()
			.create();

	public GridStatusController() {

	}

	@GetMapping(value = "/status", produces = MediaType.APPLICATION_JSON_VALUE)
	public Data json() {
		final String result = "{ \"value\": { \"ready\": true, \"message\": \"Selenium Grid ready.\", \"nodes\": [ { \"id\": \"340232e5-36dd-4014-9a86-7770e45579a6\", \"uri\": \"http:\u002f\u002f10.0.2.15:5555\", \"maxSessions\": 1, \"osInfo\": { \"arch\": \"amd64\", \"name\": \"Windows 10\", \"version\": \"10.0\" }, \"heartbeatPeriod\": 60000, \"availability\": \"UP\", \"version\": \"4.0.0 (revision 3a21814679)\", \"slots\": [ { \"lastStarted\": \"1970-01-01T00:00:00Z\", \"session\": null, \"id\": { \"hostId\": \"340232e5-36dd-4014-9a86-7770e45579a6\", \"id\": \"49d6090b-798d-4b0b-9ce7-8a7a7400e962\" }, \"stereotype\": { \"browserName\": \"firefox\", \"platformName\": \"WIN10\" } }, { \"lastStarted\": \"1970-01-01T00:00:00Z\", \"session\": null, \"id\": { \"hostId\": \"340232e5-36dd-4014-9a86-7770e45579a6\", \"id\": \"e6928dba-2a7b-4f4c-9c39-51e2ed542db6\" }, \"stereotype\": { \"browserName\": \"chrome\", \"platformName\": \"WIN10\" } } ] } ] } }";
		Data data = gson.fromJson(result, Data.class);
		Node node1 = new Node();
		node1.setAvailability("UP");
		node1.setId("node1 added manually");
		Node node2 = new Node();
		node2.setAvailability("DOWN");
		node2.setId("node2 added manually");
		// for GUID constructor
		// see also:
		// http://www.java2s.com/Code/Java/Development-Class/RandomGUID.htm
		Value value = data.getValue();
		List<Node> nodes = value.getNodes();
		nodes.add(node1);
		nodes.add(node2);
		value.setNodes(nodes);
		data.setValue(value);
		return data;
	}

	public static class Data {

		Value value;

		public Value getValue() {
			return value;
		}

		public void setValue(Value data) {
			value = data;
		}

		public Data() {
		}
	}

	// generated with help of https://www.site24x7.com/tools/json-to-java.html
	// NOTE: poor code generation
	public static class Value {
		private boolean ready;
		private String message;
		List<Node> nodes = new ArrayList<>();

		public boolean getReady() {
			return ready;
		}

		public String getMessage() {
			return message;
		}

		public void setReady(boolean data) {
			ready = data;
		}

		public void setMessage(String data) {
			message = data;
		}

		public List<Node> getNodes() {
			return nodes;
		}

		public void setNodes(List<Node> data) {
			nodes = data;
		}

	}

	public static class Node {
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

	public class OsInfo {
		private String arch;
		private String name;
		private String version;

		public String getArch() {
			return arch;
		}

		public String getName() {
			return name;
		}

		public String getVersion() {
			return version;
		}

		public void setArch(String data) {
			arch = data;
		}

		public void setName(String data) {
			name = data;
		}

		public void setVersion(String data) {
			version = data;
		}
	}

	public static class Slot {
		private String lastStarted;
		private String session = null;
		Id id;
		Stereotype stereotype;

		public String getLastStarted() {
			return lastStarted;
		}

		public String getSession() {
			return session;
		}

		public Id getId() {
			return id;
		}

		public Stereotype getStereotype() {
			return stereotype;
		}

		public void setLastStarted(String data) {
			lastStarted = data;
		}

		public void setSession(String data) {
			session = data;
		}

		public void setId(Id data) {
			id = data;
		}

		public void setStereotype(Stereotype data) {
			stereotype = data;
		}
	}

	public class Stereotype {
		private String browserName;
		private String platformName;

		public String getBrowserName() {
			return browserName;
		}

		public String getPlatformName() {
			return platformName;
		}

		public void setBrowserName(String data) {
			browserName = data;
		}

		public void setPlatformName(String data) {
			platformName = data;
		}
	}

	public class Id {
		private String hostId;
		private String id;

		public String getHostId() {
			return hostId;
		}

		public String getId() {
			return id;
		}

		public void setHostId(String data) {
			hostId = data;
		}

		public void setId(String data) {
			id = data;
		}
	}
}
