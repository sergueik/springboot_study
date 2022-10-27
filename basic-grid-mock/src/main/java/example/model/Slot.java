package example.model;

import example.model.Id;
import example.model.Stereotype;

//generated with help of https://www.site24x7.com/tools/json-to-java.html
//NOTE: poor code generation

public class Slot {

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
