package example.model.grid3;
/**
 * Copyright 2022 Serguei Kouzmine
 */

import example.model.grid3.Build;
import example.model.grid3.Java;
import example.model.grid3.Os;

// generated with help of https://www.site24x7.com/tools/json-to-java.html
// NOTE: poor code generation
public class Value {
	private boolean ready;
	private String message;
	Build build;
	Os os;
	Java java;

	// Getter Methods

	public boolean getReady() {
		return ready;
	}

	public String getMessage() {
		return message;
	}

	public Build getBuild() {
		return build;
	}

	public Os getOs() {
		return os;
	}

	public Java getJava() {
		return java;
	}

	public void setReady(boolean data) {
		ready = data;
	}

	public void setMessage(String data) {
		message = data;
	}

	public void setBuild(Build data) {
		build = data;
	}

	public void setOs(Os data) {
		os = data;
	}

	public void setJava(Java data) {
		java = data;
	}
}