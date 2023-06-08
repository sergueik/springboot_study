package com.baeldung;

import org.gradle.api.DefaultTask;
import org.gradle.api.tasks.TaskAction;
import org.gradle.api.tasks.Input;
import javax.inject.Inject;

class PrintToolVersionBuildSrcTask extends DefaultTask {
	@Input
	private String tool = "perl";

	public void setTool(String value) {
		tool = value;
	}

	@Inject
	public PrintToolVersionBuildSrcTask() {
		super();
	}

	@TaskAction
	public void printToolVersion() {
		System.out.println("Processing tool: " + tool);
		switch (tool) {
		case "java":
			System.out.println(System.getProperty("java.version"));
			break;
		case "groovy":
			System.out.println("TODO: GroovySystem.getVersion()");
			break;
		default:
			throw new IllegalArgumentException("Unknown tool: " + tool);
		}
	}
}
