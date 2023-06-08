package example;

/**
 * Copyright 2023 Serguei Kouzmine
 */

import org.gradle.api.DefaultTask;

import org.gradle.api.tasks.TaskAction;
import org.gradle.api.tasks.Input;

import java.io.IOException;
import java.io.InputStream;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import javax.inject.Inject;
import example.PropertyUpdaterBootstrap;

class PrintToolVersionBuildSrcTask extends DefaultTask {
	@Input
	private String tool = "perl";

	public void setTool(String value) {
		tool = value;
	}

	@Input
	private String fileName = "application.yaml";

	public void setFileName(String value) {
		fileName = value;
	}

	@Inject
	public PrintToolVersionBuildSrcTask() {
		super();
	}

	@TaskAction
	public void printToolVersion() throws IOException {
		PropertyUpdaterBootstrap propertyUpdaterBootstrap = new PropertyUpdaterBootstrap(
				this.tool, this.fileName);
		propertyUpdaterBootstrap.process();
		System.out.println("Done.");
	}
}
