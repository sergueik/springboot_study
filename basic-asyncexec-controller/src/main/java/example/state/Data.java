package example.state;

import example.runner.CustomApplicationRunner;

/**
 * Copyright 2022 Serguei Kouzmine
 */

public class Data {

	private static Data instance = new Data();

	private CustomApplicationRunner runner;

	public CustomApplicationRunner getApplicationRunner() {
		return runner;
	}

	public void setApplicationRunner(CustomApplicationRunner data) {
		runner = data;
	}

	private Data() {
	}

	public static Data getInstance() {
		return instance;
	}

}
