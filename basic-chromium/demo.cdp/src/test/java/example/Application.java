package example;

/**
 * Copyright 2026 Serguei Kouzmine
 */

public class Application {

	public static void main(String[] args) throws Exception {
		BrowserVersionTest test = new BrowserVersionTest();
		BaseTest.beforeClass();
		test.beforeTest();
		try {
			test.chromeVersionTest();
		} finally {
			test.afterTest();
			BaseTest.afterClass();
		}
	}
}