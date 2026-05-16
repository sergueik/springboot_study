package example;

/**
 * Copyright 2026 Serguei Kouzmine
 */

public class Application {

	public static void main(String[] args) throws Exception {
		ChromiumBrowserTest test = new ChromiumBrowserTest();
		test.setUp();
		try {
			test.chromeVersion();

		} finally {
			test.tearDown();
		}
	}
}
