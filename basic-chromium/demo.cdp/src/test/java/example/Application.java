package example;

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

// <Maven/Gradle intentionally hide test sources from main compilation
// java -cp target/test-classes:target/classes:... example.Application