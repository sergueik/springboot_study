package example;

import org.junit.Before;
import org.junit.Test;

public class EnvironmentPrinterTest {
	private static EnvironmentPrinter environmentPrinter = null;

	@Before
	public void setUp() {
		environmentPrinter = new EnvironmentPrinter();
	}

	// export TestEnvironment=value;mvn clean test 2>a.log
	@Test
	public void printEnvironmentTest() {
		environmentPrinter.printEnvironment();
	}

	// mvn -DTest.Property=property_value clean test 2>a.log

	@Test
	public void printPripertiesTest() {
		environmentPrinter.printProperties();
	}
}
