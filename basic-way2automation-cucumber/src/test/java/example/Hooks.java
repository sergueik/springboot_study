package example;

import com.cucumber.listener.Reporter;
import cucumber.api.Scenario;
import cucumber.api.java.After;
import cucumber.api.java.Before;

import java.io.IOException;

public class Hooks extends Utility {

	@Before
	public void setUp() {
		selectBrowser(PropertyReader.getInstance().getProperty("browser"));
//		Reporter.assignAuthor("Kashyap");
	}

	@After
	public void tearDown(Scenario scenario) {
		if (scenario.isFailed()) {
			String screenshotPath = Utility.getScreenshot(driver, scenario.getName().replace(" ", "_"));
			/*
			try {
				Reporter.addScreenCaptureFromPath(screenshotPath);
			} catch (IOException e) {
				e.printStackTrace();
			}
			*/
		}
		closeBrowser();
	}
}
