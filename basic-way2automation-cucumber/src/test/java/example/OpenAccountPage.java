package example;


import org.openqa.selenium.support.PageFactory;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class OpenAccountPage extends Utility {
	private static final Logger log = LoggerFactory.getLogger(BankSteps.class);

	public OpenAccountPage() {
		PageFactory.initElements(driver, this);
	}
}
