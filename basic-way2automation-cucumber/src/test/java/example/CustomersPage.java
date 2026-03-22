package example;

import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;
import org.openqa.selenium.support.PageFactory;

public class CustomersPage extends Utility {
	private static final Logger log = LogManager.getLogger(CustomersPage.class.getName());

	public CustomersPage() {
		PageFactory.initElements(driver, this);
	}

}
