package example;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.openqa.selenium.support.PageFactory;

public class CustomersPage extends Utility {
	private static final Logger log = LoggerFactory.getLogger(Utility.class);

	public CustomersPage() {
		PageFactory.initElements(driver, this);
	}

}
