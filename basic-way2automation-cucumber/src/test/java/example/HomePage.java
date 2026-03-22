package example;

import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.support.CacheLookup;
import org.openqa.selenium.support.FindBy;
import org.openqa.selenium.support.PageFactory;

public class HomePage extends Utility {

	private static final Logger log = LogManager.getLogger(HomePage.class.getName());

	public HomePage() {
		PageFactory.initElements(driver, this);
	}

	@CacheLookup
	@FindBy(xpath = "//button[normalize-space()='Bank Manager Login']")
	WebElement bankManager;

	@CacheLookup
	@FindBy(xpath = "//button[normalize-space()='Customer Login']")
	WebElement customerLogin;

	public void clickOnBankMangerLogin() {
		log.info("Click on bank manager login");
		clickOnElement(bankManager);
	}

	public void clickOnCustomerLogin() throws InterruptedException {
		Thread.sleep(1000);
		log.info("Click on customer login");
		clickOnElement(customerLogin);
	}
}
