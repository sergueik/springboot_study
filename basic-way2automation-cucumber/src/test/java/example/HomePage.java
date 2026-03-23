package example;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import org.openqa.selenium.WebElement;
import org.openqa.selenium.support.CacheLookup;
import org.openqa.selenium.support.FindBy;
import org.openqa.selenium.support.PageFactory;

public class HomePage extends Utility {

	private static final Logger log = LoggerFactory.getLogger(HomePage.class);

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
