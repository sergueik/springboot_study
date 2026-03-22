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

	// click On "Bank Manager Login" Tab
	@CacheLookup
	@FindBy(xpath = "//button[normalize-space()='Bank Manager Login']")
	WebElement bankManager;

	// click on "Customer Login" Tab
	@CacheLookup
	@FindBy(xpath = "//button[normalize-space()='Customer Login']")
	WebElement customerLogin;

	// click On "Bank Manager Login" Tab
	public void clickOnBankMangerLogin() {
		log.info("Click on bank manager login");
		clickOnElement(bankManager);
	}

	// click on "Customer Login" Tab
	public void clickOnCustomerLogin() throws InterruptedException {
		Thread.sleep(1000);
		log.info("Click on customer login");
		clickOnElement(customerLogin);
	}
}
