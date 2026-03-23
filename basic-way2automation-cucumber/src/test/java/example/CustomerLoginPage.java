package example;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import org.openqa.selenium.WebElement;
import org.openqa.selenium.support.CacheLookup;
import org.openqa.selenium.support.FindBy;
import org.openqa.selenium.support.PageFactory;

public class CustomerLoginPage extends Utility {
	private static final Logger log = LoggerFactory.getLogger(CustomerLoginPage.class);

	public CustomerLoginPage() {
		PageFactory.initElements(driver, this);
	}

	// search customer that you created.
	@CacheLookup
	@FindBy(xpath = "//select[@id='userSelect']")
	WebElement customer;
	// click on "Login" Button
	@CacheLookup
	@FindBy(xpath = "//button[normalize-space()='Login']")
	WebElement login;
	// verify "Logout" Tab displayed.
	@CacheLookup
	@FindBy(xpath = "//button[normalize-space()='Logout']")
	WebElement logout;
	// click on "Logout"
	@CacheLookup
	@FindBy(xpath = "//button[normalize-space()='Logout']")
	WebElement clickLogout;
	// verify "Your Name" text displayed.
	@CacheLookup
	@FindBy(xpath = "//label[contains(text(),'Your Name :')]")
	WebElement verifyName;

	// search customer that you created.
	public void searchCustomer() throws InterruptedException {
		Thread.sleep(2000);
		log.info("Search customer :");
		selectByVisibleTextFromDropDown(customer, "Harry Potter");
	}

	// click on "Login" Button
	public void clickOnLoginButton() throws InterruptedException {
		Thread.sleep(1000);
		log.info("Click on login button :");
		clickOnElement(login);
	}

	// verify "Logout" Tab displayed.
	public boolean isLogoutButtonPresence() throws InterruptedException {
		Thread.sleep(1000);
		log.info("Check logout button present :");
		return logout.isDisplayed();
	}

	// click on "Logout"
	public void clickOnLogoutTab() throws InterruptedException {
		Thread.sleep(1000);
		log.info("Click on logout tab");
		clickOnElement(clickLogout);
	}

	// verify "Your Name" text displayed.
	public String verifyYourNameTextIsDisplayed() throws InterruptedException {
		Thread.sleep(1000);
		log.info("verifying your name text");
		return getTextFromElement(verifyName);
	}
}
