package example;

import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.support.CacheLookup;
import org.openqa.selenium.support.FindBy;
import org.openqa.selenium.support.PageFactory;

public class AddCustomerPage extends Utility {
	private static final Logger log = LogManager.getLogger(AddCustomerPage.class.getName());

	public AddCustomerPage() {
		PageFactory.initElements(driver, this);
	}

	// click On "Add Customer" Tab
	@CacheLookup
	@FindBy(xpath = "//button[normalize-space()='Add Customer']")
	WebElement addCustomer;
	// Enter FirstName
	@CacheLookup
	@FindBy(xpath = "//input[@placeholder='First Name']")
	WebElement firstName;
	// Enter Last Name
	@CacheLookup
	@FindBy(xpath = "//input[@placeholder='Last Name']")
	WebElement lastName;
	// Enter PostCode
	@CacheLookup
	@FindBy(xpath = "//input[@placeholder='Post Code']")
	WebElement postCode;
	// Click on Add Button
	@CacheLookup
	@FindBy(xpath = "//button[@type='submit']")
	WebElement addButton;

	// * click On "Add Customer" Tab
	public void clickOnAddCustomer() {
		log.info("Click on add customer");
		clickOnElement(addCustomer);
	}

	// Enter First Name
	public void enterFirstname(String value) throws InterruptedException {
		Thread.sleep(1000);
		log.info("Enter first name ");
		sendTextToElement(firstName, value);
	}

	// Enter Last Name
	public void enterLastname(String value) throws InterruptedException {
		Thread.sleep(1000);
		log.info("Enter last name ");
		sendTextToElement(lastName, value);
	}

	// Enter PostCode
	public void enterPostCode(String value) throws InterruptedException {
		Thread.sleep(1000);
		log.info("Enter post code");
		sendTextToElement(postCode, value);
	}

	// Click On Add Button
	public void clickOnAddButton() throws InterruptedException {
		Thread.sleep(1000);
		log.info("Click on add button");
		clickOnElement(addButton);
	}

	// Popup Display
	public String verifyTextFromPopUp() {
		return getTextFromAlert();
	}

	public void acceptAlert() {
		driver.switchTo().alert().accept();
	}
}
