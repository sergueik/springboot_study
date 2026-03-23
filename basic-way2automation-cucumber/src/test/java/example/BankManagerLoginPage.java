package example;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import org.openqa.selenium.WebElement;
import org.openqa.selenium.support.CacheLookup;
import org.openqa.selenium.support.FindBy;
import org.openqa.selenium.support.PageFactory;

public class BankManagerLoginPage extends Utility {
	private static final Logger log = LoggerFactory.getLogger(BankManagerLoginPage.class);

    public BankManagerLoginPage() {
        PageFactory.initElements(driver,this);
    }
    // click On "Open Account" Tab
    @CacheLookup
    @FindBy(xpath = "//button[normalize-space()='Open Account']")
    WebElement openAccount;
    // Search customer that created in first test
    @CacheLookup
    @FindBy(xpath = "//select[@id='userSelect']")
    WebElement searchCustomer;
    // Select currency "Pound"
    @CacheLookup
    @FindBy(xpath = "//select[@id='currency']")
    WebElement currency;
    // 	click on "process" button
    @CacheLookup
    @FindBy(xpath = "//button[normalize-space()='Process']")
    WebElement process;

    // click On "Open Account" Tab
    public void clickOnOpenAccount() throws InterruptedException {
        Thread.sleep(1000);
        log.info("Click on open account");
        clickOnElement(openAccount);
    }
    // Search customer that created in first test
    public void clickAndSelectCustomerName() throws InterruptedException {
        Thread.sleep(1000);
        // clickOnElement(By.xpath("//select[@id='userSelect']"));
        log.info("Click and select customer name");
        selectByVisibleTextFromDropDown(searchCustomer,"Harry Potter");
    }
    //Select currency "Pound"
    public void selectCurrency(String currencyText) throws InterruptedException {
        Thread.sleep(1000);
        log.info("Select currency");
        selectByVisibleTextFromDropDown(currency,currencyText);
    }
    // 	click on "process" button
    public void clickOnProcessButton() throws InterruptedException {
        Thread.sleep(1000);
        log.info("CLick on process button");
        clickOnElement(process);
    }

    // Popup Displayed and verify Message
    public String verifyTextFromPopUp(){
        return getTextFromAlert();
    }
    public void acceptAlert(){
        driver.switchTo().alert().accept();
    }
}
