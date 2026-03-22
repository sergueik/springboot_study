package example;

import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.support.CacheLookup;
import org.openqa.selenium.support.FindBy;
import org.openqa.selenium.support.PageFactory;

public class AccountPage extends Utility {
    private static final Logger log = LogManager.getLogger(AccountPage.class.getName());

    public AccountPage() {
        PageFactory.initElements(driver,this);
    }
    // click on "Deposit" Tab
    @CacheLookup
    @FindBy(xpath = "//button[normalize-space()='Deposit']")
    WebElement deposit;
    // Enter amount 100
    @CacheLookup
    @FindBy(xpath = "//input[@placeholder='amount']")
    WebElement amountTab;
    //click on "Deposit" Button
    @CacheLookup
    @FindBy(xpath = "//button[@type='submit']")
    WebElement clickDeposit;
    // verify message "Deposit Successful"
    @CacheLookup
    @FindBy(xpath = "//span[contains(text(),'Deposit Successful')]")
    WebElement depositSuccessfullyText;
    //click on "Withdrawl" Tab
    @CacheLookup
    @FindBy(xpath = "//button[normalize-space()='Withdrawl']")
    WebElement withdrawl;
    // Enter amount 50
    @CacheLookup
    @FindBy(xpath = "//input[@placeholder='amount']")
    WebElement withdrawalAmountTab;
    //click on "Deposit" Button
    @CacheLookup
    @FindBy(xpath = "//button[@class='btn btn-default']")
    WebElement withdrawTab;
    //	verify message "Transaction Successful"
    @CacheLookup
    @FindBy(xpath = "//span[@class='error ng-binding']")
    WebElement withdrawalSuccessfullyText;


    //click on "Deposit" Tab
    public void clickOnDepositTab() throws InterruptedException {
        Thread.sleep(1000);
        log.info("Click on deposit tab");
        clickOnElement(deposit);
    }

    // Enter amount 100
    public void enterAmount(String amount) throws InterruptedException {
        Thread.sleep(1000);
        log.info("Enter amount");
        sendTextToElement(amountTab, amount);
    }

    //click on "Deposit" Button
    public void clickDepositTab() throws InterruptedException {
        Thread.sleep(1000);
        log.info("Click on deposit tab");
        clickOnElement(clickDeposit);
    }

    // verify message "Deposit Successful"
    public String verifyDepositSuccessfullyTextMessage() throws InterruptedException {
        Thread.sleep(1000);
        log.info("Get deposit successfully tab");
        return getTextFromElement(depositSuccessfullyText);
    }

    //click on "Withdrawl" Tab
    public void clickOnWithdrawl() throws InterruptedException {
        Thread.sleep(1000);
        log.info("Click on withdrawl");
        clickOnElement(withdrawl);
    }

    //Enter amount 50
    public void enterWithdrawlAmount(String amount) throws InterruptedException {
        Thread.sleep(1000);
        log.info("Enter withdrawl amount");
        sendTextToElement(withdrawalAmountTab, amount);
    }

    //click on "Deposit" Button
    public void clickOnWithdrawTransactionTab() throws InterruptedException {
        Thread.sleep(1000);
        log.info("Click on withdrawl transaction tab");
        clickOnElement(withdrawTab);
    }

    //	verify message "Transaction Successful"
    public String verifyWithdrawalSuccessfullyTextMessage() throws InterruptedException {
        Thread.sleep(1000);
        log.info("Get withdrawl successful tab");
        return getTextFromElement(withdrawalSuccessfullyText);
    }
}
