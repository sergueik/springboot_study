package example;

import cucumber.api.java.en.And;
import cucumber.api.java.en.Then;
import cucumber.api.java.en.When;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import org.testng.Assert;

public class BankSteps {

	private static final Logger log = LoggerFactory.getLogger(BankSteps.class);

	@When("^I click on Bank Manager Login tab$")
	public void iClickOnBankManagerLoginTab() {
		new HomePage().clickOnBankMangerLogin();
	}

	@And("^I click on Add Customer Tab$")
	public void iClickOnAddCustomerTab() {
		new AddCustomerPage().clickOnAddCustomer();
	}

	@And("^I enter firstName \"([^\"]*)\"$")
	public void iEnterFirstName(String firstName) throws InterruptedException {
		new AddCustomerPage().enterFirstname(firstName);
	}

	@And("^I enter lastName \"([^\"]*)\"$")
	public void iEnterLastName(String lastName) throws InterruptedException {
		new AddCustomerPage().enterLastname(lastName);
	}

	@And("^I enter postcode\"([^\"]*)\"$")
	public void iEnterPostcode(String postcode) throws InterruptedException {
		new AddCustomerPage().enterPostCode(postcode);
	}

	@And("^I click on Add customer button$")
	public void iClickOnAddCustomerButton() throws InterruptedException {
		new AddCustomerPage().clickOnAddButton();
	}

	@Then("^I verify the message \"([^\"]*)\"$")
	public void iVerifyTheMessage(String expText) {
		Assert.assertEquals(new AddCustomerPage().verifyTextFromPopUp(), expText);
	}

	@And("^I click on OK button on popup$")
	public void iClickOnOKButtonOnPopup() {
		new AddCustomerPage().acceptAlert();
	}

	@And("^I click on Open Account Tab$")
	public void iClickOnOpenAccountTab() throws InterruptedException {
		new BankManagerLoginPage().clickOnOpenAccount();
	}

	@And("^I search customer that created in first test$")
	public void iSearchCustomerThatCreatedInFirstTest() throws InterruptedException {
		new BankManagerLoginPage().clickAndSelectCustomerName();
	}

	@And("^I select currency \"([^\"]*)\"$")
	public void iSelectCurrency(String currency) throws InterruptedException {
		new BankManagerLoginPage().selectCurrency(currency);

	}

	@And("^I click on the process button$")
	public void iClickOnTheProcessButton() throws InterruptedException {
		new BankManagerLoginPage().clickOnProcessButton();
	}

	@Then("^I verify message \"([^\"]*)\"$")
	public void iVerifyMessage(String expTxt) {
		Assert.assertEquals(new BankManagerLoginPage().verifyTextFromPopUp(), expTxt);
	}

	@And("^I click on Ok button on popup$")
	public void iClickOnOkButtonOnPopup() {
		new BankManagerLoginPage().acceptAlert();
	}

	@When("^I click on Customer Login tab$")
	public void iClickOnCustomerLoginTab() throws InterruptedException {
		new HomePage().clickOnCustomerLogin();
	}

	@And("^I click on Login button$")
	public void iClickOnLoginButton() throws InterruptedException {
		new CustomerLoginPage().clickOnLoginButton();
	}

	@Then("^I verify Logout tab displayed$")
	public void iVerifyLogoutTabDisplayed() throws InterruptedException {
		Assert.assertTrue(new CustomerLoginPage().isLogoutButtonPresence(), "Logout tab is not Displayed");
	}

	@And("^I click on Logout$")
	public void iClickOnLogout() throws InterruptedException {
		new CustomerLoginPage().clickOnLogoutTab();
	}

	@Then("^I verify \"([^\"]*)\" text displayed$")
	public void iVerifyTextDisplayed(String expectedText) throws InterruptedException {
		Assert.assertEquals(new CustomerLoginPage().verifyYourNameTextIsDisplayed(), expectedText,
				"Your Name is not Displayed");
	}

	@And("^I click on Deposit tab$")
	public void iClickOnDepositTab() throws InterruptedException {
		new AccountPage().clickOnDepositTab();
	}

	@And("^I enter amount \"([^\"]*)\"$")
	public void iEnterAmount(String amount) throws InterruptedException {
		new AccountPage().enterAmount(amount);
	}

	@And("^I click on Deposit button$")
	public void iClickOnDepositButton() throws InterruptedException {
		new AccountPage().clickDepositTab();
	}

	@Then("^Verify the message \"([^\"]*)\"$")
	public void verifyTheMessage(String expectedText) throws InterruptedException {
		Assert.assertEquals(new AccountPage().verifyDepositSuccessfullyTextMessage(), expectedText,
				"No such message found");
	}

	@And("^I click on Withdrawl tab$")
	public void iClickOnWithdrawlTab() throws InterruptedException {
		new AccountPage().clickOnWithdrawl();
	}

	@And("^I click on Withdrawl button$")
	public void iClickOnWithdrawlButton() throws InterruptedException {
		new AccountPage().clickOnWithdrawTransactionTab();
	}

	@Then("^I verify the text \"([^\"]*)\"$")
	public void iVerifyTheText(String expectedText) throws InterruptedException {
		Assert.assertEquals(new AccountPage().verifyWithdrawalSuccessfullyTextMessage(), expectedText,
				"No such message found");
	}

}
