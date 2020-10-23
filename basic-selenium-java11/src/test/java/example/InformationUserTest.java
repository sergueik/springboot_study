package example;

import static org.junit.Assert.*;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.chrome.ChromeDriver;

import java.util.concurrent.TimeUnit;

public class InformationUserTest {
	private WebDriver browser;

	@Before
	public void setUp() {
		// Open the browser
		System.setProperty("webdriver.chrome.driver", System.getProperty("user.dir") + "/" + "chromedriver");
		browser = new ChromeDriver();
		browser.manage().timeouts().implicitlyWait(5, TimeUnit.SECONDS);

		// Open a page
		browser.get("http://www.juliodelima.com.br/taskit");
	}

	@Test
	public void testAddInformationOfUser() {
		// Click on the link that has the text "Sign in"
		browser.findElement(By.linkText("Sign in")).click();

		// Identifying the field with the name "login" that is inside the id form
		// "signinbox"
		WebElement formSignInBox = browser.findElement(By.id("signinbox"));

		// Enter the text "julio0001" in the field with the name "login" inside the id
		// form "signinbox"
		formSignInBox.findElement(By.name("login")).sendKeys("julio0001");

		// Click on the field with the name "password" inside the id form "signinbox"
		formSignInBox.findElement(By.name("password")).sendKeys("123456");

		// Click on the link with the text "SIGN IN"
		browser.findElement(By.linkText("SIGN IN")).click();

		// Validate that inside the element with class "me" is the text "Hi, Julio"
		WebElement me = browser.findElement(By.className("me"));
		String textElementMe = me.getText();
		assertEquals("Hi, Julio", textElementMe);

		// Validation
		assertEquals(1, 1);
	}

	@After
	public void tearDow() {
		// Close the browser
		browser.quit();
	}
}
