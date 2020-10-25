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
	private static final String chromeDriverPath = System.getProperty("user.dir") + "/" + "chromedriver";
	private static final String url = "http://www.juliodelima.com.br/taskit";

	@Before
	public void setUp() {
		// Open the visual browser
		System.setProperty("webdriver.chrome.driver", chromeDriverPath);
		browser = new ChromeDriver();
		browser.manage().timeouts().implicitlyWait(5, TimeUnit.SECONDS);

	}

	@Test
	public void testAddInformationOfUser() {
		browser.get(url);
		browser.findElement(By.linkText("Sign in")).click();

		WebElement formSignInBox = browser.findElement(By.id("signinbox"));

		formSignInBox.findElement(By.name("login")).sendKeys("julio0001");
		formSignInBox.findElement(By.name("password")).sendKeys("123456");

		browser.findElement(By.linkText("SIGN IN")).click();

		WebElement me = browser.findElement(By.className("me"));
		String textElementMe = me.getText();
		assertEquals("Hi, Julio", textElementMe);
	}

	@After
	public void tearDow() {
		// Close the browser
		browser.quit();
	}
}
