package example;

import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.chrome.ChromeDriver;
import org.openqa.selenium.chrome.ChromeOptions;
import java.nio.file.Paths;

import java.util.concurrent.TimeUnit;

public class App {
	private static WebDriver driver;

	public static void main(String[] args) throws InterruptedException, java.io.IOException {

		navigate();

		driver.close();
		driver.quit();

	}

	public static void navigate() throws InterruptedException, java.io.IOException   {
	/* final */ String chromeDriverPath = 
"/usr/bin/chromedriver";
//  		chromeDriverPath =  Paths.get(System.getProperty("user.home")).resolve("Downloads").resolve("chromedriver" ).toAbsolutePath().toString();
		// Open the driver
		System.setProperty("webdriver.chrome.driver", chromeDriverPath 
);
		ChromeOptions options = new ChromeOptions();
		for (String optionAgrument : (new String[] { "--headless", "--window-size=1200x800", "--remote-debugging-port=9222", "--disable-gpu"})) {
					options.addArguments(optionAgrument);
				}
		driver = new ChromeDriver(options);
		driver.manage().timeouts().implicitlyWait(5, TimeUnit.SECONDS);

		// Open a page
		driver.get("http://www.juliodelima.com.br/taskit");
		// Click on the link that has the text "Sign in"
		driver.findElement(By.linkText("Sign in")).click();

		// Identifying the field with the name "login" that is inside the id form
		// "signinbox"
		WebElement formSignInBox = driver.findElement(By.id("signinbox"));

		// Enter the text "julio0001" in the field with the name "login" inside the id
		// form "signinbox"
		formSignInBox.findElement(By.name("login")).sendKeys("julio0001");

		// Click on the field with the name "password" inside the id form "signinbox"
		formSignInBox.findElement(By.name("password")).sendKeys("123456");

		// Click on the link with the text "SIGN IN"
		driver.findElement(By.linkText("SIGN IN")).click();

		// Validate that inside the element with class "me" is the text "Hi, Julio"
		WebElement me = driver.findElement(By.className("me"));
		String text = me.getText();
System.err.println(text);
		// assertEquals("Hi, Julio", textElementMe);

		// Validation
		// assertEquals(1, 1);

	}
}
