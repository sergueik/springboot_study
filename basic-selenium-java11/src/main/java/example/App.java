package example;

import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.chrome.ChromeDriver;
import org.openqa.selenium.chrome.ChromeOptions;
import java.nio.file.Paths;
import java.io.IOException;

import java.util.concurrent.TimeUnit;

public class App {

	private static WebDriver driver;
	private static final String chromeDriverPath = System.getProperty("chromeDriverPath",
			Paths.get(System.getProperty("user.home")).resolve("Downloads").resolve("chromedriver").toAbsolutePath()
					.toString());
	private static final String url = "http://www.juliodelima.com.br/taskit";

	public static void main(String[] args) throws InterruptedException, IOException {

		System.setProperty("webdriver.chrome.driver", chromeDriverPath);
		ChromeOptions options = new ChromeOptions();
		for (String optionAgrument : (new String[] { "--headless", "--window-size=1200x800", "--no-sandbox",
				"--remote-debugging-address=0.0.0.0", "--remote-debugging-port=9222", "--disable-gpu" })) {
			options.addArguments(optionAgrument);
		}
		driver = new ChromeDriver(options);
		driver.manage().timeouts().implicitlyWait(5, TimeUnit.SECONDS);

		driver.get(url);
		driver.findElement(By.linkText("Sign in")).click();

		WebElement formSignInBox = driver.findElement(By.id("signinbox"));

		formSignInBox.findElement(By.name("login")).sendKeys("julio0001");

		formSignInBox.findElement(By.name("password")).sendKeys("123456");

		driver.findElement(By.linkText("SIGN IN")).click();

		WebElement me = driver.findElement(By.className("me"));
		String text = me.getText();
		System.err.println(text);

		driver.close();
		driver.quit();
	}
}
