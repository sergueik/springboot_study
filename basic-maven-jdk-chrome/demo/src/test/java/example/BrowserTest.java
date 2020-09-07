package example;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.chrome.ChromeDriver;
import org.openqa.selenium.chrome.ChromeOptions;

import static org.hamcrest.CoreMatchers.containsString;
import static org.junit.Assert.assertThat;

public class BrowserTest {

	private WebDriver driver;

	@Before
	public void setUp() {
		ChromeOptions options = new ChromeOptions().setHeadless(true);
		driver = new ChromeDriver(options);
	}

	@After
	public void tearDown() {
		driver.close();
	}

	@Test
	public void canDuck() {
		driver.get("https://duckduckgo.com/");
		driver.findElement(By.id("search_form_input_homepage")).sendKeys("fish");
		driver.findElement(By.id("search_button_homepage")).click();

		assertThat(driver.getTitle(), containsString("fish"));
	}
}
