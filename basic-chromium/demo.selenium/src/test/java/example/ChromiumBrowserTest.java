package example;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.chrome.ChromeDriver;
import org.openqa.selenium.chrome.ChromeOptions;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.MatcherAssert.assertThat;

public class ChromiumBrowserTest {

	private WebDriver driver;

	@Before
	public void setUp() {
		System.setProperty("webdriver.chrome.driver", "/usr/bin/chromedriver");
		ChromeOptions options = new ChromeOptions().setHeadless(true);
		// NOTE: org.openqa.selenium.WebDriverException:
		// unknown error: Chrome failed to start: exited abnormally.
		// (unknown error: DevToolsActivePort file doesn't exist)

		options.addArguments( "--headless", "--window-size=1200x800", "--no-sandbox", "--remote-debugging-address=0.0.0.0", "--remote-debugging-port=9222", "--disable-gpu" );
		options.setBinary("/usr/bin/chromium-browser");
		driver = new ChromeDriver(options);
	}

	@After
	public void tearDown() {
		driver.close();
	}

	private final static String url = "https://www.wikipedia.org/";
	private final static String cssSelector = "div.search-input > input[name=\"search\"]";
	private final static String text = "Ice";

	@Test
	public void canSearch() {
		driver.get(url);
		driver.findElement(By.cssSelector(cssSelector)).sendKeys(text);
		driver.findElement(By.cssSelector("button[type=\"submit\"]")).click();

		assertThat(driver.getTitle(), containsString(text));
	}
	// TODO:
	// [INFO] Skipping execution of surefire because it has already been run for
	// this configuration

}

