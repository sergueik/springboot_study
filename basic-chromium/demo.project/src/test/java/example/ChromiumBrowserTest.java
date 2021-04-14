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
import static org.hamcrest.CoreMatchers.is;

import java.io.File;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class ChromiumBrowserTest {

	private WebDriver driver;
	private static final List<String> plugins_disabled = new ArrayList<>();
	static {
		plugins_disabled.add("Chrome PDF Viewer");
	};
	private static final String downloadDirectory = System
			.getenv("DOWNLOAD_DIRECTORY");

	private static Map<String, Object> prefs = new HashMap<>();
	static {
		prefs.put("profile.default_content_settings.popups", 0);
		prefs.put("download.default_directory",
				downloadDirectory != null ? downloadDirectory : "/tmp");
		prefs.put("download.prompt_for_download", false);
		prefs.put("download.directory_upgrade", true);
		prefs.put("safebrowsing.enabled", false);
		prefs.put("plugins.always_open_pdf_externally", true);
		prefs.put("plugins.plugins_disabled", plugins_disabled);
	};

	private static String url = "https://www.wikipedia.org/";
	private static String cssSelector = "div.search-input > input[name=\"search\"]";
	private final static String text = "Ice";

	@Before
	public void setUp() {
		System.err
				.println("os.name: " + System.getProperty("os.name").toLowerCase());
		System.setProperty("webdriver.chrome.driver",
				System.getProperty("os.name").toLowerCase().startsWith("windows")
						? Paths.get(System.getProperty("user.home")).resolve("Downloads")
								.resolve("chromedriver.exe").toAbsolutePath().toString()
						: "/usr/bin/chromedriver");
		// https://stackoverflow.com/questions/55844788/how-to-fix-severe-bind-failed-cannot-assign-requested-address-99-while
		System.setProperty("webdriver.chrome.whitelistedIps", "");

		ChromeOptions options = new ChromeOptions().setHeadless(true);
		// NOTE: org.openqa.selenium.WebDriverException:
		// unknown error: Chrome failed to start: exited abnormally.
		// (unknown error: DevToolsActivePort file doesn't exist)

		options.addArguments("--headless", "--window-size=1200x800", "--no-sandbox",
				"--remote-debugging-address=0.0.0.0", "--remote-debugging-port=9222",
				"--disable-gpu");
		if (!System.getProperty("os.name").toLowerCase().startsWith("windows"))
			options.setBinary("/usr/bin/chromium-browser");

		ChromeOptions chromeOptions = new ChromeOptions();
		chromeOptions.setExperimentalOption("prefs", prefs);
		// @formatter:off

		// alternatively, inline - i find it less readable

		chromeOptions.setExperimentalOption("prefs", new HashMap<String, Object>() {
			{
				put("profile.default_content_settings.popups", 0);
				put("download.default_directory", downloadDirectory != null ? downloadDirectory : "/tmp");
				put("download.prompt_for_download", false);
				put("download.directory_upgrade", true);
				put("safebrowsing.enabled", false);
				put("plugins.always_open_pdf_externally", true);
				put("plugins.plugins_disabled", new ArrayList() {
					{
						add("Chrome PDF Viewer");
					}
				});
			}
		});

		// @formatter:on
		driver = new ChromeDriver(options);
	}

	@After
	public void tearDown() {
		driver.close();
	}

	@Test
	public void downloadPDF() {
		url = "http://www.africau.edu/images/default/sample.pdf";
		driver.get(url);
		try {
			Thread.sleep(5000);
		} catch (InterruptedException e) {
		}
		File file = new File(
				(downloadDirectory != null ? downloadDirectory : "/tmp") + "/"
						+ "sample.pdf");
		assertThat(file.exists(), is(false));
		file = new File(System.getProperty("user.dir") + "/" + "sample.pdf");
		assertThat(file.exists(), is(true));
	}

	@Test
	public void canSearch() {
		url = "https://www.wikipedia.org/";
		driver.get(url);
		driver.findElement(By.cssSelector(cssSelector)).sendKeys(text);
		driver.findElement(By.cssSelector("button[type=\"submit\"]")).click();

		assertThat(driver.getTitle(), containsString(text));
	}

}
