package example;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.*;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.chrome.ChromeDriver;
import org.openqa.selenium.chrome.ChromeOptions;
import org.openqa.selenium.net.PortProber;
// observed exception in setup:
// java.lang.NoClassDefFoundError: Could not initialize class org.openqa.selenium.net.PortProber
// Exception in thread "main" java.lang.NoSuchMethodError: java.io.FileReader.<init>(Ljava/io/File;Ljava/nio/charset/Charset;)V
// https://www.programcreek.com/java-api-examples/?api=org.openqa.selenium.net.PortProber
// https://github.com/SeleniumHQ/selenium/issues/7089
import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;

public class AppTest {
	private WebDriver driver;
	private static final String chromeDriverPath = System.getProperty("user.home") + "/Downloads/" + "chromedriver";
	private static String url = "http://www.juliodelima.com.br/taskit";
	private static final List<String> plugins_disabled = new ArrayList<>();
	static {
		plugins_disabled.add("Chrome PDF Viewer");
	};
	private static final String downloadDirectory = System.getenv("DOWNLOAD_DIRECTORY");
	private static Map<String, Object> prefs = new HashMap<>();
	static {
		prefs.put("profile.default_content_settings.popups", 0);
		prefs.put("download.default_directory", downloadDirectory != null ? downloadDirectory : "/tmp");
		prefs.put("download.prompt_for_download", false);
		prefs.put("download.directory_upgrade", true);
		prefs.put("safebrowsing.enabled", false);
		prefs.put("plugins.always_open_pdf_externally", true);
		prefs.put("plugins.plugins_disabled", plugins_disabled);
	};

	@Before
	public void setUp() {
		// Open the visual driver
		System.setProperty("webdriver.chrome.driver", chromeDriverPath);
		ChromeOptions chromeOptions = new ChromeOptions();
		chromeOptions.setExperimentalOption("prefs", prefs);
		PortProber.findFreePort();
		driver = new ChromeDriver(chromeOptions);
		driver.manage().timeouts().implicitlyWait(5, TimeUnit.SECONDS);
	}

	@Test
	public void downloadPDF() {
		url = "http://www.africau.edu/images/default/sample.pdf";
		driver.get(url);
		try {
			Thread.sleep(5000);
		} catch (InterruptedException e) {
		}
		String downloadDirectoryUsed = downloadDirectory != null ? downloadDirectory : "/tmp";
		File file = new File(downloadDirectoryUsed + "/" + "sample.pdf");
		assertThat("download file expected to exist", file.exists(), is(true));
		file = new File(downloadDirectoryUsed + "/" + "sample.pdf.crdownload");
		assertThat("partially download file expected to exist", file.exists(), is(true));
	}

	@Test
	public void testAddInformationOfUser() {
		url = "http://www.juliodelima.com.br/taskit";
		driver.get(url);
		driver.findElement(By.linkText("Sign in")).click();

		WebElement formSignInBox = driver.findElement(By.id("signinbox"));

		formSignInBox.findElement(By.name("login")).sendKeys("julio0001");
		formSignInBox.findElement(By.name("password")).sendKeys("123456");

		driver.findElement(By.linkText("SIGN IN")).click();

		WebElement me = driver.findElement(By.className("me"));
		String textElementMe = me.getText();
		assertEquals("Hi, Julio", textElementMe);
	}

	@After
	public void tearDow() {
		// Close the driver
		if (driver != null)
			driver.quit();
	}
}
