package example;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import org.apache.logging.log4j.core.config.Configurator;
import org.apache.logging.log4j.core.config.Configuration;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.chrome.ChromeDriver;
import org.openqa.selenium.chrome.ChromeOptions;

import org.openqa.selenium.remote.DesiredCapabilities;
import org.openqa.selenium.support.PageFactory;
import org.openqa.selenium.support.ui.WebDriverWait;

import java.nio.file.Paths;
import java.time.Duration;

public class ManageBrowser {
	private static final Logger log = LoggerFactory.getLogger(ManageBrowser.class);

	public static WebDriver driver;

	private static String osName;
	protected static WebDriverWait wait;
	protected static boolean runHeadless = false;
	protected static boolean runFullscreen = false;

	protected static int flexibleWait = 10;
	public long implicitlyWait = Long.parseLong(PropertyReader.getInstance().getProperty("implicitlyWait"));
	protected static int pollingInterval = 500;

	public String baseUrl = PropertyReader.getInstance().getProperty("baseUrl");

	public static String getOSName() {
		if (osName == null) {
			osName = System.getProperty("os.name").toLowerCase();
			if (osName.startsWith("windows")) {
				osName = "windows";
			}
		}
		return osName;
	}

	public ManageBrowser() {
		PageFactory.initElements(driver, this);
	//	Configuration configuration = new Configuration(); 
	//	Configurator.reconfigure(System.getProperty("user.dir") + "/src/test/resources/log4j2.properties");
	}

	public void selectBrowser(String browser) {
		if ((System.getenv().containsKey("HEADLESS") && System.getenv("HEADLESS").matches("(?:true|yes|1)"))
				|| (!(getOSName().equals("windows")) && !(System.getenv().containsKey("DISPLAY")))) {
			runHeadless = true;
		}

		System.setProperty("webdriver.chrome.driver", Paths.get(System.getProperty("user.home")).resolve("Downloads")
				.resolve(osName.equals("windows") ? "chromedriver.exe" : "chromedriver").toAbsolutePath().toString());

		ChromeOptions options = new ChromeOptions();
		// @formatter:off
		for (String optionAgrument : (new String[] { "--allow-insecure-localhost", "--allow-running-insecure-content",
				"--browser.download.folderList=2",
				"--browser.helperApps.neverAsk.saveToDisk=image/jpg,text/csv,text/xml,application/xml,application/vnd.ms-excel,application/x-excel,application/x-msexcel,application/excel,application/pdf",
				"--disable-blink-features=AutomationControlled", "--disable-default-app", "--disable-dev-shm-usage",
				"--disable-extensions", "--disable-gpu", "--disable-infobars", "--disable-in-process-stack-traces",
				"--disable-logging", "--disable-notifications", "--disable-popup-blocking",
				"--disable-save-password-bubble", "--disable-translate", "--disable-web-security",
				"--enable-local-file-accesses", "--ignore-certificate-errors", "--ignore-certificate-errors",
				"--ignore-ssl-errors=true", "--log-level=3", "--no-proxy-server", "--no-sandbox", "--output=/dev/null",
				"--ssl-protocol=any",
				"--user-agent=Mozilla/5.0 (Windows NT 6.1; WOW64; rv:33.0) Gecko/20120101 Firefox/33.0",
				// String.format("--browser.download.dir=%s", downloadFilepath)
				/*
				 * "--user-data-dir=/path/to/your/custom/profile",
				 * "--profile-directory=name_of_custom_profile_directory",
				 */
		})) {
			options.addArguments(optionAgrument);
		}
		// @formatter:on
		if (runFullscreen) {
			for (String optionAgrument : (new String[] { "--start-fullscreen", "--start-maximized" })) {
				options.addArguments(optionAgrument);
			}
		}
		DesiredCapabilities desiredCapabilities = new DesiredCapabilities();
		desiredCapabilities.setCapability("pageLoadStrategy", "eager");
		options.merge(desiredCapabilities);

		if (runHeadless) {
			options.addArguments("--headless=new", "--disable-gpu");
		}

		driver = new ChromeDriver(options);
		wait = new WebDriverWait(driver, Duration.ofSeconds(flexibleWait));
		wait.pollingEvery(Duration.ofMillis(pollingInterval));

		if (runFullscreen) {
			driver.manage().window().maximize();
		}
		driver.manage().timeouts().implicitlyWait(Duration.ofSeconds(implicitlyWait));
		driver.get(baseUrl);
	}

	public void closeBrowser() {
		if (driver != null) {
			driver.quit();
		}
	}
}
