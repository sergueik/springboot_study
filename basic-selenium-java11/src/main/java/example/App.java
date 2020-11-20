package example;

import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.chrome.ChromeDriver;
import org.openqa.selenium.chrome.ChromeOptions;

import java.nio.file.Paths;
import java.io.IOException;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import java.util.concurrent.TimeUnit;
import org.openqa.selenium.net.PortProber;

public class App {

	private static WebDriver driver;
	private static final String chromeDriverPath = System.getProperty("chromeDriverPath",
			Paths.get(System.getProperty("user.home")).resolve("Downloads").resolve("chromedriver").toAbsolutePath()
					.toString());
	private static String url = null;
	private static final List<String> plugins_disabled = new ArrayList<>();
	static {
		plugins_disabled.add("Chrome PDF Viewer");
	};
	private static Map<String, Object> prefs = new HashMap<>();
	private static final String downloadDirectory = System.getenv("DOWNLOAD_DIRECTORY");
	static {
		prefs.put("profile.default_content_settings.popups", 0);
		prefs.put("download.default_directory", downloadDirectory != null ? downloadDirectory : "/tmp");
		prefs.put("download.prompt_for_download", false);
		prefs.put("download.directory_upgrade", true);
		prefs.put("safebrowsing.enabled", false);
		prefs.put("plugins.always_open_pdf_externally", true);
		prefs.put("plugins.plugins_disabled", plugins_disabled);
	};

	public static void main(String[] args) throws InterruptedException, IOException {

		System.setProperty("webdriver.chrome.driver", chromeDriverPath);
		ChromeOptions chromeOptions = new ChromeOptions();
		for (String optionAgrument : (new String[] { "--headless", "--window-size=1200x800", "--no-sandbox",
				"--remote-debugging-address=0.0.0.0", "--remote-debugging-port=9222", "--disable-gpu" })) {
			chromeOptions.addArguments(optionAgrument);
		}
		chromeOptions.setExperimentalOption("prefs", prefs);
		driver = new ChromeDriver(chromeOptions);
		// add code that was failing in JDK 8
		//  the exception cannot be handled without upgrading
		//  Exception in thread "main" java.lang.NoSuchMethodError: java.io.FileReader.<init>(Ljava/io/File;Ljava/nio/charset/Charset;)V
		// at org.openqa.selenium.net.LinuxEphemeralPortRangeDetector.getInstance(LinuxEphemeralPortRangeDetector.java:36)
		// at org.openqa.selenium.net.PortProber.<clinit>(PortProber.java:42)

		try {
			int port =  PortProber.findFreePort() ;
			System.err.println("PortProber findFreePort -> " + port);
		} catch (Exception e){
			System.err.println("Exception: " + e.toString());
		}
		driver.manage().timeouts().implicitlyWait(5, TimeUnit.SECONDS);
		url = "http://www.juliodelima.com.br/taskit";
		driver.get(url);
		driver.findElement(By.linkText("Sign in")).click();

		WebElement formSignInBox = driver.findElement(By.id("signinbox"));

		formSignInBox.findElement(By.name("login")).sendKeys("julio0001");

		formSignInBox.findElement(By.name("password")).sendKeys("123456");

		driver.findElement(By.linkText("SIGN IN")).click();

		WebElement me = driver.findElement(By.className("me"));
		String text = me.getText();
		System.err.println(text);
		url = "http://www.africau.edu/images/default/sample.pdf";
		driver.get(url);

		driver.close();
		driver.quit();
	}
}
