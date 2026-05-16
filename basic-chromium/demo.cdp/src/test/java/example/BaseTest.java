package example;

/**
 * Copyright 2022,2026 Serguei Kouzmine
 */
import java.io.IOException;
import java.util.Objects;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebDriverException;
import org.openqa.selenium.chrome.ChromeDriverService;

import example.messaging.CDPClient;

import example.utils.UIUtils;
import example.utils.Utils;
import example.utils.TestUtils;

public class BaseTest {
	protected static WebDriver driver;
	protected static Utils utils;
	protected static UIUtils uiUtils;
	protected static CDPClient CDPClient;
	protected int id;
	private final static int debugPort = Integer.parseInt(TestUtils.getPropertyEnv("debugPort", "0"));
	// used to override the default port 9222
	protected static boolean debug = false;
	protected static boolean headless = false; // false;
	protected static ChromeDriverService chromeDriverService;
	protected static String webSocketURL;

	protected static String GetWebSocketURL() {
		return BaseTest.webSocketURL;
	}

	public void setDebug(boolean value) {
		BaseTest.debug = value;
	}

	public void setHeadless(boolean value) {
		BaseTest.headless = value;
	}

	@BeforeClass
	public static void beforeClass() throws IOException {
		utils = Utils.getInstance();
		uiUtils = UIUtils.getInstance();
		utils.setDebug(debug);
		if (debugPort != 0) {
			utils.setDebugPort(debugPort);
		}
		// force the headless flag to be true to support Unix console execution
		if (!(Utils.getOsName().equals("windows")) && !(System.getenv().containsKey("DISPLAY"))) {
			headless = true;
		}
		if (System.getenv().containsKey("HEADLESS") && System.getenv("HEADLESS").matches("(?:true|yes|1)")) {
			headless = true;
		}

		System.err.println("HEADLESS: " + headless);
		driver = utils.launchBrowser(headless);
		uiUtils.setDriver(driver);
		webSocketURL = utils.getWebSocketURL();
		CDPClient = new CDPClient(webSocketURL);
	}

	@Before
	public void beforeTest() throws IOException {
		id = utils.getDynamicID();
	}

	@After
	public void afterTest() {
		// org.openqa.selenium.WebDriverException:
		// unknown error: cannot determine
		// loading status
		driver.navigate().to("about:blank");
	}

	@AfterClass
	public static void afterClass() {
		if (!Objects.isNull(CDPClient))
			// NOTE: java.lang.NullPointerException
			try {
				CDPClient.disconnect();
			} catch (Exception e) {
				// ignore
			}

		try {
			utils.stopChrome();
		} catch (WebDriverException e) {
			// chrome not reachable
		}
		if (!Objects.isNull(chromeDriverService))
			chromeDriverService.stop();
	}

}
