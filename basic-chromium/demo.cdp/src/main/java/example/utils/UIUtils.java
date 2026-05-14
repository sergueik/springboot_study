package example.utils;

import org.apache.commons.io.FileUtils;
import org.openqa.selenium.*;
import org.openqa.selenium.remote.Augmenter;
import org.openqa.selenium.support.ui.FluentWait;
import org.openqa.selenium.support.ui.Wait;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.concurrent.TimeUnit;

public class UIUtils {

	private static UIUtils instance = null;
	private static final Logger LOGGER = LoggerFactory.getLogger(UIUtils.class);
	private WebDriver driver;
	private String imageName = null;
	private String imagePath = null;
	private static long highlightInterval = 100;

	public String getImagePath() {
		return imagePath;
	}

	public static UIUtils getInstance() {
		if (instance == null) {
			instance = new UIUtils();
		}

		return instance;
	}

	public UIUtils() {
	}

	public void setDriver(WebDriver driver) {
		this.driver = driver;
	}

	@SuppressWarnings("deprecation")
	public WebElement findElement(By locator, int loadTimeout) {
		@SuppressWarnings({ "unchecked", "rawtypes" })
		Wait<WebDriver> wait = (new FluentWait(driver))
				.withTimeout((long) loadTimeout, TimeUnit.SECONDS)
				.pollingEvery(1L, TimeUnit.SECONDS)
				.ignoring(NoSuchElementException.class);
		WebElement element = (WebElement) wait.until((driver) -> {
			return driver.findElement(locator);
		});
		return element;
	}

	public void takeScreenShot() {
		try {
			String start_time = (new SimpleDateFormat("yyyy-MM-dd-HH-mm-ss"))
					.format(new Date());
			imageName = "selenium_img_" + start_time + ".png";
			File imageFolder = new File(System.getProperty("user.dir") + "/target");
			if (!imageFolder.exists()) {
				imageFolder.mkdir();
			}
			imagePath = imageFolder.getAbsolutePath() + "/" + imageName;
			LOGGER.info("Screeenshot: " + imagePath);
			driver = (new Augmenter()).augment(this.driver);
			File scrFile = (File) ((TakesScreenshot) driver)
					.getScreenshotAs(OutputType.FILE);
			FileUtils.copyFile(scrFile, new File(imagePath));
		} catch (IOException e) {
			LOGGER.error("Error", e);
			throw new IllegalStateException("Error taking screenshot");
		}
	}

	public Object executeJavaScript(String script) {
		Object result = ((JavascriptExecutor) driver).executeScript(script,
				new Object[] {});
		return result;
	}

	public Object executeJavaScript(String script, WebElement element) {
		Object result = ((JavascriptExecutor) driver).executeScript(script,
				new Object[] { element });
		return result;
	}

	public void scrollToElement(WebElement element) {
		this.executeJavaScript("arguments[0].scrollIntoView(true);", element);
	}

	public void highlight(WebElement element) {
		highlight(element, highlightInterval, "solid yellow");
	}

	public void highlight(WebElement element, long highlightInterval) {
		highlight(element, highlightInterval, "solid yellow");
	}

	public void highlight(WebElement element, long highlightInterval,
			String color) {
		try {
			this.executeJavaScript(
					String.format("arguments[0].style.border='3px %s'", color), element);
			Thread.sleep(highlightInterval);
			this.executeJavaScript("arguments[0].style.border=''", element);
		} catch (InterruptedException e) {
			// System.err.println("Exception (ignored): " + e.toString());
		}
	}

}
