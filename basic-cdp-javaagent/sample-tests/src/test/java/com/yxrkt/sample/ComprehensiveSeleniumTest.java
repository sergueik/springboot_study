package com.yxrkt.sample;

import org.junit.jupiter.api.*;
import org.openqa.selenium.*;
import org.openqa.selenium.chrome.ChromeDriver;
import org.openqa.selenium.chrome.ChromeOptions;
import org.openqa.selenium.support.ui.Select;
import org.openqa.selenium.support.ui.WebDriverWait;
import org.openqa.selenium.support.ui.ExpectedConditions;
import org.openqa.selenium.interactions.Actions;
import java.time.Duration;
import java.util.List;
import java.util.Set;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;

/**
 * Comprehensive Selenium test covering all major WebDriver functions
 * Uses selenium.dev test page and example.com for testing
 */
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class ComprehensiveSeleniumTest {
    
    private static WebDriver driver;
    private static WebDriverWait wait;
    private static Actions actions;
    
    @BeforeAll
    static void setUp() {
        ChromeOptions options = new ChromeOptions();
        options.addArguments("--headless");
        options.addArguments("--no-sandbox");
        options.addArguments("--disable-dev-shm-usage");
        options.addArguments("--disable-gpu");
        options.addArguments("--window-size=1920,1080");
        
        driver = new ChromeDriver(options);
        driver.manage().timeouts().implicitlyWait(Duration.ofSeconds(10));
        wait = new WebDriverWait(driver, Duration.ofSeconds(15));
        actions = new Actions(driver);
    }
    
    @AfterAll
    static void tearDown() {
        if (driver != null) {
            driver.quit();
        }
    }
    
    @BeforeEach
    void cleanupTraceFile() {
        try {
            File traceFile = new File("seltrace.ndjson");
            if (traceFile.exists()) {
                Files.delete(Paths.get("seltrace.ndjson"));
                System.out.println("seltrace.ndjson deleted successfully!!!");
            }
        } catch (IOException e) {
            // Ignore if file doesn't exist or can't be deleted
        }
    }
    
    @Test
    @Order(1)
    @DisplayName("Basic Navigation and Page Operations")
    void testBasicNavigation() {
        // Test basic navigation
        driver.get("https://example.com");
        
        // Test page title and URL
        String title = driver.getTitle();
        String currentUrl = driver.getCurrentUrl();
        String pageSource = driver.getPageSource();
        
        Assertions.assertNotNull(title);
        Assertions.assertTrue(currentUrl.contains("example.com"));
        Assertions.assertTrue(pageSource.contains("Example Domain"));
        
        // Test window management
        Dimension windowSize = driver.manage().window().getSize();
        driver.manage().window().maximize();
        
        // Test navigation commands
        driver.navigate().refresh();
        
        // Navigate to selenium.dev test page
        driver.navigate().to("https://www.selenium.dev/selenium/web/web-form.html");
        
        // Test back and forward navigation
        driver.navigate().back();
        driver.navigate().forward();
    }
    
    @Test
    @Order(2)
    @DisplayName("Element Location and Interaction")
    void testElementInteractions() {
        driver.get("https://www.selenium.dev/selenium/web/web-form.html");
        
        // Test various element location strategies
        WebElement textInput = driver.findElement(By.name("my-text"));
        WebElement passwordInput = driver.findElement(By.name("my-password"));
        WebElement textArea = driver.findElement(By.name("my-textarea"));
        WebElement submitButton = driver.findElement(By.cssSelector("button[type='submit']"));
        
        // Test element properties
        String tagName = textInput.getTagName();
        String inputType = textInput.getAttribute("type");
        boolean isDisplayed = textInput.isDisplayed();
        boolean isEnabled = textInput.isEnabled();
        
        Assertions.assertEquals("input", tagName);
        Assertions.assertEquals("text", inputType);
        Assertions.assertTrue(isDisplayed);
        Assertions.assertTrue(isEnabled);
        
        // Test text input operations
        textInput.clear();
        textInput.sendKeys("Test Input Text");
        String inputValue = textInput.getAttribute("value");
        Assertions.assertEquals("Test Input Text", inputValue);
        
        // Test password input
        passwordInput.clear();
        passwordInput.sendKeys("SecretPassword123");
        
        // Test textarea
        textArea.clear();
        textArea.sendKeys("This is a test message in textarea");
        
        // Test click operations
        textInput.click();
        passwordInput.click();
        textArea.click();
    }
    
    @Test
    @Order(3)
    @DisplayName("Dropdown and Select Operations")
    void testSelectOperations() {
        driver.get("https://www.selenium.dev/selenium/web/web-form.html");
        
        // Test dropdown selection
        WebElement dropdown = driver.findElement(By.name("my-select"));
        Select select = new Select(dropdown);
        
        // Test select by visible text
        select.selectByVisibleText("Two");
        WebElement selectedOption = select.getFirstSelectedOption();
        Assertions.assertEquals("Two", selectedOption.getText());
        
        // Test select by value
        select.selectByValue("3");
        selectedOption = select.getFirstSelectedOption();
        Assertions.assertEquals("Three", selectedOption.getText());
        
        // Test select by index
        select.selectByIndex(0);
        selectedOption = select.getFirstSelectedOption();
        Assertions.assertEquals("Open this select menu", selectedOption.getText());
        
        // Test getting all options
        List<WebElement> allOptions = select.getOptions();
        Assertions.assertTrue(allOptions.size() > 0);
    }
    
    @Test
    @Order(4)
    @DisplayName("Checkbox and Radio Button Operations")
    void testCheckboxAndRadio() {
        driver.get("https://www.selenium.dev/selenium/web/web-form.html");
        
        // Test checkbox operations
        List<WebElement> checkboxes = driver.findElements(By.cssSelector("input[type='checkbox']"));
        if (!checkboxes.isEmpty()) {
            WebElement checkbox = checkboxes.get(0);
            
            // Test checkbox state
            boolean initialState = checkbox.isSelected();
            
            // Toggle checkbox
            checkbox.click();
            boolean afterClickState = checkbox.isSelected();
            Assertions.assertNotEquals(initialState, afterClickState);
        }
        
        // Test radio button operations
        List<WebElement> radioButtons = driver.findElements(By.cssSelector("input[type='radio']"));
        if (!radioButtons.isEmpty()) {
            WebElement radioButton = radioButtons.get(0);
            
            // Test radio button selection
            if (!radioButton.isSelected()) {
                radioButton.click();
                Assertions.assertTrue(radioButton.isSelected());
            }
        }
    }
    
    @Test
    @Order(5)
    @DisplayName("Wait Operations and Explicit Waits")
    void testWaitOperations() {
        driver.get("https://www.selenium.dev/selenium/web/web-form.html");
        
        // Test explicit wait for element to be clickable
        WebElement submitButton = wait.until(ExpectedConditions.elementToBeClickable(By.cssSelector("button[type='submit']")));
        Assertions.assertNotNull(submitButton);
        
        // Test wait for element presence
        WebElement textInput = wait.until(ExpectedConditions.presenceOfElementLocated(By.name("my-text")));
        Assertions.assertNotNull(textInput);
        
        // Test wait for element visibility
        WebElement visibleElement = wait.until(ExpectedConditions.visibilityOfElementLocated(By.name("my-text")));
        Assertions.assertNotNull(visibleElement);
        
        // Test implicit wait by changing timeout
        driver.manage().timeouts().implicitlyWait(Duration.ofSeconds(5));
        driver.manage().timeouts().implicitlyWait(Duration.ofSeconds(10)); // Reset
    }
    
    @Test
    @Order(6)
    @DisplayName("Advanced Actions and Mouse Operations")
    void testAdvancedActions() {
        driver.get("https://www.selenium.dev/selenium/web/web-form.html");
        
        WebElement textInput = driver.findElement(By.name("my-text"));
        WebElement submitButton = driver.findElement(By.cssSelector("button[type='submit']"));
        
        // Test mouse hover
        actions.moveToElement(submitButton).perform();
        
        // Test double click
        actions.doubleClick(textInput).perform();
        
        // Test right click (context click)
        actions.contextClick(textInput).perform();
        
        // Test drag and drop (if elements are available)
        actions.clickAndHold(textInput)
               .moveToElement(submitButton)
               .release()
               .perform();
        
        // Test keyboard actions
        textInput.clear();
        actions.sendKeys(textInput, "Test with Actions").perform();
        
        // Test key combinations
        actions.keyDown(Keys.CONTROL)
               .sendKeys("a")
               .keyUp(Keys.CONTROL)
               .perform();
    }
    
    @Test
    @Order(7)
    @DisplayName("JavaScript Execution")
    void testJavaScriptExecution() {
        driver.get("https://example.com");
        
        JavascriptExecutor js = (JavascriptExecutor) driver;
        
        // Test JavaScript execution
        String title = (String) js.executeScript("return document.title;");
        Assertions.assertNotNull(title);
        
        // Test scroll operations
        js.executeScript("window.scrollTo(0, document.body.scrollHeight);");
        js.executeScript("window.scrollTo(0, 0);");
        
        // Test element manipulation via JavaScript
        driver.get("https://www.selenium.dev/selenium/web/web-form.html");
        WebElement textInput = driver.findElement(By.name("my-text"));
        js.executeScript("arguments[0].style.border='3px solid red';", textInput);
        js.executeScript("arguments[0].value='Set by JavaScript';", textInput);
        
        String jsValue = (String) js.executeScript("return arguments[0].value;", textInput);
        Assertions.assertEquals("Set by JavaScript", jsValue);
    }
    
    @Test
    @Order(8)
    @DisplayName("Window and Frame Operations")
    void testWindowOperations() {
        driver.get("https://example.com");
        
        // Get current window handle
        String originalWindow = driver.getWindowHandle();
        Assertions.assertNotNull(originalWindow);
        
        // Test window size operations
        Dimension currentSize = driver.manage().window().getSize();
        driver.manage().window().setSize(new Dimension(800, 600));
        driver.manage().window().setSize(currentSize); // Reset
        
        // Test window position
        Point currentPosition = driver.manage().window().getPosition();
        driver.manage().window().setPosition(new Point(0, 0));
        driver.manage().window().setPosition(currentPosition); // Reset
        
        // Test getting all window handles
        Set<String> allWindows = driver.getWindowHandles();
        Assertions.assertTrue(allWindows.contains(originalWindow));
    }
    
    @Test
    @Order(9)
    @DisplayName("Cookie Operations")
    void testCookieOperations() {
        driver.get("https://example.com");
        
        // Add a cookie
        Cookie testCookie = new Cookie("test_cookie", "test_value");
        driver.manage().addCookie(testCookie);
        
        // Get the cookie
        Cookie retrievedCookie = driver.manage().getCookieNamed("test_cookie");
        Assertions.assertNotNull(retrievedCookie);
        Assertions.assertEquals("test_value", retrievedCookie.getValue());
        
        // Get all cookies
        Set<Cookie> allCookies = driver.manage().getCookies();
        Assertions.assertTrue(allCookies.size() > 0);
        
        // Delete the cookie
        driver.manage().deleteCookie(testCookie);
        Cookie deletedCookie = driver.manage().getCookieNamed("test_cookie");
        Assertions.assertNull(deletedCookie);
    }
    
    @Test
    @Order(10)
    @DisplayName("Form Submission and Page Interaction")
    void testFormSubmission() {
        driver.get("https://www.selenium.dev/selenium/web/web-form.html");
        
        // Fill out the form completely
        WebElement textInput = driver.findElement(By.name("my-text"));
        WebElement passwordInput = driver.findElement(By.name("my-password"));
        WebElement textArea = driver.findElement(By.name("my-textarea"));
        WebElement dropdown = driver.findElement(By.name("my-select"));
        WebElement submitButton = driver.findElement(By.cssSelector("button[type='submit']"));
        
        // Fill form fields
        textInput.clear();
        textInput.sendKeys("Comprehensive Test");
        
        passwordInput.clear();
        passwordInput.sendKeys("TestPassword123");
        
        textArea.clear();
        textArea.sendKeys("This is a comprehensive test of Selenium WebDriver functionality.");
        
        Select select = new Select(dropdown);
        select.selectByVisibleText("Two");
        
        // Submit the form
        submitButton.click();
        
        // Wait for page to load and verify submission (form redirects to submitted-form.html)
        wait.until(ExpectedConditions.urlContains("submitted-form.html"));
        
        // Verify we're on the result page or form was processed
        String currentUrl = driver.getCurrentUrl();
        Assertions.assertNotNull(currentUrl);
        Assertions.assertTrue(currentUrl.contains("submitted-form.html"), "Expected to be redirected to submitted-form.html after form submission");
    }
}