# Selenium CDP Agent

A Java agent that instruments Selenium WebDriver calls and automatically attaches Chrome DevTools Protocol (CDP) listeners for ChromeDriver. The agent captures detailed execution traces and writes events as NDJSON (Newline Delimited JSON) to a configurable output path.

## Features

- **Automatic Instrumentation**: Intercepts all Selenium WebDriver method calls without code changes
- **CDP Integration**: Automatically attaches Chrome DevTools Protocol listeners
- **Comprehensive Tracing**: Captures navigation, element interactions, JavaScript execution, and more
- **Framework Agnostic**: Works with TestNG, JUnit, Cucumber, and other Java test frameworks
- **Build Tool Support**: Easy integration with Maven and Gradle
- **Configurable Output**: Customizable trace output format and location

## Requirements

- **Java**: JDK 11 or higher
- **Build Tool**: Maven 3.6+ or Gradle 6.0+
- **Selenium**: Version 4.x (tested with 4.23.0)
- **ChromeDriver**: Compatible with CDP v127 (adjustable for other versions)

## Quick Start

### 1. Build the Agent

```bash
# Clone the repository
git clone <repository-url>
cd selenium-cdp-agent

# Build with Maven
mvn clean package -DskipTests

# The agent JAR will be created at:
# target/selenium-cdp-agent-0.1.0.jar
```

### 2. Install to Local Repository

To use the agent as a dependency in other Maven projects, install it to your local Maven repository:

```bash
# Install the agent to local Maven repository
mvn install

# This allows other projects to reference it as a dependency:
# <groupId>com.yxrkt512</groupId>
# <artifactId>selenium-cdp-agent</artifactId>
# <version>0.1.0</version>
```

### 3. Basic Usage

Add the agent to your JVM arguments:

```bash
-javaagent:/path/to/selenium-cdp-agent-0.1.0.jar=out=./seltrace.ndjson,cdp=true,stack=false
```

## Configuration Options

The agent accepts the following configuration parameters:

| Parameter | Description | Default | Example |
|-----------|-------------|---------|----------|
| `out` | Output file path for trace data | `./seltrace.ndjson` | `out=./traces/test-trace.ndjson` |
| `cdp` | Enable Chrome DevTools Protocol | `true` | `cdp=false` |
| `stack` | Include stack traces in events | `false` | `stack=true` |

**Example Configuration:**
```bash
-javaagent:selenium-cdp-agent-0.1.0.jar=out=./custom-trace.ndjson,cdp=true,stack=true
```

## Framework Integration

### TestNG Integration

#### Maven Configuration

Add to your `pom.xml`:

```xml
<dependencies>
    <!-- Your existing dependencies -->
    <dependency>
        <groupId>com.yxrkt512</groupId>
        <artifactId>selenium-cdp-agent</artifactId>
        <version>0.1.0</version>
        <scope>test</scope>
    </dependency>
    <dependency>
        <groupId>org.testng</groupId>
        <artifactId>testng</artifactId>
        <version>7.8.0</version>
        <scope>test</scope>
    </dependency>
    <dependency>
        <groupId>org.seleniumhq.selenium</groupId>
        <artifactId>selenium-java</artifactId>
        <version>4.23.0</version>
    </dependency>
</dependencies>

<build>
    <plugins>
        <plugin>
            <groupId>org.apache.maven.plugins</groupId>
            <artifactId>maven-surefire-plugin</artifactId>
            <version>3.0.0</version>
            <configuration>
                <argLine>-javaagent:${project.basedir}/lib/selenium-cdp-agent-0.1.0.jar=out=./testng-trace.ndjson,cdp=true,stack=false</argLine>
                <suiteXmlFiles>
                    <suiteXmlFile>src/test/resources/testng.xml</suiteXmlFile>
                </suiteXmlFiles>
            </configuration>
        </plugin>
    </plugins>
</build>
```

#### TestNG Test Example

```java
import org.testng.annotations.*;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.chrome.ChromeDriver;
import org.openqa.selenium.chrome.ChromeOptions;

public class TestNGSeleniumTest {
    private WebDriver driver;
    
    @BeforeMethod
    public void setUp() {
        ChromeOptions options = new ChromeOptions();
        options.addArguments("--headless");
        driver = new ChromeDriver(options);
    }
    
    @Test
    public void testWebsiteNavigation() {
        driver.get("https://example.com");
        // Your test logic here
        // All Selenium calls are automatically traced
    }
    
    @AfterMethod
    public void tearDown() {
        if (driver != null) {
            driver.quit();
        }
    }
}
```

### JUnit 5 Integration

#### Maven Configuration

```xml
<dependencies>
    <dependency>
        <groupId>com.yxrkt512</groupId>
        <artifactId>selenium-cdp-agent</artifactId>
        <version>0.1.0</version>
        <scope>test</scope>
    </dependency>
    <dependency>
        <groupId>org.junit.jupiter</groupId>
        <artifactId>junit-jupiter-api</artifactId>
        <version>5.10.0</version>
        <scope>test</scope>
    </dependency>
    <dependency>
        <groupId>org.junit.jupiter</groupId>
        <artifactId>junit-jupiter-engine</artifactId>
        <version>5.10.0</version>
        <scope>test</scope>
    </dependency>
</dependencies>

<build>
    <plugins>
        <plugin>
            <groupId>org.apache.maven.plugins</groupId>
            <artifactId>maven-surefire-plugin</artifactId>
            <version>3.0.0</version>
            <configuration>
                <argLine>-javaagent:${project.basedir}/lib/selenium-cdp-agent-0.1.0.jar=out=./junit-trace.ndjson,cdp=true</argLine>
            </configuration>
        </plugin>
    </plugins>
</build>
```

### Cucumber Integration

#### Maven Configuration

```xml
<dependencies>
    <dependency>
        <groupId>com.yxrkt512</groupId>
        <artifactId>selenium-cdp-agent</artifactId>
        <version>0.1.0</version>
        <scope>test</scope>
    </dependency>
    <dependency>
        <groupId>io.cucumber</groupId>
        <artifactId>cucumber-java</artifactId>
        <version>7.14.0</version>
        <scope>test</scope>
    </dependency>
    <dependency>
        <groupId>io.cucumber</groupId>
        <artifactId>cucumber-junit-platform-engine</artifactId>
        <version>7.14.0</version>
        <scope>test</scope>
    </dependency>
</dependencies>

<build>
    <plugins>
        <plugin>
            <groupId>org.apache.maven.plugins</groupId>
            <artifactId>maven-surefire-plugin</artifactId>
            <version>3.0.0</version>
            <configuration>
                <argLine>-javaagent:${project.basedir}/lib/selenium-cdp-agent-0.1.0.jar=out=./cucumber-trace.ndjson,cdp=true</argLine>
                <includes>
                    <include>**/CucumberTestRunner.java</include>
                </includes>
            </configuration>
        </plugin>
    </plugins>
</build>
```

#### Cucumber Step Definitions

```java
import io.cucumber.java.After;
import io.cucumber.java.Before;
import io.cucumber.java.en.*;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.chrome.ChromeDriver;

public class StepDefinitions {
    private WebDriver driver;
    
    @Before
    public void setUp() {
        driver = new ChromeDriver();
    }
    
    @Given("I navigate to {string}")
    public void i_navigate_to(String url) {
        driver.get(url);
        // Automatically traced by the agent
    }
    
    @After
    public void tearDown() {
        if (driver != null) {
            driver.quit();
        }
    }
}
```

## Build Tool Configurations

### Maven

#### Complete pom.xml Example

```xml
<?xml version="1.0" encoding="UTF-8"?>
<project xmlns="http://maven.apache.org/POM/4.0.0"
         xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
         xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 
         http://maven.apache.org/xsd/maven-4.0.0.xsd">
    <modelVersion>4.0.0</modelVersion>
    
    <groupId>com.example</groupId>
    <artifactId>selenium-tests</artifactId>
    <version>1.0.0</version>
    <packaging>jar</packaging>
    
    <properties>
        <maven.compiler.source>11</maven.compiler.source>
        <maven.compiler.target>11</maven.compiler.target>
        <selenium.version>4.23.0</selenium.version>
        <testng.version>7.8.0</testng.version>
        <agent.path>${project.basedir}/lib/selenium-cdp-agent-0.1.0.jar</agent.path>
    </properties>
    
    <dependencies>
        <dependency>
            <groupId>com.yxrkt512</groupId>
            <artifactId>selenium-cdp-agent</artifactId>
            <version>0.1.0</version>
            <scope>test</scope>
        </dependency>
        <dependency>
            <groupId>org.seleniumhq.selenium</groupId>
            <artifactId>selenium-java</artifactId>
            <version>${selenium.version}</version>
        </dependency>
        <dependency>
            <groupId>org.testng</groupId>
            <artifactId>testng</artifactId>
            <version>${testng.version}</version>
            <scope>test</scope>
        </dependency>
    </dependencies>
    
    <build>
        <plugins>
            <plugin>
                <groupId>org.apache.maven.plugins</groupId>
                <artifactId>maven-compiler-plugin</artifactId>
                <version>3.11.0</version>
            </plugin>
            
            <plugin>
                <groupId>org.apache.maven.plugins</groupId>
                <artifactId>maven-surefire-plugin</artifactId>
                <version>3.0.0</version>
                <configuration>
                    <argLine>-javaagent:${agent.path}=out=./seltrace.ndjson,cdp=true,stack=false</argLine>
                    <parallel>methods</parallel>
                    <threadCount>3</threadCount>
                </configuration>
            </plugin>
            
            <!-- Copy agent JAR to lib directory -->
            <plugin>
                <groupId>org.apache.maven.plugins</groupId>
                <artifactId>maven-dependency-plugin</artifactId>
                <version>3.2.0</version>
                <executions>
                    <execution>
                        <id>copy-agent</id>
                        <phase>process-test-resources</phase>
                        <goals>
                            <goal>copy-dependencies</goal>
                        </goals>
                        <configuration>
                            <includeArtifactIds>selenium-cdp-agent</includeArtifactIds>
                            <outputDirectory>${project.basedir}/lib</outputDirectory>
                        </configuration>
                    </execution>
                </executions>
            </plugin>
        </plugins>
    </build>
</project>
```

### Gradle

#### Complete build.gradle Example

```gradle
plugins {
    id 'java'
    id 'java-library'
}

group = 'com.example'
version = '1.0.0'

java {
    sourceCompatibility = JavaVersion.VERSION_11
    targetCompatibility = JavaVersion.VERSION_11
}

repositories {
    mavenCentral()
    mavenLocal() // For local selenium-cdp-agent
}

dependencies {
    testImplementation 'com.yxrkt:selenium-cdp-agent:0.1.0'
    testImplementation 'org.seleniumhq.selenium:selenium-java:4.23.0'
    testImplementation 'org.testng:testng:7.8.0'
    testImplementation 'org.junit.jupiter:junit-jupiter-api:5.10.0'
    testRuntimeOnly 'org.junit.jupiter:junit-jupiter-engine:5.10.0'
}

// Task to copy agent JAR
task copyAgent(type: Copy) {
    from configurations.testRuntimeClasspath.filter { it.name.contains('selenium-cdp-agent') }
    into 'lib'
    rename { 'selenium-cdp-agent-0.1.0.jar' }
}

// Configure test task
test {
    dependsOn copyAgent
    useTestNG() // or useJUnitPlatform() for JUnit 5
    
    jvmArgs = [
        "-javaagent:${projectDir}/lib/selenium-cdp-agent-0.1.0.jar=out=./gradle-trace.ndjson,cdp=true,stack=false"
    ]
    
    // Parallel execution
    maxParallelForks = 3
    
    // Test output
    testLogging {
        events "passed", "skipped", "failed"
        exceptionFormat "full"
    }
}

// Task for running specific test suites
task testSmoke(type: Test) {
    dependsOn copyAgent
    useTestNG()
    
    jvmArgs = [
        "-javaagent:${projectDir}/lib/selenium-cdp-agent-0.1.0.jar=out=./smoke-trace.ndjson,cdp=true"
    ]
    
    include '**/SmokeTest*'
}
```

#### Gradle with Kotlin DSL (build.gradle.kts)

```kotlin
plugins {
    java
    `java-library`
}

group = "com.example"
version = "1.0.0"

java {
    sourceCompatibility = JavaVersion.VERSION_11
    targetCompatibility = JavaVersion.VERSION_11
}

repositories {
    mavenCentral()
    mavenLocal()
}

dependencies {
    testImplementation("com.yxrkt:selenium-cdp-agent:0.1.0")
    testImplementation("org.seleniumhq.selenium:selenium-java:4.23.0")
    testImplementation("org.testng:testng:7.8.0")
}

val copyAgent by tasks.registering(Copy::class) {
    from(configurations.testRuntimeClasspath.get().filter { it.name.contains("selenium-cdp-agent") })
    into("lib")
    rename { "selenium-cdp-agent-0.1.0.jar" }
}

tasks.test {
    dependsOn(copyAgent)
    useTestNG()
    
    jvmArgs = listOf(
        "-javaagent:${projectDir}/lib/selenium-cdp-agent-0.1.0.jar=out=./kotlin-trace.ndjson,cdp=true,stack=false"
    )
    
    maxParallelForks = 3
}
```

## Advanced Configuration

### Environment-Specific Configurations

#### Development Environment
```bash
-javaagent:selenium-cdp-agent-0.1.0.jar=out=./dev-trace.ndjson,cdp=true,stack=true
```

#### CI/CD Environment
```bash
-javaagent:selenium-cdp-agent-0.1.0.jar=out=./ci-trace.ndjson,cdp=false,stack=false
```

#### Production Monitoring
```bash
-javaagent:selenium-cdp-agent-0.1.0.jar=out=/var/log/selenium/prod-trace.ndjson,cdp=true,stack=false
```

### IDE Integration

#### IntelliJ IDEA
1. Go to **Run/Debug Configurations**
2. Select your test configuration
3. In **VM Options**, add:
   ```
   -javaagent:lib/selenium-cdp-agent-0.1.0.jar=out=./ide-trace.ndjson,cdp=true
   ```

#### Eclipse
1. Right-click your test class → **Run As** → **Run Configurations**
2. Go to **Arguments** tab
3. In **VM Arguments**, add:
   ```
   -javaagent:lib/selenium-cdp-agent-0.1.0.jar=out=./eclipse-trace.ndjson,cdp=true
   ```

### Docker Integration

```dockerfile
FROM openjdk:11-jre-slim

# Copy agent and test JAR
COPY lib/selenium-cdp-agent-0.1.0.jar /app/agent.jar
COPY target/test-classes /app/tests

# Install Chrome for testing
RUN apt-get update && apt-get install -y \
    wget \
    gnupg \
    && wget -q -O - https://dl.google.com/linux/linux_signing_key.pub | apt-key add - \
    && echo "deb [arch=amd64] http://dl.google.com/linux/chrome/deb/ stable main" > /etc/apt/sources.list.d/google-chrome.list \
    && apt-get update \
    && apt-get install -y google-chrome-stable

# Run tests with agent
CMD ["java", "-javaagent:/app/agent.jar=out=/app/traces/docker-trace.ndjson,cdp=true", "-cp", "/app/tests", "org.testng.TestNG", "testng.xml"]
```

## Trace Output Format

The agent generates NDJSON files with detailed trace information:

```json
{"timestamp":"2024-01-15T10:30:45.123Z","type":"navigation","method":"get","url":"https://example.com","duration":1250}
{"timestamp":"2024-01-15T10:30:46.456Z","type":"element_interaction","method":"click","selector":"#submit-button","duration":45}
{"timestamp":"2024-01-15T10:30:46.789Z","type":"cdp_event","domain":"Page","event":"loadEventFired","params":{}}
```

## Troubleshooting

### Common Issues

1. **Agent Not Loading**
   - Verify the agent JAR path is correct
   - Ensure JDK 11+ is being used
   - Check that the JAR file exists and is readable

2. **No Trace Output**
   - Verify the output directory exists and is writable
   - Check agent configuration parameters
   - Ensure Selenium tests are actually running

3. **CDP Version Mismatch**
   - Update the CDP domain versions in the source code
   - Rebuild the agent for your ChromeDriver version
   - Check ChromeDriver compatibility

4. **Performance Impact**
   - Disable stack traces in production: `stack=false`
   - Reduce CDP events if not needed: `cdp=false`
   - Use appropriate log levels

### Debug Mode

Enable verbose logging:
```bash
-javaagent:selenium-cdp-agent-0.1.0.jar=out=./debug-trace.ndjson,cdp=true,stack=true -Dselenium.cdp.agent.debug=true
```

## Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Add tests for new functionality
5. Submit a pull request

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Captured Interactions and Events

The following table details which Selenium WebDriver interactions and Chrome DevTools Protocol (CDP) events are captured by the agent:

### Selenium WebDriver Interactions

| Category | Method/Action | Interceptor Class | Data Captured |
|----------|---------------|-------------------|---------------|
| **Navigation** | `WebDriver.get(url)` | `WebDriverGet` | URL, page title, window handle, timing |
| | `Navigation.to(url)` | `WebDriverNavigation` | URL, navigation type, timing |
| | `Navigation.back()` | `WebDriverNavigation` | Current URL, timing |
| | `Navigation.forward()` | `WebDriverNavigation` | Current URL, timing |
| | `Navigation.refresh()` | `WebDriverNavigation` | Current URL, timing |
| **Element Location** | `findElement(locator)` | `Finders` | Locator type/value, result count, timing |
| | `findElements(locator)` | `Finders` | Locator type/value, result count, timing |
| **Element Interaction** | `WebElement.click()` | `WebElementNoArgs` | Element locator, timing |
| | `WebElement.sendKeys(text)` | `WebElementSendKeys` | Element locator, input text (redacted for passwords), timing |
| | `WebElement.clear()` | `WebElementNoArgs` | Element locator, timing |
| | `WebElement.submit()` | `WebElementNoArgs` | Element locator, timing |
| **Element State** | `WebElement.getText()` | `WebElementGetText` | Element locator, returned text, timing |
| | `WebElement.isDisplayed()` | `WebElementIsDisplayed` | Element locator, visibility state, timing |
| | `WebElement.isEnabled()` | `WebElementIsEnabled` | Element locator, enabled state, timing |
| | `WebElement.isSelected()` | `WebElementIsSelected` | Element locator, selected state, timing |
| | `WebElement.getAttribute(name)` | `WebElementGetAttribute` | Element locator, attribute name/value, timing |
| | `WebElement.getTagName()` | `WebElementNoArgs` | Element locator, tag name, timing |
| | `WebElement.getCssValue(property)` | `WebElementNoArgs` | Element locator, CSS property/value, timing |
| **Window Management** | `TargetLocator.window(handle)` | `WebDriverWindow` | Window handle, timing |
| | `TargetLocator.frame(locator)` | `WebDriverWindow` | Frame locator, timing |
| | `TargetLocator.defaultContent()` | `WebDriverWindow` | Context switch, timing |
| | `TargetLocator.activeElement()` | `WebDriverWindow` | Active element, timing |
| **Waits & Timing** | `WebDriverWait.until(condition)` | `WebDriverWaitUntil` | Condition, timeout, polling interval, timing |
| | `Timeouts.implicitlyWait(duration)` | `ImplicitWait` | Timeout duration, timing |
| | `Thread.sleep(millis)` | `ThreadSleep` | Sleep duration, timing |
| **JavaScript** | `JavascriptExecutor.executeScript()` | `JsExec` | Script content, timing |
| **Actions API** | `Actions.perform()` | `ActionsPerform` | Action chain, timing |
| **Enhanced Tracking** | All WebElement methods | `WebElementEnhanced` | Error categorization, retry statistics |
| | Critical operations | `WebElementWithRetry` | Retry recommendations, operation IDs |

### Chrome DevTools Protocol (CDP) Events

| Category | CDP Event | Handler | Data Captured |
|----------|-----------|---------|---------------|
| **Network** | `Network.requestWillBeSent` | `ChromeDriverHook` | Request URL, method, headers, timing |
| | `Network.responseReceived` | `ChromeDriverHook` | Response status, headers, timing |
| **Page Events** | `Page.loadEventFired` | `ChromeDriverHook` | Page load completion, timing |
| **Console & Logs** | `Runtime.consoleAPICalled` | `ChromeDriverHook` | Console messages, log levels |
| | `Log.entryAdded` | `ChromeDriverHook` | Browser logs, error messages |

### Error Handling & Diagnostics

| Feature | Description | Data Captured |
|---------|-------------|---------------|
| **Error Categorization** | Automatic classification of Selenium exceptions | Error type, category, retry recommendations |
| **Stale Element Detection** | Detection of stale element references | Element state, retry suggestions |
| **Timeout Analysis** | Analysis of wait timeouts and their causes | Wait conditions, timeout durations |
| **Performance Metrics** | Timing data for all operations | Start/end times, duration, performance insights |

### Output Format

All captured data is written to `seltrace.ndjson` in newline-delimited JSON format:

```json
{"timestamp":1699123456789,"type":"selenium","action":"click","element":"#submit-btn","duration":150}
{"timestamp":1699123456890,"type":"cdp","domain":"Network","method":"requestWillBeSent","url":"https://example.com/api"}
```

## Support

For issues and questions:
- Create an issue in the GitHub repository
- Check the troubleshooting section
- Review the sample tests for usage examples

---

**Note**: The CDP domain versions in the source use `v127`. Adjust if your Selenium/ChromeDriver uses different versions.