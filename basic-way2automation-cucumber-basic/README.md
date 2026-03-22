### Info

### Usage

### Background

#### Integration of Cucumber-JVM and TestNG

Cucumber-JVM has native integration with Junit and it uses Junit as a test runner. But if you want to integrate TestNG with Cucumber-JVM
then there is no proper documentation available to do this. So I decided to create a sample project where I can demonstrate this integration along with some better reporting.

#### Basic Integration

To have a simple integration you just need to create a runner class and just extend it from `AbstractTestNGCucumberTests`.


```java
package com.cucumber.testng.examples;

import cucumber.api.Scenario;
import cucumber.api.java.After;
import cucumber.api.java.Before;

public class BaseStepDefs {
    @Before()
    public void before(Scenario scenario) {
        scenario.getId();
        System.out.println("This is before Scenario: " + scenario.getName().toString());
    }

    @After
    public void after(Scenario scenario) {
        System.out.println("This is after Scenario: " + scenario.getName().toString());
    }
}
```

If you will run this java file as a testNG class file then it will simply pick the features mentioned in the @CucumberOptions.
We can point to a folder here with multiple feature files or we can point to a specific feature file.
For Reporting, we are using the native cucumber-jvm reports which will generate a cucumber1.json file and will also generate a
pretty html file.

```
mvn clean test -Dcucumber.filter.tags="@smoke"
```
