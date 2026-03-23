Feature: Bank Test

  As a bank customer self service

  Background:
    * configure driver = {type: 'chromedriver', executable: 'C:/Users/Kouzm/Downloads/chromedriver.exe' , webDriverSession: { desiredCapabilities: { browserName: 'chrome' , "goog:chromeOptions": { args: ['window-size=1280,720' ] }  } } }

  Scenario:  bank customer find name, log in, then log out

    Given driver 'http://localhost:8080/application#/login'
    And driver.maximize ()
    And click("//button[normalize-space()='Customer Login']")
    And waitFor('{label}Your Name :');
    And waitFor("//label[contains(text(),'Your Name :')]")
    And waitFor('#userSelect option')
    And select('#userSelect', 'Harry Potter')
    * delay(10000)
    And click("//button[normalize-space()='Login']")
    Then waitFor('{span}Harry Potter')
    * delay(15000)
    And click("//button[normalize-space()='Logout']")
    * delay(5000)
    And match driver.url == 'http://localhost:8080/application#/customer'

