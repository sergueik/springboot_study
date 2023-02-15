Feature: Example Browser Config Loop Feature
# origin: https://github.com/mbzebra/karategradletest/blob/main/src/test/java/ui/Test.feature

  Scenario Outline: Go to Yahoo, perform search for news
    * configure driver = <config>
    Given driver 'https://google.com'
    Then waitFor("input[name='q']")

    Examples:
    | config |
    | {type: 'chromedriver', executable: '/home/sergueik/Downloads/chromedriver' , webDriverSession: { desiredCapabilities: { browserName: 'chrome' , "goog:chromeOptions": { args: [ '--headless', 'window-size=1280,720' ] }  } } } |
    | {type: 'geckodriver', executable: '/home/sergueik/Downloads/geckodriver', webDriverSession: { "capabilities": { "alwaysMatch": { "moz:firefoxOptions": { args: ["-headless"] } } } } }|

