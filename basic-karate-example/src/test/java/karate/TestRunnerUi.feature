Feature: Karate browser automation 

  Background:
    * configure driver = { type: 'chrome' }

  Scenario: google search, land on the YouTube, and search for knoldus.

    Given driver 'https://google.com'
	Then waitForUrl('https://www.youtube.com/')
    And input('input[name=q]', 'Youtube')
    And waitFor("input[name=btnK]");
    And click('input[name=btnK]')
    And waitFor("h3[class*='DKV0Md']");
    When click("h3[class*='DKV0Md']")
    Then waitForUrl('https://www.youtube.com/')
    And click('input[id=search]')
    And input('input[id=search]', 'knoldus')
    And waitFor('button[id=search-icon-legacy]');
    And click('button[id=search-icon-legacy]')
    And match driver.url == 'https://www.youtube.com/results?search_query=knoldus'

