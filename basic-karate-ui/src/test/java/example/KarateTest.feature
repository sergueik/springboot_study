Feature: Karate browser automation 

  Background:
    * configure driver = { type: 'chrome' }

  Scenario: google search, land on the YouTube, and search for knoldus.

    Given driver 'https://google.com'
    # NOTE: vulnerable to presence of other browsers on visible desktop:
    # failed to get reply for: [id: 6, method: Page.navigate, params: {url=https://google.com}]
    And driver.maximize ()
    # NOTE: not the: 'input[name="btnK"]'
    # javascript evaluation failed twice:
    # var e = document.querySelector("input[name="q"]")
    And input("input[name='q']", 'Youtube')
    And click("input[name='btnK']")
    And waitFor('{h3}YouTube');
    When click('{h3}YouTube')
    Then waitForUrl('https://www.youtube.com/')
    And click('input[id=search]')
    And input('input[id=search]', 'knoldus')
    * delay(1000)
    And waitFor('button[id=search-icon-legacy]');
    And click('button[id=search-icon-legacy]')
    * delay(5000)
    And match driver.url == 'https://www.youtube.com/results?search_query=knoldus'

