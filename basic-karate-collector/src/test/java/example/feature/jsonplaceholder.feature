Feature: Tests for the json placeholder page

Background: Get the home page
    Given url 'https://jsonplaceholder.typicode.com/'

Scenario: Get todo
    Given path 'todos/1'
    When method Get
    Then status 200
    And match response.userId == 1
    And match response.title == '#string'
    
Scenario: List todos
    Given path 'todos'
    When method Get
    Then status 200
    And match response[0].title == '#string'
    And match response[0].userId == 1