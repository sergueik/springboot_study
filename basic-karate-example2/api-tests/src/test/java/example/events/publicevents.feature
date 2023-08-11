Feature: Obtain Public Events

 Background:
  * url 'http://localhost:8080/api'
  
  Scenario: List Public Events
  Given path 'publicevents'
  When method get
  Then status 200

  # TODO: add expectation on the response payload