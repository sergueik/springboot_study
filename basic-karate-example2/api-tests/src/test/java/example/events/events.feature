Feature: Test Event

 Background:
  * url 'http://localhost:8080/api'
  # NOTE: "read" argument path is relative
  # org.graalvm.polyglot.PolyglotException: java.io.FileNotFoundException: 
  # ...\target\test-classes\example\events\token.feature (The system cannot find the file specified)
  * def tokenFeature = callonce read('../token/token.feature')
  * def access_token = tokenFeature.access_token
  
  Scenario: Get list of events for current user
  
  Given path 'events'
  When method get
  Then status 401
  
  Given path 'events'
  And header Authorization = 'Bearer ' + access_token
  When method get
  Then status 200
  
  * header Authorization = 'Bearer ' + access_token
  Given path 'events'
  When method get
  Then status 200 