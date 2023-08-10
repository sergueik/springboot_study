Feature: Obtain and keep authentication token

 Background:
  * url 'http://localhost:8080/api'
  
  Scenario: Get access token
  Given path 'token'
  And request { username: 'johns@pocisoft.com', password: 'password' }
  When method post
  Then status 200
  * def access_token = response.token
  * print "Access Token: " + access_token

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