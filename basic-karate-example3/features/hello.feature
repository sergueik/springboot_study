Feature: API hello with credentials

Background:
  * url 'http://localhost:8085'

  # System property for username (default Pete)
  * def username = karate.properties['username'] || 'test'
  * def password = karate.properties['password'] || 'test'

  * print 'USERNAME:', username
  * print 'PASSWORD:', password

Scenario: Authenticate and call protected API

  # 1. Request token with credentials
  Given path '/auth/token'
  And request { username: '#(username)', password: '#(password)' }
  When method post
  Then status 200

  # Extract token
  * def token = response.access_token
  * print 'TOKEN:', token

  # 2. Call protected API
  Given path '/api/hello'
  And header Authorization = 'Bearer ' + token
  When method get
  Then status 200

  # 3. Assert response
  * print 'RESPONSE:', response
  * match response == 'hello ' + username
