Feature: API hello with credentials and retry semantics

Background:
  * url 'http://localhost:8085'

  * def username = karate.properties['username'] || 'test'
  * def password = karate.properties['password'] || 'test'

  * print 'USERNAME:', username
  * print 'PASSWORD:', password

  * def retryableStatuses = [302, 429]
  * def successStatus = 200
  # API Gateway often masks the backend 50x with 504
  * def fatalStatuses = [503, 504, 405]
  # TODO: handle 422 unprocessable Content and 400 bad request to reveal possible business  
  # built-in retry + fixed backoff
  * configure retry = { count: 6, interval: 1000 }

Scenario: Authenticate and call protected API with retry

  Given path '/auth/token'
  And request { username: '#(username)', password: '#(password)' }
  When method post
  Then status 200

  * def token = response.access_token
  * print 'TOKEN:', token

  Given path '/api/hello'
  And header Authorization = 'Bearer ' + token

  # Karate has retry and exponential backoff built-in 

  # Karate built-in retry is expression-based
  # keep retrying while status is 302 or 429, quit if 
  And retry until responseStatus == successStatus || fatalStatuses.contains(responseStatus)
  When method get

  * if (responseStatus == 429) karate.log('WARN: server is throttling requests')

  # NOTE: a plain karate.log() is too soft
  # unlike other logging frameworks Karate does not have karate.error() 

  if (fatalStatuses.contains(responseStatus))
    karate.fail('SEVERE: status=' + responseStatus)
 
  * match responseStatus == successStatus

  * print 'RESPONSE:', response
  * match response contains username

