Feature: API hello with credentials and retry semantics

Background:
  * url 'http://localhost:8085'

  * def username = karate.properties['username'] || 'test'
  * def password = karate.properties['password'] || 'test'

  * print 'USERNAME:', username
  * print 'PASSWORD:', password

  # transient statuses worth retrying
  * def retryableStatuses = [202, 302, 304, 429]

  * def successStatus = 200

  # infra / gateway / backend outage
  * def fatalStatuses = [503, 504]

  # request / business contract issues
  * def seriousClientStatuses = [400, 422, 404, 405]

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

  # retry until success, infra failure, or business failure
  And retry until responseStatus == successStatus
    || fatalStatuses.contains(responseStatus)
    || seriousClientStatuses.contains(responseStatus)
  When method get

  * if (responseStatus == 429) karate.log('WARN: throttling in progress')

  * if (fatalStatuses.contains(responseStatus))
    karate.fail('SEVERE infrastructure failure, status=' + responseStatus)

  * if (seriousClientStatuses.contains(responseStatus))
    karate.fail('BUSINESS/API CONTRACT failure, status=' + responseStatus + ', body=' + response)

  * match responseStatus == successStatus
  * print 'RESPONSE:', response
  * match response contains username
