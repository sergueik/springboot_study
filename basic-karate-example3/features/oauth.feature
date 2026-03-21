Feature: OAuth2 JWT token generation (stub server)

Background:
  * url 'http://localhost:8085'
  * def credentials = { username: 'test', password: 'test' }

Scenario: Obtain access token

  Given path 'token'
  And request credentials
  When method post
  Then status 200

  # basic response validation
  And match response.access_token != null
  And match response.token_type == 'Bearer'
  And match response.expires_in == 3600

  # extract token
  * def token = response.access_token
  # examine JWT parts
  * def parts = java.util.Arrays.asList(token.split("."))
  * def getMethods = function(obj){ return Object.getOwnPropertyNames(obj) }
  # * def methods = getMethods(parts)
  # * print 'Methods of the object:', methods

  # workaround the Unknown identifier: fromBase64 error
  * def decode = function(obj){ return new java.lang.String(java.util.Base64.getUrlDecoder().decode(obj)) }

  * def headerJson = decode(parts[0])
  * print headerJson
  * def payloadJson = decode(parts[1])
  * print payloadJson
