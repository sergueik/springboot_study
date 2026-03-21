Feature: OAuth2 JWT token generation (stub server)

Background:
  * url 'http://localhost:8085'
  * def credentials = { username: 'test', password: 'test' }

Scenario: Obtain access token

  Given path 'token'
  And request credentials
  When method post
  Then status 200

  And match response.access_token != null
  And match response.token_type == 'Bearer'
  And match response.expires_in == 3600

  # extract token
  * def token = response.access_token
  * print token

  # split JWT correctly
  * def parts = token.split('\\.')
  * match parts.length == 3

  # decode header + payload
  # header validation
  * def headerpart = karate.fromBase64(parts[0])
  * print headerpart
  * match headerpart contains { alg: 'HS256' }

  # payload validation
  * def payload = karate.fromBase64(parts[1])
  * print payload
  * match payload.sub == 'test'
  * match payload.role == 'USER'
  * match payload.iss == 'http://localhost:8085'

  # timestamps
  * match payload.iat == '#number'
  * match payload.exp == '#number'
  * assert payload.exp > payload.iat

