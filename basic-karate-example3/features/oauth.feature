Feature: OAuth2 JWT token generation (stub server)

Background:
  * url 'http://localhost:8085'
  * def credentials = { username: 'test', password: 'test' }

Scenario: Obtain access token

  Given path 'auth/token'
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
  * def len = parts.length
  * match len == 3

  * def decode = function(obj){ return new java.lang.String(java.util.Base64.getUrlDecoder().decode(obj)) }

  * def headerpart = karate.fromString(decode(parts[0]))
  * print headerpart
  * match headerpart contains { alg: 'HS256' }

  * def payload = karate.fromString(decode(parts[1]))
  * print payload
  * match payload.sub == 'test'
  * match payload.role == 'USER'
  * match payload.iss == 'http://localhost:8085'

  # timestamps
  * match payload.iat == '#number'
  * match payload.exp == '#number'
  * assert payload.exp > payload.iat

  # 2. Call protected endpoint
  Given path '/api/hello'
  And header Authorization = 'Bearer ' + token
  When method get

  # 3. Verify HTTP response
  Then status 200

  # 4. Parse and validate response body
  * def responseText = response
  * print 'RESPONSE:', responseText

  # Example assertion
  * match responseText contains 'hello test' 
