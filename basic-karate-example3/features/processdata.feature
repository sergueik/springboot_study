Feature: Process data API with JWT authentication

Background:
  * url 'http://localhost:8085'
  * def credentials = { username: 'test', password: 'test' }

Scenario: Obtain token and call processdata endpoint

  # 1. Get JWT token
  Given path 'auth/token'
  And request credentials
  When method post
  Then status 200

  # Basic token validation
  And match response.access_token != null
  And match response.token_type == 'Bearer'
  And match response.expires_in == 3600

  # Extract token
  * def token = response.access_token
  * print 'TOKEN:', token

  # Optional JWT structure validation
  # examine JWT parts
  * def parts = java.util.Arrays.asList(token.split("."))
  * def len = parts.length
  * match len == 3

  * def decode = function(obj){ return new java.lang.String(java.util.Base64.getUrlDecoder().decode(obj)) }

  * def headerpart = karate.fromString(decode(parts[0]))

  * def payload = karate.fromString(decode(parts[1]))
  * print 'JWT PAYLOAD:', payload

  * match payload.sub == 'test'
  * match payload.role == 'USER'

  # 2. Call protected processdata endpoint
  Given path 'api/processdata'
  And header Authorization = 'Bearer ' + token
  And request
  """
  {
    "customer": "test",
    "what": "karate validation"
  }
  """
  When method post
  Then status 200

  # 3. Validate response body
  * print 'RESPONSE:', response

  * match response.customerId == 'test'
  * match response.messageType == 'processed'
  * match response.user == 'test'

  * match response.payload.originalWhat == 'karate validation'
  * match response.payload.normalizedWhat == 'KARATE VALIDATION'
  * match response.payload.length == 17
  * match response.payload.isEmpty == false
 
