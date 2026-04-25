Feature: OperationController full lifecycle test (POST → PUT → GET → DELETE)
Background:
  * url 'http://localhost:8085'
  * def credentials = { username: 'test', password: 'test' }

Scenario: full lifecycle of an operation resource

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

  # 1. CREATE
  Given path 'operation'
  And header Authorization = 'Bearer ' + token
  And request { what: 'hello' }
  When method post
  Then status 201
  And match $.id == '#string'

  * def id = response.id
  * print 'CREATED ID:', id

  # 2. UPDATE
  Given path 'operation', id
  And request { what: 'updated' }
  And header Authorization = 'Bearer ' + token
  When method put
  Then status 200

  # 3. GET (verify update)
  Given path 'operation', id
  And header Authorization = 'Bearer ' + token
  When method get
  Then status 200
  And match $.what == 'updated'

  # 4. DELETE
  Given path 'operation', id
  And header Authorization = 'Bearer ' + token
  When method delete
  Then status 204

  # 5. VERIFY DELETION
  Given path 'operation', id
  And header Authorization = 'Bearer ' + token
  When method get
  Then status 404
