Feature: Test Event

 Background:
  * url 'http://localhost:8080/api'
  * def tokenFeature = callonce read('../token/token.feature')
  * def access_token = tokenFeature.access_token

  Scenario: Get list of events for current user

  Given path 'events'
  When method get
  Then status 401
  * print response.errorMessage
  # no step-definition method match found for: match response.errorMessage '#null'
  * match response == { status: '#number', message: '##string', timestamp: '#number', error: 'Unauthorized', path: '/api/events'}
  * assert response.message == ''
  Given path 'events'
  And header Authorization = 'Bearer ' + access_token
  When method get
  Then status 200

  * header Authorization = 'Bearer ' + access_token
  Given path 'events'
  When method get
  Then status 200

  Scenario: Filter Events
  * def str = 'Fitness'
  Given path 'events'
  And header Authorization = 'Bearer ' + access_token
  And params { eventName: '#(str)' }
  When method get
  # when aserisk is forgotten the test error is:
  # Runtime mismatched input 'm' expecting <EOF>
  * match each response.data[*].name == '#regex .*'+ str +'.*'
  * match response.data[*].id contains '#number'
  * match response.data[*].id contains 5
  # fragile. commented
  # * assert response.data[0] contains {'id': '#number', 'name': '#string', 'description': '##string', 'maxCapacity': '#ignore', 'date': '#ignore',  'organizer': '#ignore',  'location': '#ignore',  'startTime': '#ignore',  'numberOfHours': '#ignore',  'currentCapacity': '#ignore',  'userId': '#ignore' }
  # org.graalvm.polyglot.PolyglotException: SyntaxError: Unnamed:1:17 Expected ; but found contains
  * match response.data[0].id == 5
  Then status 200

  Scenario: Try Delete
  Given path 'events', 1
  And header Authorization = 'Bearer ' + access_token
  When method delete
  Then status 403
  * assert response.errorMessage == 'User not allowed to delete this event'
