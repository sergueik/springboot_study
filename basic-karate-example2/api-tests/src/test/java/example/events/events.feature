Feature: Test Event

 Background:
  * url 'http://localhost:8080/api'
  * def tokenFeature = callonce read('../token/token.feature')
  * def access_token = tokenFeature.access_token
   * def tokenDetails = tokenFeature.tokenDetails

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

  Scenario Outline: Filter Events
  * def str = <str>
  * def id = <id>
  Given path 'events'
  And header Authorization = 'Bearer ' + access_token
  And params { eventName: '#(str)' }
  When method get
  * match each response.data[*].name == '#regex' + ' ' + '.*'+ str +'.*'
  * match response.data[*].id contains '#number'
  * match response.data[*].id contains id
  Then status 200
  # NOTE:  does not handle space in str
  Examples:
	| id | str        |
	| 5  | 'Fitness'  |
	| 30  | 'Business Conference'  |
	| 30  | 'Conference'  |

  Scenario: Try Delete
  Given path 'events', 1
  And header Authorization = 'Bearer ' + access_token
  When method delete
  Then status 403
  * assert response.errorMessage == 'User not allowed to delete this event'

  
  Scenario Outline: Create Event  - <name>
  Given path 'events' 
  And header Authorization = 'Bearer ' + access_token
  And request 
      """
      {
      "date":<date>,
       "description":<description>,
      "location":<location>,
      "maxCapacity":<maxCapacity>,
      "name":<name>,
      "numberOfHours":<numberOfHours>,
      "organizer":<organizer>,
      "startTime":<startTime>
      }
      """
  When method post
  Then status 201
  Then match response.data == 
    """
      {
    'userId': '#(tokenDetails.uid)',
    'date': '#ignore',
    'description': <description>,
    'location': <location>,
    'maxCapacity': <maxCapacity>,
    'name': <name>,
    'numberOfHours': <numberOfHours>,
    'organizer': <organizer>,
    'startTime': <startTime>,
    'id': '#present',
    'currentCapacity':0
      }
     """
    Examples:
     | read('newevents.json') |  