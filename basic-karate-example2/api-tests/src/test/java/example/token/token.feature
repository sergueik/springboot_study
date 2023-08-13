Feature: Obtain and keep authentication token
# the called feature will be displayed in "karate-summary.html" 
# and in its own html, in our case,"example.token.token.html" but not in the callee report 

   Background:
   * url 'http://localhost:8080/api'
   # engage inline JS and Java code to get credentials from JWT
   # JSON Web Tokens consist of three parts separated by dots (.), which are:
   # * Header
   # * Payload
   # * Signature
   # therefore the raw JWT looks like the following.
   # xxxxx.yyyyy.zzzzz
   * def parseJwtPayload =
   """
   function(token) {
      var encodedPayload = token.split('.')[1];
      var data = encodedPayload.replace(/-/g, '+').replace(/_/g, '/');
      var decoded = Java.type('java.util.Base64').getDecoder().decode(data);
      return JSON.parse(new java.lang.String(decoded, 'UTF-8'));
   }
   """
  
  
  Scenario: Get access token
  Given path 'token'
  And request { username: 'johns@pocisoft.com', password: 'password' }
  When method post
  Then status 200
  * def access_token = response.token
  * print "Access Token: " + access_token
  * def tokenDetails = parseJwtPayload(access_token)
  * print 'Parsed JWT Token:', tokenDetails
