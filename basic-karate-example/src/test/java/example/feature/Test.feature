Feature: Cookie decode test

Scenario: Decode cookie and verify value

    Given url 'http://localhost:8085/cookie'
    When method get
    Then status 200

    # get cookie value
    * def encoded = responseCookies.question.value

    # base64 decode using Java
    * def decoded = new java.lang.String(java.util.Base64.getDecoder().decode(encoded))

    # extract number from "value=42"
    * def value = decoded.match(/value=(\d+)/)[1]

    # validation
    * match value == '42'
