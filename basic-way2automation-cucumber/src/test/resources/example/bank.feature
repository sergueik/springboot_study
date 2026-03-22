@tag
Feature: Bank Test

  As a bank manager I want to login and add a customer and create a customer account

  Background:
    Given I am on homepage

  @customer
  Scenario: Customer should login and logout successfully
    When I click on Customer Login tab
    And I search customer that created in first test
    And I click on Login button
    Then I verify Logout tab displayed
    And I click on Logout
    Then I verify "Your Name :" text displayed

  @manager
  Scenario Outline: Bank Manager should add customer successfully
    When I click on Bank Manager Login tab
    And I click on Add Customer Tab
    And I enter firstName "<FirstName>"
    And I enter lastName "<LastName>"
    And I enter postcode"<PostCode>"
    And I click on Add customer button
    Then I verify the message "Customer added successfully with customer id :6"
    And I click on OK button on popup
    Examples:
      | FirstName  | LastName | PostCode |
      | Prime      | Testing  | CR0 0AQ  |
      | Automation | Tester   | HA8 9BU  |
      | Automation | Engineer | SE15 6HU |

  @manager
  Scenario: Bank Manager should open account successfully
    When I click on Bank Manager Login tab
    And I click on Open Account Tab
    And I search customer that created in first test
    And I select currency "Pound"
    And I click on the process button
    Then I verify message "Account created successfully with account Number :1016"
    And I click on Ok button on popup

  @sanity @customer
  Scenario: Customer should deposit money successfully
    When I click on Customer Login tab
    And I search customer that created in first test
    And I click on Login button
    And I click on Deposit tab
    And I enter amount "100"
    And I click on Deposit button
    Then Verify the message "Deposit Successful"

  @sanity @customer
  Scenario: Customer should withdraw money successfully
    When I click on Customer Login tab
    And I search customer that created in first test
    And I click on Login button
    And I click on Deposit tab
    And I enter amount "100"
    And I click on Deposit button
    And I click on Withdrawl tab
    And I enter amount "50"
    And I click on Withdrawl button
    Then I verify the text "Transaction successful"