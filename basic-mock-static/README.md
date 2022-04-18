### Info

  This directory  contains  a practice code based on the [article](https://www.baeldung.com/mockito-mock-static-methods)

### Note

The project is barely working with __Junit 5__: the most annoying error is:
```text
org.mockito.exceptions.base.MockitoException:

The static mock created at 
-> at example.AppMethod2Test.test1(AppMethod2Test.java:37)
is already resolved and cannot longer be used
```
the error shows even when there is just one test class /test method (everything else temporarily removed). After switching back to __Junit 4__  the tests magically begin to work

### See Also

  * [Detroit vs. London system](https://habr.com/ru/company/jugru/blog/571126/) (in Russian)
  *  [forum question](https://qna.habr.com/q/1142084) (in Russian)
  * [hint](https://stackoverflow.com/questions/65845312/how-to-solve-to-create-a-new-mock-the-existing-mock-registration-must-be-dereg) for solving `org.mockito.exceptions.base.MockitoException`: *For example.Utils, static mocking is already registered in the current thread.To create a new mock, the existing static mock registration must be deregistered*

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
