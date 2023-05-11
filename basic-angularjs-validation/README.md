### Info

Static Spring boot Application hosting a basic AngularJS

custom form input validation example taken from __AngularJS Form Validation__ [article](https://www.geeksforgeeks.org/angularjs-form-validation/)

![Page](https://github.com/sergueik/springboot_study/blob/master/basic-angularjs-validation/screenshots/capture-page.png)

### NOTE:

the controller rule will override the existing static page:

add `src/main/resourcses/static/page.html`
and endpoint
```java
 @ResponseBody
        @GetMapping("/page")
        public String getPage() {
                return "page is here";
        }

```
then
```sh
curl http://192.168.0.64:8080/page.html

```
will return the text produced by controller:

```text
page is here
```
### Testing
```sh
mvn -Ddebug=true -Dapplication=application test
```

The `debug` flag will trigger printing additional debugging information

### Running
```sh
mvn clean -Dmaven.test.skip=true spring-boot:run
```
followed by opening in the browser

`curl http://localhost:8080/input-validation.html`
this will show form with two inputs, validated. Only the second validation tiggels the button
### See Also
  * AngularJS Form $setValidity [post](https://medium.com/@lily.lsps/angularjs-form-setvalidity-1f2485ad9b22)
  * `form.FormController` [documentation](https://docs.angularjs.org/api/ng/type/form.FormController)
  * https://stackoverflow.com/questions/14363656/using-setvalidity-inside-a-controller
  * [validating User Input on a Form in Angular JS](https://www.infragistics.com/community/blogs/b/dhananjay_kumar/posts/validating-user-input-on-a-form-in-angular-js)
  * https://stackoverflow.com/questions/42624184/how-to-call-any-function-in-input-type-text-in-angularjs
### Author

[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
