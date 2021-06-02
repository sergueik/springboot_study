### Info

This directory contains a tweaked replica of [](https://www.petrikainulainen.net/programming/tips-and-tricks/creating-profile-specific-configuration-files-with-maven/)
[project] (https://github.com/pkainulainen/maven-examples/tree/master/properties-filtering) to enable profile based property filtering in test.

By default the [maven Resources Plugin Filtering](https://maven.apache.org/plugins/maven-resources-plugin/examples/filter.html) is only visible in 
`target/classes` files, not in `target/test-classes` and one has to utilize a custom `PropertiesParser` class to observe the `-P` argument have the desired eddect which may be valuable when the profile changes are numerous.

### Usage
* no arg run
```sh
mvn test
```
this will use the active profile:
```sh
Active Profiles for Project 'example:basic-profiles:jar:0.1.0-SNAPSHOT':
The following profiles are active:
 - chrome (source: example:basic-profiles:0.1.0-SNAPSHOT)
```
which in turn will make the file
`profiles/chrome/config.properties` be parsed  to define the property:
```java
browser.setting=chrome
```
which will be used in parsing the project property file `src/main/resources/application.properties`:
```java
browser=${browser.setting}
```
to become in `target/classes/application.properties`:
```java
browser=chrome
```

The test uses this value as a default in the 
```java
private final String value = System.getProperty("browser", "chrome");
```
hence test passes

* specify profile and additional property, added for the sake of the test to pass
```sh
mvn -P filefox -Dbrowser=firefox test
```

here the `-P` argument leads to filtering of the `src/main/resources/appplication.properties` (and other files in that directory) against the 
`profiles/firefox/config.properties`, and the `-Dbrowser=firefox` is passed the expected value of the property into the test the other way.


### Implementation

Under the hood the test utilizes a custom `PropertiesParser` class 

```java
PropertiesParser.getProperties("application.properties", System.getProperty("user.dir") + "/target/classes/", false) 
```

which loads the `application.properties` from the path specified as the seond argument. 
The class in question can also load the properties file from the thread execution context if the last argument is set to `true`.

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
