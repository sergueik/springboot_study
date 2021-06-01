### Info

This directory contains a tweaked replica of [](https://www.petrikainulainen.net/programming/tips-and-tricks/creating-profile-specific-configuration-files-with-maven/)
[project] (https://github.com/pkainulainen/maven-examples/tree/master/properties-filtering) to enable profile based property filtering in test.

By default the [maven Resources Plugin Filtering](https://maven.apache.org/plugins/maven-resources-plugin/examples/filter.html) is only visible in 
`target/classes` files, not in `target/test-classes` and one has to utilize a custom `PropertiesParser` class to observe the `-P` argument have the desired eddect which may be valuable when the profile changes are numerous.

### Usage
```sh
mvn -P filefox -Dbrowser=firefox test
```

here the `-P` argument leads to filtering of the `src/main/resources/appplication.properties` (and other files in that directory) against the 
`profiles/firefox/config.properties`, and the `-Dbrowser=firefox` is passed the expected value of the propery into the test the other way

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
