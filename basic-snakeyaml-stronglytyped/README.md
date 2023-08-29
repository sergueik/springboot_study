###  Info

This directory  contains replica of
[snake-yaml(https://github.com/TaimoorChoudhary/snake-yaml) repository 
with demo code for Snake YAML loading complex types from provided YAML text
covered in [reading and Writing YAML Files in Java with SnakeYAML](https://stackabuse.com/reading-and-writing-yaml-files-in-java-with-snakeyaml/)  by Taimoor Choudhary


### Usage
```sh
mvn package
```

```sh
java -cp target\snake-yaml-0.6.0-SNAPSHOT.jar;target\lib\*  example.StudentReader
```
this will print all read/write YAML examples executed:

```text
***** SNAKE-YAML *****

**** READER ****

*** Read YAML ***
{id=20, name=Bruce, year=2020, address=Gotham City, department=Computer Science}


*** Read YAML containing Collection ***
{id=20, name=Bruce, year=2020, address=Gotham City, department=Computer Science,
 courses=[{name=Algorithms, credits=6}, {name=Data Structure, credits=5}, {name=
Design patters, credits=3}]}

*** Read YAML as Bean ***
Student[Person[id=20, name='Bruce', address='Gotham City'], year=2020, departmen
t='Computer Science', courses=null]

*** Read YAML as Bean with Nested Class ***
Student[Person[id=20, name='Bruce', address='Gotham City'], year=2020, departmen
t='Computer Science', courses=[Course[name='Algorithms', credits=6.0], Course[na
me='Data Structure', credits=5.0], Course[name='Design patters', credits=3.0]]]

**** WRITER ****

*** Write YAML ***
*** Write YAML Basic ***
!!example.model.Student
address: Night City
courses: null
department: Cyberware
id: 21
name: Tim
year: 2077

*** Write YAML with Collection ***
java.io.PrintWriter@490d6c15
```

### TODO

support the YAML:
```YAML
services:
  - microservice:
      name: db
      image: mysql
  - microservice:
      name: web
      image: apache

settings:
  boolean_setting: true
  integer_setting: 42
  string_setting: somethings

version: 1.0

```
currently failed to load it, get a null after:
```java
Yaml yaml = new Yaml(new Constructor(Configuration.class));
Configuration data = yaml.load(inputStream);

```


the flat YAML
```YAML
id: 20
name: Bruce
year: 2020
address: Gotham City
department: Computer Science
courses:
  - name: Algorithms
    credits: 6
  - name: Data Structure
    credits: 5
  - name: Design patters
    credits: 3
services:
  - microservice: { name: db, image: mysql }
  - microservice: { name: web, image: apache }

```

### OS-Specific Configuration

it seems easiest to support Map in the pojo:
```java
public class ComplexConfiguration {
	private String version;
	private Settings settings;
	private Map<String, String> extradata = new HashMap<String, String>();

	public Map<String, String> getExtradata() {
		return extradata;
	}

	public void setExtradata(Map<String, String> extradata) {
		this.extradata = extradata;
	}

```
and YAML
```YAML
extradata:
  windows: c:\windows\system32
  linux: /home
```
then print debugging info:
```java
Yaml yaml = new Yaml(new Constructor(ComplexConfiguration.class));
ComplexConfiguration data = yaml.load(inputStream);

System.out.println("Read data: " + data);
System.out.println("Read data version: " + data.getVersion());
System.out.println("Read data extradata for Windows: "
	+ data.getExtradata().get("windows"));

```
this will print

```text
Read data: example.model.configuration.ComplexConfiguration@12bb4df8
Read data version: 1.0
Read data extradata for Windows: c:\windows\system32
```
### See Also

  * [Parsing YAML with SnakeYAML](https://www.baeldung.com/java-snake-yaml)
  * https://stackabuse.com/reading-and-writing-yaml-files-in-java-with-snakeyaml/
  * https://bitbucket.org/snakeyaml/snakeyaml/wiki/Documentation

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
