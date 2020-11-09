### Info
This directory contains few elementary tabbitmq java examples 
### Usage
#### Rabbitmq server installed locally
```
mvn package
```
* launch standalone logging server

```cmd
java -cp target\example.rabbitmq-messaging-0.1.0-SNAPSHOT.jar;target\lib\* example.Server 127.0.0.1 5672 guest guest /

```
* launch Publisher
```cmd
java -cp target\example.rabbitmq-messaging-0.1.0-SNAPSHOT.jar;target\lib\* example.Publisher my_company store.messages
```
* launch consumer
```cmd
java -cp target\example.rabbitmq-messaging-0.1.0-SNAPSHOT.jar;target\lib\* example.Consumer store.messages
```
### See Also
  * https://www.rabbitmq.com/tutorials/tutorial-three-java.html
  * https://github.com/Gsantomaggio/rabbitmqexample
  * https://github.com/maxwellyue/rabbitmq-tutorial-java
  * https://github.com/nyholmniklas/rabbitmq-tutorial