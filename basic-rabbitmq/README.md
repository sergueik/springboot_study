### Info
This directory contains few elementary rabbitmq java examples with pure java server proxy interacting with publisher and consumer defined in the same project
### Usage

#### Rabbitmq server installed locally

start the standalone rabbitmq server

```sh
docker-compose up --build
```
or if container was already built
```sh
docker-compose ps -a
```
```text
rabbitmq            rabbitmq:latest     "docker-entrypoint.s…"   rabbitmq            4 hours ago         Exited (0) 4 hours ago
```
```sh
docker-compose start
```
build application
```sh
mvn package
```
* launch standalone logging server proxy
```ssh
java -cp target/example.rabbitmq-messaging-0.2.0-SNAPSHOT.jar:target/lib/* example.Server 127.0.0.1 5672 guest guest /
```
this will log
```txt
Starting server..
Trying new connection with host: 127.0.0.1 port: 5672 virtual host: /
Connection done: 127.0.0.1
Created exchange my_company
Created queue store.messages
Server started
```
the rabbitmq server container log will indicate the connection:
```text
rabbitmq  | 2023-12-21 20:17:20.282911+00:00 [info] <0.667.0> accepting AMQP connection <0.667.0> (172.19.0.1:47796 -> 172.19.0.2:5672)
rabbitmq  | 2023-12-21 20:17:20.410039+00:00 [info] <0.667.0> connection <0.667.0> (172.19.0.1:47796 -> 172.19.0.2:5672): user 'guest' authenticated and granted access to vhost '/'
```

on Windows update the path and list separators in the command 
```cmd
java -cp target\example.rabbitmq-messaging-0.2.0-SNAPSHOT.jar;target\lib\* example.Server 127.0.0.1 5672 guest guest /
```

* launch consumer
```cmd
java -cp target/example.rabbitmq-messaging-0.2.0-SNAPSHOT.jar:target/lib/* example.Consumer store.messages
```
this will initially print in console
```text
Waiting for messages in: store.messages
To exit press CTRL+C
```
* launch publisher
```cmd
java -cp target/example.rabbitmq-messaging-0.2.0-SNAPSHOT.jar:target/lib/* example.Publisher my_company store.messages
```

the publisher console will log:
```text
Published message: Message number 0
Published message: Message number 1
Published message: Message number 2
Published message: Message number 3
Published message: Message number 4
...
```

the server console will log:
```text
processing message 'Message number 0'...store.messages
 waiting .......Insert for 'Message number 0' done
Precessed: 1
processing message 'Message number 1'...store.messages
 waiting ......Insert for 'Message number 1' done
Precessed: 2
processing message 'Message number 2'...store.messages
 waiting ......Insert for 'Message number 2' done
Precessed: 3
processing message 'Message number 3'...store.messages
 waiting ........Insert for 'Message number 3' done
Precessed: 4
processing message 'Message number 4'...store.messages
 waiting .....Insert for 'Message number 4' done
Precessed: 5
processing message 'Message number 5'...store.messages
 waiting ........Insert for 'Message number 5' done
Precessed: 6
processing message 'Message number 6'...store.messages
 waiting .......Insert for 'Message number 6' done
Precessed: 7
processing message 'Message number 7'...store.messages
 waiting .......
```

(the delay added for illustration)



The consumer console will log:
```text
 [x] Received 'Message number 1'
 [x] Done
 [x] Received 'Message number 2'
 [x] Done
...
```

### See Also
  * https://www.rabbitmq.com/tutorials/tutorial-three-java.html
  * https://github.com/Gsantomaggio/rabbitmqexample
  * https://github.com/maxwellyue/rabbitmq-tutorial-java
  * https://github.com/nyholmniklas/rabbitmq-tutorial

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
