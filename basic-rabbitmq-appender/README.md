### Info
This directory contains a replica of [
rabbitmq-log4j-appender](https://github.com/plant42/rabbitmq-log4j-appender)
updated to work with latest amqp-client, java 8.
### Usage

* local testing
```sh
wget -O - "https://packagecloud.io/rabbitmq/rabbitmq-server/gpgkey" | sudo apt-key add -
```

A simple log4j appender to publish messages to a RabbitMQ queue.  The appender includes a layout which converts all log messages to JSON objects.

https://www.baeldung.com/log4j2-custom-appender
