### info

replica of [logback-http-appender](https://github.com/ofer-velich/logback-http-appender) used to test the [log4jna](https://github.com/dblock/log4jna) log appender

### Usage

start http server to send logs to

```sh
IMAGE=basic-perl-apache
```
build image if necessary
```sh
docker build -t $IMAGE -f Dockerfile . --progress=plain
docker run -d -p 9090:80 -p 9443:443 --name $NAME $IMAGE
```
* alternatively start run default command in the background
```sh
NAME=basic-perl-cgi
ID=$(docker container ls -a | grep $NAME|cut -f 1 -d ' ')
docker start $ID
```
* run the application
```cmd
mvn package
java -cp target/basic-logback-custom-appender.jar;target/lib/* example.Main
```
or
```cmd
java -jar target\basic-logback-custom-appender.jar example.Main
```
this will log to console
```text
18:45:02,357 |-INFO in ch.qos.logback.core.model.processor.AppenderModelHandler- Processing appender named [logzio]
18:45:02,358 |-INFO in ch.qos.logback.core.model.processor.AppenderModelHandler- About to instantiate appender of type [ch.qos.logback.ext.logzio.LogzioAppnder]
18:45:02,548 |-INFO in ch.qos.logback.classic.model.processor.RootLoggerModelHandler - Setting level of ROOT logger to INFO
18:45:02,548 |-INFO in ch.qos.logback.core.model.processor.AppenderRefModelHandler - Attaching appender named [logzio] to Logger[ROOT]
18:45:02,549 |-INFO in ch.qos.logback.core.model.processor.DefaultProcessor@2bbaf4f0 - End of configuration.
18:45:02,550 |-INFO in ch.qos.logback.classic.joran.JoranConfigurator@4e41089d - Registering current configuration as safe fallback point
18:45:02,551 |-INFO in ch.qos.logback.classic.util.ContextInitializer@52af6cff - ch.qos.logback.classic.util.DefaultJoranConfigurator.configure() call lasted 486 milliseconds. ExecutionStatus=DO_NOT_INVOKE_NEXT_IF_ANY
Sending: {"timestamp":"1705275902554","level":"INFO","thread":"main","logger":"example.Main","message":"Hello World from Logback!","context":"default"}

```
* connect to container interactively
```sh
docker exec -it $NAME sh
```
check apache log in container:
```sh
vi /var/www/logs/error.log
```
NOTE: since cgi-bin prints debugging information to STDERR, it will be found in the `error.log`:
```txt
 $VAR1 = {
           'context' => 'default',
           'referer' => undef,
           'thread' => 'main',
           'level' => 'INFO',
           'message' => 'Hello World from Logback!',
           'remote_addr' => '192.168.99.1',
           'timestamp' => '1705264059209',
           'logger' => 'example.Main',
         };: /var/www/localhost/cgi-bin/echo_json.cgi

```


### See Also
   * https://github.com/maricn/logback-slack-appender
   * https://github.com/cyfrania/logback-slack-appender
   * https://github.com/ArpNetworking/logback-steno
   * [Simple CLI tool to test logback on command line](https://github.com/wlanboy/logbacktest)
   * https://github.com/jukka/logback-tray (AWT)
   * https://github.com/ofer-velich/logback-http-appender

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
