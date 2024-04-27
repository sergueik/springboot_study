### info

replica of [logback-http-appender](https://github.com/ofer-velich/logback-http-appender) that demonstrates performing REST server 
logging through `org.apache.http.client.HttpClient` [package](https://hc.apache.org/httpcomponents-client-4.5.x/)
used to port the [log4jna](https://github.com/dblock/log4jna) log appender from [log4j](https://logging.apache.org/log4j/) to [logback](https://logback.qos.ch/).

### Usage

* start http server to send logs to

```sh
pushd basic-perl-cgi
```
```sh
IMAGE=basic-perl-apache
```
* build image if necessary
```sh
NAME=basic-perl-cgi
docker build -t $IMAGE -f Dockerfile . --progress=plain
docker run -d -p 9090:80 -p 9443:443 --name $NAME $IMAGE
```
* alternatively start run default command in the background
```sh
NAME=basic-perl-cgi
ID=$(docker container ls -a | grep $NAME|cut -f 1 -d ' ')
docker start $ID
```
* check the Log rest endpoint is functional

```sh
 curl -sX POST -d '{"foo": "bar"}' http://192.168.99.100:9090/cgi-bin/echo_json.cgi
```
this will echo JSON back:
```JSON
{
   "foo" : "bar",
   "remote_addr" : "192.168.99.101",
   "referer" : null
}
```
* update `logback.xml` with the hostname of the log rest server. Currrently it only tested operating over http.
```XML
<configuration debug="true">
    <appender name="rest" class="example.logback.CustomAppender">
        <token></token>
        <hostname>192.168.99.100</hostname>
        <port>9090</port>
        <protocol>http</protocol>
        <uri>/cgi-bin/echo_json.cgi</uri>
        <layout class="ch.qos.logback.contrib.json.classic.JsonLayout">
            <jsonFormatter class="ch.qos.logback.contrib.jackson.JacksonJsonFormatter">
                <!-- prettyPrint is probably ok in dev, but usually not ideal in production: -->
                <prettyPrint>false</prettyPrint>
            </jsonFormatter>
        </layout>
    </appender>
    <root level="info">
        <appender-ref ref="rest" />
    </root>
</configuration>
```
* run the application
```cmd
mvn package
```
```cmd
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
18:45:02,551 |-INFO in ch.qos.logback.classic.util.ContextInitializer@52af6cff - ch.qos.logback.classic.util.DefaultJoranConfigurator.configure() call lasted 486 milliseconds. ExecutionStatus=DO_NOT_INVOKE_NEXT_IF_ANY Sending: {"timestamp":"1705275902554","level":"INFO","thread":"main","logger":"example.Main","message":"Hello World from Logback!","context":"default"}
```
* connect to log server container interactively
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

### Cleanup
```sh
docker container stop $NAME
docker container rm $NAME
docker image prune -f 
docker volume prune -f  
```

### See Also

  * [apache HttpClient Tutorial](https://www.baeldung.com/httpclient-guide)
  * [Simple CLI tool to test logback on command line](https://github.com/wlanboy/logbacktest)
  * misc. custom logback appenders
    + https://github.com/maricn/logback-slack-appender
    + https://github.com/cyfrania/logback-slack-appender
    + https://github.com/ArpNetworking/logback-steno
    + https://github.com/jukka/logback-tray (AWT)
    + https://github.com/ofer-velich/logback-http-appender
    + https://github.com/bckfnn/influxdb-logback-appender
    + https://github.com/omnecon/loganalytics-logback-appender
    + https://github.com/opensourceteams/n_01001_maven_logback_httpclient
    + https://github.com/themodernway/logback-json-gson
    + https://github.com/Bali8/Google-PubSub-Logback-Appender
    + https://github.com/DTForce/logback-google-cloud (stackdriver)
    + https://github.com/godbles4me/logbackx
    + https://github.com/bcoste/sample-filebeat-docker-logging
    + https://github.com/codewinkel/logback-extensions httpappendr config
    + https://github.com/michl-b/logback-msteams-appender
    + https://github.com/carlspring/logback-configuration
    + https://logback.qos.ch/manual/index.html
    + https://github.com/sswayney/seq-logback-appender

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
