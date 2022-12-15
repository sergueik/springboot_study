### Info

this directory contains a trimmed replica of [skeleton microservice-like SOAP webservice application with Spring Boot and Apache CXF](https://github.com/gbdevw/spring-soap-ws-hellow) to serve as a basic application to instrument with APM and due to chalenges with dockerization of [the other one](https://github.com/sergueik/springboot_study/tree/master/basic-soap-server-client)


The modifications include
removal of monitoring and health checks and added validation dependency to address runtime errors:

```text
The Bean Validation API is on the classpath but no implementation could be found
```

and
```text
 Servlet.service() for servlet [CXFServlet] in context with path [/hellow] threw exception [Filter execution threw an exception] with root cause
java.lang.NoSuchMethodError: io.micrometer.core.instrument.distribution.DistributionStatisticConfig$Builder.serviceLevelObjectives([D)Lio/micrometer/core/instrument/distribution/DistributionStatisticConfig$Builder;
        at org.springframework.boot.actuate.autoconfigure.metrics.PropertiesMeterFilter.configure(PropertiesMeterFilter.java:86)
        at io.micrometer.core.instrument.MeterRegistry.getOrCreateMeter(MeterRegistry.java:568)
        at io.micrometer.core.instrument.MeterRegistry.registerMeterIfNecessary(MeterRegistry.java:529)
        at io.micrometer.core.instrument.MeterRegistry.timer(MeterRegistry.java:269)
        at io.micrometer.core.instrument.Timer$Builder.register(Timer.java:473)
        at org.springframework.boot.actuate.metrics.web.servlet.WebMvcMetricsFilter.getTimer(WebMvcMetricsFilter.java:169)

```

The legacy style SOAP Client application is yet to be added

### Usage

```sh
mvn package
export IMAGE=soap-server
docker build -t $IMAGE -f Dockerfile .
export NAME=soap-server
docker container rm $NAME
docker run --name $NAME -p 8080:8080 -it $IMAGE
```
```sh
curl -s http://192.168.0.92:8080/hellow/services
```
```XML
<?xml version="1.0"?>
<!DOCTYPE html PUBLIC "-//W3C//DTD html 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html>
  <head>
    <LINK type="text/css" rel="stylesheet" href="/hellow/services/?stylesheet=1"/>
    <meta http-equiv="content-type" content="text/html; charset=UTF-8"/>
    <title>CXF - Service list</title>
  </head>
  <body>
    <span class="heading">Available SOAP services:</span>
    <br/>
    <table cellpadding="1" cellspacing="1" border="1" width="100%">
      <tr>
        <td>
          <span class="porttypename">sayHelloInterface</span>
          <ul>
            <li>sayHello</li>
          </ul>
        </td>
        <td>
          <span class="field">Endpoint address:</span>
          <span class="value">http://192.168.0.92:8080/hellow/services/sayhello</span>
          <br/>
          <span class="field">WSDL :</span>
          <a href="http://192.168.0.92:8080/hellow/services/sayhello?wsdl">{http://springsoapwshellow.guillaumebraibant/soap}sayHelloService</a>
          <br/>
          <span class="field">Target namespace:</span>
          <span class="value">http://springsoapwshellow.guillaumebraibant/soap</span>
        </td>
      </tr>
    </table>
    <br/>
    <br/>
  </body>
</html>
```

```sh
curl -s http://192.168.0.92:8080/hellow/services/sayhello?wsdl
```
```XML
<?xml version='1.0' encoding='UTF-8'?><definitions xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xs="http://www.w3.org/2001/XMLSchema" xmlns:tns="http://springsoapwshellow.guillaumebraibant/soap" xmlns:soapenc="http://schemas.xmlsoap.org/soap/encoding/" xmlns:soap12="http://schemas.xmlsoap.org/wsdl/soap12/" xmlns:soap="http://schemas.xmlsoap.org/wsdl/soap/" xmlns:mime="http://schemas.xmlsoap.org/wsdl/mime/" xmlns:http="http://schemas.xmlsoap.org/wsdl/http/" xmlns="http://schemas.xmlsoap.org/wsdl/" targetNamespace="http://springsoapwshellow.guillaumebraibant/soap">
  <types>
<xs:schema xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xs="http://www.w3.org/2001/XMLSchema" xmlns:tns="http://springsoapwshellow.guillaumebraibant/soap" xmlns:soapenc="http://schemas.xmlsoap.org/soap/encoding/" xmlns:soap12="http://schemas.xmlsoap.org/wsdl/soap12/" xmlns:soap="http://schemas.xmlsoap.org/wsdl/soap/" xmlns:mime="http://schemas.xmlsoap.org/wsdl/mime/" xmlns:http="http://schemas.xmlsoap.org/wsdl/http/" xmlns="http://schemas.xmlsoap.org/wsdl/">

  <xs:import namespace="http://springsoapwshellow.guillaumebraibant/soap" schemaLocation="http://192.168.0.92:8080/hellow/services/sayhello?xsd=full-name.xsd"/>

</xs:schema>
  </types>
  <message name="sayHelloOutput">
    <part name="response" type="xs:string">
    </part>
  </message>
  <message name="sayHelloInput">
    <part name="username" type="tns:fullName">
    </part>
  </message>
  <portType name="sayHelloInterface">
<documentation>This interface exposes several method that compute a greeting message based on the provided username</documentation>
    <operation name="sayHello">
<documentation>This operation takes in a parameter a user name and return a greeting message</documentation>
      <input message="tns:sayHelloInput" name="request">
    </input>
      <output message="tns:sayHelloOutput" name="response">
    </output>
    </operation>
  </portType>
  <binding name="sayHelloSoapHttpBinding" type="tns:sayHelloInterface">
    <soap12:binding style="document" transport="http://schemas.xmlsoap.org/soap/http"/>
    <operation name="sayHello">
      <input>
        <soap:body use="literal"/>
      </input>
      <output>
        <soap:body use="literal"/>
      </output>
    </operation>
  </binding>
  <service name="sayHelloService">
    <port binding="tns:sayHelloSoapHttpBinding" name="sayHelloInterface">
      <soap:address location="http://192.168.0.92:8080/hellow/services/sayhello"/>
    </port>
  </service>
</definitions>
```
### See Also

  * original repository author [article](https://guillaume-braibant.medium.com/cloud-native-soap-microservices-37712cc1a399)
  * another [example app](https://github.com/Gueka/docker-soap)  - NOTE, need to convert gradle SOAP specific build tasks from `build.gradle` to `pom.xml`
  * [invoking a SOAP Web Service in Java](https://www.baeldung.com/java-soap-web-service)
  * `jaxws:wsimport` [parameters](https://www.mojohaus.org/jaxws-maven-plugin/wsimport-mojo.html)
