### Info 

replica of the 
__SOAP Microservices with Spring Boot and Apache CXF__ in Docker container created with Spring Boot exposing a SOAP endpoint for a legacy client 
[project](https://github.com/jpontdia/ws-employee-soapcxf) downgraded to Java 1.8

### Usage

```sh
mvn package
export IMAGE=soap-server
docker build -t $IMAGE -f Dockerfile .
export NAME=soap-server
docker container rm $NAME
PORT=8081
docker run --name $NAME -p $PORT:$PORT -it $IMAGE
```
```sh
curl -s http://192.168.0.92:8080/hellow/services
```

```sh
curl -s http://192.168.0.92:8081/soap
```
```HTML
<?xml version="1.0"?>
<!DOCTYPE html PUBLIC "-//W3C//DTD html 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html>
  <head>
    <link type="text/css" rel="stylesheet" href="/soap/?stylesheet=1"/>
    <meta http-equiv="content-type" content="text/html; charset=UTF-8"/>
    <title>CXF - Service list</title>
  </head>
  <body>
    <span class="heading">Available SOAP services:</span>
    <br/>
    <table cellpadding="1" cellspacing="1" border="1" width="100%">
      <tr>
        <td>
          <span class="porttypename">EmployeeServicePortType</span>
          <ul>
            <li>GetEmployeesByName</li>
            <li>GetEmployeeById</li>
          </ul>
        </td>
        <td>
          <span class="field">Endpoint address:</span>
          <span class="value">http://192.168.0.92:8081/soap/service/employee</span>
          <br/>
          <span class="field">WSDL :</span>
          <a href="http://192.168.0.92:8081/soap/service/employee?wsdl">{http://service.datajdbc.jpworks.com/}EmployeeEndpointService</a>
          <br/>
          <span class="field">Target namespace:</span>
          <span class="value">http://service.datajdbc.jpworks.com/</span>
        </td>
      </tr>
    </table>
    <br/>
    <br/>
  </body>
</html>

```
```sh
curl -s http://192.168.0.92:8081/soap/service/employee?wsdl
```

```xml
<?xml version='1.0' encoding='UTF-8'?><wsdl:definitions xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:wsdl="http://schemas.xmlsoap.org/wsdl/" xmlns:tns="http://service.datajdbc.jpworks.com/" xmlns:soap="http://schemas.xmlsoap.org/wsdl/soap/" xmlns:ns2="http://schemas.xmlsoap.org/soap/http" xmlns:ns1="http://www.jpworks.com/employee" name="EmployeeEndpointService" targetNamespace="http://service.datajdbc.jpworks.com/">
  <wsdl:import location="http://192.168.0.92:8081/soap/service/employee?wsdl=EmployeeServicePortType.wsdl" namespace="http://www.jpworks.com/employee">
    </wsdl:import>
  <wsdl:binding name="EmployeeEndpointServiceSoapBinding" type="ns1:EmployeeServicePortType">
    <soap:binding style="document" transport="http://schemas.xmlsoap.org/soap/http"/>
    <wsdl:operation name="GetEmployeesByName">
      <soap:operation soapAction="http://www.jpworks.com/employee/GetEmployeesByName" style="document"/>
      <wsdl:input name="GetEmployeesByName">
        <soap:body use="literal"/>
      </wsdl:input>
      <wsdl:output name="GetEmployeesByNameResponse">
        <soap:body use="literal"/>
      </wsdl:output>
    </wsdl:operation>
    <wsdl:operation name="GetEmployeeById">
      <soap:operation soapAction="http://www.jpworks.com/employee/GetEmployeeById" style="document"/>
      <wsdl:input name="GetEmployeeById">
        <soap:body use="literal"/>
      </wsdl:input>
      <wsdl:output name="GetEmployeeByIdResponse">
        <soap:body use="literal"/>
      </wsdl:output>
    </wsdl:operation>
  </wsdl:binding>
  <wsdl:service name="EmployeeEndpointService">
    <wsdl:port binding="tns:EmployeeEndpointServiceSoapBinding" name="EmployeeEndpointPort">
      <soap:address location="http://192.168.0.92:8081/soap/service/employee"/>
    </wsdl:port>
  </wsdl:service>
</wsdl:definitions>

```
* generatee client code


```sh
mvn clean jaxws:wsimport
```

NOTE: the original project `pom.xml` the `cxf-codegen-plugin` plugin is not configured properly:

```text
[INFO] No WSDLs are found to process, Specify at least one of the following parameters: wsdlFiles, wsdlDirectory or wsdlUrls.[INFO] No WSDLs are found to process, Specify at least one of the following parameters: wsdlFiles, wsdlDirectory or wsdlUrls.
```

for generation of the client code use the [example project](https://github.com/eugenp/tutorials/tree/master/web-modules/jee-7)

The generated proxy code 

```text
src
└── main
    └── java
        └── example
            └── client
                └── generated
                    ├── Address.java
                    ├── EmployeeByIdRequest.java
                    ├── EmployeeByNameRequest.java
                    ├── EmployeeEndpointService.java
                    ├── EmployeeResponse.java
                    ├── EmployeeServicePortType.java
                    ├── EmployeesResponse.java
                    ├── ObjectFactory.java
                    └── package-info.java

```

is checked in

### TODO
The application dependency error in runtime:
```text
2022-12-15 19:35:00.608  WARN 12003 --- [           main] ConfigServletWebServerApplicationContext : Exception encountered during context initialization - cancelling refresh attempt: org.springframework.beans.factory.UnsatisfiedDependencyException: Error creating bean with name 'application': Unsatisfied dependency expressed through field 'webServiceTemplate'; nested exception is org.springframework.beans.factory.NoSuchBeanDefinitionException: No qualifying bean of type 'org.springframework.ws.client.core.WebServiceTemplate' available: expected at least 1 bean which qualifies as autowire candidate. Dependency annotations: {@org.springframework.beans.factory.annotation.Autowired(required=true)}
2022-12-15 19:35:00.612  INFO 12003 --- [           main] o.apache.catalina.core.StandardService   : Stopping service [Tomcat]
2022-12-15 19:35:00.627  INFO 12003 --- [           main] ConditionEvaluationReportLoggingListener :

Error starting ApplicationContext. To display the conditions report re-run your application with 'debug' enabled.
2022-12-15 19:35:00.912 ERROR 12003 --- [           main] o.s.b.d.LoggingFailureAnalysisReporter   :

***************************
APPLICATION FAILED TO START
***************************

Description:

Field webServiceTemplate in example.Application required a bean of type 'org.springframework.ws.client.core.WebServiceTemplate' that could not be found.

The injection point has the following annotations:
        - @org.springframework.beans.factory.annotation.Autowired(required=true)


Action:

Consider defining a bean of type 'org.springframework.ws.client.core.WebServiceTemplate' in your configuration.


```

### See Also

  * [invoking a SOAP Web Service in Java](https://www.baeldung.com/java-soap-web-service)
  * `jaxws:wsimport` [parameters](https://www.mojohaus.org/jaxws-maven-plugin/wsimport-mojo.html)
