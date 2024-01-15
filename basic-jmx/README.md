### Info

based on [carlosCharz/springbootjmx](https://github.com/carlosCharz/springbootjmx), which is entirely generic and [jay-bill/springboot-jmxDemo](https://github.com/jay-bill/springboot-jmxDemo) and [Murugar/SpringBootJmx](https://github.com/Murugar/SpringBootJmx)   

### Usage
options to provide tomcat/ embedded tomcat with

```sh
mvn  -Dcom.sun.management.jmxremote.port=7091 -Dcom.sun.management.jmxremote.rmi.port=7091 -Dcom.sun.management.jmxremote.authenticate=false -Dcom.sun.management.jmxremote.ssl=false spring-boot:run
```
### See Also:

  * [Basic Introduction to JMX](https://www.baeldung.com/java-management-extensions) the example [repo](https://github.com/eugenp/tutorials/tree/master/core-java-modules/core-java-perf/src/main/java/com/baeldung/jmx) and [clone of tutorial repo](https://github.com/yiminyangguang520/spring-boot-tutorials/blob/master/core-java-perf/src/main/java/com/baeldung/jmx/Game.java)
  * [how JMX Monitoring Works for Java Applications](https://www.eginnovations.com/blog/how-does-jmx-monitoring-work/)
  * [An Overview of JMX](https://dzone.com/articles/an-overview-of-jmx)
  * [Spring - Annotation Based Spring JMX Integration](https://www.logicbig.com/tutorials/spring-framework/spring-integration/annotation-based-spring-jmx-integration.html)

  * [stackoverflow](https://stackoverflow.com/questions/25308744/creating-mbean-in-java) discussion on JMX MBean 
  * [Spring 5 Integration: Monitoring, System Management and Debugging](https://app.pluralsight.com/library/courses/spring-integration-monitoring-system-management-debugging) (Managing and Controlling section) (advanced)
 

  * [Java Platform, Standard Edition Monitoring and Management Guide](https://docs.oracle.com/javase/9/management/toc.htm)
     + [Overview of Java SE Monitoring and Management](https://docs.oracle.com/javase/9/management/overview-java-se-monitoring-and-management.htm#JSMGM-GUID-EA3CFF69-F0D3-47AB-9AED-EF1CBF7F2B24)
     + [Monitoring and Management Using JMX Technology](https://docs.oracle.com/javase/9/management/monitoring-and-management-using-jmx-technology.htm#JSMGM-GUID-805517EC-2D33-4D61-81D8-4D0FA770D1B8)
     + [Using JConsole](https://docs.oracle.com/javase/9/management/using-jconsole.htm#JSMGM-GUID-77416B38-7F15-4E35-B3D1-34BFD88350B5)
     + [Using the Platform MBean Server and Platform MXBeans](https://docs.oracle.com/javase/9/management/using-platform-mbean-server-and-platform-mxbeans.htm#JSMGM-GUID-6239456C-FD46-46EA-96EF-DC02800D58EC)
     + [SNMP Monitoring and Management](https://docs.oracle.com/javase/9/management/snmp-monitoring-and-management.htm#JSMGM-GUID-D6FA8012-625C-4412-986F-867896F89696)
     + [Java Discovery Protocol (JDP)](https://docs.oracle.com/javase/9/management/java-discovery-protocol.htm#JSMGM-GUID-4FE44ECD-E1A7-42CE-9AD8-A2E582C55C43)

  * https://github.com/ekim197711/springboot-jmx-actuator
https://docs.oracle.com/javase/8/docs/technotes/guides/management/agent.html

  * Misc.
     + [exposing JMX Beans via HTTP for Prometheus consumption](https://github.com/prometheus/jmx_exporter)
     + [interactive JMX CLI written in Java](https://docs.cyclopsgroup.org/jmxterm)
     + [simple command line Java tool and Python module to query and collect Java metrics via JM](https://github.com/dgildeh/JMXQuery)
     + [JMX enumeration and attacking tool](https://github.com/qtc-de/beanshooter#generic-info)
     + [Java Agent based JMX metrics exporter](https://github.com/jmxtrans/jmxtrans-agent)

