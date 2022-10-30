### Info

This directory contains a [grafana-json-datasource](https://github.com/gsuveti/spring-grafana/tree/master/grafana-json-datasource) subproject providing 
backend implementation of [simple-json-datasource](https://grafana.com/grafana/plugins/grafana-simple-json-datasource/)
API for a Spring application from a bigger __Spring & Grafana__ [project](https://github.com/gsuveti/spring-grafana).
There is nothing but a skeleton implementation of `/tag-values`, `/tag-keys`, `/annotations`,`/query`, `/search` endpoint controller
methods in the original project

### Note

Authentication has been removed from the original project, as it does not belong  to Grafana Datasource: The original project `SecurityConfiguration`
class has been replaced and `org.springframework.security.core.Authentication`
argument removed from controller methods, the `security.oauth2.resource.jwt` public key from the `application.yml` and the whole `org.springframework.cloud.spring-cloud-dependencies`
dependency chain removed from `pom.xml`

For cosmetic reasons, the `org.projectlombok.lombok` dependency was also replaced with direct generated source code in pojo classes

### See Also
  * `grafanalib.py` python package for generate Grafana dashboards [project](https://github.com/weaveworks/grafanalib)
  * [DashboardGenerator](https://github.com/bhattchaitanya/Grafana-Dashboard-Generator) Java Jmeter Grafana dashboard generator (single source class, no dependencies, translates Jmeter  config file
  * [Spring Boot Metrics Micrometer InfluxDB Example](https://github.com/gysel/spring-boot-metrics-influxdb)
  * [Grafana dashboard builder in Java](https://github.com/szmg/grafana-dashboard-generator-java) - big project, stalled
  * [app](https://github.com/piomin/sample-spring-graphite) and [article](https://piotrminkowski.com/2017/07/13/custom-metrics-visualization-with-grafana-and-influxdb/) - generating and exporting metrics from spring-boot application to influxdb, visualizing them in grafana
  * [app](https://github.com/devcon5io/grafana-json-datasource) - Java based implementation for grafana/simple-json-datasource
  * [app](https://github.com/fkjellberg/spring-boot-micrometer-influxdb-grafana) to demonstrate a Spring Boot application using Micrometer to push metrics to InfluxDB and Grafana, run in provided containwers
  * [app](https://github.com/ypvillazon/spring-boot-metrics-to-influxdb) collects the metrics provided by the spring-actuator and send them to the InfluxDB time series database



### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
