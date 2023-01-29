``### Info

This directory contins a  replica of the Prometheus [application runtime metric exporter](https://github.com/nlighten/tomcat_exporter) for Apache Tomcat, deployed in the same containeer with a legacy app combined with an
ultra basic static html file put into tomcat docker container playing the role of the legacy app

### Testing

TBD. 

The servlet `pom.xml` was modified to not rely on the parent pom.
NOTE: the `tomcat_exporter_servlet.war` needs to be deployed intoi `${CATALINA_HOME}/webapps/` as `metrics.war`. The dependency jars `simpleclient.jar`, `simpleclient_common.jar`,`simpleclient_hotspot.jar`, `simpleclient_servlet_common.jar`, and `tomcat_exporter_client.jar` has to be put into `${CATALINA_HOME}/lib`

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)


