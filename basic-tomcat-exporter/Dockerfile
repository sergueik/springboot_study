FROM tomcat:8.5.27-jre8-alpine
ENV CATALINA_HOME="/usr/local/tomcat"
ARG CATALINA_HOME=${CATALINA_HOME}
ARG app_war="dummy.war"
ADD "app/target/${app_war}" ${CATALINA_HOME}/webapps
# will create a app 'dummy'

RUN mkdir ${CATALINA_HOME}/webapps/manual
ADD "app/src/main/webapp/index.html" ${CATALINA_HOME}/webapps/manual
# will create a page 'manual/index.html'


RUN test -d ${CATALINA_HOME}/webapps/ROOT || mkdir ${CATALINA_HOME}/webapps/ROOT
ADD "app/src/main/webapp/index.html" ${CATALINA_HOME}/webapps/ROOT
# will create a custom 'index page'
# see also: https://stackoverflow.com/questions/30256386/how-to-copy-multiple-files-in-one-layer-using-a-dockerfile
ADD exporter/target/lib ${CATALINA_HOME}/lib/
ARG exporter_servlet_war=prometheus_exporter_servlet-0.1.0-SNAPSHOT.war
ADD exporter/target/${exporter_servlet_war} ${CATALINA_HOME}/webapps/metrics.war
EXPOSE 8080
ENTRYPOINT ${CATALINA_HOME}/bin/catalina.sh run
