FROM tomcat:8.5.27-jre8-alpine
ENV CATALINA_HOME="/usr/local/tomcat"
ARG CATALINA_HOME=${CATALINA_HOME}
ARG app_war="dummy.war"
ADD "target/${app_war}" ${CATALINA_HOME}/webapps
# will create a app 'dummy'
RUN mkdir ${CATALINA_HOME}/webapps/manual
ADD "src/main/webapp/index.html" ${CATALINA_HOME}/webapps/manual
# will create a app 'manual'

EXPOSE 8080
ENTRYPOINT ${CATALINA_HOME}/bin/catalina.sh run


