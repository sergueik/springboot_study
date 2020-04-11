FROM tomcat:8.0-alpine
ARG app_war="demo.war"
ENV CATALINA_HOME="/opt/tomcat"
ARG CATALINA_HOME=${CATALINA_HOME}
ARG CATALINA_HOME="/opt/tomcat"
RUN mkdir /opt && ln -s /usr/local/tomcat  ${CATALINA_HOME}
# ADD "target/${app_war}" ${CATALINA_HOME}/webapps/demo.war
ADD "springboot-war/target/${app_war}" ${CATALINA_HOME}/webapps/demo.war
ADD tomcat-users.xml ${CATALINA_HOME}/conf/tomcat-users.xml
# Workaround of that the directory layout is different
RUN echo "export JAVA_OPTS=\"-Dapp.env=staging\"" | tee ${CATALINA_HOME}/bin/setenv.sh

EXPOSE 8080
ENTRYPOINT ["/opt/tomcat/bin/catalina.sh","run"]
# TODO: arguments?
# ENTRYPOINT ["${CATALINA_HOME}/bin/catalina.sh","run"]
