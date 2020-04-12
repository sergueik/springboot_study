FROM tomcat:8.0-alpine
ARG app_war="demo.war"
ENV CATALINA_HOME="/opt/tomcat"
ARG CATALINA_HOME=${CATALINA_HOME}
# Workaround of that the directory layout is different
ARG CATALINA_HOME="/opt/tomcat"
RUN mkdir /opt && ln -s /usr/local/tomcat  ${CATALINA_HOME}
ADD "target/${app_war}" ${CATALINA_HOME}/webapps/demo.war
ADD tomcat-users.xml ${CATALINA_HOME}/conf/tomcat-users.xml
ADD log4j2.xml ${CATALINA_HOME}/conf/log4j2.xml
ADD log4j.dtd ${CATALINA_HOME}/conf/log4j.dtd
# RUN echo "export JAVA_OPTS=\"-Dlog4j2.debug=true -Dapp.env=staging -Dlog4j.configuration=file://\$CATALINA_BASE/conf/log4j2.xml\"" | tee ${CATALINA_HOME}/bin/setenv.sh
RUN echo "export JAVA_OPTS=\"-Dlog4j2.debug=true -Dapp.env=staging -Dlog4j.configuration=\$CATALINA_BASE/conf/log4j2.xml\"" | tee ${CATALINA_HOME}/bin/setenv.sh

EXPOSE 8080
ENTRYPOINT ["/opt/tomcat/bin/catalina.sh","run"]
# TODO: arguments?
# ENTRYPOINT ["${CATALINA_HOME}/bin/catalina.sh","run"]
