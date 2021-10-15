FROM tomcat:8.0-alpine
SHELL ["/bin/ash", "-c"]
ARG app_war="example.servlet.war"
ENV CATALINA_HOME="/opt/tomcat"
ARG CATALINA_HOME=${CATALINA_HOME}
# Workaround of that the directory layout is different
ARG CATALINA_HOME="/opt/tomcat"
RUN mkdir /opt && ln -s /usr/local/tomcat ${CATALINA_HOME}
ADD "target/${app_war}" ${CATALINA_HOME}/webapps/demo.war
ENTRYPOINT ${CATALINA_HOME}/bin/catalina.sh run $@
