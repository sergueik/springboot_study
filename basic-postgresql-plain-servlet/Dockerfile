FROM tomcat:8.5-alpine
SHELL ["/bin/ash", "-c"]
ARG app_war="example.servlet.war"
ENV CATALINA_HOME="/usr/local/tomcat"
ARG CATALINA_HOME=${CATALINA_HOME}
ADD "target/${app_war}" ${CATALINA_HOME}/webapps/demo.war
ENTRYPOINT ${CATALINA_HOME}/bin/catalina.sh run $@

