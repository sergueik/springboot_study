FROM tomcat:8.0-alpine
SHELL ["/bin/ash", "-c"]
ARG app_war="demo.war"
# NOTE: the contents of the /opt/tomcat/webapps/demo may be misleading
ENV CATALINA_HOME="/opt/tomcat"
ARG CATALINA_HOME=${CATALINA_HOME}
# Workaround of that the directory layout is different
ARG CATALINA_HOME="/opt/tomcat"
RUN mkdir /opt && ln -s /usr/local/tomcat ${CATALINA_HOME}
ADD "target/${app_war}" ${CATALINA_HOME}/webapps/demo.war
ADD catalina.sh ${CATALINA_HOME}/bin/catalina.sh 
ADD src/main/webapp/application.properties ${CATALINA_HOME}/conf/
RUN echo "org.apache.tomcat.util.digester.PROPERTY_SOURCE=org.apache.tomcat.util.digester.Digester\$EnvironmentPropertySource"|tee -a ${CATALINA_HOME}/conf/catalina.properties
RUN echo "org.apache.tomcat.util.digester.REPLACE_SYSTEM_PROPERTIES=true"|tee -a ${CATALINA_HOME}/conf/catalina.properties
RUN echo "export CLASSPATH=\$CLASSPATH:/opt/tomcat/conf" | tee -a ${CATALINA_HOME}/bin/setenv.sh
RUN echo "export APP_SERVER=value" | tee -a ${CATALINA_HOME}/bin/setenv.sh

EXPOSE 8080
ENTRYPOINT ${CATALINA_HOME}/bin/catalina.sh run $@
