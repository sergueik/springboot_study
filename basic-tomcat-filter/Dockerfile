FROM tomcat_8.5.61_jre8_alpine39
ENV CATALINA_HOME="/opt/tomcat"
ARG CATALINA_HOME=${CATALINA_HOME}
ARG CATALINA_HOME="/opt/tomcat"
# RUN mkdir /opt && ln -s /usr/local/tomcat ${CATALINA_HOME}
ARG app_jar="example.headers-filter.jar"
ADD "target/${app_war}" ${CATALINA_HOME}/lib/
# Optional:
# Do the web.xml modifications right in the container
# ARG setuptool_jar="example.setuptool.jar"
# ADD "setuptool/target/${setuptool_jar}" /tmp
# RUN java -cp /tmp/${setuptool_jar} example.MergeDocumentFragments -in ${CATALINA_HOME}/conf/web.xml -out /tmp/new.xml
# RUN cp /tmp/new.xml ${CATALINA_HOME}/conf/web.xml
# Experiment with environment properties
# error class
# RUN echo "org.apache.tomcat.util.digester.PROPERTY_SOURCE=org.apache.tomcat.util.digester.Digester\$EnvironmentPropertySource"|tee -a ${CATALINA_HOME}/conf/catalina.properties
RUN echo "org.apache.tomcat.util.digester.PROPERTY_SOURCE=org.apache.tomcat.util.digester.EnvironmentPropertySource"|tee -a ${CATALINA_HOME}/conf/catalina.properties
RUN echo "org.apache.tomcat.util.digester.REPLACE_SYSTEM_PROPERTIES=true"|tee -a ${CATALINA_HOME}/conf/catalina.properties
RUN echo "export CLASSPATH=\$CLASSPATH:${CATALINA_HOME}/conf:${CATALINA_HOME}/lib/servlet-api.jar:${CATALINA_HOME}/lib/example.headers-filter.jar" | tee -a ${CATALINA_HOME}/bin/setenv.sh
RUN echo "export APP_SERVER=value" | tee -a ${CATALINA_HOME}/bin/setenv.sh
# NOTE: if setup tool failed, copy modified web.xml directly
ADD new.xml ${CATALINA_HOME}/conf/web.xml
EXPOSE 8080
ENTRYPOINT ["/opt/tomcat/bin/catalina.sh","run"]


