FROM tomcat:8.5.27-jre8-alpine
ENV CATALINA_HOME="/opt/tomcat"
ARG CATALINA_HOME=${CATALINA_HOME}
ARG CATALINA_HOME="/opt/tomcat"
RUN mkdir /opt && ln -s /usr/local/tomcat ${CATALINA_HOME}
ARG app_jar="example.headers-filter.jar"
ADD "target/${app_war}" ${CATALINA_HOME}/lib/
# Do the web.xml modifications right in the container
ARG setuptool_jar="example.setuptool.jar"
ADD "setuptool/target/${setuptool_jar}" /tmp
RUN java -cp /tmp/${setuptool_jar} example.MergeDocumentFragments -in ${CATALINA_HOME}/conf/web.xml -out /tmp/new.xml
RUN cp /tmp/new.xml ${CATALINA_HOME}/conf/web.xml
# NOTE: if setup tool failed, copy modified web.xml directly
ADD new.xml ${CATALINA_HOME}/conf/web.xml
EXPOSE 8080
ENTRYPOINT ["/opt/tomcat/bin/catalina.sh","run"]

