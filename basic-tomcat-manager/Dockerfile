FROM tomcat:8.0-alpine
SHELL ["/bin/ash", "-c"]
ARG app_war="example.managed_app.war"
ENV CATALINA_HOME="/opt/tomcat"
ARG CATALINA_HOME=${CATALINA_HOME}
# Workaround of that the directory layout is different
ARG CATALINA_HOME="/opt/tomcat"
RUN mkdir /opt && ln -s /usr/local/tomcat ${CATALINA_HOME}
# NOTE: renaming for testing sake
ADD "target/${app_war}" ${CATALINA_HOME}/webapps/demo.war
ADD tomcat-users.xml ${CATALINA_HOME}/conf/tomcat-users.xml
ADD log4j2.xml ${CATALINA_HOME}/conf/log4j2.xml
ADD src/main/resources/application.properties ${CATALINA_HOME}/conf/application.properties
ADD catalina.sh ${CATALINA_HOME}/bin/catalina.sh 
# Remove CRLF to prevent the
# ': No such file or directory 
# error
RUN sed -i 's|\r||' ${CATALINA_HOME}/bin/catalina.sh
RUN echo "export JAVA_OPTS=\"-Dlog4j2.debug=true -Dapp.env=staging -Dlog4j.configurationFile=\$CATALINA_BASE/conf/log4j2.xml\"" | tee ${CATALINA_HOME}/bin/setenv.sh
RUN echo "export CLASSPATH=\$CLASSPATH:/opt/tomcat/conf" | tee -a ${CATALINA_HOME}/bin/setenv.sh

# NOTE
# ash - does not recognize ANSI-C quoting
# 
ARG UMASK=0027
RUN printf "\n\
export UMASK=${UMASK}\n\
" >> ${CATALINA_HOME}/bin/setenv.sh

# NOTE: There is no alternative to tweaking the setenv.sh
# setting the environment variable like
# ENV log4j.configurationFile=$CATALINA_BASE/conf/log4j2.xml 
# alone does not work
# to confirm, need to uncomment and comment two statements
EXPOSE 8080
# https://stackoverflow.com/questions/37904682/how-do-i-use-docker-environment-variable-in-entrypoint-array
# NOTE: when switching exec to shell syntax 
# one has to collapse original arguments into one token past "-c"
# ENTRYPOINT [ "sh" ,"-c", "${CATALINA_HOME}/bin/catalina.sh ru $@"]
# alternatively
ENTRYPOINT ${CATALINA_HOME}/bin/catalina.sh run $@
