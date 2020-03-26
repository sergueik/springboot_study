FROM davidcaste/alpine-tomcat
ARG app_jar="headers-servlet-filter-0.2.0.jar"
ADD "target/${app_jar}" /opt/tomcat/lib
ADD new.xml /opt/tomcat/conf/web.xml
# Alternatively do it right there
EXPOSE 8080
ENTRYPOINT ["/opt/tomcat/bin/catalina.sh","run"]

