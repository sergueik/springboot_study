FROM davidcaste/alpine-tomcat
ARG app_jar="example.headers-filter.jar"
ADD "target/${app_jar}" /opt/tomcat/lib
ARG setuptool_jar="example.setuptool.jar"
ADD "setuptool/target/${setuptool_jar}" /tmp
CMD java -cp /tmp/${setuptool_jar} example.MergeDocumentFragments -in /opt/tomcat/conf/web.xmll -out /tmp/new.xml
CMD cp /tmp/new.xml /opt/tomcat/conf/web.xml
# ADD new.xml /opt/tomcat/conf/web.xml
# Alternatively do it right there
EXPOSE 8080
ENTRYPOINT ["/opt/tomcat/bin/catalina.sh","run"]

