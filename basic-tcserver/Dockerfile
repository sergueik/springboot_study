# build is-tcserver:latest
# docker build tcserver -t is-tcserver:latest
FROM centos:7
EXPOSE 6969 8080 8443

RUN yum install -y vim wget

# install yum repo
#RUN rpm --import http://packages.gopivotal.com/pub/rpm/rhel6/app-suite/RPM-GPG-KEY-PIVOTAL-APP-SUITE-EL6
RUN rpm --import http://packages.gopivotal.com/pub/rpm/rhel7/app-suite/RPM-GPG-KEY-PIVOTAL-APP-SUITE-EL7
RUN yum-config-manager --add-repo http://packages.pivotal.io/pub/rpm/rhel7/app-suite/x86_64

# install tcserver
RUN yum install -y pivotal-tc-server-standard-3.2.1-RELEASE

# add ojdbc6 and ojdbc7 to tomcat/lib directories
# ADD lib/ojdbc*.jar /tmp/
# RUN find /opt/pivotal/pivotal-tc-server-standard/tomcat-*.RELEASE -type d -name 'lib' -exec cp /tmp/ojdbc7*.jar {} \;

ENV JAVA_VERSION 8u271
ENV BUILD_VERSION b13
# install java, set JAVA_HOME
RUN cd ~
# RUN wget -nv --no-cookies --no-check-certificate --header "Cookie: oraclelicense=accept-securebackup-cookie" "http://download.oracle.com/otn-pub/java/jdk/$JAVA_VERSION-$BUILD_VERSION/jdk-$JAVA_VERSION-linux-x64.rpm" -O /tmp/jdk-8-linux-x64.rpm

# RUN wget -nv --no-cookies --no-check-certificate --header "Cookie: gpw_e24=http%3A%2F%2Fwww.oracle.com%2F; oraclelicense=accept-securebackup-cookie" "http://download.oracle.com/otn-pub/java/jdk/8u111-b14/jdk-8u111-linux-x64.rpm"
# RUN yum -y localinstall jdk-8u111-linux-x64.rpm
# RUN rm jdk-*.rpm
RUN yum install -qy java-1.8.0-openjdk-devel
# ENV JAVA_HOME /usr/java/jdk1.8.0_111/jre

# TODO
ENV JAVA_HOME /usr/lib/jvm/java-1.8.0-openjdk-1.8.0.275.b01-0.el7_9.x86_64/jre
# install tcserver instance (01)
RUN mkdir -p /web/tcserver
RUN /opt/pivotal/pivotal-tc-server-standard/tcruntime-instance.sh create -i /web/tcserver 01

# add default manager.war
ADD webapps/manager.war /web/tcserver/01/webapps


# start the tcserver instance
RUN /web/tcserver/01/bin/tcruntime-ctl.sh start /web/tcserver/01


# keep the container alive (PID=1)
# CMD tail -f /web/tcserver/01/logs/catalina.out
#ENTRYPOINT tail -f /dev/null
#CMD [ "/web/tcserver/01/bin/tcruntime-ctl.sh", "start", "/web/tcserver/01" ]

# TODO: JAVA_HOME
# CMD /web/tcserver/01/bin/tcruntime-ctl.sh start && tail -f /web/tcserver/01/logs/catalina.out
