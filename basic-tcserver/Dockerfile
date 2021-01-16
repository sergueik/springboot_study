FROM parrotstream/centos-openjdk:8

EXPOSE 6969 8080 8443

RUN yum install -q -y vim wget net-tools

# configure yum repo
RUN rpm --import http://packages.gopivotal.com/pub/rpm/rhel7/app-suite/RPM-GPG-KEY-PIVOTAL-APP-SUITE-EL7
RUN yum-config-manager --add-repo http://packages.pivotal.io/pub/rpm/rhel7/app-suite/x86_64
RUN yum install -q -y pivotal-tc-server-standard-3.2.1-RELEASE

# install tcserver instance (01)
RUN mkdir -p /web/tcserver
RUN /opt/pivotal/pivotal-tc-server-standard/tcruntime-instance.sh create -i /web/tcserver 01
# add default manager.war
ADD webapps/manager.war /web/tcserver/01/webapps
# Add to CLASSPATH
# NOTE: noticed high CPU load after adding
# RUN echo "CLASSPATH=\"\$CLASSPATH\":/web/tcserver/01/conf" | tee -a /web/tcserver/01/bin/setenv.sh
# RUN echo "CLASSPATH=/web/tcserver/01/conf" | tee -a /web/tcserver/01/bin/setenv.sh
RUN printf "\n\
if [ ! -z \"\$CLASSPATH\" ] ; then\n\
  CLASSPATH=\$CLASSPATH:/web/tcserver/01/conf\n\
else\n\
  CLASSPATH=/web/tcserver/01/conf\n\
fi\n\
" | tee -a /web/tcserver/01/bin/setenv.sh

ADD application.properties /web/tcserver/01/conf/
# NOTE: the tcserver instance start command cannot be used as ENRTYPOINT
# as it quits shortly
RUN touch /web/tcserver/01/logs/catalina.out
CMD /web/tcserver/01/bin/tcruntime-ctl.sh start && tail -f /web/tcserver/01/logs/catalina.out
