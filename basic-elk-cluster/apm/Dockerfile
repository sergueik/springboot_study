FROM docker.elastic.co/apm/apm-server:6.2.4
USER root
RUN yum install -q -y python3 pip3
RUN pip3 install flask elastic-apm[flask]
USER apm-server

