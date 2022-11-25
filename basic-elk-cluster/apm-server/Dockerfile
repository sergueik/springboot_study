FROM docker.elastic.co/apm/apm-server:7.1.0
USER root
RUN yum install -q -y python3 
RUN pip3 install flask elastic-apm[flask]
USER apm-server
