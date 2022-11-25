FROM docker.elastic.co/apm/apm-server:7.1.0
USER root
RUN yum install -q -y python3 
RUN pip3 install flask elastic-apm[flask]
# NOTE cannot fix the file permissions via Dockerfile command -  the volume is mounted
RUN chmod go-w /usr/share/apm-server/apm-server.yml
USER apm-server
