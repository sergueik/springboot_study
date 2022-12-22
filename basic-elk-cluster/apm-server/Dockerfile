FROM docker.elastic.co/apm/apm-server:7.17.7
# NOTE cannot fix the file permissions via Dockerfile command if the file is mounted
USER root
RUN apt-get update
RUN apt-get install -q -y vim-tiny
ADD config/apm-server.yml /usr/share/apm-server/apm-server.yml
RUN chown apm-server  /usr/share/apm-server/apm-server.yml
RUN chmod 644 /usr/share/apm-server/apm-server.yml
ADD config/fields.yml /usr/share/apm-server/fields.yml
RUN chown apm-server  /usr/share/apm-server/fields.yml
RUN chmod 644 /usr/share/apm-server/fields.yml
USER apm-server
