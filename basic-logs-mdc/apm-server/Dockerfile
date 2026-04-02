FROM docker.elastic.co/apm/apm-server:8.17.8
# NOTE cannot fix the file permissions via Dockerfile command if the file is mounted
USER root
RUN apt-get update && apt -y --fix-broken install
RUN apt-get install -q -y curl
ADD config/apm-server.yml /usr/share/apm-server/apm-server.yml
RUN chown apm-server  /usr/share/apm-server/apm-server.yml
RUN chmod 644 /usr/share/apm-server/apm-server.yml
ADD config/fields.yml /usr/share/apm-server/fields.yml
RUN chown apm-server  /usr/share/apm-server/fields.yml
RUN chmod 644 /usr/share/apm-server/fields.yml
USER apm-server
