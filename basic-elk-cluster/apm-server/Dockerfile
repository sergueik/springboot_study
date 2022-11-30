FROM docker.elastic.co/apm/apm-server:7.17.7
# NOTE cannot fix the file permissions via Dockerfile command if the file is mounted
USER root
ADD config/apm-server.yml /usr/share/apm-server/apm-server.yml
RUN chmod go-w /usr/share/apm-server/apm-server.yml
USER apm-server
