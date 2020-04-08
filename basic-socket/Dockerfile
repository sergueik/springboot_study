FROM openjdk:8-jre-alpine3.9
ARG packages="socat"
ARG app_jar="example.basic-unixsocket-jar-with-dependencies.jar"
ARG socket_dir="/tmp/socket"
ENV socket_dir_env=$socket_dir 
RUN mkdir -p ${socket_dir}
RUN apk add --update --no-cache $packages; rm -rf /var/cache/apk/*; apk add
ADD "target/${app_jar}" app.jar
# TODO: manifest
# TODO: pass ${socket_dir} through properties
ENTRYPOINT ["java", "-Dsocket_dir=${socket_dir_env}", "-cp", "app.jar", "example.BasicSocketServer"]
