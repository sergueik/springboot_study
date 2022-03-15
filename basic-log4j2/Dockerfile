FROM openjdk:8-jre-alpine3.9
ARG WORKDIR="/work"
ENV UID=1000
# Note: will update UID on the fly with $(id -u) of the host user before building
ENV EDITOR=nano
ARG app_jar="example.log4j2.jar"
ARG USER_NAME=myuser
ARG GROUP_NAME=myuser
RUN echo "@edge http://nl.alpinelinux.org/alpine/edge/main" >> /etc/apk/repositories
RUN apk update && apk add --no-cache nano
RUN addgroup -S $GROUP_NAME && adduser -h /home/$USER_NAME -u $UID -s /bin/sh -D -G $GROUP_NAME $USER_NAME
RUN mkdir -p $WORKDIR/logs && chown -R $USER_NAME:$GROUP_NAME $WORKDIR/logs
USER $USER_NAME
ADD "target/${app_jar}" $WORKDIR/app.jar
ADD "src/main/resources/log4j2.properties" $WORKDIR
EXPOSE 8080
WORKDIR $WORKDIR
ENTRYPOINT ["java", "-jar", "app.jar"]
