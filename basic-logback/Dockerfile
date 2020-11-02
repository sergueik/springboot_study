FROM openjdk:8-jre-alpine3.9
# Note: will update UID on the fly with $(id -u) of the host user before building
ARG APP_JAR="example.logback.jar"
ARG GROUP_NAME=myuser
ENV UID=1000
ARG WORKDIR="/work"
ARG USER_NAME=myuser
RUN echo "@edge http://nl.alpinelinux.org/alpine/edge/main" >> /etc/apk/repositories && \
    apk update && apk add --no-cache nano && \
    addgroup -S $GROUP_NAME && adduser -h /home/$USER_NAME -u $UID -s /bin/sh -D -G $GROUP_NAME $USER_NAME && \
  mkdir -p $WORKDIR/logs && chown -R $USER_NAME:$GROUP_NAME $WORKDIR/logs
USER $USER_NAME
ADD "target/${APP_JAR}" $WORKDIR/app.jar

WORKDIR $WORKDIR
EXPOSE 8080
# NOTE: cannot use ARG here: not interpolated:  
# uncommenting the following line
# ENTRYPOINT java -jar "/${WOKRDIR}/app.jar"
# will lead to the error when running the container
# unable to access jarfile /app.jar
ENTRYPOINT ["java", "-jar", "/work/app.jar"]
