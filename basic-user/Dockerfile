FROM alpine:3.9.5
# NOTE: isses observed underDocker Toolbox on Windows host
# Note: this Dockerfile is updated on the fly with $(id -u) of the host user
ARG UID=1000
ARG GID=1000
ARG PASSWORD=TeStP_w0rD

ENV PASSWORD=${PASSWORD}
ENV UID=${UID}
ENV GID=${GID}
ENV EDITOR=nano

ARG USER_NAME=docker_user
ARG GROUP_NAME=docker_user
RUN echo "@edge http://nl.alpinelinux.org/alpine/edge/main" >> /etc/apk/repositories && \
    apk --allow-untrusted update && apk add --allow-untrusted --no-cache $EDITOR
RUN getent group $GROUP_NAME >/dev/null || addgroup -S $GROUP_NAME
# solving
RUN getent passwd $UID || adduser -h /home/$USER_NAME -u $UID -s /bin/sh -D -G $GROUP_NAME $USER_NAME 
RUN mkdir -p /home/$USER_NAME && chown -R $USER_NAME:$GROUP_NAME /home/$USER_NAME
USER root
RUN echo -e "$PASSWORD\n$PASSWORD" | passwd $USER_NAME
USER $USER_NAME
RUN echo "Run following commands as $(whoami)"
# NOTE: There is no real need to specify VOLUME through Dockerfile, except for documenting the project
WORKDIR /home/$USER_NAME

ADD test.txt ./
RUN touch /home/$USER_NAME/test2.txt
VOLUME /home/$USER_NAME
ENTRYPOINT [ "sh","-c","$EDITOR" ]
