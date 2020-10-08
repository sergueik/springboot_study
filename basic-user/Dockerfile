FROM alpine:3.9.5
ENV UID=1000
# Note: updated on the fly with $(id -u) of the host user
ENV EDITOR=nano
ARG USER_NAME=docker_user
ARG GROUP_NAME=docker_user
RUN echo "@edge http://nl.alpinelinux.org/alpine/edge/main" >> /etc/apk/repositories && \
    apk update && apk add --no-cache $EDITOR && \
    addgroup -S $GROUP_NAME && adduser -h /home/$USER_NAME -u $UID -s /bin/sh -D -G $GROUP_NAME $USER_NAME && \
  mkdir -p /home/$USER_NAME && chown -R $USER_NAME:$GROUP_NAME /home/$USER_NAME
USER $USER_NAME
# NORE: No need to specify volume through Dockerfile
# VOLUME /home/$USER_NAME
WORKDIR /home/$USER_NAME
ENTRYPOINT [ "sh","-c","$EDITOR" ]
